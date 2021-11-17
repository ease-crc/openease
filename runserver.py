#!/usr/bin/env python

# This file starts the WSGI web application.
# - Heroku starts gunicorn, which loads Procfile, which starts runserver.py
# - Developers can run it from the command line: python runserver.py

import os

import atexit
from apscheduler.schedulers.background import BackgroundScheduler
from config.settings import DOWNLOADS_DIR_PATH

from tornado.httpserver import HTTPServer
from tornado.ioloop import IOLoop
from tornado.wsgi import WSGIContainer

from flask_mail import Mail
from flask_user import UserManager, SQLAlchemyAdapter
from flask.ext.babel import Babel
from wtforms.validators import ValidationError
from pathlib2 import Path

from app_and_db import app, db
from utility import random_string, oe_password_validator
from pages.publications import load_default_publications_and_papers, download_and_update_papers_and_bibtex
from postgres.users import Role, User, add_user, create_role
from pages.overview import download_neem_files, load_default_overview_files

# default password for admin user
ADMIN_USER_DEFAULT_PW = '1234'

USER_ROLES = [
    'admin',
    'reviewer',
    'user',
    'editor'
]

OVERVIEW_SCHEDULER_JOB_ID = 'overview'
PUBLICATIONS_SCHEDULER_JOB_ID = 'publications'

BACKGROUND_SCHEDULER = BackgroundScheduler()

def init_app(extra_config_settings={}):
    # Initialize app config settings
    app.config.from_object('config.settings')  # Read config from 'app/settings.py' file
    app.config.update(extra_config_settings)  # Overwrite with 'extra_config_settings' parameter
    if app.testing:
        app.config['WTF_CSRF_ENABLED'] = False  # Disable CSRF checks while testing
    if os.environ['EASE_DEBUG'] == 'true':
        app.config['DEBUG'] = True
        app.config['SECRET_KEY'] = app.config['DEV_SECRET_KEY']
    else:
        try:
            app.config['SECRET_KEY'] = open('/etc/ease_secret/secret', 'rb').read()
        except IOError:
            app.config['SECRET_KEY'] = random_string(64)
    
    if os.environ['DOWNLOAD_DEFAULT_PAPERS'] == 'true':
        app.config['DOWNLOAD_DEFAULT_PAPERS'] = True
    else:
        app.config['DOWNLOAD_DEFAULT_PAPERS'] = False

    if os.environ['PREPARE_DOWNLOADABLE_FILES'] == 'true':
        app.config['PREPARE_DOWNLOADABLE_FILES'] = True
    else:
        app.config['PREPARE_DOWNLOADABLE_FILES'] = False
    
    # Setup Flask-Mail
    mail = Mail(app)
    babel = Babel(app)

    # Setup Flask-User to handle user account related forms
    from postgres.users import User
    db_adapter = SQLAlchemyAdapter(db, User)
    # Init Flask-User and bind to app
    app.user_manager = UserManager(db_adapter, app,
                                   password_validator=oe_password_validator)

    # Load all models.py files to register db.Models with SQLAlchemy
    from postgres import users
    from postgres import settings
    # Automatically create all registered DB tables
    db.create_all()
    db.session.commit()

    for role in USER_ROLES:
        create_role(role)

    # Load all views.py files to register @app.routes() with Flask
    from pages import main
    from pages import api
    from pages import neem_discovery
    from pages import editor
    from pages import tutorials
    from pages import oauth
    from pages import content

    add_user(user_manager=app.user_manager,
             name='admin',
             mail=os.environ.get('OPENEASE_MAIL_USERNAME', 'admin@openease.org'),
             pw=ADMIN_USER_DEFAULT_PW,
             roles=['admin'])

    # create downloads-folder
    if not Path(DOWNLOADS_DIR_PATH).is_dir():
        Path(DOWNLOADS_DIR_PATH).mkdir(parents=True, exist_ok=True)

    # Start background scheduler to load markdowns for overview pages
    if _config_is_debug():
        # load defaults, instead of fetching updates
        # saves a lot of time on start-up
        load_default_overview_files()
        load_default_publications_and_papers()
    else:
        # initial download of files
        download_neem_files()
        download_and_update_papers_and_bibtex()
        # start background jobs for periodic fetching
        _start_background_scheduler()
    
    app.logger.info("Webapp started.")
    return app


def _config_is_debug():
    # if environment variable 'EASE_DEBUG' is set to true, then
    # 'DEBUG' in app.config will be set to true by init_app.py
    return 'DEBUG' in app.config and app.config['DEBUG']


def _start_background_scheduler():
    BACKGROUND_SCHEDULER.add_job(func=download_neem_files, trigger="interval", hours=3, coalesce=True, id=OVERVIEW_SCHEDULER_JOB_ID)
    BACKGROUND_SCHEDULER.add_job(func=download_and_update_papers_and_bibtex, trigger="interval", days=1, coalesce=True, id=PUBLICATIONS_SCHEDULER_JOB_ID)
    BACKGROUND_SCHEDULER.start()

    # Shut down the scheduler when exiting the app
    atexit.register(lambda: BACKGROUND_SCHEDULER.shutdown())


def _run_server():
    http_server = HTTPServer(WSGIContainer(app))
    http_server.listen(5000)
    IOLoop.instance().start()


if __name__ == '__main__':
    init_app()
    _run_server()
