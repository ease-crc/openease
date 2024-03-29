#!/usr/bin/env python

# This file starts the WSGI web application.
# - Heroku starts gunicorn, which loads Procfile, which starts runserver.py
# - Developers can run it from the command line: python runserver.py

import os
from pages.neem_overview import NEEM_OVERVIEW_IMAGES_STATIC_DIR_PATH, NEEM_OVERVIEW_MARKDOWNS_PATH
from pages.publications import ALL_PUBLICATIONS_PATH
from postgres.db import get_table_row_count, table_empty

from tornado.httpserver import HTTPServer
from tornado.ioloop import IOLoop
from tornado.wsgi import WSGIContainer

from flask_mail import Mail
from flask_user import UserManager, SQLAlchemyAdapter
from flask.ext.babel import Babel
from wtforms.validators import ValidationError

from app_and_db import app, db
from config.settings import DOWNLOADS_DIR_PATH
from helpers.utility import random_string, oe_password_validator
from helpers.file_handler import make_dir, number_of_files_in_dir, number_of_subdirs_in_dir, path_is_dir, path_is_file
from postgres.users import Role, User, add_user, create_role
from postgres.settings import ContentSettings, ContentState, UpdateState
from postgres.content import NeemOverviewData, PublicationsData, PublicationsKeywords

# default password for admin user
ADMIN_USER_DEFAULT_PW = '1234'

USER_ROLES = [
    'admin',
    'reviewer',
    'user',
    'editor'
]

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
    from postgres import content
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
    from pages import neem_overview
    from pages import publications
    from pages import content
    from pages import news

    add_user(user_manager=app.user_manager,
             name='admin',
             mail=os.environ.get('OPENEASE_MAIL_USERNAME', 'admin@openease.org'),
             pw=ADMIN_USER_DEFAULT_PW,
             roles=['admin'])
    
    # set content-settings accordingly
    # if the app is restarted after being run in production-mode
    # all the settings will be set to true (from the previous run);
    # they can be set to false in the content settings

    ContentSettings.init_last_update_settings(
        _neem_overview_data_and_files_exist(),
        _publications_data_and_files_exist()
    )

    if not _config_is_debug():
        ContentSettings.set_download_default_papers(True)
        ContentSettings.set_prepare_downloadable_files(True)
        ContentSettings.set_update_state_neem_overview(UpdateState.ACTIVE)
        ContentSettings.set_update_state_publications_and_papers(UpdateState.ACTIVE)

    _create_downloads_folder()

    # the following imports need to be declared here
    # If declared at the beginning of the file and the neemhub
    # settings are not yet configured (for example on a fresh
    # install), then the container will just error out. That is
    # because the neem_overview module will try to access the
    # neemhub instance which depends on the db tables to have
    # already been created. This is not the case at the time the
    # imports are loaded, hence the error.
    from helpers.background_scheduler import start_background_scheduler
    from pages.publications import automatic_update_publications_and_papers, load_default_publications_and_papers
    from pages.neem_overview import automatic_update_neem_overview_files, load_default_overview_files

    # start background jobs for periodic fetching
    # this needs to executed even if the config is debug because
    # the admin can still change settings for the jobs later
    start_background_scheduler()

    if _config_is_debug():
        # if content files are present in the volume and data in the db, 
        # no default files are loaded to save start up time
        #
        # otherwise load defaults if update-state is set to paused,
        # instead of fetching updates; saves a lot of time on start-up

        if _neem_overview_data_and_files_exist():
            app.logger.info('Found neem-overview data in db and files in volume thus not (re)loading default files. \n In case of problems, reload content files via the admin content panel.')
        elif _update_state_neem_overview_job_is_active():
            automatic_update_neem_overview_files()
        else:
            load_default_overview_files()
        
        if _publications_data_and_files_exist():
            app.logger.info('Found publications data in db and/or files in volume, thus not (re)loading default files. \n In case of problems, reload content files via the admin content panel.')
        elif _update_state_publications_and_papers_job_is_active():
            automatic_update_publications_and_papers()
        else:
            load_default_publications_and_papers()
    else:
        # initial download of files in production mode
        automatic_update_neem_overview_files()
        automatic_update_publications_and_papers()
    
    app.logger.info("Webapp started.")
    return app


def _neem_overview_data_and_files_exist():    
    if table_empty(NeemOverviewData) or \
        not path_is_dir(NEEM_OVERVIEW_MARKDOWNS_PATH) or \
        not path_is_dir(NEEM_OVERVIEW_IMAGES_STATIC_DIR_PATH):
        return False
    if get_table_row_count(NeemOverviewData) > \
        number_of_files_in_dir(NEEM_OVERVIEW_MARKDOWNS_PATH) or \
        get_table_row_count(NeemOverviewData) > \
        number_of_subdirs_in_dir(NEEM_OVERVIEW_IMAGES_STATIC_DIR_PATH):
        return False
    return True


def _publications_data_and_files_exist():
    if table_empty(PublicationsData) or table_empty(PublicationsKeywords):
        return False
    if ContentSettings.get_settings().content_type_publications == ContentState.LATEST and \
        not path_is_file(ALL_PUBLICATIONS_PATH):
        return False
    return True


def _update_state_neem_overview_job_is_active():
    return ContentSettings.get_settings().update_state_neem_overview == UpdateState.ACTIVE


def _update_state_publications_and_papers_job_is_active():
    return ContentSettings.get_settings().update_state_publications_and_papers == UpdateState.ACTIVE


def _create_downloads_folder():
    if not path_is_dir(DOWNLOADS_DIR_PATH):
        make_dir(DOWNLOADS_DIR_PATH, make_parents=True, path_exist_ok=True)


def _config_is_debug():
    # if environment variable 'EASE_DEBUG' is set to true, then
    # 'DEBUG' in app.config will be set to true by init_app.py
    return 'DEBUG' in app.config and app.config['DEBUG']


def _run_server():
    http_server = HTTPServer(WSGIContainer(app))
    http_server.listen(5000)
    IOLoop.instance().start()


if __name__ == '__main__':
    init_app()
    _run_server()
