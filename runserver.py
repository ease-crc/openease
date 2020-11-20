#!/usr/bin/env python

# This file starts the WSGI web application.
# - Heroku starts gunicorn, which loads Procfile, which starts runserver.py
# - Developers can run it from the command line: python runserver.py

import os

from tornado.httpserver import HTTPServer
from tornado.ioloop import IOLoop
from tornado.wsgi import WSGIContainer

from flask_mail import Mail
from flask_user import UserManager, SQLAlchemyAdapter
from flask.ext.babel import Babel

from app_and_db import app, db
from utility import random_string
from postgres.users import Role, User, add_user

# default password for admin user
ADMIN_USER_DEFAULT_PW = '1234'


def _config_is_debug():
    # if environment variable 'EASE_DEBUG' is set to true, then
    # 'DEBUG' in app.config will be set to true by init_app.py
    return 'DEBUG' in app.config and app.config['DEBUG']


def _run_debug_server():
    # print 'Run web server in DEBUG mode'
    # app.run(host='0.0.0.0', debug=True)
    print 'DEBUG mode currently does not work'
    print 'Start normal web server'
    _run_server()


def _run_server():
    http_server = HTTPServer(WSGIContainer(app))
    http_server.listen(5000)
    print 'Web server is running. Listening on {}'.format(5000)
    IOLoop.instance().start()


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
    app.user_manager = UserManager(db_adapter, app)  # Init Flask-User and bind to app

    # Load all models.py files to register db.Models with SQLAlchemy
    from postgres import users
    from postgres import settings
    # Automatically create all registered DB tables
    db.create_all()
    db.session.commit()

    # Load all views.py files to register @app.routes() with Flask
    from pages import main
    from pages import api
    from pages import neem_discovery
    from pages import editor
    from pages import tutorials
    from pages import oauth

    add_user(user_manager=app.user_manager,
             name='admin',
             mail=os.environ.get('OPENEASE_MAIL_USERNAME', 'admin@openease.org'),
             pw=ADMIN_USER_DEFAULT_PW,
             roles=['admin'])

    app.logger.info("Webapp started.")
    return app


init_app()

# Start a development web server if executed from the command line
if __name__ == '__main__':
    if _config_is_debug():
        _run_debug_server()
    else:
        _run_server()
