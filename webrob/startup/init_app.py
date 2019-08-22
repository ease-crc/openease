#
# Based on https://github.com/lingthio/Flask-User-starter-app
# 
# Copyright 2014 SolidBuilds.com. All rights reserved
#
# Authors: Ling Thio <ling.thio@gmail.com>


import datetime

import webrob.utility.system_environment_variable_getter as evg

from flask_mail import Mail
from flask_user import UserManager, SQLAlchemyAdapter
from flask.ext.babel import Babel

from webrob.config.settings import Config
from webrob.utility.random_string_builder import random_string
from webrob.startup.init_db import *
from webrob.startup.init_webapp import *
from webrob.models.users import Role, User


def add_user(app, db, user_manager, name, mail, pw, display_name='', remote_app='', roles=[]):
    if not _check_password_and_display_message_on_error(app, name, pw):
        return

    user = _get_user_from_db(name)

    if not user:
        user = _create_new_user_and_add_to_db(app, db, user_manager, name, mail, pw, display_name, remote_app, roles)

    return user


def _check_password_and_display_message_on_error(app, name, pw):
    if pw is None:
        app.logger.warn("User %s has no password specified." % name)
    elif not _password_criteria_fulfilled(pw):
        app.logger.warn(
            "Password of user %s needs to have 6 or more characters, one lowercase, one uppercase letter, and a number." % name)
    else:
        return True
    return False


def _password_criteria_fulfilled(pw):
    if _has_six_or_more_chars(pw) and _contains_number(pw) and _contains_lowercase_letter(
            pw) and _contains_uppercase_letter(pw):
        return True
    else:
        return False


def _has_six_or_more_chars(str):
    return len(str) >= 6


def _contains_number(str):
    return any(char.isdigit() for char in str)


def _contains_lowercase_letter(str):
    return any(char.islower() for char in str)


def _contains_uppercase_letter(str):
    return any(char.isupper() for char in str)


def _get_user_from_db(name):
    return User.query.filter(User.username == name).first()


def _create_new_user_and_add_to_db(app, db, user_manager, name, mail, pw, display_name, remote_app, roles):
    user = User(active=True,
                username=name,
                displayname=display_name,
                remoteapp=remote_app,
                email=mail,
                confirmed_at=datetime.datetime.utcnow(),
                password=user_manager.hash_password(pw))

    user = _append_roles_to_user_object(app, user, roles)
    _add_user_to_db(app, db, user)

    return user


def _append_roles_to_user_object(app, user, roles):
    user_with_roles = user

    for r in roles:
        curr_role = _get_role_from_db(r)
        if curr_role is None:
            _log_role_query_failure(app, r)
        else:
            user_with_roles.roles.append(curr_role)

    return user_with_roles


def _get_role_from_db(role):
    return Role.query.filter(Role.name == role).first()


def _log_role_query_failure(app, role):
    app.logger.info("Unable to find role: " + str(role))


def _add_user_to_db(app, db, user):
    if got_db_connection(app, db):
        db.session.add(user)
        db.session.commit()

def init_app(app, db_instance, extra_config_settings={}):
    _init_app_config_settings(app, extra_config_settings)

    # Setup Flask-Mail
    mail = Mail(app)

    babel = Babel(app)

    # Setup Flask-User to handle user account related forms
    from webrob.models.users import User
    db_adapter = SQLAlchemyAdapter(db_instance, User)
    app.user_manager = UserManager(db_adapter, app)  # Init Flask-User and bind to app

    # Load all models.py files to register db.Models with SQLAlchemy
    # Needs to remain in code, even if IDEs might show it's unused, Flask will use them during runtime
    from webrob.models import users
    from webrob.models import tutorials
    from webrob.models import teaching
    from webrob.models import experiments

    # Load all views.py files to register @app.routes() with Flask
    # Needs to remain in code, even if IDEs might show it's unused, Flask will use them during runtime
    from webrob.pages import api
    from webrob.pages import db
    from webrob.pages import editor
    from webrob.pages import experiments
    from webrob.pages import knowrob
    from webrob.pages import login
    from webrob.pages import meshes
    from webrob.pages import mongo
    from webrob.pages import tutorials
    from webrob.pages import oauth

    init_db(app, db_instance)
    init_webapp(app, db_instance)

    add_user(app=app, db=db_instance, user_manager=app.user_manager,
             name='admin',
             mail=evg.get_variable_with_default('OPENEASE_MAIL_USERNAME', 'admin@openease.org'),
             pw=evg.get_required_variable('OPENEASE_ADMIN_PASSWORD'),
             roles=['admin'])

    _log_webapp_started(app)
    return app


def _init_app_config_settings(app, extra_config_settings):
    Config.init_vars()
    app.config.from_object('webrob.config.settings.Config')  # Read config from 'app/settings.py' file
    app.config.update(extra_config_settings)  # Overwrite with 'extra_config_settings' parameter
    if app.testing:
        app.config['WTF_CSRF_ENABLED'] = False  # Disable CSRF checks while testing
    if evg.get_variable_with_default('EASE_DEBUG', 'false') == 'true':
        app.config['DEBUG'] = True
        app.config['SECRET_KEY'] = app.config['DEV_SECRET_KEY']
    else:
        try:
            app.config['SECRET_KEY'] = open('/etc/ease_secret/secret', 'rb').read()
        except IOError:
            app.config['SECRET_KEY'] = random_string(64)


def _log_webapp_started(app):
    app.logger.info("Webapp started.")
