#!/usr/bin/python
# -*- coding: iso-8859-15 -*-
# @author Daniel Beßler

from functools import wraps

from flask import session
from flask_user import current_app
from flask_user import current_user

from webrob.app_and_db import app
from webrob.utility.directory_handler import make_dirs
from webrob.utility.path_handler import path_exists


# TODO: Move utilities to own modules
def get_user_dir():
    user_dir = "/home/ros/user_data/" + session['user_container_name']
    if not path_exists(user_dir):
        app.logger.info("Creating user directory at " + user_dir)
        make_dirs(user_dir)
    return user_dir


def admin_required(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        if not current_user.is_authenticated or not current_user.has_role('admin'):
            return current_app.login_manager.unauthorized()
        return f(*args, **kwargs)

    return decorated_function
