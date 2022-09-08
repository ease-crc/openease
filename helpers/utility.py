#!/usr/bin/python
# -*- coding: iso-8859-15 -*-
# @author Daniel Be�ler

import os
import string

from urlparse import urlparse
from flask import session
from flask_user import current_user
from flask_user import current_app
from functools import wraps

from app_and_db import app
from Crypto.Random import random

def get_user_dir():
    userDir = "/home/ros/user_data/" + session['user_container_name']
    if not os.path.exists(userDir):
        app.logger.info("Creating user directory at " + userDir)
        os.makedirs(userDir)
    return userDir


def random_string(length):
    return "".join([random.choice(string.ascii_letters + string.digits) for n in xrange(length)])


def admin_required(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        if not current_user.is_authenticated or not current_user.has_role('admin'):
           return current_app.login_manager.unauthorized()
        return f(*args, **kwargs)
    return decorated_function


def oe_password_validator(form, field):
    password = field.data
    if len(password) < 3:
        raise ValidationError(('Password must have at least 3 characters'))


def type_str(obj):
    return str(type(obj))


def is_url(url):
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False
