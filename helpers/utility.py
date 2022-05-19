#!/usr/bin/python
# -*- coding: iso-8859-15 -*-
# @author Daniel Be�ler

import os
import string
import requests

from urlparse import urlparse
from flask import session
from config.settings import WEBROB_PATH
from flask_user import current_user
from flask_user import current_app
from functools import wraps
from threading import Lock, Thread
from pathlib2 import Path
from concurrent.futures import ThreadPoolExecutor

from app_and_db import app
from helpers.file_handler import move_file, write_binary_file
from Crypto.Random import random

THREAD_POOL_EXECUTOR = ThreadPoolExecutor(max_workers=3)

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


def download_file(url, file_path):
    try:
        r = requests.get(url, allow_redirects=True)
    except Exception as e:
        app.logger.error('Sending the request to download file had some issues.\n\n' + e.__str__())
        return
    
    if r.status_code == 200:
        temp_downloads_dir = WEBROB_PATH + 'temp_downloads/'
        Path(temp_downloads_dir).mkdir(parents=True, exist_ok=True)
        temp_file = temp_downloads_dir + Path(file_path).name
        write_binary_file(r.content, temp_file)

        Path(file_path).parent.mkdir(parents=True, exist_ok=True)
        move_file(temp_file, file_path)

        try:
            if not any(Path(temp_downloads_dir).iterdir()):
                Path(temp_downloads_dir).rmdir()
        except Exception as e:
            app.logger.info('Could not remove temp dir.\n\n' + e.__str__())


def start_thread(target_func):
    THREAD_POOL_EXECUTOR.submit(target_func)


def mutex_lock(mutex=None):
    """ decorator function that provides a mutex-lock
    
    If no mutex lock is assigned, a per-function mutex is used.
    
    The lock is thread-blocking, so in order to not block
    the current thread create a new one to run the desired
    function. You can do that, for example, with "start_thread" """
    
    def wrap_inner(func):
        p_mutex = _determine_mutex(mutex, func)

        @wraps(func)
        def inner(*args, **kwargs):
            p_mutex.acquire()
            try:
                func(*args, **kwargs)
            finally:
                p_mutex.release()

        return inner
    return wrap_inner


def _determine_mutex(mutex, func):
    if mutex is None:
        func.lock = Lock()
        return func.lock
    else:
        return mutex


def type_str(obj):
    return str(type(obj))


def is_url(url):
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False
