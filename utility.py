#!/usr/bin/python
# -*- coding: iso-8859-15 -*-
# @author Daniel Be�ler

import os
import shutil
import string
import requests
import json

from flask import session
from config.settings import WEBROB_PATH
from flask_user import current_user
from flask_user import current_app
from functools import wraps

from zipfile import ZipFile
from pathlib2 import Path

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


def download_file(url, file_path):
    try:
        r = requests.get(url, allow_redirects=True)
    except Exception as e:
        app.logger.error('Sending the request to download file had some issues.\n\n' + e.__str__())
        return
    
    if r.status_code == 200:
        parent_dirs = Path(file_path).parent
        Path(parent_dirs).mkdir(parents=True, exist_ok=True)
        file = Path(file_path)

        if file.is_file():
            file.unlink()
        
        write_binary_file(r.content, file_path)


def copy_file(src, dest):
    shutil.copy(src, dest)


def copy_dir(src, dest):
    shutil.copytree(src, dest)


def move_file(src, dest, overwrite=False):
    if overwrite and Path(dest).is_file():
        app.logger.info('Cannot move file, because file already exists at destination and overwrite-flag is set to "False".')
        return
    
    shutil.move(src, dest)


def remove_if_is_dir(path):
    if Path(path).is_dir():
        shutil.rmtree(path)


def remove_if_is_file(path):
    if Path(path).is_file():
        Path(path).unlink()


def unzip_file(src, dest):
    with ZipFile(src) as zip_obj:
        zip_obj.extractall(dest)


def dump_dict_to_json(data, dest):
    with open(dest, "w") as fp:
        json.dump(data, fp)


def get_dict_from_json(src):
    with open(src, 'r') as fp:
        data = json.load(fp)
    return data


def read_file(src):
    with open(src, 'r') as file:
        file_str = file.read()
    return file_str


def write_non_binary_file(data, dest):
    _write_file(data, dest, 'w')


def write_binary_file(data, dest):
    _write_file(data, dest, 'wb')


def _write_file(data, dest, mode):
    with open(dest, mode) as file:
        file.write(data)


def make_archive_of_files_and_dirs(sources, dest):
    # src has to be a list, even if it just has one item
    temp = WEBROB_PATH + 'temp'
    Path(temp).mkdir(parents=True)

    for item in sources:
        if Path(item).is_dir():
            shutil.copytree(item, temp + '/' + Path(item).stem)
        else:
            shutil.copy(item, temp)

    zip = '.zip'
    if zip in dest:
        p_dest = dest.replace(zip, '')
    
    shutil.make_archive(p_dest, 'zip', root_dir=temp)

    remove_if_is_dir(temp)
