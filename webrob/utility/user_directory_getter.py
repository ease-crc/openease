from flask import session

from webrob.app_and_db import app
from webrob.utility.directory_handler import create_directories_from_path
from webrob.utility.path_handler import path_exists


def get_user_dir():
    user_dir = "/home/ros/user_data/" + session['user_container_name']
    if not path_exists(user_dir):
        app.logger.info("Creating user directory at " + user_dir)
        create_directories_from_path(user_dir)
    return user_dir
