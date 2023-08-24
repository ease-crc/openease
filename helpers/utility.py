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
from html_sanitizer import Sanitizer
from html_sanitizer.sanitizer import sanitize_href, bold_span_to_strong, italic_span_to_em, target_blank_noopener, tag_replacer

from app_and_db import app
from helpers.file_handler import dir_has_any_items, get_path_name, get_path_parent, make_dir, move_file, remove_empty_dir, write_binary_file
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
        temp_downloads_dir = WEBROB_PATH + 'temp_downloads/'
        make_dir(temp_downloads_dir, make_parents=True, path_exist_ok=True)
        temp_file = temp_downloads_dir + get_path_name(file_path)
        write_binary_file(r.content, temp_file)

        make_dir(get_path_parent(file_path), make_parents=True, path_exist_ok=True)
        move_file(temp_file, file_path, overwrite=True)

        try:
            if not dir_has_any_items(temp_downloads_dir):
                remove_empty_dir(temp_downloads_dir)
        except Exception as e:
            app.logger.info('Could not remove temp dir.\n\n' + e.__str__())


def type_str(obj):
    return str(type(obj))


def is_url(url):
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False


def sanitize_html(html_str):
    # need to sanitize the input, because the template loads the values
    # as 'safe', which could otherwise allow XSS-exploits
    sanitizer = _get_html_sanitizer()
    return sanitizer.sanitize(html_str)


def _get_html_sanitizer():
    """ When tags or items from the markdown are not displayed correctly,
    it might hint to the sanitizer removing unallowed tags. To allow 
    these tags to pass, adjust the sanitizer-config from get_sanitizer()
    in # pages/overview.py. Afterwards adjust the styling in 
    static/css/overview.scss.
    
    When in doubt, refer to
      https://github.com/trentm/python-markdown2
    and
      https://github.com/matthiask/html-sanitizer """
    
    return Sanitizer({
        'tags': {
            'a', 'h1', 'h2', 'h3', 'strong', 'em', 'p', 'ul', 'ol',
            'li', 'br', 'sub', 'sup', 'hr', 'img', 'blockquote',
            'table', 'thead', 'tbody', 'tr', 'th', 'td', 'figure',
            'figcaption'
        },
        'attributes': {
            'a': ('href', 'name', 'target', 'title', 'id', 'rel'),
            'img': ('src', 'alt', 'width', 'height'),
        },
        'empty': {'hr', 'a', 'br', 'img', 'tr', 'th', 'td'},
        'separate': {
            'a', 'p', 'li', 'img', 'table', 'tr', 'th', 'td', 'blockquote',
            'figure'
        },
        'whitespace': {'br'},
        'keep_typographic_whitespace': False,
        'add_nofollow': False,
        'autolink': False,
        'sanitize_href': sanitize_href,
        'element_preprocessors': [
            # convert span elements into em/strong if a matching style rule
            # has been found. strong has precedence, strong & em at the same
            # time is not supported
            bold_span_to_strong,
            italic_span_to_em,
            tag_replacer('b', 'strong'),
            tag_replacer('i', 'em'),
            tag_replacer('form', 'p'),
            target_blank_noopener,
        ],
        'element_postprocessors': [],
        'is_mergeable': lambda e1, e2: True
    })
