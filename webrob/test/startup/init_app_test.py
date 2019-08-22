from webrob.startup.init_app import _check_password_and_display_message_on_error, _password_criteria_fulfilled, _has_six_or_more_chars, \
    _contains_number, _contains_lowercase_letter, _contains_uppercase_letter, _create_new_user_and_add_to_db,_append_roles_to_user_object

from webrob.app_and_db import app, db
from webrob.startup import  init_app
# from flask_user import UserManager
import pytest
import datetime

class UserManagerMock:
    def __init__(self):
        pass

    def hash_password(self, str):
        return str

def test_get_user():
    from webrob.models.users import User
    user_manager = UserManagerMock()
    test_user = User(active=True,
                username='Paul',
                displayname='',
                remoteapp='',
                email='paul@example.com',
                confirmed_at=datetime.datetime.utcnow(),
                password=user_manager.hash_password('AAaa11'))
    _append_roles_to_user_object(app, test_user, roles=[])

    print 'test   = {}'.format(test_user)
    return test_user

def test_create_and_add_new_user():
    # from webrob.startup.init_app import init_app
    # init_app(app, db)
    user_manager = UserManagerMock()
    _create_new_user_and_add_to_db(app, db, user_manager, name = 'Paul', mail = 'paul@example.com', pw = 'AAaa11', display_name = '',
                                   remote_app = '', roles=[])
