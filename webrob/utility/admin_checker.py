from functools import wraps

from flask import current_app
from flask_login import current_user


def admin_required(func):
    @wraps(func)
    def decorated_function(*args, **kwargs):
        if not current_user.is_authenticated or not current_user.has_role('admin'):
            return current_app.login_manager.unauthorized()
        return func(*args, **kwargs)

    return decorated_function
