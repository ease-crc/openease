from flask import render_template
from flask_user import login_required

from webrob.app_and_db import app

__author__ = 'danielb@cs.uni-bremen.de'

@app.route('/admin/cookie')
@login_required
def admin_cookie():
    return render_template('admin/cookie.html', **locals())
