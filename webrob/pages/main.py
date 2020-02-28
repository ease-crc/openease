from flask import session, request, redirect, url_for, render_template, send_from_directory
from flask.ext.user.signals import user_logged_in
from flask.ext.user.signals import user_logged_out
from flask_user import current_user, login_required

import traceback
import os

from webrob.app_and_db import app
from webrob.app_and_db import db
from webrob.docker import docker_interface
from flask_wtf import Form
from wtforms import StringField, PasswordField
from wtforms.validators import DataRequired, Length, EqualTo, Email


class PasswordForm(Form):
    password = PasswordField('Password', validators=[DataRequired()])


@user_logged_in.connect_via(app)
def track_login(sender, user, **extra):
    app.logger.info("Logged in " + str(user.username))
    # TODO: why not just use username key?
    session['user_container_name'] = user.username
    session['username'] = user.username
    session['api_token'] = user.api_token


@user_logged_out.connect_via(app)
def track_logout(sender, user, **extra):
    if 'user_container_name' in session:
        docker_interface.stop_user_container(session['username'])
        session.pop('user_container_name')


@app.errorhandler(Exception)
def redirect_unhandled_exception(e):
    app.logger.error('Unhandled Exception: %s', (e))
    app.logger.error(traceback.format_exc())
    # TODO: show an appology page to the user
    return redirect(url_for('user.login'))


# method to send woff font files from node_modules
@app.route('/user/<node_modules>/<typeface_oswald>/<files>/<path:file_name>')
@app.route('/<node_modules>/<typeface_oswald>/<files>/<path:file_name>')
def send_woff_file_from_neem_directory(node_modules, typeface_oswald, files, file_name):
    app.logger.info("Send_woff_file_from_neem_directory: " + file_name)
    file_path = "/tmp/npm/" + node_modules + "/" + typeface_oswald + "/" + files + "/"
    app.logger.info("path file_path : " + file_path)
    with open(file_path + file_name, 'r') as f:
        file_content = f.read()
    return file_content


@app.route('/userdata')
@login_required
def render_user_data():
    return render_template('flask_user/user_data.html', **locals())


@app.route('/userdata/<path:filename>')
def download_user_data(filename):
    # TODO: does it work without KnowRob being started?
    return docker_interface.file_read(session['user_container_name'], filename)


@app.route('/static/<path:filename>')
def send_from_static_directory(filename):
    return send_from_directory(os.path.join(app.root_path, "static"), filename)


@app.route('/')
def render_main():
    if not current_user.is_authenticated:
        return redirect(url_for('user.login'))
    if 'user_container_name' not in session:
        return redirect(url_for('user.logout'))

    # error = ""
    # determine hostname/IP we are currently using
    # (needed for accessing container)
    # host_url = urlparse(request.host_url).hostname
    # container_name = session['user_container_name']
    # role_names = _get_user_roles()

    # return render_template('main.html', **locals())
    return redirect(url_for('render_QA_page'))


# get call handling method for changing password
@app.route('/change_password_get')
@login_required
def render_change_password_get():
    form = PasswordForm()
    return render_template('flask_user/change_password.html', title='Change Password', form=form)


# post call handling method for changing password
@app.route('/change_password_post', methods=["POST"])
@login_required
def render_change_password_post():
    form = PasswordForm()
    if "Update" in request.form:
        user = current_user
        user.password = app.user_manager.hash_password(form.password.data)
        db.session.add(user)
        db.session.commit()
        app.logger.info('  Password has been updated!')
        return redirect(url_for('render_user_data'))
    elif "Cancel" in request.form:
        app.logger.info('  Cancelling the request!')
        return redirect(url_for('render_user_data'))

# def _get_user_roles():
#    role_names = []
#    if hasattr(current_user, 'roles'):
#        role_names = map(lambda x: str(x.name), current_user.roles)
#    return role_names

# TODO: look how to re-enable this (connect to host ROS)
# @app.route('/remote')
# def openease_remote():
# if not current_user.is_authenticated:
# return redirect(url_for('user.login'))
# if 'user_container_name' not in session:
# return redirect(url_for('user.logout'))

# error = ""
## determine hostname/IP we are currently using
## (needed for accessing container)
# host_url = urlparse(request.host_url).hostname
# container_name = 'remote/172.17.42.1'
# role_names = _get_user_roles()

# category = _get_exp_category()
# exp = _get_exp_name()

## TODO: show_user_data never shown, could be re-enabled as iframe
# return render_template('main.html', **locals())
