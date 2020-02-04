from flask import session, request, redirect, url_for, render_template
from flask.ext.user.signals import user_logged_in
from flask.ext.user.signals import user_logged_out
from flask_user import current_user, login_required

from urlparse import urlparse
import traceback
import os

from random import choice
from string import lowercase

from webrob.app_and_db import app
from webrob.docker import docker_interface

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

    #error = ""
    # determine hostname/IP we are currently using
    # (needed for accessing container)
    #host_url = urlparse(request.host_url).hostname
    #container_name = session['user_container_name']
    #role_names = _get_user_roles()
    
    #return render_template('main.html', **locals())
    return redirect(url_for('render_QA_page'))

#def _get_user_roles():
#    role_names = []
#    if hasattr(current_user, 'roles'):
#        role_names = map(lambda x: str(x.name), current_user.roles)
#    return role_names

# TODO: look how to re-enable this (connect to host ROS)
#@app.route('/remote')
#def openease_remote():
    #if not current_user.is_authenticated:
        #return redirect(url_for('user.login'))
    #if 'user_container_name' not in session:
        #return redirect(url_for('user.logout'))

    #error = ""
    ## determine hostname/IP we are currently using
    ## (needed for accessing container)
    #host_url = urlparse(request.host_url).hostname
    #container_name = 'remote/172.17.42.1'
    #role_names = _get_user_roles()

    #category = _get_exp_category()
    #exp = _get_exp_name()

    ## TODO: show_user_data never shown, could be re-enabled as iframe
    #return render_template('main.html', **locals())
