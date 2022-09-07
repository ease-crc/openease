from flask import jsonify, request, session, render_template, redirect
from flask_login import current_user, login_user
from flask_user import login_required
import time
from urlparse import urlparse
from app_and_db import app, db
import knowrob.container as docker_interface
from knowrob.container import generate_mac
from postgres.users import User, add_user
from helpers.utility import random_string

__author__ = 'mhorst@cs.uni-bremen.de'


@app.route('/api/v1.0/auth_by_session', methods=['GET'])
def login_by_session():
    """
    Returns authentication information for the currently logged in user as required by the knowrob.js authentication
    request
    """
    ip = docker_interface.get_container_ip(session['user_container_name'])
    return _generate_rosauth(session['user_container_name'], ip, True)


def _generate_rosauth(user_container_name, dest, cache=False):
    """
    Generate the mac for use with rosauth and compile a json object with all necessary information to authenticate
    with the server.
    :param user_container_name: Name of the user container
    :param dest: IP of the destination
    :return: a json object for ros
    """
    client = request.remote_addr

    rand = random_string(30)

    t = int(time.time())
    level = "user"
    end = int(t + 3600)

    return jsonify({
        'mac': generate_mac(user_container_name, client, dest, rand, t, level, end, cache),
        'client': client,
        'dest': dest,
        'rand': rand,
        't': t,
        'level': level,
        'end': end
    })


@app.route('/api/v1.0/refresh_by_session', methods=['GET'])
def refresh_by_session():
    """
    Refreshes the running session for a currently logged in user. This prevents a users container from being terminated
    automatically.
    """
    docker_interface.refresh(session['user_container_name'])
    return jsonify({'result': 'success'})


@app.route('/api/v1.0/auth_by_token/<string:token>', methods=['GET'])
def login_by_token(token):
    """
    Returns authentication information for the user assigned to the given API token. This is needed to authenticate
    against the rosbridge by third party clients.
    """
    user = _user_by_token(token)
    if user is None:
        return jsonify({'error': 'wrong api token'})
    ip = docker_interface.get_container_ip(user.username)
    return _generate_rosauth(user.username, ip)


def _user_by_token(token):
    """
    Returns the user object for the given API token, or None if no matching user could be found.
    """
    return User.query.filter_by(api_token=token).first()


@app.route('/api/v1.0/start_container/<string:token>', methods=['GET'])
def start_container(token):
    """
    Starts the container of the user assigned to the given API token. The WebSocket url to the users rosbridge instance
    will be returned on success.
    """
    user = _user_by_token(token)
    if user is None:
        return jsonify({'error': 'wrong api token'})
    
    # FIXME: ensure container is started
    #docker_interface.start_user_container(_generate_user_image_name(), user.username, ROS_DISTRIBUTION)
    
    host_url = urlparse(request.host_url).hostname
    return jsonify({'result': 'success',
                   'url': '//' + host_url + '/ws/' + user.username + '_knowrob/'})


#def _generate_user_image_name():
    #"""
    #Returns the image name to be used for user containers
    #"""
    #return 'openease/' + ROS_DISTRIBUTION + '-knowrob-daemon'


@app.route('/api/v1.0/stop_container/<string:token>', methods=['GET'])
def stop_container(token):
    """
    Stops the container of the user assigned to the given API token.
    """
    user = _user_by_token(token)
    if user is None:
        return jsonify({'error': 'wrong api token'})
    
    #docker_interface.stop_container(user.username)
    
    return jsonify({'result': 'success'})


@app.route('/api/v1.0/refresh_by_token/<string:token>', methods=['GET'])
def refresh_by_token(token):
    """
    Refreshes the running session for the user assigned to the given API token. This prevents a users container from
    being terminated automatically.
    """
    user = _user_by_token(token)
    if user is None:
        return jsonify({'error': 'wrong api token'})
    docker_interface.refresh(user.username)
    return jsonify({'result': 'success'})


@app.route('/create_api_token', methods=['GET'])
@login_required
def create_api_token():
    _create_token()
    return render_template('flask_user/user_data.html', **locals())


def _create_token():
    current_user.api_token = random_string(64)
    db.session.commit()
    session['api_token'] = current_user.api_token

@app.login_manager.request_loader
def authenticate_user_by_token(request):
    api_token = request.args.get('token')
    if api_token:
        user = _user_by_token(api_token)
        if user:
            session['user_container_name'] = user.username
            session['username'] = user.username
            login_user(user)
            return user
    return None

def _create_user():
    username = "tmp_" + random_string(6)
    email = username+"@openease.org"
    password = random_string(10)
    session['user_container_name'] = username
    session['username'] = username
    flask_user = add_user(user_manager=app.user_manager,
                        name= username,
                        mail = email,
                        pw = password)
    return flask_user

@app.route('/ease2')
def create_temp_new_user():
    try:
        flask_user = _create_user()
        login_user(flask_user)
        app.logger.info("Logged in successfully " + flask_user.username)
        preferred_neem = request.args.get('neem')
        if preferred_neem:
            url_string = "/QA?neem_id="+str(preferred_neem)
            return redirect(session['next'] or url_string)
        return redirect(session['next'] or '/')
    except:
        app.logger.warn('Problem with creating a temp user. ')
        return redirect('/')
    return None
