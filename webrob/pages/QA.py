import os
import json

from flask import session, request, redirect, render_template, url_for, jsonify
from flask_user import current_user
from urlparse import urlparse

from webrob.app_and_db import app
from webrob.pages.neems import neem_manager
from webrob.docker import docker_interface

from webrob.config.settings import MAX_HISTORY_LINES, USE_HOST_KNOWROB

__author__ = 'danielb@uni-bremen.de'

@app.route('/QA')
def render_QA_page():
    neem = neem_manager.get_requested(request)
    if neem is None:
        return redirect(url_for('render_neems'))
    neem.activate()
    # determine hostname/IP we are currently using
    # (needed for accessing container)
    host_url = urlparse(request.host_url).hostname
    if USE_HOST_KNOWROB:
        container_name = "host"
        authentication = False
    else:
        container_name = current_user.username + "_knowrob"
    return render_template('pages/QA.html', **locals())

@app.route('/video')
def render_video_page():
    neem = neem_manager.get_requested(request)
    if neem is None:
        return redirect(url_for('render_neems'))
    neem.activate()
    # determine hostname/IP we are currently using
    # (needed for accessing container)
    host_url = urlparse(request.host_url).hostname
    if USE_HOST_KNOWROB:
        container_name = "host"
        authentication = False
    else:
        container_name = current_user.username + "_knowrob"
    return render_template('pages/video.html', **locals())
