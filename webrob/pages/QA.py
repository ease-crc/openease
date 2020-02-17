import os
import json

from flask import session, request, redirect, render_template, url_for, jsonify
from flask_user import current_user
from urlparse import urlparse

from webrob.app_and_db import app
from webrob.pages.neems import neem_manager
from webrob.docker import docker_interface

from webrob.config.settings import MAX_HISTORY_LINES

__author__ = 'danielb@uni-bremen.de'

@app.route('/QA')
def render_QA_page():
    neem = neem_manager.get_requested(request)
    if neem is None:
        return redirect(url_for('render_neems'))
    # TODO show progress to user, probably better do REST calls in QA.html
    neem.checkout()
    neem.activate()
    # determine hostname/IP we are currently using
    # (needed for accessing container)
    host_url = urlparse(request.host_url).hostname
    container_name = current_user.username + "_knowrob"
    return render_template('pages/QA.html', **locals())

@app.route('/QA/history/add', methods=['POST'])
def post_qa_history_add():
    query = json.loads(request.data)['query']
    # read history from user data container
    # FIXME: this is terrible slow, better directly interact with knowrob container
    history_data = docker_interface.file_read(current_user.username, 'query.history')
    if history_data != None:
        lines = history_data.split("\n\n")
    else:
        lines = []
    # add the query
    lines.append(query + ".")
    # truncate
    numLines = len(lines)
    lines = lines[max(0, numLines - MAX_HISTORY_LINES):numLines]
    new_data = "\n\n".join(lines)
    # write to user data container
    docker_interface.file_write(current_user.username, new_data, 'query.history')
    return jsonify(result=None)

@app.route('/QA/history/get', methods=['POST'])
def post_qa_history_get():
    index = json.loads(request.data)['index']
    if index < 0:
        return jsonify(item="", index=-1)
    
    # FIXME: this is terrible slow, better directly interact with knowrob container
    history_data = docker_interface.file_read(current_user.username, 'query.history')
    if history_data==None:
        return jsonify(item="", index=-1)
    lines = history_data.split("\n\n")
    # Clamp index
    if index < 0:
        index = 0
    if index >= len(lines):
        index = len(lines) - 1
    item = lines[len(lines) - index - 1]

    return jsonify(item=item, index=index)
