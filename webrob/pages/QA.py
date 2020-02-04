import os
import json

from flask import session, request, redirect, render_template, url_for
from flask_user import current_user
from urlparse import urlparse

from webrob.app_and_db import app
from webrob.pages.neems import neem_manager

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

# TODO: I think docker-io needs to be used
#@app.route('/knowrob/add_history_item', methods=['POST'])
#def add_history_item():
    #query = json.loads(request.data)['query']
    #hfile = get_history_file()
    ## Remove newline characters
    ## query.replace("\n", " ")

    ## Read history
    #lines = []
    #if os.path.isfile(hfile):
        #f = open(hfile)
        #lines = f.readlines()
        #f.close()
    ## Remove old history items
    #history = ''.join(lines).split("\n\n")
    #history = map(lambda x: x + '\n\n', history)
    ## Append the last query
    #history.append(query + ".")

    #numLines = len(history)
    #history = history[max(0, numLines - MAX_HISTORY_LINES):numLines]

    #with open(hfile, "w") as f:
        #f.writelines(history)

    #return jsonify(result=None)


#@app.route('/knowrob/get_history_item', methods=['POST'])
#def get_history_item():
    #index = json.loads(request.data)['index']

    #if index < 0:
        #return jsonify(item="", index=-1)

    #hfile = get_history_file()
    #if os.path.isfile(hfile):
        ## Read file content
        #f = open(hfile)
        #lines = f.readlines()
        #f.close()

        ## Clamp index
        #if index < 0:
            #index = 0
        #if index >= len(lines):
            #index = len(lines) - 1
        #if index < 0:
            #return jsonify(item="", index=-1)

        ## History items are separated with empty line (\n\n)
        #history = ''.join(lines).split("\n\n")

        #item = history[len(history) - index - 1]
        #if len(item) > 0 and item[len(item) - 1] == '\n':
            #item = item[:len(item) - 1]

        #return jsonify(item=item, index=index)

    #else:
        #return jsonify(item="", index=-1)

#def get_history_file():
    #userDir = get_user_dir()
    #return os.path.join(get_user_dir(), "query.history")
