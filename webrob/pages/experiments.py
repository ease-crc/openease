from flask import session, send_from_directory, jsonify

import os
import json
import random
import string

from webrob.app_and_db import app

__author__ = 'danielb@cs.uni-bremen.de'

@app.route('/episode_set/<category>/<episode>')
def episode_set(category, episode):
    session['exp-category'] = category
    session['exp-name'] = episode
    return jsonify(result=None)

@app.route('/knowrob/exp_data/<category>/<exp>')
def episode_data(category, exp):
    create_queries_file(category, exp)
    return send_from_directory('/episodes/' + category + '/' + exp, 'queries.json')

def get_experiment_download_url():
    if 'exp-category' in session and 'exp-name' in session:
        episode_url = '/knowrob/exp_data/'
        if len(session['exp-category']) > 0:
            episode_url += session['exp-category'] + '/'
        episode_url += session['exp-name']
        return episode_url
    else:
        return None

def get_experiment_list():
    out = []
    root_path = "/episodes"

    for category in os.listdir(root_path):
        if '.' in category:
            continue
        p = os.path.join(root_path, category)
        if not os.path.isdir(p):
            continue

        for experiment in os.listdir(p):
            if experiment.startswith('.'):
                continue
            out.append((category, experiment))

    return out

def get_experiment_path(category, exp):
    return "/episodes/" + category + "/" + exp

def get_episode_owl_file(episode_path):
    if not os.path.isdir(episode_path):
        return None
    for owl_file in os.listdir(episode_path):
        if owl_file.endswith('.owl'):
            return os.path.join(episode_path, owl_file)
    return None

def create_queries_file(cat, exp):
    path = get_experiment_path(cat, exp)
    if not os.path.isdir(path):
        return
    f = os.path.join(path, 'queries.json')
    if os.path.isfile(f):
        return

    data = {"meta":
        {
            "date": "2015-07-18T12:00:00",
            "name": ''.join(random.choice(string.ascii_uppercase + string.digits) for _ in range(10)),
            "description": ''.join(random.choice(string.ascii_uppercase + string.digits) for _ in range(10)),
            "tags": [],
            "platforms": []
        },
        "query": [],
        "video": []
    }
    with open(f, 'w') as data_file:
        json.dump(data, data_file)

def experiment_load_queries(category, exp):
    create_queries_file(category, exp)
    episode_file = "/episodes/" + category + "/" + exp + "/queries.json"
    if not os.path.isfile(episode_file):
        return None
    data = None
    with open(episode_file) as data_file:
        data = json.load(data_file)
    return data
