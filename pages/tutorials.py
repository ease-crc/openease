import json
from urlparse import urlparse

from flask import request, render_template, Markup, jsonify
from flask.ext.misaka import markdown
from flask_user import current_user, login_required

from app_and_db import app
from config.settings import USE_HOST_KNOWROB


@app.route('/tutorials/')
@login_required
def render_tutorials():
    tutorial_names = [
        {'id': 'prolog', 'title': 'Logic Programming', 'selected': True,
         'description': """
         In this tutorial you can learn about the openEASE querying language which is based
         on Prolog."""
         },
        {'id': 'semweb', 'title': 'Semantic Web introduction', 'selected': False,
         'description': """
         In this tutorial you can learn about the semantic web."""
         }
    ]
    return render_template('pages/tutorials.html', **locals())


@app.route('/tutorials/get', methods=['POST'])
def get_tutorial_data():
    data = json.loads(request.data)

    # TODO: validate that tut_id is not a path
    tut_id = data['tutorial']
    tut_file = '/opt/webapp/webrob/tutorials/'+tut_id+'.md'

    # read tutorial md data
    page_data = ""
    with open(tut_file, 'r') as f:
        page_data = f.read()
    # convert to HTML
    page_data = Markup(markdown(page_data, fenced_code=True))
    # split into pages at <h2> tags
    pages = page_data.split('<h2>')[1:]
    pages = map(lambda p: p.split('</h2>'), pages)

    return jsonify({'pages': pages})
