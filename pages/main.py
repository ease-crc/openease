import yaml
from flask import session, request, redirect, url_for, render_template, send_from_directory, jsonify, flash
from flask.ext.user.signals import user_logged_in
from flask.ext.user.signals import user_logged_out
from flask_user import current_user, login_required

from urlparse import urlparse, urljoin
import urllib
import traceback
import os
import random

import re
import markdown2

from app_and_db import app
from app_and_db import db
from utility import admin_required
from pages.overview import get_sanitizer
import knowrob.container as docker_interface
from flask_wtf import Form
from wtforms import PasswordField
from wtforms.validators import DataRequired
from config.settings import USE_HOST_KNOWROB

from neems.neemhub import instance as neemhub, NEEMHubConnectionError 
from neems.neem import FEATURED_NEEM_IDS

from postgres.AlchemyEncoder import AlchemyEncoder
from postgres.settings import get_neemhub_settings
import json

__author__ = 'danielb@uni-bremen.de'

# path for node modules stored in openEASE container
NODE_MODULES_PATH = "/tmp/npm/node_modules/"

FUNDING_DATA=[
    {'index': 0, 'class': 'active', 'img_id': 'robohow-logo',
     'name': 'RoboHow', 'href': 'http://www.robohow.eu',
     'img': 'img/robohow-logo.png'},
    {'index': 1, 'class': '', 'img_id': 'roboearth-logo',
     'name': 'RoboEarth', 'href': 'http://www.roboearth.org',
     'img': 'img/roboearth-logo.png'},
    {'index': 2, 'class': '', 'img_id': 'refills-logo',
     'name': 'Refills', 'href': 'http://www.refills-project.eu/',
     'img': 'img/refills.png'},
    {'index': 3, 'class': '', 'img_id': 'sherpa-logo',
     'name': 'SHERPA', 'href': 'http://www.sherpa-project.eu',
     'img': 'img/sherpa-logo.png'},
    {'index': 4, 'class': '', 'img_id': 'saphari-logo',
     'name': 'SAPHARI', 'href': 'http://www.saphari.eu',
     'img': 'img/saphari-logo.png'}
]


class QueryExamples(object):
    _instance = None

    @staticmethod
    def get():
        if QueryExamples._instance is None:
            QueryExamples._instance = QueryExamples()
        return QueryExamples._instance

    def __init__(self):
        self.query_counter = 0
        self.topic_map = {}
        self.binding_map = {}
        self.query_data = []
        self.query_list = []
        self.read_yaml()

    def random_query(self):
        return random.choice(self.query_list)

    def entity_queries(self, entity_type):
        if entity_type in self.binding_map:
            return self.binding_map[entity_type]
        else:
            return []

    def add_binding(self, binding, queries):
        entity_type = binding['type']
        if entity_type not in self.binding_map:
            self.binding_map[entity_type] = []
        self.binding_map[entity_type].append({
            'description': binding['description'],
            'var': binding['var'],
            'query': queries[0]['text']
        })

    def read_yaml(self):
        path = os.path.join(os.path.join(app.root_path, "static"), "example-queries.yaml")
        self.query_counter = 0
        self.topic_map = {}
        self.query_data = []
        self.query_list = []

        with open(path) as f:
            yaml_data = yaml.load(f, Loader=yaml.FullLoader)

            # first load sections & topics
            self.query_data = yaml_data['sections']
            for section in self.query_data:
                for topic in section['topics']:
                    self.topic_map[topic['id']] = topic
                    topic['sub_topics'] = []

            # gather subtopics
            for sub_topic in yaml_data['sub_topics']:
                if 'query_groups' in sub_topic:
                    for query_group in sub_topic['query_groups']:
                        queries = []
                        for query0 in query_group['queries']:
                            query = query0.rstrip()
                            queries.append({'id': self.query_counter, 'text': query})
                            self.query_list.append(query)
                            self.query_counter += 1
                        if 'binding' in query_group:
                            self.add_binding(
                                query_group['binding'],
                                queries)
                        query_group['queries'] = queries
                topic = self.topic_map[sub_topic['topic']]
                topic['sub_topics'].append(sub_topic)


# PasswordForm used for validating given password field
class PasswordForm(Form):
    password = PasswordField('Password', validators=[DataRequired()])

@app.context_processor
def inject_ros_params():
    # Inject parameters needed to connect to ROS to
    # every HTML template.
    container_name = "host"
    if current_user.is_authenticated and not USE_HOST_KNOWROB:
        container_name = current_user.username + "_knowrob"
    return dict(
        ros_url=urlparse(request.host_url).hostname,
        ros_auth=not USE_HOST_KNOWROB,
        ros_container=container_name
    )

@app.context_processor
def inject_neem_params():
    try:
        act_neem=neemhub.get_neem(session.get('neem_id', None))
        if act_neem is None:
            act_neem={}
    except Exception as exc:
        act_neem={}
    return dict(active_neem=act_neem)

@user_logged_in.connect_via(app)
def track_login(sender, user, **extra):
    app.logger.info("Logged in " + str(user.username))
    # TODO: why not just use username key?
    session['user_container_name'] = user.username
    session['username'] = user.username
    session['api_token'] = user.api_token
    if not USE_HOST_KNOWROB:
        sql = get_neemhub_settings()
        docker_interface.start_user_container(user.username,
                                     json.dumps(sql, cls=AlchemyEncoder),
                                     "knowrob",
                                     "latest")

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

@app.route('/meshes/<path:mesh>')
def download_mesh(mesh):
    return redirect(urljoin(neemhub.get_mesh_server(), mesh))

# method to send font files from node_modules dir
@app.route('/<path:url_path>/node_modules/<path:file_path>')
def send_from_node_modules(url_path, file_path):
    # remove possibility for miss use of file_path by removing any sort
    # of directory path manipulation
    file_path.replace("..", "").replace("./", "")
    with open(os.path.join(NODE_MODULES_PATH, file_path), 'r') as f:
        file_content = f.read()
    return file_content

@app.route('/node_modules/<path:file_path>')
def send_from_node_modules_root(file_path):
    return send_from_node_modules('/', file_path)

@app.route('/')
def render_main():
    if not current_user.is_authenticated:
        return redirect(url_for('user.login'))
    if 'user_container_name' not in session:
        return redirect(url_for('user.logout'))
    return redirect(url_for('render_homepage'))

@app.route('/QA')
@login_required
def render_QA_page():
    # Check if the connection to the neemhub is established:
    # connect_mongo will try to connect to the mongo server. 
    # If this fails, a NEEMHubConnectionError will be thrown.
    # In neem_discovery the NEEMHubConnectionError is handled
    # by redirecting to the invalid settings page
    neemhub.connect_mongo()
    # Check if a neem is choosen
    neem = neemhub.get_requested_neem(request)
    if neem is None:
        return redirect(url_for('render_neems'))
    neem.activate()
    # read query from URL parameter "q" if any
    query_text = request.args.get('q', default='')
    has_query = (query_text is not '')
    # show info about funding agencies
    funding = FUNDING_DATA
    return render_template('pages/QA.html', **locals())

@app.route('/QA/random', methods=['POST'])
def get_random_example_query():
    return jsonify(q=QueryExamples.get().random_query())

@app.route('/QA/entity_queries', methods=['POST'])
def get_example_entity_queries():
    data = json.loads(request.data)
    entity_type = data['entity_type']
    return jsonify(queries=QueryExamples.get().entity_queries(entity_type))

@app.route('/examples')
def render_examples_page():
    url_quote = lambda x: urllib.pathname2url(x)
    example_query_data = QueryExamples.get().query_data
    return render_template('pages/QA-examples.html', **locals())

# get call handling method for changing password
@app.route('/change_password_get')
@login_required
def render_change_password_get():
    # PasswordForm used for validating given password field
    form = PasswordForm()
    return render_template('flask_user/change_password.html', title='Change Password', form=form)

# post call handling method for changing password
@app.route('/change_password_post', methods=["POST"])
@login_required
def render_change_password_post():
    # PasswordForm used for validating given password field
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

#def _get_user_roles():
#    role_names = []
#    if hasattr(current_user, 'roles'):
#        role_names = map(lambda x: str(x.name), current_user.roles)
#    return role_names

@app.route('/admin/cookie')
@admin_required
def admin_cookie():
    return render_template('settings/cookies.html', **locals())

@app.route('/homepage')
def render_homepage():
    could_connect = True

    try:
        matching_neems = neemhub.get_neem_ids('', True)
        neems = list(map(lambda (x): neemhub.get_neem(x), matching_neems))
    except NEEMHubConnectionError as e:
        could_connect = False
        app.logger.error('Could not connect to Neemhub to fetch neems for neem cards.\n\n' + e.__str__())

        message = 'Our apologies! The content for this section could not be loaded. Refresh the page or try again later.'
    else:
        featured_neems = []
        for id in FEATURED_NEEM_IDS:
            for neem in neems:
                if id == neem.neem_id:
                    featured_neems.append(neem)
                    break

        recent_neems = neems
        for neem in featured_neems:
            recent_neems.remove(neem)
        recent_neems.sort(reverse=True, key=lambda x: x.last_updated)
        recent_neems = recent_neems[0:6]

    return render_template('pages/homepage.html', **locals())


@app.route('/overview/<neem_path>')
def render_neem_overview_page(neem_path=None):
    # When tags or items from the markdown are not displayed correctly,
    # it might hint to the sanitizer removing unallowed tags. To allow 
    # these tags to pass, adjust the sanitizer-config from get_sanitizer()
    # in # pages/overview.py. Afterwards adjust the styling in 
    # static/css/overview.scss.
    #
    # When in doubt, refer to
    #   https://github.com/trentm/python-markdown2
    # and
    #   https://github.com/matthiask/html-sanitizer

    try:
        with open('/opt/webapp/webrob/overview-contents/' + neem_path + '.md', 'r') as file_in:
            file_str = file_in.read()
    except IOError as e:
        app.logger.error('Could not find markdown-file for neem, therefore cannot render the overview page.\n\n' + e.message)
        flash('Our apologies! Could not load selected overview page. Please try again later!', "warning")
        return redirect(url_for('render_homepage'))

    # markdown to html-conversion
    md_content = markdown2.markdown(file_str, extras=['target-blank-links', 'nofollow', 'tables'])
    # add noreferrer to links; admittedely not the nicest way of doing this
    md_content = md_content.replace('rel=\"nofollow noopener\"', 'rel=\"nofollow noopener noreferrer\"')

    # need to sanitize the input, because the template loads the values
    # as safe, which would otherwise allow for XSS
    sanitizer = get_sanitizer()
    md_content = sanitizer.sanitize( md_content )

    # get neem name
    neem_name = neem_path
    match = re.match(r'<h1.*?>(?P<neem_name>.*?)</h1>', md_content)
    if match is not None:
        neem_name = match.group('neem_name')
    
    return render_template('pages/overview.html', **locals())
