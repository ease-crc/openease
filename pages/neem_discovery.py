from flask import jsonify, request, \
    render_template, redirect, \
    url_for, flash
from flask_paginate import Pagination
from flask_user import current_user

from app_and_db import app
from helpers.utility import admin_required

from neems.neemhub import instance as neem_hub, NEEMHubConnectionError

from wtforms.validators import DataRequired
from flask_wtf import Form
from flask_user import current_user, login_required
from wtforms import PasswordField
from app_and_db import db

from knowrob.container import container_started
from config.settings import USE_HOST_KNOWROB
from postgres.AlchemyEncoder import AlchemyEncoder
from postgres.settings import get_neemhub_settings
import json
import knowrob.container as docker_interface

__author__ = 'danielb@cs.uni-bremen.de'


# PasswordForm used for validating given password field
class PasswordForm(Form):
    password = PasswordField('Password', validators=[DataRequired()])


@app.route('/neems')
def render_neems():
    show_all = request.args.get('show_all', default=True, type=bool)
    per_page = request.args.get('limit', default=12, type=int)
    query = request.args.get('neem_query', default='', type=str)
    # compute pagination offset
    current_page = int(request.args.get('page', 1))
    current_offset = (current_page - 1) * per_page
    next_offset = current_offset + per_page
    # get neems
    matching_neems = neem_hub.get_neem_ids(query,neem_hub.get_neem_visibility_flag())
    neems = list(map(lambda (x): neem_hub.get_neem(x),
                     matching_neems[current_offset:next_offset]))

    # TODO: what does this mean?
    search = False
    q = request.args.get('q')
    if q:
        search = True
    pagination = Pagination(page=current_page,
                            per_page=per_page,
                            offset=current_offset,
                            total=len(matching_neems),
                            css_framework='bootstrap4',
                            search=search)

    if not USE_HOST_KNOWROB and not container_started(current_user.username):
        sql = get_neemhub_settings()
        docker_interface.start_user_container(current_user.username,
                                 json.dumps(sql, cls=AlchemyEncoder),
                                 "knowrob",
                                 "latest")

    return render_template('pages/neems.html', **locals())


@app.route('/like/<neem_id>/<action>', methods=["POST"])
@login_required
def like_action(neem_id, action):
    if action == 'toggle':
        if current_user.has_liked_neem(neem_id):
            current_user.unlike_neem(neem_id)
        else:
            current_user.like_neem(neem_id)
    elif action == 'like':
        current_user.like_neem(neem_id)
    elif action == 'unlike':
        current_user.unlike_neem(neem_id)
    db.session.commit()
    return jsonify(result=True)


@app.route('/neems/<neem_group>/<neem_name>/info')
def route_neem_meta(neem_group, neem_name):
    neem = neem_hub.get_neem(neem_group, neem_name)
    return jsonify(result=neem.get_info())


@app.route('/settings/neem_hub/edit')
@admin_required
def render_neem_hub_settings():
    # PasswordForm used for validating given password field
    return render_template('settings/neemhub.html',
                           form=PasswordForm(),
                           neem_hub=neem_hub)


@app.route('/settings/neem_hub/save', methods=["POST"])
@admin_required
def post_neem_hub_settings():
    req = request.form
    if req is not None:
        neem_hub.set_mongo_host(req.get("mongo_host"))
        neem_hub.set_mongo_port(req.get("mongo_port"))
        neem_hub.set_mongo_user(req.get("mongo_user"))
        neem_hub.set_mongo_db(req.get("mongo_db"))
        neem_hub.set_mongo_pass(req.get("mongo_pass"))
        if req.get("visibility_flag") is None:
            neem_hub.set_neem_visibility_flag(False)
        else:
            neem_hub.set_neem_visibility_flag(True)
        neem_hub.set_urdf_server(req.get("urdf_server"))
        neem_hub.set_mesh_server(req.get("mesh_server"))
        neem_hub.store_settings()
    else:
        flash('Null request is submitted while form submission!', "warning")
        return redirect(url_for('render_neem_hub_settings'))
    app.logger.info("NEEMHub settings have been updated")
    flash('NEEM Hub configuration setting is stored!', "success")
    return redirect(url_for('render_neems'))


@app.errorhandler(NEEMHubConnectionError)
def handle_neemhub_connection_error(err):
    """
    handles all NEEMMetaException and redirects to neems without settings page
    :param err:
    :return:
    """
    app.logger.error(err)
    flash(str(err), "warning")
    settings_category = 'NEEM Hub'
    settings_url = url_for('render_neem_hub_settings')
    return render_template('settings/invalid_settings.html', **locals())
