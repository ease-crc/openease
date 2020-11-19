from flask import jsonify, request,\
    render_template, redirect,\
    url_for, flash
from flask_paginate import Pagination

from webrob.app_and_db import app, db, get_mongo_db_meta_collection
from webrob.utility import admin_required

from webrob.neems.manager import NEEM_Manager
from webrob.models.NEEMHubSettings import NEEMHubSettings, get_settings, get_settings_count
from webrob.models.NEEMMetaException import NEEMMetaException

from wtforms.validators import DataRequired
from flask_wtf import Form
from wtforms import PasswordField

__author__ = 'danielb@cs.uni-bremen.de'

neem_manager = NEEM_Manager()

@app.route('/neems')
def render_neems():
    mongoDBMetaCollection = get_mongo_db_meta_collection()
    show_all = request.args.get('show_all', default=True, type=bool)
    per_page = request.args.get('limit', default=12, type=int)
    query = request.args.get('neem_query', default='', type=str)
    # compute pagination offset
    current_page = int(request.args.get('page', 1))
    current_offset = (current_page - 1) * per_page
    next_offset = current_offset + per_page
    # get neems
    matching_neems = neem_manager.query_neem_ids(query)

    neems = list(map(lambda (x): neem_manager.get(x),
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

    return render_template('neems/search.html', **locals())

@app.route('/neems/<neem_group>/<neem_name>/info')
def route_neem_meta(neem_group,neem_name):
    neem = neem_manager.get(neem_group,neem_name)
    return jsonify(result=neem.get_info())

# PasswordForm used for validating given password field
class PasswordForm(Form):
    password = PasswordField('Password', validators=[DataRequired()])

@app.route('/edit_neem_hub_settings')
@admin_required
def render_neem_hub_settings_page_get():
    # PasswordForm used for validating given password field
    return render_template('admin/neem_hub_settings.html',
                           form=PasswordForm(),
                           neemHubSettings=get_settings())

@app.route('/save_edited_neem_hub_settings', methods=['POST'])
@admin_required
def render_neem_hub_settings_post():
    app.logger.debug('render neem hub settings post method.... ')
    req = request.form
    if req is not None:
        neemHubSettings = NEEMHubSettings()

        # first find if there is any existing entry in db
        count = get_settings_count()
        if count > 0:
            neemHubSettings = get_settings()

        neemHubSettings.MONGO_HOST = req.get("MONGO_HOST")
        neemHubSettings.MONGO_PORT = req.get("MONGO_PORT")
        neemHubSettings.MONGO_USER = req.get("MONGO_USER")
        neemHubSettings.MONGO_DB = req.get("MONGO_DB")
        neemHubSettings.MONGO_PASS = req.get("MONGO_PASS")

        db.session.add(neemHubSettings)
        db.session.commit()
        app.logger.info('Configuration has been stored!')

        # check if connection is secured
        get_mongo_db_meta_collection()
        # once connection is secured then update neem_ids from
        # mongodb meta collection so that neem discovery page has latest updates
        neem_manager.set_neem_ids()
        app.logger.debug('------------ neem manager updated ------------')

    else:
        flash('Null request is submitted while form submission!', "warning")
        return redirect(url_for('render_neem_hub_settings_page_get'))

    flash('NEEM Hub configuration setting is stored!', "success")
    return redirect(url_for('render_neems'))

# handles all NEEMMetaException and redirects to neems without settings page
@app.errorhandler(NEEMMetaException)
def handle_neem_hub_meta_exception_with_mongodb(neemMetaException):
    app.logger.error(neemMetaException.get_exc())
    flash(neemMetaException.get_message(), "warning")
    return render_template('admin/neems_without_settings_page.html', **locals())
