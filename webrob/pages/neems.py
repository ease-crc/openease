from flask import session, send_from_directory, jsonify, request, render_template, redirect, url_for, flash
from flask_paginate import Pagination, get_page_args

from webrob.app_and_db import app, getNeemHubSettingFromDb, checkConnection


from webrob.neems.manager import NEEM_Manager

from webrob.models.NEEMHubSettings import get_settings_count, get_settings

from pymongo import MongoClient

__author__ = 'danielb@cs.uni-bremen.de'



@app.route('/neems')
def render_neems():

    # TODO: check if NEEM Hub configuration is correctly set or not?
    # at first check if there is settings stored in db
    neemHubSettings = get_settings(1)
    if checkConnection(neemHubSettings) is None:
        flash('Failure connecting with mongodb with given credentials, please check inputs!', "warning")
        return render_template('admin/neems_without_settings_page.html', **locals())

    show_all = request.args.get('show_all', default=True, type=bool)
    per_page = request.args.get('limit', default=12, type=int)
    query = request.args.get('neem_query', default='', type=str)
    # compute pagination offset
    current_page = int(request.args.get('page', 1))
    current_offset = (current_page - 1) * per_page
    next_offset = current_offset + per_page
    # get neems
    neem_manager = NEEM_Manager()
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
    neem_manager = NEEM_Manager()
    neem = neem_manager.get(neem_group,neem_name)
    return jsonify(result=neem.get_info())
