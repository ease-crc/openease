from flask import session, send_from_directory, jsonify, request, render_template
from flask_paginate import Pagination, get_page_args

from webrob.app_and_db import app, mongoDbClient

from webrob.neems.manager import NEEM_Manager

from webrob.neems.neem import NEEM

__author__ = 'danielb@cs.uni-bremen.de'

neem_manager = NEEM_Manager()

@app.route('/neems')
def render_neems():
    app.logger.info("calling neems from mongoDb collection")
    show_all = request.args.get('show_all', default=True, type=bool)
    per_page = request.args.get('limit', default=12, type=int)
    query = request.args.get('neem_query', default='', type=str)
    # compute pagination offset
    current_page = int(request.args.get('page', 1))
    current_offset = (current_page - 1) * per_page
    next_offset = current_offset + per_page
    # get neems meta information from mongo collection
    neemsMeta = mongoDbClient["meta"]

    matching_neems = neemsMeta.find()
    neems = matching_neems

    # for each doc from neemsMeta, create a NEEM instance
    # todo: see why we can not use neems list here instread have to call from db
    for neem in neemsMeta.find():
        NEEM('test', neem['description'], neem['created_by'])

    # TODO: what does this mean?
    search = False
    q = request.args.get('q')
    if q:
        search = True
    pagination = Pagination(page=current_page,
                            per_page=per_page,
                            offset=current_offset,
                            total=neemsMeta.count(),
                            css_framework='bootstrap4',
                            search=search)
    return render_template('neems/search.html', **locals())

@app.route('/neems/<neem_group>/<neem_name>/info')
def route_neem_meta(neem_group,neem_name):
    neem = neem_manager.get(neem_group,neem_name)
    return jsonify(result=neem.get_info())

#@app.route('/neems/<neem_group>/<neem_name>/pull')
#def route_pull_neem(neem_group,neem_name):
    ## TODO show progress to user
    #neem = neem_manager.get(neem_group,neem_name)
    #neem.pull()
    #return jsonify(result=None)

@app.route('/neems/<neem_group>/<neem_name>/activate')
def route_activate_neem(neem_group,neem_name):
    neem = neem_manager.get(neem_group,neem_name)
    # TODO show progress to user
    neem.checkout()
#    neem.activate()
    return jsonify(success=True)


#@app.route('/neems/<neem_group>/<neem_name>/static/<path:filename>')
#def send_from_neem_directory(neem_group,neem_name, filename):
#    neem = neem_manager.get(neem_group,neem_name)
#    neem.checkout()
#    return send_from_directory(neem.get_directory(), filename)
