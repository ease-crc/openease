from flask import session, send_from_directory, jsonify, request, render_template
from flask_paginate import Pagination, get_page_args

from webrob.app_and_db import app

from webrob.neems.manager import NEEM_Manager

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

#@app.route('/neems/<neem_group>/<neem_name>/pull')
#def route_pull_neem(neem_group,neem_name):
    ## TODO show progress to user
    #neem = neem_manager.get(neem_group,neem_name)
    #neem.pull()
    #return jsonify(result=None)

@app.route('/neems/<neem_id>/activate')
def route_activate_neem(neem_id):
    neem = neem_manager.get(neem_id)
    # TODO show progress to user
    app.logger.info('Activate neem rout')
    neem.checkout()
    neem.activate()
    return jsonify(success=True)

@app.route('/neems/<neem_id>/static/<path:filename>')
def send_from_neem_directory(neem_id, filename):
    neem = neem_manager.get(neem_id)
    neem.checkout()
    return send_from_directory(neem.get_directory(), filename)
