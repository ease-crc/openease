from flask import request, render_template, jsonify, abort, redirect, url_for, flash, session

import json

from webrob.app_and_db import app, get_mongo_db_meta_collection
from webrob.app_and_db import db
from webrob.utility import admin_required
from webrob.models.db import *
from webrob.models.NEEMHubSettings import NEEMHubSettings, get_settings, get_settings_count
from wtforms.validators import DataRequired
from flask_wtf import Form
from wtforms import PasswordField
from webrob.pages.neems import neem_manager
from sqlalchemy.exc import SQLAlchemyError
from pymongo.errors import ConnectionFailure, PyMongoError
from webrob.models.NEEMMetaException import NEEMMetaException

__author__ = 'danielb@cs.uni-bremen.de'

# PasswordForm used for validating given password field
class PasswordForm(Form):
    password = PasswordField('Password', validators=[DataRequired()])


@app.route('/db/docu/<key>', methods=['POST'])
def db_docu_text(key):
    docs = _get_all_docs()
    for d in docs:  # find specific
        if d.key == key:
            return jsonify(result=d.text)
    _display_warning_when_unable_to_find_docu(key)


def _get_all_docs():
    db_adapter = current_app.user_manager.db_adapter
    return db_adapter.find_all_objects(db_table_class('docu'))


def _display_warning_when_unable_to_find_docu(key):
    app.logger.warn("Unable to find docu for: " + str(key) + ".")
    abort(404)


@app.route('/db/page/user_roles')
@admin_required
def db_page_user_roles():
    return render_template('admin/db_user_roles.html', **locals())


@app.route('/db/find/user_roles', methods=['POST'])
@admin_required
def db_find_user_roles():
    users = _get_all_users()
    user_roles = []
    for u in users:  # find user roles
        user_roles.append({
            'id': u.id,
            'name': u.username,
            'roles': map(lambda r: {'name': r.name}, u.roles)
        })
    return jsonify(result=user_roles)


def _get_all_users():
    db_adapter = current_app.user_manager.db_adapter
    return db_adapter.find_all_objects(db_table_class('user'))


@app.route('/db/save/user_roles', methods=['POST'])
@admin_required
def db_save_user_roles():
    data = json.loads(request.data)
    if 'roles' in data and 'id' in data:
        _save_roles_to_users_and_commit_to_db(data)
    else:
        app.logger.warn("Invalid input data.")
    return jsonify(result=None)


def _save_roles_to_users_and_commit_to_db(data):
    db_adapter = current_app.user_manager.db_adapter
    user = db_find(db_table_class('user'), data['id'])
    for r in data['roles']:
        role = db_find_name(db_table_class('role'), r['name'])
        if role is not None:
            user.roles.append(role)
    db_adapter.commit()


@app.route('/db/page/<table>')
@admin_required
def db_page_route(table):
    table_class = db_table_class(table)
    if table_class is None:
        app.logger.warn("Unable to find table for: " + str(table) + ".")
        abort(404)

    columns = db_columns(table_class)
    for c in columns:
        if str(c['type']) == 'BOOLEAN':
            c['type'] = 'boolean'
        elif str(c['type']) == 'DATETIME':
            c['type'] = 'date'
        elif str(c['type']) == 'INTEGER':
            c['type'] = 'number'
            c['format'] = '{0:d}'
        # elif str(c['type']) == 'BLOB':
        else:
            c['type'] = 'string'

        c['editable'] = c['name'] is not 'id'
        if c['editable']:
            c['editable'] = 'true'
        else:
            c['editable'] = 'false'

        if c['nullable']:
            c['nullable'] = 'true'
        else:
            c['nullable'] = 'false'

    return render_template('admin/db_table.html', **locals())


@app.route('/db/find/<table>', methods=['POST'])
@admin_required
def db_find_route(table):
    return jsonify(result=db_find_all(db_table_class(table)))


@app.route('/db/save/<table>', methods=['POST'])
@admin_required
def db_update_route(table):
    data = json.loads(request.data)
    cls = db_table_class(table)
    if data is not None:
        if 'id' in data and db_find(cls, data['id']):
            db_update(cls, data['id'], data)
        else:
            db_create(cls, data)
    return jsonify(result=None)


@app.route('/db/new/<table>', methods=['POST'])
@admin_required
def db_create_route(table):
    data = json.loads(request.data)
    if data is not None:
        db_create(db_table_class(table), data)
    return jsonify(result=None)


@app.route('/db/delete/<table>', methods=['POST'])
@admin_required
def db_remove_route(table):
    data = json.loads(request.data)
    cls = db_table_class(table)
    if data is not None and 'id' in data and db_find(cls, data['id']):
        db_remove(cls, data['id'])
    return jsonify(result=None)


@app.route('/edit_neem_hub_settings')
@admin_required
def render_neem_hub_settings_page_get():
    # PasswordForm used for validating given password field
    form = PasswordForm()

    neemHubSettings = get_settings()

    return render_template('admin/neem_hub_settings.html', form=form, neemHubSettings=neemHubSettings)


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
        # once connection is secured then update neem_ids from mongodb meta collection so that neem discovery page has latest updates
        neem_manager.set_neem_ids()
        app.logger.debug('------------ neem manager updated ------------')

    else:
        flash('Null request is submitted while form submission!', "warning")
        return redirect(url_for('render_neem_hub_settings_page_get'))

    flash('NEEM Hub configuration setting is stored!', "success")
    return redirect(url_for('render_neems'))

# handles all ConnectionFailure and redirects to neems without settings page
@app.errorhandler(ConnectionFailure)
def handle_neem_hub_meta_exception_with_mongodb(e):
    app.logger.error(e)
    flash(e, "warning")
    return render_template('admin/neems_without_settings_page.html', **locals())


# handles all NEEMMetaException and redirects to neems without settings page
@app.errorhandler(NEEMMetaException)
def handle_neem_hub_meta_exception_with_mongodb(neemMetaException):
    app.logger.error(neemMetaException.get_exc())
    flash(neemMetaException.get_message(), "warning")
    return render_template('admin/neems_without_settings_page.html', **locals())


# handles all pymongoerros and redirects to neems without settings page
@app.errorhandler(PyMongoError)
def handle_neem_hub_meta_exception_with_mongodb(e):
    app.logger.error(e)
    return render_template('admin/neems_without_settings_page.html', **locals())


# handles all the SQLAlchemyErrors and redirects to neem hub settings page
@app.errorhandler(SQLAlchemyError)
def handle_neem_hub_sqlalchemy_exception_with_mongodb(e):
    # PasswordForm used for validating given password field
    form = PasswordForm()
    app.logger.error(e)
    return render_template('admin/neem_hub_settings.html', form=form, neemHubSettings=NEEMHubSettings())
