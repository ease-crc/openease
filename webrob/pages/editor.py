import sys
import json
import zipfile
import traceback

from urlparse import urlparse

from flask import session, request, render_template, jsonify, send_file
from flask_user import current_app

from webrob.app_and_db import app
from webrob.docker import docker_interface
from webrob.docker.docker_interface import LFTransfer
from webrob.utility.path_handler import path_exists, join_paths, split_extension, relative_path, split_path
from webrob.utility.directory_handler import walk_directories
from webrob.utility.admin_checker import admin_required
from webrob.utility.template_file_copyer import copy_template_file_and_replace_keywords
from webrob.models.teaching import CourseExercise

__author__ = 'danielb@cs.uni-bremen.de'


class PackageError(Exception):
    status_code = 400

    def __init__(self, msg):
        Exception.__init__(self)
        self.msg = msg

    def __str__(self):
        return 'Unable to create package: ' + self.msg


@app.errorhandler(PackageError)
def handle_pkg_error(error):
    response = jsonify(str(error))
    response.status_code = error.status_code
    return response


@app.route('/editor')
def render_editor(filename=""):
    error = ""
    # determine hostname/IP we are currently using
    # (needed for accessing container)
    host_url = urlparse(request.host_url).hostname
    container_name = session['user_container_name']
    return render_template('editor.html', **locals())


@app.route('/pkg/new', methods=['POST'])
def create_new_pkg():
    pkg_name = _check_pkg_name_and_raise_error_if_already_exists(json.loads(request.data)['packageName'])
    template_path = _check_template_path_and_raise_error_if_does_not_exist('/opt/webapp/webrob/templates/package')

    try:
        _transfer_pkg_to_user_container(pkg_name, template_path)
    except Exception:  # catch *all* exceptions
        _log_unable_to_create_pkg_error()
        delete_pkg(pkg_name)
        raise PackageError("An internal error occurred.")

    return jsonify(success=1)


def _check_pkg_name_and_raise_error_if_already_exists(pkg_name):
    if docker_interface.file_exists(session['user_container_name'], pkg_name):
        raise PackageError("A package with the name '" + pkg_name + "' already exists.")
    return pkg_name


def _check_template_path_and_raise_error_if_does_not_exist(path):
    if not path_exists(path):
        raise PackageError("Package template could not be found.")
    return path


def _transfer_pkg_to_user_container(pkg_name, template_path):
    with LFTransfer(session['user_container_name']) as lft:
        pkg_path = _build_container_path(lft, pkg_name)
        _copy_pkg_to_user_container_and_replace_keywords(pkg_name, pkg_path, template_path)
        lft.to_container(pkg_name, pkg_name)


def _build_container_path(large_file_transferer, name):
    return join_paths(large_file_transferer.get_filetransfer_folder(), name)


def _copy_pkg_to_user_container_and_replace_keywords(pkg_name, pkg_path, template_path):
    for root, dirs, files in walk_directories(template_path):
        for f in files:
            abs_path = join_paths(root, f)
            rel_path = relative_path(abs_path, template_path)
            user_path = join_paths(pkg_path, rel_path)

            copy_template_file_and_replace_keywords(abs_path, user_path, {
                "pkgName": pkg_name,
                "userName": session['user_container_name']
            })


def _log_unable_to_create_pkg_error():
    app.logger.error("Unable to create package.")
    app.logger.error(str(sys.exc_info()[0]))
    app.logger.error(str(traceback.format_exc()))


@app.route('/pkg/del', methods=['POST'])
def delete_pkg(package_name=None):
    pkg_name = _check_pkg_name_and_if_none_get_pkg_name_from_session(package_name)

    try:
        docker_interface.file_rm(session['user_container_name'], pkg_name, True)
    except Exception:  # catch *all* exceptions
        _log_unable_to_delete_pkg_error()
        raise PackageError("An internal error occurred.")

    return jsonify(success=1)


def _check_pkg_name_and_if_none_get_pkg_name_from_session(package_name):
    if package_name is None:
        return session['pkg']
    else:
        return package_name


def _log_unable_to_delete_pkg_error():
    app.logger.error("Unable to delete package. " + str(sys.exc_info()[0]))


@app.route('/pkg/set', methods=['POST'])
def set_pkg():
    # Update package name
    data = json.loads(request.data)
    if 'packageName' in data and len(data['packageName']) > 0:
        session['pkg'] = data['packageName']
    return _get_pkg_tree()


def _get_pkg_tree():
    # List files in package dir
    pkg_path = session['pkg']
    root_files = _get_root_files_from_container(pkg_path)
    # Return list of files
    return jsonify(result=root_files)


def _get_root_files_from_container(pkg_path):
    return docker_interface.file_ls(session['user_container_name'], pkg_path, True)['children']


@app.route('/pkg/list', methods=['POST'])
def pkg_list():
    # Return list of packages
    files = filter(lambda s: s['isdir'], docker_interface.file_ls(session['user_container_name'], '.')['children'])
    filenames = map(lambda s: s['name'], files)
    return jsonify(result=filenames)


@app.route('/pkg/read', methods=['POST'])
def read_pkg():
    path = _get_file_path(json.loads(request.data)['file'])
    try:
        # Read the file
        content = _read_file_from_container(path)
        return jsonify(result=content)
    except Exception:  # catch *all* exceptions
        _log_unable_to_read_file_error()
        raise PackageError("An internal error occurred.")


def _get_file_path(file_name):
    """
    Selects package subdir based on file extension.
    """
    path = session['pkg']
    (_, ext) = split_extension(file_name)
    if ext == ".pl":
        path = join_paths(path, "prolog")
    elif ext == ".owl":
        path = join_paths(path, "owl")
    return join_paths(path, file_name)


def _read_file_from_container(path):
    return docker_interface.file_read(session['user_container_name'], path).splitlines(True)


def _log_unable_to_read_file_error():
    app.logger.error("Unable to read file. " + str(sys.exc_info()[0]))


@app.route('/pkg/down', methods=['POST'])
def download_pkg():
    try:
        with LFTransfer(session['user_container_name']) as lft:
            zip_path = _zip_dir_to_container_and_return_dest_path(lft, session['pkg'])
            return send_file(zip_path,
                             mimetype="application/zip",
                             as_attachment=True,
                             attachment_filename=split_path(zip_path)[1])   # returns zip-file-name
    except Exception:  # catch *all* exceptions
        _log_unable_to_read_file_error()
        raise PackageError("An internal error occurred.")


def _zip_dir_to_container_and_return_dest_path(large_file_transferer, dir_name, source_dir_from_container=None):
    zip_name = dir_name + '.zip'
    # transfer files from container
    large_file_transferer.from_container(source_dir_from_container or dir_name, dir_name)
    dir_path = _build_container_path(large_file_transferer, dir_name)
    zip_path = _build_container_path(large_file_transferer, zip_name)
    _zip_dir_to_dest(large_file_transferer, dir_path, zip_path)
    return zip_path


def _zip_dir_to_dest(large_file_transferer, src, dest):
    zip_file_writer = zipfile.ZipFile(dest, 'w')
    _zip_dir(src, large_file_transferer.get_filetransfer_folder(), zip_file_writer)
    zip_file_writer.close()


def _zip_dir(src_path, path_prefix, zip_file_writer):
    for root, dirs, files in walk_directories(src_path):
        for f in files:
            abs_p = join_paths(root, f)
            rel_p = relative_path(abs_p, path_prefix)
            zip_file_writer.write(abs_p, rel_p)


@app.route('/pkg/save_exercise', methods=['POST'])
@admin_required
def pkg_save_exercise():
    db_adapter = current_app.user_manager.db_adapter
    exercise = _get_exercise_from_db()

    try:
        with LFTransfer(session['user_container_name']) as lft:
            # Create archive file
            zip_path = _zip_dir_to_container_and_return_dest_path(lft, exercise.title, session['pkg'])
            _read_archive_and_save_in_db(db_adapter, exercise, zip_path)
        return jsonify(success=1)
    except Exception:  # catch *all* exceptions
        _log_unable_to_save_exercise_error()
        raise PackageError("An internal error occurred.")


def _get_exercise_from_db():
    # Query exercise DB object
    exercise_id = json.loads(request.data)['exercise_id']
    exercise = CourseExercise.query.filter_by(id=exercise_id).first()
    if exercise is None:
        raise PackageError("Exercise with id '" + exercise_id + "' does not exist.")
    return exercise


def _read_archive_and_save_in_db(db_adapter, exercise, zip_path):
    # Read archive as binary blob and save in SQL DB
    zipfb = open(zip_path, 'rb')
    # NOTE: need to convert to base64 so that jsonify does not complain
    exercise.archive = zipfb.read().encode('base64')
    db_adapter.commit()
    zipfb.close()


def _log_unable_to_save_exercise_error():
    app.logger.error("Unable to save exercise. " + str(sys.exc_info()[0]))


@app.route('/pkg/load_exercise', methods=['POST'])
def pkg_load_exercise():
    exercise = _get_exercise_from_db()
    pkg_name = _check_pkg_name_and_raise_error_if_already_exists(exercise.title)

    try:
        with LFTransfer(session['user_container_name']) as lft:
            _create_pkg_zip_and_unzip_in_filetransfer_folder(exercise, lft, pkg_name)
            _copy_exercise_to_user_container(lft, pkg_name)
        return jsonify(success=1)
    except Exception:  # catch *all* exceptions
        _log_unable_to_load_exercise_error()
        raise PackageError("An internal error occurred.")


def _create_pkg_zip_and_unzip_in_filetransfer_folder(exercise, large_file_transferer, pkg_name):
    zip_path = join_paths(large_file_transferer.get_filetransfer_folder(), pkg_name + '.zip')
    _create_pkg_zip_in_filetransfer_folder(exercise, zip_path)
    _unzip_file_to_filetransfer_folder(large_file_transferer, zip_path)


def _create_pkg_zip_in_filetransfer_folder(exercise, zip_path):
    zipfb = open(zip_path, 'wb')
    zipfb.write(exercise.archive.decode('base64'))
    zipfb.close()


def _unzip_file_to_filetransfer_folder(large_file_transferer, zip_src_path):
    zip_file_handler = zipfile.ZipFile(zip_src_path, 'r')
    zip_file_handler.extractall(large_file_transferer.get_filetransfer_folder())
    zip_file_handler.close()


def _copy_exercise_to_user_container(large_file_transferer, pkg_name):
    large_file_transferer.to_container(pkg_name, pkg_name)


def _log_unable_to_load_exercise_error():
    app.logger.error("Unable to load exercise. " + str(sys.exc_info()[0]))


@app.route('/pkg/file_write', methods=['POST'])
def write_file_to_container():
    data = json.loads(request.data)
    path = _get_file_path(data['file'])
    try:
        _write_file_to_user_container(data, path)
        return jsonify(success=1)
    except Exception:  # catch *all* exceptions
        _log_unable_to_write_file_error()
        raise PackageError("An internal error occurred.")


def _write_file_to_user_container(data, path):
    docker_interface.file_write(session['user_container_name'], data['content'], path)


def _log_unable_to_write_file_error():
    app.logger.error("Unable to write file. " + str(sys.exc_info()[0]))


@app.route('/pkg/file_del', methods=['POST'])
def _delete_file_from_container():
    path = _get_file_path(json.loads(request.data)['file'])
    try:
        _remove_file_from_user_container(path)
        return _get_pkg_tree()
    except Exception:  # catch *all* exceptions
        _log_unable_to_delete_file_error()
        raise PackageError("An internal error occurred.")


def _remove_file_from_user_container(path):
    docker_interface.file_rm(session['user_container_name'], path, True)


def _log_unable_to_delete_file_error():
    app.logger.error("Unable to delete file. " + str(sys.exc_info()[0]))
