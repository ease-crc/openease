__author__ = 'danielb@cs.uni-bremen.de'

import os

from flask import send_from_directory, jsonify
from urllib import urlopen
from subprocess import call
import thread

from webrob.app_and_db import app
from webrob.config.settings import Config
from webrob.utility.directory_handler import change_directory
from webrob.utility.path_handler import join_paths, path_exists, get_parent_dir_name, get_path_basename, \
    get_unix_style_path_basename

ROS_MESH_DATA_DIR = '/home/ros/mesh_data'


def is_mesh_url_valid(url):
    return urlopen(url).getcode() == 200


def update_meshes():
    thread.start_new_thread(_update_meshes_run, ())


def _update_meshes_run():
    _change_to_mesh_data_directory()
    _update_mesh_repositories()
    _convert_tif_images_to_png()


def _change_to_mesh_data_directory():
    change_directory(ROS_MESH_DATA_DIR)


def _update_mesh_repositories():
    for repo in Config.MESH_REPOSITORIES:
        _update_if_svn_or_git_repository(repo)


def _update_if_svn_or_git_repository(repo):
    try:
        (tool, url) = repo
        if _tool_is_svn_repository(tool):
            _update_meshes_in_repository(url, "/usr/bin/svn", "update", "co")
        elif _tool_is_git_repository(tool):
            _update_meshes_in_repository(url, "/usr/bin/git", "pull", "clone")
    except Exception:
        app.logger.warn("Unable to update repository: '" + str(repo) + "'.")


def _tool_is_svn_repository(tool):
    return tool == "svn"


def _tool_is_git_repository(tool):
    return tool == "git"


def _update_meshes_in_repository(url, repo_dir, repo_update_cmd, repo_clone_cmd):
    repo_name = get_unix_style_path_basename(url)
    if _repository_exists(repo_name):
        _update_repository(repo_name, repo_dir, repo_update_cmd)
    else:
        _clone_repository(repo_dir, repo_clone_cmd, url)


def _repository_exists(repo_name):
    path_exists(repo_name)


def _update_repository(repo_name, repo_dir, repo_update_cmd):
    change_directory(repo_name)
    call([repo_dir, repo_update_cmd])
    change_directory('..')


def _clone_repository(repo_dir, repo_clone_cmd, url):
    call([repo_dir, repo_clone_cmd, url])


def _convert_tif_images_to_png():
    call(['/opt/webapp/convert-recursive', ROS_MESH_DATA_DIR])


@app.route('/meshes/<path:mesh>')
def download_mesh(mesh):
    mesh_file = _get_mesh_file(mesh)

    if mesh_file is None:
        _log_download_fail_message(mesh)
        return jsonify(result=None)

    return _send_mesh_file(mesh_file)


def _get_mesh_file(mesh):
    m_file = _get_m_file_from_repos(mesh)

    if m_file is None:
        m_file = _get_m_file_from_root(mesh)

    return m_file


def _get_m_file_from_repos(mesh):
    m_file = None

    for repo in os.listdir(ROS_MESH_DATA_DIR):
        repo_path = join_paths(ROS_MESH_DATA_DIR, repo)
        mesh_path = join_paths(repo_path, mesh)
        if path_exists(mesh_path):
            m_file = mesh_path

    return m_file


def _get_m_file_from_root(mesh):
    if path_exists(mesh):
        return mesh
    elif path_exists('/' + mesh):
        return '/' + mesh


def _log_download_fail_message(mesh):
    app.logger.info("Unable to download mesh " + mesh)


def _send_mesh_file(mesh_file):
    return send_from_directory(
        get_parent_dir_name(mesh_file),
        get_path_basename(mesh_file))
