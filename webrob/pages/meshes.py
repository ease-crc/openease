
import os
import sys
import traceback

from flask import request
from flask import redirect, jsonify, send_from_directory
from flask_user import login_required
from urllib import urlopen, urlretrieve
from subprocess import call
from posixpath import basename
import thread

from webrob.app_and_db import app
from webrob.config.settings import MESH_URDF_SERVER
from webrob.pages.neems import neem_manager

__author__ = 'danielb@cs.uni-bremen.de'

def is_mesh_url_valid(url):
    return urlopen(url).getcode() == 200

@app.route('/meshes/<path:mesh>')
def download_mesh(mesh):
    for repo in os.listdir( '/home/ros/mesh_data' ):
        repoPath = os.path.join('/home/ros/mesh_data', repo)
        meshPath = os.path.join(repoPath, mesh)
        if os.path.exists(meshPath):
          return send_from_directory(
            os.path.dirname(meshPath),
            os.path.basename(meshPath))

    return redirect(MESH_URDF_SERVER + mesh)