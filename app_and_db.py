# This file declares the Flask Singletons 'app' and 'db'
# 'app' and 'db' are defined in a separate file to avoid circular imports
# Usage: from app.app_and_db import app, db
#
# Copyright 2014 SolidBuilds.com. All rights reserved
#
# Authors: Ling Thio <ling.thio@gmail.com>

import os
from flask import Flask
from flask_sqlalchemy import SQLAlchemy
import logging

# This is the WSGI compliant web application object
app = Flask(__name__)
app.logger.addHandler(logging.StreamHandler())
app.logger.setLevel(logging.INFO)

# set up flask SQLAlchemy db uri
app.config['SQLALCHEMY_DATABASE_URI'] =  'postgresql://docker@' + \
os.environ['POSTGRES_PORT_5432_TCP_ADDR'] + ':' + \
os.environ['POSTGRES_PORT_5432_TCP_PORT'] + '/docker'

# This is the SQLAlchemy ORM object
db = SQLAlchemy(app)
