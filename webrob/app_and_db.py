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
from pymongo import MongoClient

# MONGO_HOST = os.environ.get('KNOWROB_MONGO_HOST')
# MONGO_PORT = int(os.environ.get('KNOWROB_MONGO_PORT'))
# MONGO_DB = os.environ.get('KNOWROB_MONGO_DB')
# MONGO_USER = os.environ.get('KNOWROB_MONGO_USER')
# MONGO_PASS = os.environ.get('KNOWROB_MONGO_PASS')

MONGO_HOST = "mongodb://data.open-ease.org"
MONGO_PORT = 28015
MONGO_DB = "neems"
MONGO_USER = "neemReader"
MONGO_PASS = "qEWRqc9UdN5TD7No7cjymUA8QEweNz"

# This is the WSGI compliant web application object
app = Flask(__name__)
app.logger.addHandler(logging.StreamHandler())
app.logger.setLevel(logging.INFO)

# remote mongo client connection
connection = MongoClient(MONGO_HOST, MONGO_PORT)
mongoDbClient = connection[MONGO_DB]
mongoDbClient.authenticate(MONGO_USER, MONGO_PASS)
mongoDBMetaCollection = mongoDbClient["meta"]
if mongoDBMetaCollection.count() > 0:
    app.logger.info('------------ mongoDb collection contains some document values ------------')

# This is the SQLAlchemy ORM object
db = SQLAlchemy(app)
