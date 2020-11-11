# This file declares the Flask Singletons 'app' and 'db'
# 'app' and 'db' are defined in a separate file to avoid circular imports
# Usage: from app.app_and_db import app, db
#
# Copyright 2014 SolidBuilds.com. All rights reserved
#
# Authors: Ling Thio <ling.thio@gmail.com>

import os
from flask import Flask, session
from flask_sqlalchemy import SQLAlchemy
import logging
from pymongo import MongoClient
import pymongo
from sqlalchemy.exc import SQLAlchemyError



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

from webrob.models.NEEMHubSettings import get_settings

# This method will check db connection with given settings.
# If settings are correct then will return meta collection from the db
def get_mongo_db_meta_collection():

    # get settings from postgresql db
    try:
        neemHubSettings = get_settings()
        if neemHubSettings is not None:
                connection = MongoClient(neemHubSettings.MONGO_HOST, neemHubSettings.MONGO_PORT)
                mongoDbClient = connection[neemHubSettings.MONGO_DB]
                mongoDbClient.authenticate(neemHubSettings.MONGO_USER, neemHubSettings.MONGO_PASS)

                mongoDBMetaCollection = mongoDbClient["meta"]
                return mongoDBMetaCollection

    except SQLAlchemyError as e:
        app.logger.error("while connecting to sql db returns null")
        app.logger.error(e)








