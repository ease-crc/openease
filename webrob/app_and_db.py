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


from webrob.models.NEEMHubSettings import NEEMHubSettings, get_settings_count, get_settings



def getNeemHubSettingFromDb():
    # check for neem hub configuration
    settings_count = get_settings_count()
    if settings_count == 1 :
        neemHubSettings = get_settings(1)
        app.logger.info('retrieved object from postgresql......')
        app.logger.info(neemHubSettings.MONGO_HOST)
        app.logger.info(neemHubSettings.MONGO_PORT)
        app.logger.info(neemHubSettings.MONGO_DB)
        app.logger.info(neemHubSettings.MONGO_USER)
        app.logger.info(neemHubSettings.MONGO_PASS)
        return neemHubSettings
    else:
        return None


def checkConnection(neemHubSettings):
    if neemHubSettings is not None:
        try:
            connection = MongoClient(neemHubSettings.MONGO_HOST, neemHubSettings.MONGO_PORT)
            mongoDbClient = connection[neemHubSettings.MONGO_DB]
            mongoDbClient.authenticate(neemHubSettings.MONGO_USER, neemHubSettings.MONGO_PASS)

            mongoDBMetaCollection = mongoDbClient["meta"]

            if mongoDBMetaCollection.count() > 0:
                app.logger.info('------------ mongoDb collection contains some document values ------------')
                return mongoDBMetaCollection
            else:
                app.logger.info('---  MongoDB connection is established but collection does not contain any values  ---')
                return mongoDBMetaCollection
        except:
            app.logger.error('------------ mongoDb connection can not be created ------------')
            return None
    else:
        return None
# else :
# MONGO_HOST = "mongodb://data.open-ease.org"
# MONGO_PORT = 28015
# MONGO_DB = "neems"
# MONGO_USER = "neemReader"
# MONGO_PASS = "qEWRqc9UdN5TD7No7cjymUA8QEweNz"

# remote mongo client connection



