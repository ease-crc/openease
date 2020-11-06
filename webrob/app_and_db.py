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
from pymongo.errors import ConnectionFailure, PyMongoError
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


# This method will check db connection with given settings.
# If settings are correct then will return meta collection from the db
def getMongoDBMetaCollection(neemHubSettings):

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
        except ConnectionFailure as e:
            app.logger.error('------------ mongoDb connection can not be created ------------')
            app.logger.error(e)
            return None
        except PyMongoError as e:
            app.logger.error('------------ mongoDb connection can not be created ------------')
            app.logger.error(e)
            return None

    else:
        return None




