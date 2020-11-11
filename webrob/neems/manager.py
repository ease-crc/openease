import os
from flask import session
from webrob.neems.neem import NEEM
from webrob.app_and_db import app, get_mongo_db_meta_collection
from pymongo.errors import ConnectionFailure, PyMongoError

NEEM_DIR = "/neems"


class NEEM_Manager:
    def __init__(self):
        self.neem_ids = self.set_neem_ids()

    def set_neem_ids(self):
        self.neem_ids = []

        # get all neem ids information from collection
        try:
            mongoDBMetaCollection = get_mongo_db_meta_collection()
            if mongoDBMetaCollection is not None and mongoDBMetaCollection.count() > 0:
                self.neem_ids = mongoDBMetaCollection.find().distinct('_id')
                # TODO: remove again multiply with 20 and return only self.neem_ids,
                #  just for testing pagination as long as only few neems are there
                self.neem_ids = self.neem_ids * 20

        except ConnectionFailure as e:
            app.logger.error('------------ mongoDb connection can not be created ------------')
            app.logger.error(e)

        except PyMongoError as e:
            app.logger.error('------------ mongoDb connection can not be created ------------')
            app.logger.error(e)

        return self.neem_ids

    def get_requested(self, request):
        neem_id = request.args.get('neem_id',
                                      default=session.get('neem_id', None))
        if neem_id is None:
            return None
        else:
            return self.get(neem_id)

    def get(self, neem_id):
        return NEEM(neem_id)

    def query_neem_ids(self, query_string):
        if query_string is '':
            return self.neem_ids
        else:
            return self.filter_neems(query_string)

    def filter_neems(self, query_string):
        # FIXME: keywords are ignored at the moment
        #   - can an array be added to text index?
        #   - regex search possible, but only without index!!
        #       db.meta.find({"keywords": {"$regex": ".*robot.*","$options": 'i'}},{_id: 1}).pretty()
        try:
            mongoDBMetaCollection = get_mongo_db_meta_collection()
            if mongoDBMetaCollection is not None and mongoDBMetaCollection.count() > 0:
                return map(lambda doc: doc["_id"], mongoDBMetaCollection.find(
                    {"$text": {"$search": query_string}}, {"_id": 1}))

        except ConnectionFailure as e:
            app.logger.error('------------ mongoDb connection can not be created ------------')
            app.logger.error(e)

        except PyMongoError as e:
            app.logger.error('------------ mongoDb connection can not be created ------------')
            app.logger.error(e)

