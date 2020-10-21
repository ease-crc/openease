import os
from flask import session
from webrob.neems.neem import NEEM
from webrob.app_and_db import app, checkConnection, getNeemHubSettingFromDb

NEEM_DIR = "/neems"


class NEEM_Manager:
    def __init__(self):
        self.neem_ids = self.__get_neem_ids__()
        # TODO: remove again, just for testing pagination as long as only few neems
        #        are there
        self.neem_ids = self.neem_ids * 20

    def __get_neem_ids__(self):
        neem_ids = []
        # get all neem ids information from collection
        neemHubSettings = getNeemHubSettingFromDb()
        mongoDBMetaCollection = checkConnection(neemHubSettings)
        if mongoDBMetaCollection is not None:
            app.logger.info('neem ids are not  null .....')
            neem_ids = mongoDBMetaCollection.find().distinct('_id')
            app.logger.info(neem_ids)
            return neem_ids
        else:
            return []
    def get_requested(self, request):
        neem_id = request.args.get('neem_id',
                                      default=session.get('neem_id', None))
        if neem_id is None:
            return None
        else:
            return self.get(neem_id)

    def get(self, neem_id):
        app.logger.info('manager.py get neem....')
        app.logger.info(NEEM(neem_id))
        return NEEM(neem_id)

    def query_neem_ids(self, query_string):
        if query_string is '':
            app.logger.info('manager.py query self.neem_ids....')
            app.logger.info(self.neem_ids)
            return self.neem_ids
        else:
            app.logger.info('manager.py filter_neems(query_string)....')
            app.logger.info(self.filter_neems(query_string))
            return self.filter_neems(query_string)

    def filter_neems(self, query_string):
        # FIXME: keywords are ignored at the moment
        #   - can an array be added to text index?
        #   - regex search possible, but only without index!!
        #       db.meta.find({"keywords": {"$regex": ".*robot.*","$options": 'i'}},{_id: 1}).pretty()
        neemHubSettings = getNeemHubSettingFromDb()
        mongoDBMetaCollection = checkConnection(neemHubSettings)
        if mongoDBMetaCollection is not None:
            return map(lambda doc: doc["_id"], mongoDBMetaCollection.find(
                {"$text": {"$search": query_string}}, {"_id": 1}))
