import os
from flask import session
from webrob.neems.neem import NEEM
from webrob.app_and_db import app, getNeemHubSettingFromDb, checkConnection

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
        if checkConnection(neemHubSettings) is not None:
            neem_ids = checkConnection(neemHubSettings).find().distinct('_id')
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
        neemHubSettings = getNeemHubSettingFromDb()
        if checkConnection(neemHubSettings) is not None:
            return map(lambda doc: doc["_id"], checkConnection(neemHubSettings).find(
                {"$text": {"$search": query_string}}, {"_id": 1}))
