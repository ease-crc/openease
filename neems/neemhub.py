from flask import session
import bson
from pymongo import MongoClient
from neems.neem import NEEM
from app_and_db import db
from models.settings import get_neemhub_settings


class NEEMHubConnectionError(Exception):
    """
    An error indicating that openEASE cannot connect to the NEEMHub.
    Two main reasons for this error are:
    1. The NEEMHub is not configured properly.
    2. The NEEMHub is down
    """
    def __init__(self, exc):
        Exception.__init__(self)
        self.exc = exc

    def __str__(self):
        return self.exc.__str__()


class NEEMHub:
    def __init__(self):
        # get the SQL settings object
        sql = get_neemhub_settings()
        self.mongo_host = sql.MONGO_HOST
        self.mongo_port = int(sql.MONGO_PORT)
        self.mongo_db_name = sql.MONGO_DB
        self.mongo_user = sql.MONGO_USER
        self.mongo_pass = sql.MONGO_PASS
        self.mongo_client = None
        self.mongo_db = None

    def store_settings(self):
        sql = get_neemhub_settings()
        sql.MONGO_HOST = self.mongo_host
        sql.MONGO_PORT = self.mongo_port
        sql.MONGO_DB = self.mongo_db_name
        sql.MONGO_USER = self.mongo_user
        sql.MONGO_PASS = self.mongo_pass
        self.mongo_db = None
        db.session.commit()

    def get_mongo_host(self):
        return self.mongo_host

    def set_mongo_host(self, host):
        self.mongo_host = host

    def get_mongo_port(self):
        return self.mongo_port

    def set_mongo_port(self, port):
        self.mongo_port = int(port)

    def get_mongo_db(self):
        return self.mongo_db_name

    def set_mongo_db(self, db_name):
        self.mongo_db_name = db_name

    def get_mongo_user(self):
        return self.mongo_user

    def set_mongo_user(self, user):
        self.mongo_user = user

    def get_mongo_pass(self):
        return self.mongo_pass

    def set_mongo_pass(self, password):
        self.mongo_pass = password

    def connect_mongo(self):
        try:
            if self.mongo_db is None:
                self.mongo_client = MongoClient(self.mongo_host, self.mongo_port)
                self.mongo_db = self.mongo_client[self.mongo_db_name]
                self.mongo_db.authenticate(self.mongo_user, self.mongo_pass)
            return self.mongo_db["meta"]
        except Exception as exc:
            self.mongo_db = None
            raise NEEMHubConnectionError(exc)

    def get_neem_ids(self, query_string):
        # FIXME: keywords are ignored at the moment
        #   - can an array be added to text index?
        #   - regex search possible, but only without index!!
        #       db.meta.find({"keywords": {"$regex": ".*robot.*","$options": 'i'}},{_id: 1}).pretty()
        mongo = self.connect_mongo()
        if query_string is '':
            return mongo.find().distinct('_id')
        else:
            return map(lambda doc: doc["_id"], mongo.find(
                {"$text": {"$search": query_string}}, {"_id": 1}))

    def get_requested_neem(self, request):
        neem_id = request.args.get('neem_id',
                                   default=session.get('neem_id', None))
        if neem_id is None:
            return None
        else:
            return self.get_neem(neem_id)

    def get_neem(self, neem_id):
        mongo = self.connect_mongo()
        if isinstance(neem_id, unicode):
            neem_id = bson.objectid.ObjectId(neem_id)

        neem_info = mongo.find_one({"_id": neem_id})
        if neem_info is None:
            return None
        else:
            return NEEM(self, neem_info)


# create a shared NEEMHb instance
instance = NEEMHub()
