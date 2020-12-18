from flask import session
import bson
from pymongo import MongoClient
from neems.neem import NEEM
from app_and_db import db, app
from postgres.settings import get_neemhub_settings


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
        self.mongo_host = sql.mongo_host
        self.mongo_port = int(sql.mongo_port)
        self.mongo_db_name = sql.mongo_db
        self.mongo_user = sql.mongo_user
        self.mongo_pass = sql.mongo_pass
        self.visibility_flag = sql.visibility_flag
        self.urdf_server = sql.urdf_server
        self.mesh_server = sql.mesh_server
        self.mongo_client = None
        self.mongo_db = None

    def store_settings(self):
        sql = get_neemhub_settings()
        sql.mongo_host = self.mongo_host
        sql.mongo_port = self.mongo_port
        sql.mongo_db = self.mongo_db_name
        sql.mongo_user = self.mongo_user
        sql.mongo_pass = self.mongo_pass
        sql.visibility_flag = self.visibility_flag
        sql.urdf_server = self.urdf_server
        sql.mesh_server = self.mesh_server
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

    def get_neem_visibility_flag(self):
        return self.visibility_flag

    def set_neem_visibility_flag(self, visibility_flag):
        self.visibility_flag = visibility_flag

    def get_urdf_server(self):
        return self.urdf_server

    def set_urdf_server(self, urdf_server):
        self.urdf_server = urdf_server

    def get_mesh_server(self):
        return self.mesh_server

    def set_mesh_server(self, mesh_server):
        self.mesh_server = mesh_server

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

    def get_neem_ids(self, query_string, visibility_flag):
        # FIXME: keywords are ignored at the moment
        #   - can an array be added to text index?
        #   - regex search possible, but only without index!!
        #       db.meta.find({"keywords": {"$regex": ".*robot.*","$options": 'i'}},{_id: 1}).pretty()
        mongo = self.connect_mongo()
        if visibility_flag and query_string is '':
            return map(lambda doc: doc["_id"], mongo.find(
                {"visibility": True}, {"_id": 1}))
        elif visibility_flag:
            return map(lambda doc: doc["_id"], mongo.find(
                {"$text": {"$search": query_string}, "visibility": True}, {"_id": 1}))
        elif query_string is '':
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
            session['neem_id'] = neem_id
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
