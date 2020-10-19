from webrob.app_and_db import db

class NEEMHubSettings:
    def __init__(self):
        self.MONGO_HOST = "mongodb://data.open-ease.org"
        self.MONGO_PORT = 28015
        self.MONGO_DB = "neems"
        self.MONGO_USER = "neemReader"
        self.MONGO_PASS = "qEWRqc9UdN5TD7No7cjymUA8QEweNz"