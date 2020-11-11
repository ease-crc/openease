from webrob.app_and_db import db, app

# DB model class for storing neem-hub configuration into postgresql.
# Here at the moment only one entry should be stored in this table for neem-hub settings

# default settings id points to db id here, since we are only retrieving first raw of the table
settings_Id = 1

class NEEMHubSettings(db.Model):
    def __init__(self):
        self.MONGO_HOST = ""
        self.MONGO_PORT = 0
        self.MONGO_DB = ""
        self.MONGO_USER = ""
        self.MONGO_PASS = ""

    id = db.Column(db.Integer, primary_key=True)
    MONGO_HOST = db.Column(db.String(255), nullable=False, default='')
    MONGO_PORT = db.Column(db.Integer())
    MONGO_DB = db.Column(db.String(255), nullable=False, default='')
    MONGO_USER = db.Column(db.String(255), nullable=False, default='')
    MONGO_PASS = db.Column(db.String(255), nullable=False, default='')

# get setting values from db(with id==1)
def get_settings():
        return NEEMHubSettings.query.filter_by(id=settings_Id).one()