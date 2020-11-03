from webrob.app_and_db import db, app
from sqlalchemy.exc import SQLAlchemyError

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


def get_settings(id):
    try:
        values = NEEMHubSettings.query.filter_by(id=id).one()
        return values

    except SQLAlchemyError as e:
        app.logger.info("get_settings: while connecting to sql db returns null")
        return NEEMHubSettings()

def get_settings_count():
    try:
        value = len(NEEMHubSettings.query.all())
        return value
    except SQLAlchemyError as e:
        app.logger.info("get_settings_count: while connecting to sql db returns ZERO count")
        return 0