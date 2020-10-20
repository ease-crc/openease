from webrob.app_and_db import db


class NEEMHubSettings(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    MONGO_HOST = db.Column(db.String(255), nullable=False, default='')
    MONGO_PORT = db.Column(db.Integer())
    MONGO_DB = db.Column(db.String(255), nullable=False, default='')
    MONGO_USER = db.Column(db.String(255), nullable=False, default='')
    MONGO_PASS = db.Column(db.String(255), nullable=False, default='')


def get_settings(id):
    return NEEMHubSettings.query.filter_by(id=id).one()


def get_settings_count():
    return len(NEEMHubSettings.query.all())
