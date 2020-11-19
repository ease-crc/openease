from app_and_db import db

FIRST_DOCUMENT_ID = 1


class NEEMHubSettings(db.Model):
    """
    DB model class for storing neem-hub configuration into postgresql.
    """
    id = db.Column(db.Integer, primary_key=True)
    MONGO_HOST = db.Column(db.String(255), nullable=False, default='data.open-ease.org')
    MONGO_PORT = db.Column(db.Integer(), default=28015)
    MONGO_DB = db.Column(db.String(255), nullable=False, default='neems')
    MONGO_USER = db.Column(db.String(255), nullable=False, default='')
    MONGO_PASS = db.Column(db.String(255), nullable=False, default='')


def get_neemhub_settings():
    """
    :return: neemhub settings from sql db(with id==1)
    """
    def query():
        return NEEMHubSettings.query.filter_by(id=FIRST_DOCUMENT_ID).one()

    try:
        # get the settings object, will raise exception is no settings were added to DB before
        return query()
    except:
        # create settings if no settings could be queried
        neemhub_settings = NEEMHubSettings()
        db.session.add(neemhub_settings)
        db.session.commit()
        # try again
        return query()
