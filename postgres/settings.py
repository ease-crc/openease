from app_and_db import db

FIRST_DOCUMENT_ID = 1


class OAuthModel(db.Model):
    """
    DB model class for storing OAuth configuration into postgresql.
    """
    id = db.Column(db.Integer, primary_key=True)
    # Twitter
    twitter_app_id = db.Column(db.String(255), nullable=False, default='')
    twitter_app_secret = db.Column(db.String(255), nullable=False, default='')
    # GitHub
    github_app_id = db.Column(db.String(255), nullable=False, default='')
    github_app_secret = db.Column(db.String(255), nullable=False, default='')
    # Google
    google_app_id = db.Column(db.String(255), nullable=False, default='')
    google_app_secret = db.Column(db.String(255), nullable=False, default='')

    @staticmethod
    def first():
        try:
            return OAuthModel.query.filter_by(id=FIRST_DOCUMENT_ID).one()
        except:
            x = OAuthModel()
            db.session.add(x)
            db.session.commit()
            return OAuthModel.first()

    @staticmethod
    def get_settings():
        that = OAuthModel.first()
        return {
            'twitter': (that.twitter_app_id, that.twitter_app_secret),
            'github': (that.github_app_id, that.github_app_secret),
            'google': (that.google_app_id, that.google_app_secret)
        }


class NEEMHubSettings(db.Model):
    """
    DB model class for storing neem-hub configuration into postgresql.
    """
    id = db.Column(db.Integer, primary_key=True)
    mongo_host = db.Column(db.String(255), nullable=False,
                           default='data.open-ease.org')
    mongo_port = db.Column(db.Integer(), default=28015)
    mongo_db = db.Column(db.String(255), nullable=False, default='neems')
    mongo_user = db.Column(db.String(255), nullable=False, default='')
    mongo_pass = db.Column(db.String(255), nullable=False, default='')
    urdf_server = db.Column(db.String(255), nullable=False,
                            default='http://neem-1.informatik.uni-bremen.de/data/kinematics/')
    visibility_flag = db.Column(db.Boolean(), nullable=False, default=False)


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
