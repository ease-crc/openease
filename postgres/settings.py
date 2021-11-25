import enum

from datetime import datetime

from app_and_db import app, db

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
                           default='134.102.137.85')
    mongo_port = db.Column(db.Integer(), default=28015)
    mongo_db = db.Column(db.String(255), nullable=False, default='neems')
    mongo_user = db.Column(db.String(255), nullable=False, default='')
    mongo_pass = db.Column(db.String(255), nullable=False, default='')
    urdf_server = db.Column(db.String(255), nullable=False,
                            default='http://neem-1.informatik.uni-bremen.de/data/kinematics/')
    mesh_server = db.Column(db.String(255), nullable=False,
                            default='http://neem-1.informatik.uni-bremen.de/data/')
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


@enum.unique
class UpdateState(enum.Enum):
    PAUSED = 0
    ACTIVE = 1


@enum.unique
class UpdateMethod(enum.Enum):
    AUTOMATIC = 0
    MANUAL = 1
    NO_UPDATE = 2


@enum.unique
class ContentState(enum.Enum):
    DEFAULT = 0
    LATEST = 1
    BLANK = 2


class ContentSettings(db.Model):
    """
    DB model class for storing content settings into postgresql.
    """
    id = db.Column(db.Integer, primary_key=True)
    download_default_papers = db.Column(db.Boolean(), nullable=False, default=False)
    prepare_downloadable_files = db.Column(db.Boolean(), nullable=False, default=False)
    update_state_neem_overview = db.Column(db.Enum(UpdateState), nullable=False, default=UpdateState.PAUSED)
    update_state_publications_and_papers = db.Column(db.Enum(UpdateState), nullable=False, default=UpdateState.PAUSED)
    last_update_neem_overview = db.Column(db.DateTime(), nullable=False, default=datetime.min)
    last_update_publications_and_papers = db.Column(db.DateTime(), nullable=False, default=datetime.min)
    last_update_type_neem_overview = db.Column(db.Enum(UpdateMethod), nullable=False, default=UpdateMethod.NO_UPDATE)
    last_update_type_publications_and_papers = db.Column(db.Enum(UpdateMethod), nullable=False, default=UpdateMethod.NO_UPDATE)
    content_type_neem_overview = db.Column(db.Enum(ContentState), nullable=False, default=ContentState.DEFAULT)
    content_type_publications = db.Column(db.Enum(ContentState), nullable=False, default=ContentState.DEFAULT)
    content_type_papers = db.Column(db.Enum(ContentState), nullable=False, default=ContentState.BLANK)

    @staticmethod
    def create_first_entry():
        if db.session.query(ContentSettings).count() > 0:
            app.logger.info('First entry already available. Will not create new entry for content settings table.')

        first_entry = ContentSettings()
        db.session.add(first_entry)
        db.session.commit()

    @staticmethod
    def _get_first_entry():
        if db.session.query(ContentSettings).count() == 0:
            ContentSettings.create_first_entry()
        
        return ContentSettings.query.filter_by(id=FIRST_DOCUMENT_ID).one()

    @staticmethod
    def get_settings():
        return ContentSettings._get_first_entry()
    
    @staticmethod
    def set_download_default_papers(bool_value):
        ContentSettings._set_attribute('download_default_papers', bool_value)

    @staticmethod
    def set_prepare_downloadable_files(bool_value):
        ContentSettings._set_attribute('prepare_downloadable_files', bool_value)

    @staticmethod
    def set_update_state_neem_overview(update_state_enum_value):
        ContentSettings._set_attribute('update_state_neem_overview', update_state_enum_value)

    @staticmethod
    def set_update_state_publications_and_papers(update_state_enum_value):
        ContentSettings._set_attribute('update_state_publications_and_papers', update_state_enum_value)

    @staticmethod
    def set_last_update_neem_overview(datetime_value):
        ContentSettings._set_attribute('last_update_neem_overview', datetime_value)

    @staticmethod
    def set_last_update_publications_and_papers(datetime_value):
        ContentSettings._set_attribute('last_update_publications_and_papers', datetime_value)

    @staticmethod
    def set_last_update_type_neem_overview(update_method_enum_value):
        ContentSettings._set_attribute('last_update_type_neem_overview', update_method_enum_value)

    @staticmethod
    def set_last_update_type_publications_and_papers(update_method_enum_value):
        ContentSettings._set_attribute('last_update_type_publications_and_papers', update_method_enum_value)

    @staticmethod
    def set_content_type_neem_overview(content_state_enum_value):
        ContentSettings._set_attribute('content_type_neem_overview', content_state_enum_value)

    @staticmethod
    def set_content_type_publications(content_state_enum_value):
        ContentSettings._set_attribute('content_type_publications', content_state_enum_value)

    @staticmethod
    def set_content_type_papers(content_state_enum_value):
        ContentSettings._set_attribute('content_type_papers', content_state_enum_value)
        
    @staticmethod
    def _set_attribute(attr_as_str, value):
        content_settings = ContentSettings.get_settings()
        setattr(content_settings, attr_as_str, value)
        db.session.commit()

    @staticmethod
    def init_last_update_settings():
        """ Resets content type and last update time for all content. """
        content_settings = ContentSettings.get_settings()
        ContentSettings.last_update_neem_overview = datetime.min
        ContentSettings.last_update_publications_and_papers = datetime.min
        ContentSettings.last_update_type_neem_overview = UpdateMethod.NO_UPDATE
        ContentSettings.last_update_type_publications_and_papers = UpdateMethod.NO_UPDATE
        ContentSettings.content_type_neem_overview = ContentState.DEFAULT
        ContentSettings.content_type_publications = ContentState.DEFAULT
        ContentSettings.content_type_papers = ContentState.BLANK
        db.session.commit()

    @staticmethod
    def set_debug_settings():
        content_settings = ContentSettings.get_settings()
        ContentSettings.download_default_papers = False
        ContentSettings.prepare_downloadable_files = False
        ContentSettings.update_state_neem_overview = UpdateState.PAUSED
        ContentSettings.update_state_publications_and_papers = UpdateState.PAUSED
        db.session.commit()
