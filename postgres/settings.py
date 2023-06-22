import enum

from datetime import datetime
from flask import flash

from app_and_db import app, db
from config.settings import LOCAL_PUBLICATIONS_AND_PAPERS
from db import table_empty
from helpers.file_handler import get_file_extension, path_is_file
from helpers.utility import type_str, is_url

FIRST_DOCUMENT_ID = 1
DATETIME_MIN = datetime.min

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
                           default='neem-3.informatik.uni-bremen.de')
    mongo_port = db.Column(db.Integer(), default=28015)
    mongo_db = db.Column(db.String(255), nullable=False, default='neems')
    mongo_user = db.Column(db.String(255), nullable=False, default='')
    mongo_pass = db.Column(db.String(255), nullable=False, default='')
    urdf_server = db.Column(db.String(255), nullable=False,
                            default='http://neem-data.informatik.uni-bremen.de/data/kinematics/')
    mesh_server = db.Column(db.String(255), nullable=False,
                            default='http://neem-data.informatik.uni-bremen.de/data/')
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
    NO_UPDATE = 0
    MANUAL = 1
    AUTOMATIC = 2


@enum.unique
class ContentState(enum.Enum):
    NONE = 0
    DEFAULT = 1
    LATEST = 2


class ContentSettings(db.Model):
    """
    DB model class for storing content settings into postgresql.
    """
    id = db.Column(db.Integer, primary_key=True)
    download_default_papers = db.Column(db.Boolean(), nullable=False, default=False)
    prepare_downloadable_files = db.Column(db.Boolean(), nullable=False, default=False)
    update_state_neem_overview = db.Column(db.Enum(UpdateState), nullable=False, default=UpdateState.PAUSED)
    update_state_publications_and_papers = db.Column(db.Enum(UpdateState), nullable=False, default=UpdateState.PAUSED)
    last_update_neem_overview = db.Column(db.DateTime(), nullable=False, default=DATETIME_MIN)
    last_update_publications_and_papers = db.Column(db.DateTime(), nullable=False, default=DATETIME_MIN)
    last_update_type_neem_overview = db.Column(db.Enum(UpdateMethod), nullable=False, default=UpdateMethod.NO_UPDATE)
    last_update_type_publications_and_papers = db.Column(db.Enum(UpdateMethod), nullable=False, default=UpdateMethod.NO_UPDATE)
    content_type_neem_overview = db.Column(db.Enum(ContentState), nullable=False, default=ContentState.NONE)
    content_type_publications = db.Column(db.Enum(ContentState), nullable=False, default=ContentState.NONE)
    content_type_papers = db.Column(db.Enum(ContentState), nullable=False, default=ContentState.NONE)
    publications_bibtex_url = db.Column(db.String(255), nullable=False, default='')
    default_papers_zip_url = db.Column(db.String(255), nullable=False, default='')
    papers_zip_url = db.Column(db.String(255), nullable=False, default='')
    news_cms_url = db.Column(db.String(255), nullable=False, default='')

    @staticmethod
    def create_first_entry():
        if not table_empty(ContentSettings):
            app.logger.info('First entry already available. Will not create new entry for content settings table.')

        first_entry = ContentSettings()
        db.session.add(first_entry)
        db.session.commit()

    @staticmethod
    def _get_first_entry():
        if table_empty(ContentSettings):
            ContentSettings.create_first_entry()
        
        return ContentSettings.query.filter_by(id=FIRST_DOCUMENT_ID).first()

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
    def _str_is_url_or_valid_file(str_value, valid_file_extension):
        if not str_value:
            app.logger.info('String is empty. Will not update field.')
            return False
        if not is_url(str_value) and not path_is_file(LOCAL_PUBLICATIONS_AND_PAPERS + str_value):
            flash(str_value + ': Cannot find file at correct path. Will not update field.')
            app.logger.info('Cannot find file at correct path. Will not update field.')
            return False
        if not is_url(str_value) and not get_file_extension(str_value) == valid_file_extension:
            flash(str_value + ': File has the wrong extension. Will not update field.')
            app.logger.info('File has the wrong extension. Will not update field.')
            return False
        return True

    @staticmethod
    def _set_url_attribute_if_str_is_valid(attribute, str_value, valid_file_extension):
        content_settings = ContentSettings.get_settings()
        if getattr(content_settings, attribute) and not str_value:
            pass
        elif not ContentSettings._str_is_url_or_valid_file(str_value, valid_file_extension):
            return
        
        ContentSettings._set_attribute(attribute, str_value)

    @staticmethod
    def set_default_papers_zip_url(str_value):
        ContentSettings._set_url_attribute_if_str_is_valid('default_papers_zip_url', str_value, '.zip')

    @staticmethod
    def set_papers_zip_url(str_value):        
        ContentSettings._set_url_attribute_if_str_is_valid('papers_zip_url', str_value, '.zip')

    @staticmethod
    def set_publications_bibtex_url(str_value):
        ContentSettings._set_url_attribute_if_str_is_valid('publications_bibtex_url', str_value, '.bib')

    @staticmethod
    def set_news_cms_url(str_value):
        content_settings = ContentSettings.get_settings()
        if getattr(content_settings, 'news_cms_url') and not str_value:
            pass
        elif not str_value or not is_url(str_value):
            flash(str_value + ': String is not an url. Will not update field.')
            app.logger.info('String is not an url. Will not update field.')
            return

        ContentSettings._set_attribute('news_cms_url', str_value)
        
    @staticmethod
    def _set_attribute(attr_as_str, value):
        content_settings = ContentSettings.get_settings()

        if not isinstance(value, type(getattr(content_settings, attr_as_str))):
            app.logger.error('Wrong parameter type passed for content-settings ' + attr_as_str + '\nExpected ' + type_str(getattr(content_settings, attr_as_str)) + ', received ' + type_str(value))
            return

        setattr(content_settings, attr_as_str, value)
        db.session.commit()

    @staticmethod
    def init_last_update_settings(neem_overview_content_exists , publications_content_exists):
        """ Resets content type and last update time for all content. """
        settings = ContentSettings.get_settings()
        settings.last_update_neem_overview = datetime.min
        settings.last_update_publications_and_papers = datetime.min
        settings.last_update_type_neem_overview = UpdateMethod.NO_UPDATE
        settings.last_update_type_publications_and_papers = UpdateMethod.NO_UPDATE
        if not neem_overview_content_exists:
            settings.content_type_neem_overview = ContentState.NONE
        if not publications_content_exists:
            settings.content_type_publications = ContentState.NONE
            settings.content_type_papers = ContentState.NONE
        db.session.commit()

    @staticmethod
    def set_debug_settings():
        settings = ContentSettings.get_settings()
        settings.download_default_papers = False
        settings.prepare_downloadable_files = False
        settings.update_state_neem_overview = UpdateState.PAUSED
        settings.update_state_publications_and_papers = UpdateState.PAUSED
        db.session.commit()
