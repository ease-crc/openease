from flask_user import current_user

from webrob.docker.docker_interface import start_user_container, container_started
from webrob.app_and_db import app, get_mongo_db_meta_collection
from pymongo.errors import ConnectionFailure, PyMongoError

from webrob.config.settings import USE_HOST_KNOWROB
import bson
import json
from dateutil import parser
from webrob.AlchemyEncoder import AlchemyEncoder
from webrob.models.NEEMHubSettings import get_settings
from sqlalchemy.exc import SQLAlchemyError

NEEM_DOWNLOAD_URL_PREFIX = "https://neemgit.informatik.uni-bremen.de/"

class NEEM:
    def __init__(self,
                 neem_id):
        # collect neem by id
        if isinstance(neem_id, unicode):
            b_id = bson.objectid.ObjectId(neem_id)
        else:
            b_id = neem_id

        try:
            mongoDBMetaCollection = get_mongo_db_meta_collection()
            if mongoDBMetaCollection is not None and mongoDBMetaCollection.count() > 0:
                neem = mongoDBMetaCollection.find_one({"_id": b_id})
                self.neem_id = str(neem['_id'])
                # TODO: Tag could be useful for versioning
                self.neem_tag = ''
                self.name = neem['name']
                self.description = neem['description']
                self.created_by = neem['created_by']
                self.created_at = parser.parse(neem['created_at']).strftime('%m/%d/%y %H:%M')
                self.model_version = neem['model_version']
                self.downloadUrl = NEEM_DOWNLOAD_URL_PREFIX + neem['url']
                self.neem_repo_path = neem['url']
                self.knowrob_image = 'knowrob'
                self.knowrob_tag = 'latest'
                self.maintainer = neem['created_by']
                self.authors = neem['created_by']
                self.acknowledgements = ''
                self.environment = neem['environment']
                self.activity = neem['activity']
                self.agent = neem['agent']
                self.keywords = neem['keywords']
                self.image = neem['image']
        except ConnectionFailure as e:
            app.logger.error('------------ mongoDb connection can not be created ------------')
            app.logger.error(e)

        except PyMongoError as e:
            app.logger.error('------------ mongoDb connection can not be created ------------')
            app.logger.error(e)

    def get_info(self):
        return {
            'neem_id': self.neem_id,
            'neem_tag': self.neem_tag,
            'name': self.name,
            'description': self.description,
            'maintainer': self.maintainer,
            'authors': self.authors,
            'acknowledgements': self.acknowledgements,
            'image': self.image,
            'image_tag': self.knowrob_tag,
            'environment': self.environment,
            'activity': self.activity,
            'agent': self.agent,
            'downloadUrl': self.downloadUrl,
            'keywords': self.keywords,
            'neem_repo_path': self.neem_repo_path
        }

    def activate(self):
        app.logger.info('Activate neem')
        try:
            neemHubSettings = get_settings()
            if not USE_HOST_KNOWROB and not container_started(current_user.username) and neemHubSettings:
                start_user_container(current_user.username,
                                     self.neem_id,
                                     json.dumps(neemHubSettings, cls=AlchemyEncoder),
                                     self.neem_tag,
                                     self.knowrob_image,
                                     self.knowrob_tag)
        except SQLAlchemyError as e:
            app.logger.error("while connecting to sql db returns null")
            app.logger.error(e)

    def matches(self, query_string):
        # TODO
        return True
