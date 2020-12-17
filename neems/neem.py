from flask_user import current_user

from knowrob.container import start_user_container, container_started
from app_and_db import app

from config.settings import USE_HOST_KNOWROB
import json
from dateutil import parser
from postgres.AlchemyEncoder import AlchemyEncoder
from postgres.settings import get_neemhub_settings

NEEM_DOWNLOAD_URL_PREFIX = "https://neemgit.informatik.uni-bremen.de/"

class NEEM:
    def __init__(self, neem_hub, neem_info):

        self.neem_hub = neem_hub
        self.neem_id = str(neem_info['_id'])
        # TODO: Tag could be useful for versioning
        self.neem_tag = ''
        self.name = neem_info['name']
        self.description = neem_info['description']
        self.created_by = neem_info['created_by']
        self.created_at = parser.parse(neem_info['created_at']).strftime('%m/%d/%y %H:%M')
        self.downloadUrl = NEEM_DOWNLOAD_URL_PREFIX + neem_info['url']
        self.neem_repo_path = neem_info['url']
        self.maintainer = neem_info['created_by']
        self.authors = neem_info['created_by']
        self.acknowledgements = ''
        self.keywords = neem_info['keywords']
        if 'visibility' in neem_info:
            self.visibility = neem_info['visibility']
        else:
            self.visibility = False

        if 'image' in neem_info:
            self.image = neem_info['image']
        else:
            self.image = 'None'

        if 'model_version' in neem_info:
            self.model_version = neem_info['model_version']
        else:
            self.model_version = '1.0'

        if 'environment' in neem_info:
            self.environment = neem_info['environment']
        else:
            self.environment = 'Unknown'

        if 'activity' in neem_info:
            self.activity = neem_info['activity']
        else:
            self.activity = 'Unknown'

        if 'agent' in neem_info:
            self.agent = neem_info['agent']
        else:
            self.agent = 'Unknown'

    def get_info(self):
        return {
            'neem_id': self.neem_id,
            'neem_tag': self.neem_tag,
            'name': self.name,
            'description': self.description,
            'maintainer': self.maintainer,
            'authors': self.authors,
            'acknowledgements': self.acknowledgements,
            'environment': self.environment,
            'activity': self.activity,
            'agent': self.agent,
            'downloadUrl': self.downloadUrl,
            'keywords': self.keywords,
            'neem_repo_path': self.neem_repo_path
        }

    def activate(self):
        app.logger.info('Activate neem')
        if not USE_HOST_KNOWROB and not container_started(current_user.username):
            sql = get_neemhub_settings()
            start_user_container(current_user.username,
                                 self.neem_id,
                                 json.dumps(sql, cls=AlchemyEncoder),
                                 self.neem_tag,
                                 "knowrob",
                                 "latest")
