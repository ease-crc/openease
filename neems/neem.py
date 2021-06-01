import requests

from flask_user import current_user

from knowrob.container import start_user_container, container_started
from app_and_db import app

from config.settings import USE_HOST_KNOWROB
import json
from dateutil import parser
from postgres.AlchemyEncoder import AlchemyEncoder
from postgres.settings import get_neemhub_settings

NEEM_DOWNLOAD_URL_PREFIX = "https://neemgit.informatik.uni-bremen.de/"
DEFAULT_IMAGE_PATH = 'static/img/default.jpg'
DEFAULT_IMAGE_PATH_NO_STATIC = 'img/default.jpg'

class NEEM:
    def __init__(self, neem_hub, neem_info):

        self.neem_hub = neem_hub
        self.neem_id = str(neem_info['_id'])
        self.name = neem_info['name']
        self.description = neem_info['description']
        self.created_by = neem_info['created_by']
        self.created_at = parser.parse(neem_info['created_at']).strftime('%m/%d/%y %H:%M')
        self.maintainer = neem_info['created_by']

        if 'mail' in neem_info:
            self.mail = neem_info['mail']
        else:
            self.mail = None

        self.downloadUrl = neem_info['url']
        if not self.downloadUrl.startswith('http'):
            self.downloadUrl += NEEM_DOWNLOAD_URL_PREFIX

        if 'repo' in neem_info:
            self.neem_repo_path = neem_info['repo']
        else:
            self.neem_repo_path = self.downloadUrl.split("/")[-1]

        if 'projects' in neem_info:
            self.projects = neem_info['projects']
        else:
            self.projects = []

        if 'visibility' in neem_info:
            self.visibility = neem_info['visibility']
        else:
            self.visibility = False

        if 'image' in neem_info:
            self.image = neem_info['image']
        else:
            self.image = DEFAULT_IMAGE_PATH

    def fetch_last_updated(self):
        api_prefix = NEEM_DOWNLOAD_URL_PREFIX + "api/v4/projects/neems%2F"
        api_suffix = "/repository/commits/master"
        url = api_prefix + self.downloadUrl.split("/")[-1] + api_suffix
        response = json.loads(str(requests.get(url, allow_redirects=True).content))
        
        if 'committed_date' in response:
            return response['committed_date']
        else:
            return 0

    def get_info(self):
        return {
            'neem_id': self.neem_id,
            'name': self.name,
            'description': self.description,
            'maintainer': self.maintainer,
            'downloadUrl': self.downloadUrl,
            'neem_repo_path': self.neem_repo_path,
            'image': self.image
        }
    
    def get_info_with_last_updated(self):
        info = self.get_info()
        info['last_updated'] = self.fetch_last_updated()
        return info

    def activate(self):
        app.logger.info('Activate neem')
        if not USE_HOST_KNOWROB and not container_started(current_user.username):
            sql = get_neemhub_settings()
            start_user_container(current_user.username,
                                 json.dumps(sql, cls=AlchemyEncoder),
                                 "knowrob",
                                 "latest")
