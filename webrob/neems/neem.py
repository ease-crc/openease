import os
import yaml
from flask import session
from flask_user import current_user

from webrob.docker.docker_interface import start_user_container
from webrob.app_and_db import app, mongoDBMetaCollection


class NEEM:
    def __init__(self,
                 neem_id):
        # collect neem by id
        if neem_id is None:
            return None
        else:
            neem = mongoDBMetaCollection.find_one({"_id": neem_id})

        if neem is None:
            return None
        else:
            self.neem_id = neem['_id']
            self.name = neem['name']
            self.description = neem['description']
            self.created_by = neem['created_by']
            self.created_at = neem['created_at']
            self.model_version = neem['model_version']
            self.downloadUrl = neem['url']
            self.knowrob_image = 'knowrob'
            self.knowrob_tag = 'latest'
            self.maintainer = neem['created_by']
            self.authors = neem['created_by']
            self.acknowledgements = ''
            self.environments = ''
            self.activities = ''
            self.agents = ''

    def get_info(self):
        return {
            'neem_id': self.neem_id,
            'name': self.name,
            'description': self.description,
            'maintainer': self.maintainer,
            'authors': self.authors,
            'acknowledgements': self.acknowledgements,
            'image': self.knowrob_image,
            'image_tag': self.knowrob_tag,
            'environments': self.environments,
            'activities': self.activities,
            'agents': self.agents,
            'downloadUrl': self.downloadUrl
        }

    def checkout(self):
        pass

    def activate(self):
        pass
        #session['neem_group'] = self.repo_group
        #session['neem_name'] = self.repo_name  # + ":" + self.repo_tag


    def matches(self, query_string):
        # TODO
        return True
