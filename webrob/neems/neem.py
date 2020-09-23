from flask_user import current_user

from webrob.docker.docker_interface import start_user_container
from webrob.app_and_db import app, mongoDBMetaCollection

import bson

class NEEM:
    def __init__(self,
                 neem_id):
        # collect neem by id
        if isinstance(neem_id, unicode):
            app.logger.info('getting neem from mongo with id: ')
            app.logger.info(neem_id)
            b_id = bson.objectid.ObjectId(neem_id)
        else:
            b_id = neem_id
        neem = mongoDBMetaCollection.find_one({"_id": b_id})
        app.logger.info(neem)
        self.neem_id = str(neem['_id'])
        app.logger.info("self neemid is ")
        app.logger.info(type(self.neem_id))
        # TODO: Tag could be useful for versioning
        self.neem_tag = ''
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
        self.keywords = neem['keywords']
        self.image = 'https://blogs.3ds.com/northamerica/wp-content/uploads/sites/4/2019/08/Robots-Square-300x300.jpg'

    def get_info(self):
        return {
            'neem_id': self.neem_id,
            'neem_tag': self.neem_tag,
            'name': self.name,
            'description': self.description,
            'maintainer': self.maintainer,
            'authors': self.authors,
            'acknowledgements': self.acknowledgements,
            'image': 'https://blogs.3ds.com/northamerica/wp-content/uploads/sites/4/2019/08/Robots-Square-300x300.jpg',
            'image_tag': self.knowrob_tag,
            'environments': self.environments,
            'activities': self.activities,
            'agents': self.agents,
            'downloadUrl': self.downloadUrl,
            'keywords': self.keywords
        }

    def checkout(self):
        app.logger.info('Checkout neem')
        pass

    def activate(self):
        app.logger.info('Activate neem')
        start_user_container(current_user.username,
                             self.neem_id,
                             self.neem_tag,
                             self.knowrob_image,
                             self.knowrob_tag)

    def matches(self, query_string):
        # TODO
        return True