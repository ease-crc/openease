import os
import yaml
from flask import session
from flask_user import current_user

from webrob.docker.docker_interface import start_user_container

class NEEM:
    def __init__(self,
                 name,
                 description,
                 createdBy):
        self.name           = name
        self.description    = description
        self.createdBy      = createdBy
        self.knowrob_image  = 'knowrob'
        self.knowrob_tag    = 'latest'
        self.maintainer     = createdBy
        self.authors        = createdBy
        self.acknowledgements = ''
        self.environments   = ''
        self.activities     = ''
        self.agents         = ''

    def get_info(self):
        return {
            'name':             self.name,
            'description':      self.description,
            'maintainer':       self.maintainer,
            'authors':          self.authors,
            'acknowledgements': self.acknowledgements,
            'image':            self.knowrob_image,
            'image_tag':        self.knowrob_tag,
            'environments':     self.environments,
            'activities':       self.activities,
            'agents':           self.agents
        }
    
    def checkout(self):
        pass
    
    def activate(self):
        session['neem_group'] = self.repo_group
        session['neem_name']  = self.repo_name #+ ":" + self.repo_tag
        start_user_container(current_user.username,
                             self.repo_group,
                             self.repo_name,
                             self.repo_tag,
                             self.knowrob_image,
                             self.knowrob_tag)
    
    def matches(self,query_string):
        # TODO
        return True
