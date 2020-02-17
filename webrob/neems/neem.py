import os
import yaml
from flask import session
from flask_user import current_user

from webrob.docker.docker_interface import start_user_container

class NEEM:
    def __init__(self,
                 repo_group,
                 repo_name,
                 repo_tag,
                 repo_dir):
        self.repo_group    = repo_group
        self.repo_name     = repo_name
        self.repo_path     = repo_group+"/"+repo_name
        self.repo_tag      = repo_tag
        self.repo_dir      = repo_dir
        self.knowrob_image = 'knowrob'
        self.knowrob_tag   = 'latest'
        #
        with open(os.path.join(self.get_directory(), 'NEEM.yaml')) as yaml_file:
            yaml_data = yaml.load(yaml_file)
            self.name             = yaml_data.get('name',None)
            self.description      = yaml_data.get('description',None)
            self.maintainer       = yaml_data.get('maintainer',None)
            self.authors          = yaml_data.get('authors',None)
            self.acknowledgements = yaml_data.get('acknowledgements',[])
            self.namespaces       = yaml_data.get('namespaces',[])
            self.imports          = yaml_data.get('imports',[])
            self.environments     = yaml_data.get('environments',[])
            self.activities       = yaml_data.get('activities',[])
            self.agents           = yaml_data.get('agents',[])
            ##
            image = yaml_data.get('image',None)
            #if image!=None:
                #self.knowrob_image = image.get('name',self.knowrob_image)
                # TODO: handle knowrob version
                #self.knowrob_tag   = image.get('tag',self.knowrob_tag)
    
    def get_directory(self):
        return os.path.join(self.repo_dir, self.repo_group+'/'+self.repo_name)
    
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
