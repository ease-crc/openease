
import os
from flask import session
from webrob.neems.neem import NEEM

NEEM_DIR="/neems"

class NEEM_Manager:
    def __init__(self):
        self.neem_ids=self.__get_neem_ids__()
        # TODO: remove again, just for testing pagination as long as only few neems
        #        are there
        self.neem_ids=self.neem_ids*20
    
    def __get_neem_ids__(self):
        neem_ids=[]
        neem_groups = os.listdir(NEEM_DIR)
        neem_groups.sort()
        for neem_group in neem_groups:
            group_dir = os.path.join(NEEM_DIR,neem_group)
            if os.path.isdir(group_dir):
                neem_names = os.listdir(group_dir)
                neem_names.sort()
                for neem_name in neem_names:
                    if os.path.isdir(os.path.join(group_dir,neem_name)):
                        neem_ids.append((neem_group,neem_name))
        return neem_ids
    
    def get_requested(self, request):
        neem_group = request.args.get('neem_group',
                                      default=session.get('neem_group',None))
        neem_name = request.args.get('neem_name',
                                     default=session.get('neem_name',None))
        if neem_group is None or neem_name is None:
            return None
        else:
            return self.get(neem_group,neem_name)
    
    def get(self, neem_group, neem_name):
        x = neem_name.split(':')
        if len(x)==1:
            neem_name_plain = neem_name
            neem_version = 'latest'
        else:
            neem_name_plain = x[0]
            neem_version = x[1]
        return NEEM(neem_group,
                    neem_name_plain,
                    neem_version,
                    repo_dir=NEEM_DIR)
    
    def query_neem_ids(self, query_string):
        if query_string is '':
            return self.neem_ids
        else:
            return self.filter_neems(self.neem_ids,query_string)
    
    def filter_neems(self, neem_ids, query_string):
        return neem_ids # TODO
        #return filter(lambda x: x.matches(query_string), neem_ids)
