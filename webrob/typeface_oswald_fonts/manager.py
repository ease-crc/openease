
import os
from flask import session
class FONTS_Manager:
    def __init__(self):
        self.fonts_ids=self.__get_fonts_ids__()
        # TODO: remove again, just for testing pagination as long as only few neems
        #        are there
        self.fonts_ids= self.fonts_ids * 20

    def __get_fonts_ids__(self):
        fonts_ids=[]
        fonts_groups = os.listdir(FONTS_DIR)
        fonts_groups.sort()
        for font_group in fonts_groups:
            group_dir = os.path.join(FONTS_DIR, font_group)
            if os.path.isdir(group_dir):
                font_names = os.listdir(group_dir)
                font_names.sort()
                for font_name in font_names:
                    if os.path.isdir(os.path.join(group_dir,font_name)):
                        fonts_ids.append((font_group,font_name))
        return fonts_ids

    def get_requested(self, request):
        fonts_group = request.args.get('fonts_group',
                                      default=session.get('fonts_group',None))
        fonts_name = request.args.get('fonts_name',
                                     default=session.get('fonts_name',None))
        if fonts_group is None or fonts_name is None:
            return None
        else:
            return self.get(fonts_group,fonts_name)

    def get(self, fonts_group, fonts_name, file_name):
        x = fonts_name.split(':')
        if len(x)==1:
            fonts_name_plain = fonts_name
            fonts_version = 'latest'
        else:
            fonts_name_plain = x[0]
            fonts_version = x[1]
        return FONTS(fonts_group,
                    fonts_name_plain,
                    fonts_version,
                    FONTS_DIR,
                     file_name)

    def query_fonts_ids(self, query_string):
        if query_string is '':
            return self.fonts_ids
        else:
            return self.filter_fonts(self.fonts_ids, query_string)

    def filter_fonts(self, fonts_ids, query_string):
        return fonts_ids # TODO
        #return filter(lambda x: x.matches(query_string), neem_ids)

from webrob.typeface_oswald_fonts.fonts import FONTS

FONTS_DIR= "/tmp"
