import re

from webrob.utility.directory_handler import make_dirs
from webrob.utility.file_handler import read_file, create_file
from webrob.utility.path_handler import path_exists, get_parent_dir_name


def copy_template_file_and_replace_keywords(src, dst, args):
    template = read_file(src)
    _create_parent_dir(dst)
    _copy_file_and_replace_keywords(dst, template, args)


def _create_parent_dir(dst):
    parent = get_parent_dir_name(dst)
    if not path_exists(parent):
        make_dirs(parent)


def _copy_file_and_replace_keywords(dst, template, args):
    content = _format_template(template, args)
    create_file(dst, content)


def _format_template(template, args):
    if _get_number_of_template_fillers(template) < len(args):
        raise IndexError('number of arguments for str.format() is more than the number of fillers in the template')
    elif _get_number_of_template_fillers(template) > len(args):
        raise IndexError('number of arguments for str.format() is less than the number of fillers in the template')
    return template.format(*args)


def _get_number_of_template_fillers(template):
    # find all occurences of {[integer number]} in template
    string_matches = re.findall('({[0-9]+})', template)
    return len(string_matches)
