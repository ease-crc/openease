import os

from webrob.utility.path_handler import path_exists


def read_file(path):
    file_src = open(path, 'r')
    file_content = file_src.read()
    file_src.close()
    return file_content


def write_to_file(path, content):
    if path_exists(path) is False:
        raise IOError('File does not exist.')

    dst_f = open(path, 'w')
    dst_f.write(content)
    dst_f.close()


def create_file(path, content=None):
    if path_exists(path) is True:
        raise IOError('Files cannot be overwritten with this method.')

    dst_f = open(path, 'w+')
    if content is not None:
        dst_f.write(content)
    dst_f.close()


def remove_file(path):
    os.remove(path)
