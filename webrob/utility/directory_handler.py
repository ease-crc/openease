import os
import shutil


def mk_dir(path):
    os.mkdir(path)


def make_dirs(path):
    os.makedirs(path)


def rm_empty_dir(path):
    os.rmdir(path)


def rm_nonempty_dir(path):
    shutil.rmtree(path)


def ch_dir(path):
    os.chdir(path)


def get_current_working_directory():
    return os.getcwd()


def list_directories(path):
    return os.listdir(path)


def walk_directories(top, topdown=True, onerror=None, followlinks=False):
    return os.walk(top, topdown, onerror, followlinks)
