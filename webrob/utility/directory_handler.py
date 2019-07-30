import os
import shutil


def create_directory(path):
    os.mkdir(path)


def create_directories_from_path(path):
    os.makedirs(path)


def remove_empty_directory(path):
    os.rmdir(path)


def remove_nonempty_directory(path):
    shutil.rmtree(path)


def change_directory(path):
    os.chdir(path)


def get_current_working_directory():
    return os.getcwd()


def list_directories(path):
    return os.listdir(path)


def walk_directories(top, topdown=True, onerror=None, followlinks=False):
    return os.walk(top, topdown, onerror, followlinks)
