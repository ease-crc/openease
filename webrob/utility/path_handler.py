import os
import posixpath


def join_paths(path, *paths):
    return os.path.join(path, *paths)


def path_exists(path):
    return os.path.exists(path)


def absolute_path(path):
    return os.path.abspath(path)


def relative_path(path, start):
    return os.path.relpath(path, start)


def get_parent_dir_name(path):
    return os.path.dirname(path)


def get_path_basename(path):
    return os.path.basename(path)


def get_unix_style_path_basename(path):
    return posixpath.basename(path)


def is_directory(path):
    return os.path.isdir(path)


def get_path_size(path):
    return os.path.getsize(path)


def split_path(path):
    return os.path.split(path)


def split_extension(path):
    return os.path.splitext(path)
