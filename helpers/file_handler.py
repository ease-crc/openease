import json
import shutil

from pathlib2 import Path
from zipfile import ZipFile

from app_and_db import app
from config.settings import WEBROB_PATH


def copy_file(src, dest):
    shutil.copy(src, dest)


def copy_dir(src, dest):
    shutil.copytree(src, dest)


def path_is_file(path):
    return Path(path).is_file()


def path_is_dir(path):
    return Path(path).is_dir()


def get_path_parent(path):
    return Path(path).parent


def get_path_stem(path):
    return Path(path).stem


def get_path_name(path):
    return Path(path).name


def get_file_extension(file_path):
    '''file_path can be either a file name or a path to the file'''
    return Path(file_path).suffix


def remove_empty_dir(path):
    Path(path).rmdir()


def remove_dir_with_contents(path):
    shutil.rmtree(path)


def remove_if_is_dir(path):
    if path_is_dir(path):
        remove_dir_with_contents(path)


def remove_file(path):
    Path(path).unlink()


def remove_if_is_file(path):
    if path_is_file(path):
        remove_file(path)


def number_of_subdirs_in_dir(dir_path):
    return _number_of_specified_items_in_dir(dir_path, path_is_dir)


def number_of_files_in_dir(dir_path):
    return _number_of_specified_items_in_dir(dir_path, path_is_file)


def _number_of_specified_items_in_dir(dir_path, filter_function):
    return len([item for item in return_all_items_in_dir(dir_path) \
             if filter_function(item)])


def return_all_items_in_dir(dir_path):
    if not path_is_dir(dir_path):
        raise ValueError('The path does not point to a directory.')

    return Path(dir_path).iterdir()


def dir_has_any_items(dir_path):
    return any(return_all_items_in_dir(dir_path))


def unzip_file(src, dest):
    with ZipFile(src) as zip_obj:
        zip_obj.extractall(dest)


def dump_dict_to_json(data, dest):
    with open(dest, "w") as fp:
        json.dump(data, fp)


def get_dict_from_json(src):
    with open(src, 'r') as fp:
        data = json.load(fp)
    return data


def make_dir(path, make_parents=False, path_exist_ok=False):
    Path(path).mkdir(parents=make_parents, exist_ok=path_exist_ok)


def read_file(src):
    with open(src, 'r') as file:
        file_str = file.read()
    return file_str


def write_non_binary_file(data, dest):
    _write_file(data, dest, 'w')


def write_binary_file(data, dest):
    _write_file(data, dest, 'wb')


def _write_file(data, dest, mode):
    with open(dest, mode) as file:
        file.write(data)


def make_archive_of_files_and_dirs(sources, dest):
    # src has to be a list, even if it just has one item
    temp = WEBROB_PATH + 'temp'
    make_dir(temp, make_parents=True)

    for item in sources:
        if path_is_dir(item):
            shutil.copytree(item, temp + '/' + get_path_stem(item))
        else:
            shutil.copy(item, temp)

    zip = '.zip'
    if zip in dest:
        p_dest = dest.replace(zip, '')

    shutil.make_archive(p_dest, 'zip', root_dir=temp)

    remove_if_is_dir(temp)


def move_file(src, dest, overwrite=False):
    if not overwrite and path_is_file(dest):
        app.logger.info('Cannot move file, because file already exists at destination and overwrite-flag is set to "False".')
        return

    shutil.move(src, dest)
