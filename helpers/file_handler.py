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


def remove_if_is_dir(path):
    if Path(path).is_dir():
        shutil.rmtree(path)


def remove_if_is_file(path):
    if Path(path).is_file():
        Path(path).unlink()


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
    Path(temp).mkdir(parents=True)

    for item in sources:
        if Path(item).is_dir():
            shutil.copytree(item, temp + '/' + Path(item).stem)
        else:
            shutil.copy(item, temp)

    zip = '.zip'
    if zip in dest:
        p_dest = dest.replace(zip, '')
    
    shutil.make_archive(p_dest, 'zip', root_dir=temp)

    remove_if_is_dir(temp)



def move_file(src, dest, overwrite=False):
    if overwrite and Path(dest).is_file():
        app.logger.info('Cannot move file, because file already exists at destination and overwrite-flag is set to "False".')
        return
    
    shutil.move(src, dest)
