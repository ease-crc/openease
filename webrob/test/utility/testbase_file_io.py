from webrob.utility.directory_handler import rm_nonempty_dir, mk_dir
from webrob.utility.file_handler import create_file, remove_file
from webrob.utility.path_handler import path_exists

TEMP_DIR = '../temp'
TEMP_FILE_WITH_CONTENT = '../temp/not-empty.txt'
TEMP_FILE_CONTENT = 'something'
EMPTY_TEMP_FILE = '../temp/empty.txt'
NOT_EXISTING_FILE = '../temp/nothing.txt'


def create_temp_dir():
    if path_exists(TEMP_DIR):   # for the case that due to debugging errors teardown wasn't executed
        rm_nonempty_dir(TEMP_DIR)
    mk_dir(TEMP_DIR)


def delete_temp_dir():
    rm_nonempty_dir(TEMP_DIR)


def create_empty_temp_file():
    create_file(EMPTY_TEMP_FILE)


def delete_empty_temp_file():
    remove_file(EMPTY_TEMP_FILE)


def create_temp_file_with_content():
    create_file(TEMP_FILE_WITH_CONTENT, TEMP_FILE_CONTENT)


def delete_temp_file_with_content():
    remove_file(TEMP_FILE_WITH_CONTENT)
