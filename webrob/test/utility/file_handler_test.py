import pytest

from webrob.test.utility.testbase_file_io import delete_temp_dir, TEMP_FILE_WITH_CONTENT, create_temp_dir, \
    TEMP_FILE_CONTENT, EMPTY_TEMP_FILE, NOT_EXISTING_FILE
from webrob.utility.file_handler import create_file, remove_file, read_file, write_to_file
from webrob.utility.path_handler import path_exists


# both setup_function() and teardown_function() have to use python
# file read/write-functions instead of the file_handler as it's
# being tested in this module
def setup_function():
    create_temp_dir()
    create_file_with_os(EMPTY_TEMP_FILE)


def create_file_with_os(path, content=None):
    dst_f = open(path, 'w+')
    if content is not None:
        dst_f.write(content)
    dst_f.close()


def teardown_function():
    delete_temp_dir()


# -------------------------------TESTS---------------------------------


def test_read_existing_file():
    create_file(TEMP_FILE_WITH_CONTENT, TEMP_FILE_CONTENT)
    assert read_file(TEMP_FILE_WITH_CONTENT) == TEMP_FILE_CONTENT


def test_read_not_existing_file():
    with pytest.raises(IOError):
        read_file(NOT_EXISTING_FILE)


def test_write_to_existing_file():
    write_to_file(EMPTY_TEMP_FILE, TEMP_FILE_CONTENT)
    assert read_file(EMPTY_TEMP_FILE) == TEMP_FILE_CONTENT


def test_write_to_not_existing_file():
    with pytest.raises(IOError):
        write_to_file(NOT_EXISTING_FILE, TEMP_FILE_CONTENT)


def test_create_file():
    create_file(TEMP_FILE_WITH_CONTENT, TEMP_FILE_CONTENT)
    assert path_exists(TEMP_FILE_WITH_CONTENT) is True
    assert read_file(TEMP_FILE_WITH_CONTENT) == TEMP_FILE_CONTENT


def test_create_existing_file():
    with pytest.raises(IOError):
        create_file(EMPTY_TEMP_FILE, TEMP_FILE_CONTENT)


def test_remove_existing_file():
    remove_file(EMPTY_TEMP_FILE)
    assert path_exists(EMPTY_TEMP_FILE) is False


def test_remove_not_existing_file():
    with pytest.raises(OSError):
        remove_file(NOT_EXISTING_FILE)
