import os
import shutil
import random

import pytest

from webrob.test.utility.testbase_file_io import TEMP_DIR
from webrob.utility.directory_handler import rm_nonempty_dir, make_dirs, rm_empty_dir, mk_dir, \
    get_current_working_directory, ch_dir, list_directories, walk_directories
from webrob.utility.path_handler import join_paths, path_exists

TEST_DIR = join_paths(TEMP_DIR, 'test')
TEST_DIR_NESTED = join_paths(TEST_DIR, 'test')


# both setup_function() and teardown_function() have to use the os-module
# instead of the directory_handler as it's being tested in this module
def setup_function():
    remove_directory_if_exists(TEMP_DIR)
    os.mkdir(TEMP_DIR)


def remove_directory_if_exists(path):
    if path_exists(path):
        shutil.rmtree(path)


def teardown_function():
    remove_directory_if_exists(TEMP_DIR)


# -------------------------------TESTS---------------------------------


def test_making_simple_directory():
    assert_make_dir_function(TEST_DIR, mk_dir)
    assert_make_dir_function(TEST_DIR, make_dirs)


def assert_make_dir_function(path, make_dir_function):
    make_dir_function(path)
    assert path_exists(path) is True
    remove_directory_if_exists(path)


def test_making_nested_directory():
    with pytest.raises(OSError):
        assert_make_dir_function(TEST_DIR_NESTED, mk_dir)
    assert_make_dir_function(TEST_DIR_NESTED, make_dirs)


def test_make_existing_directory():
    os.mkdir(TEST_DIR)
    with pytest.raises(OSError):
        assert_make_dir_function(TEST_DIR, mk_dir)
    with pytest.raises(OSError):
        assert_make_dir_function(TEST_DIR, make_dirs)


def test_remove_empty_dir():
    assert_remove_function(TEST_DIR, rm_empty_dir)
    assert_remove_function(TEST_DIR, rm_nonempty_dir)


def assert_remove_function(path, remove_function, check_path=None):
    setup_for_remove_test(path)

    if check_path is None:      # path created and to check are the same
        execute_and_check_remove(path, remove_function)
    else:                       # path created and to check are different
        execute_and_check_remove(check_path, remove_function)


def setup_for_remove_test(path):
    # deleting the old directory (if it exists) and make a new one
    # not doing this might cause errors
    remove_directory_if_exists(path)
    make_dirs(path)


def execute_and_check_remove(path, remove_function):
    remove_function(path)
    assert path_exists(path) is False


def test_rm_nonempty_dir():
    with pytest.raises(OSError):
        assert_remove_function(TEST_DIR_NESTED, rm_empty_dir, TEST_DIR)
    assert_remove_function(TEST_DIR_NESTED, rm_nonempty_dir, TEST_DIR)


# Figure out why this runs locally but not on Travis, https://github.com/code-iai/openEASE-flask/issues/2
# def test_change_directory():
#     # have to get path before changing dir, because otherwise relative path will change
#     # maybe consider changing constant file-paths in testbase from relative to absolute
#     expected_result = os.path.abspath(TEMP_DIR)
#     ch_dir(TEMP_DIR)
#     assert os.getcwd() == expected_result
#     # not sure why the following two lines are needed, but if not executed, a temp folder
#     # will be left in the test or webrob directory
#     ch_dir('../utility')
#     remove_directory_if_exists(TEMP_DIR)


def test_change_to_non_existent_directory():
    with pytest.raises(OSError):
        ch_dir(TEST_DIR)


# Figure out why this runs locally but not on Travis, https://github.com/code-iai/openEASE-flask/issues/2
# def test_get_current_working_directory():
#    assert get_current_working_directory() == os.getcwd()


def test_list_directories():
    dir_list = []
    os.makedirs(TEST_DIR)
    _check_directory_list(dir_list, TEST_DIR)

    for i in range(0, random.randint(5, 11)):
        dir_list.append(str(i))
        os.makedirs(join_paths(TEST_DIR, str(i)))
        _check_directory_list(dir_list, TEST_DIR)

    remove_directory_if_exists(TEST_DIR)


def _check_directory_list(actual_dir_names, parent_dir):
    dir_list = list_directories(parent_dir)
    assert len(dir_list) is len(actual_dir_names)

    for i in range(0, len(dir_list)):
        assert dir_list[i] in actual_dir_names


def test_walk_directories():
    # TODO
    return
