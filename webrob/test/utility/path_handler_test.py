import os

from webrob.test.utility.testbase_file_io import TEMP_DIR, EMPTY_TEMP_FILE, TEMP_FILE_WITH_CONTENT, \
    create_empty_temp_file, create_temp_file_with_content, remove_file
from webrob.utility.directory_handler import mk_dir, rm_nonempty_dir
from webrob.utility.path_handler import join_paths, path_exists, absolute_path, get_parent_dir_name, \
    get_path_basename, get_unix_style_path_basename, is_directory, get_path_size, relative_path, split_path, \
    split_extension

EXISTING_PATH = TEMP_DIR
NOT_EXISTING_PATH = join_paths(TEMP_DIR, 'nothing')

BASENAME = 'base'
BASENAME_TEST_DIR = join_paths(EXISTING_PATH, BASENAME)
BASENAME_TEST_DIR_UNIX_STLYE = join_paths(BASENAME_TEST_DIR, '')

EMPTY_STRING = ''
ONLY_EXTENSION = get_path_basename(EMPTY_TEMP_FILE)
PATH, FILE_NAME = os.path.split(EMPTY_TEMP_FILE)
PATH_JOINED_WITH_ONLY_EXTENSION = join_paths(PATH, ONLY_EXTENSION)


# cannot use testbase_file_io.create_temp() as it uses functionality of this module
# instead use directory_handler and create and delete temp_dir manually
def setup_module():
    if os.path.exists(EXISTING_PATH):  # for the case that due to debugging errors teardown wasn't executed
        rm_nonempty_dir(EXISTING_PATH)
    mk_dir(EXISTING_PATH)
    return


def teardown_module():
    rm_nonempty_dir(EXISTING_PATH)
    return


# -------------------------------TESTS---------------------------------


def test_path_exists():
    assert path_exists(EXISTING_PATH) is True


def test_path_does_not_exist():
    assert path_exists(NOT_EXISTING_PATH) is False


def test_join_paths():
    path1 = 'first'
    path2 = 'second'
    path3 = 'third'

    join_paths_and_assert_correctness(path1)
    join_paths_and_assert_correctness(path1, path2)
    join_paths_and_assert_correctness(path1, path2, path3)


def join_paths_and_assert_correctness(*args):
    template_string = build_template_string(len(args))
    expected_result_path = template_string.format(*args)
    builder_result_path = join_paths(*args)
    # replace is needed so tests run on uniformly on all OS
    assert builder_result_path.replace(os.sep, '/') == expected_result_path


def build_template_string(number_of_placeholders):
    template = ''
    for x in xrange(number_of_placeholders):
        template += '{' + str(x) + '}'
        if x < number_of_placeholders - 1:
            template += '/'

    return template


# Figure out why this runs locally but not on Travis, https://github.com/code-iai/openEASE-flask/issues/2
# def test_absolute_path():
#    return absolute_path(TEMP_DIR) == os.path.abspath(TEMP_DIR)


def test_get_parent_dir_name():
    assert get_parent_dir_name(NOT_EXISTING_PATH) == TEMP_DIR


def test_get_path_basename():
    assert get_path_basename(BASENAME_TEST_DIR) == BASENAME
    assert get_path_basename(BASENAME_TEST_DIR_UNIX_STLYE) == ''


def test_get_unix_style_path_basename():
    # replace is needed so tests run on uniformly on all OS
    assert get_unix_style_path_basename(BASENAME_TEST_DIR.replace(os.sep, '/')) == BASENAME
    assert get_unix_style_path_basename(BASENAME_TEST_DIR_UNIX_STLYE.replace(os.sep, '/')) == ''


def test_is_directory():
    assert is_directory(NOT_EXISTING_PATH) is False
    create_empty_temp_file()
    assert is_directory(EMPTY_TEMP_FILE) is False
    assert is_directory(TEMP_DIR) is True
    remove_file(EMPTY_TEMP_FILE)


def test_get_size():
    create_temp_file_with_content()
    assert get_path_size(TEMP_FILE_WITH_CONTENT) == os.path.getsize(TEMP_FILE_WITH_CONTENT)
    remove_file(TEMP_FILE_WITH_CONTENT)


def test_relative_path():
    assert relative_path(BASENAME_TEST_DIR, EXISTING_PATH) == os.path.relpath(BASENAME_TEST_DIR, EXISTING_PATH)


def test_split_path():
    assert split_path(EMPTY_STRING) == os.path.split(EMPTY_STRING)
    assert split_path(ONLY_EXTENSION) == os.path.split(ONLY_EXTENSION)
    assert split_path(FILE_NAME) == os.path.split(FILE_NAME)
    assert split_path(PATH) == os.path.split(PATH)
    assert split_path(PATH_JOINED_WITH_ONLY_EXTENSION) == os.path.split(PATH_JOINED_WITH_ONLY_EXTENSION)
    assert split_path(EMPTY_TEMP_FILE) == os.path.split(EMPTY_TEMP_FILE)
    assert split_path(BASENAME_TEST_DIR) == os.path.split(BASENAME_TEST_DIR)


def test_split_extension():
    assert split_extension(EMPTY_STRING) == os.path.splitext(EMPTY_STRING)
    assert split_extension(ONLY_EXTENSION) == os.path.splitext(ONLY_EXTENSION)
    assert split_extension(FILE_NAME) == os.path.splitext(FILE_NAME)
    assert split_extension(PATH) == os.path.splitext(PATH)
    assert split_extension(PATH_JOINED_WITH_ONLY_EXTENSION) == os.path.splitext(PATH_JOINED_WITH_ONLY_EXTENSION)
    assert split_extension(EMPTY_TEMP_FILE) == os.path.splitext(EMPTY_TEMP_FILE)
    assert split_extension(BASENAME_TEST_DIR) == os.path.splitext(BASENAME_TEST_DIR)
