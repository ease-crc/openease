import pytest
import webrob.utility.user_directory_getter as user_directory_getter

from flask import Flask
from webrob.utility.user_directory_getter import get_user_dir


class MockAppLogger:
    def __init__(self):
        return

    def info(self, message):
        return


USER_DIR_BASE = '/home/ros/user_data/'
USER_DIR_EXT = 'johnny_cash'
USER_DIR = '{0}{1}'.format(USER_DIR_BASE, USER_DIR_EXT)
MOCK_LOGGER = MockAppLogger()
MOCK_SESSION = {'user_container_name': USER_DIR_EXT}

count_calls_on_create_dirs = 0


def teardown_function():
    global count_calls_on_create_dirs
    count_calls_on_create_dirs = 0


@pytest.fixture
def monkeypatch_setup(monkeypatch):
    def mock_create_dirs(path):
        global count_calls_on_create_dirs
        count_calls_on_create_dirs += 1

    monkeypatch.setattr(Flask, 'logger', MOCK_LOGGER)
    monkeypatch.setattr(user_directory_getter, 'session', MOCK_SESSION)
    monkeypatch.setattr(user_directory_getter, 'create_directories_from_path', mock_create_dirs)
    return monkeypatch


# -------------------------------TESTS---------------------------------


def test_if_path_exists(monkeypatch_setup):
    def mock_path_exists(path):
        return True

    monkeypatch_setup.setattr(user_directory_getter, 'path_exists', mock_path_exists)
    assert get_user_dir() == USER_DIR
    assert count_calls_on_create_dirs == 0


def test_if_path_does_not_exist(monkeypatch, monkeypatch_setup):
    def mock_path_exists(path):
        return False

    monkeypatch_setup.setattr(user_directory_getter, 'path_exists', mock_path_exists)
    assert get_user_dir() == USER_DIR
    assert count_calls_on_create_dirs == 1
