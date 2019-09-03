import os

import pytest

ENVIRONMENT_VARIABLE_NAME = 'ENV1'
ENVIRONMENT_VARIABLE_VALUE = 'myval'
ENVIRONMENT_VARIABLE_DEFAULT_VALUE = 'default'


def test_environ_success(monkeypatch):
    monkeypatch.setitem(os.environ, ENVIRONMENT_VARIABLE_NAME, ENVIRONMENT_VARIABLE_VALUE)
    assert os.environ[ENVIRONMENT_VARIABLE_NAME] == ENVIRONMENT_VARIABLE_VALUE


def test_environ_raise_exception():
    with pytest.raises(KeyError, match=ENVIRONMENT_VARIABLE_NAME):
        os.environ[ENVIRONMENT_VARIABLE_NAME]


def test_getenv_success(monkeypatch):
    monkeypatch.setitem(os.environ, ENVIRONMENT_VARIABLE_NAME, ENVIRONMENT_VARIABLE_VALUE)
    assert os.getenv(ENVIRONMENT_VARIABLE_NAME) == ENVIRONMENT_VARIABLE_VALUE
    assert os.getenv(ENVIRONMENT_VARIABLE_NAME, ENVIRONMENT_VARIABLE_DEFAULT_VALUE) == ENVIRONMENT_VARIABLE_VALUE


def test_getenv_default():
    assert os.getenv(ENVIRONMENT_VARIABLE_NAME, ENVIRONMENT_VARIABLE_DEFAULT_VALUE) == ENVIRONMENT_VARIABLE_DEFAULT_VALUE


def test_getenv_empty_default():
    assert os.getenv(ENVIRONMENT_VARIABLE_NAME) is None
