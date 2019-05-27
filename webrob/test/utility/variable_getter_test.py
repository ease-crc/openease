import os

import pytest

from webrob.utility.environment_variable_getter import get_required_variable, get_variable_with_default_none, \
    get_variable_with_default

ENVIRONMENT_VARIABLE_NAME = 'ENV1'
ENVIRONMENT_VARIABLE_VALUE = 'myval'
ENVIRONMENT_VARIABLE_DEFAULT_VALUE = 'default'


def test_successfully_get_required_variable(monkeypatch):
    monkeypatch.setitem(os.environ, ENVIRONMENT_VARIABLE_NAME, ENVIRONMENT_VARIABLE_VALUE)
    assert get_required_variable(ENVIRONMENT_VARIABLE_NAME) == ENVIRONMENT_VARIABLE_VALUE


def test_required_variable_exception():
    with pytest.raises(KeyError, match=ENVIRONMENT_VARIABLE_NAME):
        get_required_variable(ENVIRONMENT_VARIABLE_NAME)


def test_get_variable_with_default(monkeypatch):
    monkeypatch.setitem(os.environ, ENVIRONMENT_VARIABLE_NAME, ENVIRONMENT_VARIABLE_VALUE)
    assert get_variable_with_default(
        ENVIRONMENT_VARIABLE_NAME,
        ENVIRONMENT_VARIABLE_DEFAULT_VALUE) == ENVIRONMENT_VARIABLE_VALUE


def test_get_default_value():
    assert get_variable_with_default(
        ENVIRONMENT_VARIABLE_NAME,
        ENVIRONMENT_VARIABLE_DEFAULT_VALUE) == ENVIRONMENT_VARIABLE_DEFAULT_VALUE


def test_get_variable_with_default_none(monkeypatch):
    assert get_variable_with_default_none(ENVIRONMENT_VARIABLE_NAME) is None
    monkeypatch.setitem(os.environ, ENVIRONMENT_VARIABLE_NAME, ENVIRONMENT_VARIABLE_VALUE)
    assert get_variable_with_default_none(ENVIRONMENT_VARIABLE_NAME) == ENVIRONMENT_VARIABLE_VALUE
