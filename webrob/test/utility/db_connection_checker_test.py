import pytest

from webrob.utility.db_connection_checker import got_db_connection
from webrob.app_and_db import app, db
from flask import Flask
from flask_sqlalchemy import SQLAlchemy


class MockAppLogger:
    def __init__(self):
        return

    def info(self, message):
        return


class MockDbEngineNormal:
    def __init__(self):
        self.cmd = "SELECT 1"

    def execute(self, command):
        # FIXME: Find way to remove hard-coding mock-method
        if command == self.cmd:
            return True
        else:
            raise Exception


class MockDbEngineRaiseError:
    def __init__(self):
        return

    def execute(self, command):
        raise Exception


MOCK_LOGGER = MockAppLogger()
MOCK_DB_ENGINE = MockDbEngineNormal()
MOCK_DB_ENGINE_MALFUNCTIONING = MockDbEngineRaiseError()


@pytest.fixture
def monkeypatch_setup(monkeypatch):
    monkeypatch.setattr(Flask, 'logger', MOCK_LOGGER)
    return monkeypatch


# -------------------------------TESTS---------------------------------


def test_having_db_connection(monkeypatch_setup):
    monkeypatch_setup.setattr(SQLAlchemy, 'engine', MOCK_DB_ENGINE)
    assert got_db_connection(app, db) is True


def test_unable_to_connect_to_db(monkeypatch_setup):
    monkeypatch_setup.setattr(SQLAlchemy, 'engine', MOCK_DB_ENGINE_MALFUNCTIONING)
    assert got_db_connection(app, db) is False
