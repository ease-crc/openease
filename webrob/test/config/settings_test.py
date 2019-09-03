import pytest

from webrob.config.settings import Config

MAIL_PASSWORD = 'abc'

@pytest.fixture
def monkeypatch_setup(monkeypatch):
    monkeypatch.setenv('OPENEASE_MAIL_PASSWORD', MAIL_PASSWORD)

    # TODO: mock all other environment variables, so it does not need to be done twice
    return monkeypatch


# -------------------------------TESTS---------------------------------

# TODO:
#   add tests for all other environment variables

def test_retrieve_mail_password(monkeypatch_setup):
    Config._retrieve_mail_password()
    assert Config.MAIL_PASSWORD == MAIL_PASSWORD


def test_retrieve_mail_password_error():
    with pytest.raises(KeyError):
        Config._retrieve_mail_password()
    assert Config.MAIL_PASSWORD is None
