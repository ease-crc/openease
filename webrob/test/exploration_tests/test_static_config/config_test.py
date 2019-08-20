import pytest
import webrob.test.exploration_tests.test_static_config.config as config
from webrob.test.exploration_tests.test_static_config.access import get_var


def test_config_vars():
    assert config.VAR == 0
    config.init_vars()
    assert config.VAR == 12


def test_no_init():
    assert get_var() == 0


def test_init():
    assert get_var() == 0
    config.init_vars()
    assert get_var() == 12
