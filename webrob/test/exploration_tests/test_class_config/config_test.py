import pytest

from webrob.test.exploration_tests.test_class_config.access import *
from webrob.test.exploration_tests.test_class_config.config import *


def test_config():
    assert Config.VAR == ZERO
    assert get_var() == ZERO
    Config.init_var()          # init_vars() works as well
    assert Config.VAR == CONST
    assert get_var() == CONST
