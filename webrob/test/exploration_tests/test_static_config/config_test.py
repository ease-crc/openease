import pytest

import webrob.test.exploration_tests.test_static_config.config as config
from webrob.test.exploration_tests.test_static_config.access import get_int_var, get_str_var

from webrob.test.exploration_tests.test_static_config.config import *


# int and str are not reloaded, INT_VAR == INT_CONST (without config.INT_VAR) will cause AssertionError
def test_config_int():
    assert config.INT_VAR == ZERO
    init_vars()
    assert config.INT_VAR == INT_CONST


def test_config_str():
    assert config.STR_VAR == EMPTY_STR
    init_vars()
    assert config.STR_VAR == STR_CONST


# dictionary is reloaded
def test_config_dict():
    assert CONFIG_DICT['VAR'] == ZERO
    init_vars()
    assert CONFIG_DICT['VAR'] == INT_CONST


def test_no_init():
    assert get_int_var() == ZERO
    assert get_str_var() == EMPTY_STR


def test_init():
    assert get_int_var() == 0
    init_vars()
    assert get_int_var() == INT_CONST
