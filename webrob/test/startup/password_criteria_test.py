from webrob.startup.init_app import _password_criteria_fulfilled, _has_six_or_more_chars, \
    _contains_number, _contains_lowercase_letter, _contains_uppercase_letter

import pytest


# test for length > 6, containing a number, a lower- and a uppercase letter together
def test_password_criteria():
    assert _password_criteria_fulfilled('') is False
    assert _password_criteria_fulfilled('a') is False
    assert _password_criteria_fulfilled('A') is False
    assert _password_criteria_fulfilled('1') is False
    assert _password_criteria_fulfilled('aA') is False
    assert _password_criteria_fulfilled('a1') is False
    assert _password_criteria_fulfilled('1A') is False
    assert _password_criteria_fulfilled('Aa1') is False
    assert _password_criteria_fulfilled('AAAAAA') is False
    assert _password_criteria_fulfilled('aaaaaa') is False
    assert _password_criteria_fulfilled('111111') is False
    assert _password_criteria_fulfilled('aaa111') is False
    assert _password_criteria_fulfilled('AAA111') is False
    assert _password_criteria_fulfilled('AAAaaa') is False
    assert _password_criteria_fulfilled('AAaa11') is True


def test_has_six_or_more_character():
    assert _has_six_or_more_chars('') is False
    assert _has_six_or_more_chars('a') is False
    assert _has_six_or_more_chars('abcde') is False
    assert _has_six_or_more_chars('abcdef') is True
    assert _has_six_or_more_chars('abcdefgh') is True


def test_contains_number():
    assert _contains_number('') is False
    assert _contains_number('abc') is False
    assert _contains_number('1') is True
    assert _contains_number('abc1') is True


def test_contains_lowercase():
    assert _contains_lowercase_letter('') is False
    assert _contains_lowercase_letter('1') is False
    assert _contains_lowercase_letter('ABC') is False
    assert _contains_lowercase_letter('a') is True
    assert _contains_lowercase_letter('AbC') is True


def test_contains_uppercase():
    assert _contains_uppercase_letter('') is False
    assert _contains_uppercase_letter('1') is False
    assert _contains_uppercase_letter('abc') is False
    assert _contains_uppercase_letter('A') is True
    assert _contains_uppercase_letter('AbC') is True
