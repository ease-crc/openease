from webrob.app_and_db import app
from webrob.startup.init_app import _check_password_and_display_message_on_error, _password_criteria_fulfilled, \
    _has_six_or_more_chars, \
    _contains_number, _contains_lowercase_letter, _contains_uppercase_letter

PASSWORD_CASES_CRITERIA = ["a", "A", "1", "aA", "a1", "1A", "Aa1", "AAAAAA", "aaaaaa", "111111", "aaa111", "AAA111"]


# test for length > 6, containing a number, a lower- and a uppercase letter together
def test_password_criteria():
    assert _password_criteria_fulfilled('') is False
    assert _password_criteria_fulfilled('AAaa11') is True
    for pw in PASSWORD_CASES_CRITERIA:
        assert _password_criteria_fulfilled(pw) is False


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


def test_check_password_and_display_message_on_error():
    for pw in PASSWORD_CASES_CRITERIA:
        assert _check_password_and_display_message_on_error(app, 'Paul', pw) is False


def test_check_password_and_display_error_message_for_none_password():
    assert _check_password_and_display_message_on_error(app, 'Paul', None) is False


def test_check_for_correct_password():
    assert _check_password_and_display_message_on_error(app, 'Paul', 'AAaa11') is True
