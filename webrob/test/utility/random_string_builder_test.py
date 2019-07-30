import pytest

from webrob.utility.random_string_builder import random_string

'''
Due to the nature of randomness, it is possible that some generated strings
will not contain either numbers or letters. It is also possible to get the
same generated string when the method is run several times. Since it is vir-
tually impossible to check for true randomness, we will just check with cer-
tain tolerances whether certain events occurs, i.e., how often strings do 
not contain either numbers or letters, or how often generated strings repeat.
'''

STRING_LENGTH = 16
RANDOM_STRINGS = []
LIST_LENGTH = 1000
NON_OCCURRENCE_TOLERANCE = 0.1
DUPLICATION_TOLERANCE = 0.01


def setup_function():
    for n in range(LIST_LENGTH):
        RANDOM_STRINGS.append(random_string(STRING_LENGTH))


def teardown_function():
    for n in range(LIST_LENGTH):
        RANDOM_STRINGS.pop(0)


def get_occurrence_percentage_in_random_strings(count_function):
    count = 0
    for s in RANDOM_STRINGS:
        count += count_function(s)

    return calculate_percentage(count, LIST_LENGTH)


def calculate_percentage(part, total):
    return float(part) / float(total)


# -------------------------------TESTS---------------------------------


def test_string_contains_number():
    def string_contains_no_digit(s):
        if not any(char.isdigit() for char in s):
            return 1
        else:
            return 0

    assert get_occurrence_percentage_in_random_strings(string_contains_no_digit) <= NON_OCCURRENCE_TOLERANCE


def test_string_contains_ascii_letter():
    def string_contains_no_ascii_letter(s):
        if not any(char.isalpha() for char in s):
            return 1
        else:
            return 0

    assert get_occurrence_percentage_in_random_strings(string_contains_no_ascii_letter) <= NON_OCCURRENCE_TOLERANCE


def test_strings_are_semi_random():
    def string_in_random_strings_more_than_once(s):
        occurrences = 0
        for t in RANDOM_STRINGS:
            if s == t:
                occurrences += 1
                if occurrences > 1:
                    return 1
        return 0

    assert get_occurrence_percentage_in_random_strings(string_in_random_strings_more_than_once) <= DUPLICATION_TOLERANCE

