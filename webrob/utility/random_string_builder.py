import string

from Crypto.Random import random


def random_string(length):
    ascii_letters_and_digits = string.ascii_letters + string.digits
    return "".join([random.choice(ascii_letters_and_digits) for n in xrange(length)])
