import os


def get_required_variable(var_name):
    try:
        return os.environ[var_name]
    except KeyError:
        raise KeyError("""Environment variable {0} is required but does not exist.
        Please create it and set its value according to the documentation.""".format(var_name))


def get_variable_with_default(var_name, default):
    try:
        return get_required_variable(var_name)
    except KeyError:
        return default


def get_variable_with_default_none(var_name):
    return get_variable_with_default(var_name, None)
