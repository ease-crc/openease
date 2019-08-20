VAR = 0

_var_initialized = False


def get_vars_initialized():
    return _var_initialized


def _set_vars_init_true():
    global _var_initialized
    _var_initialized = True


def init_vars():
    global VAR
    VAR = 12

    _set_vars_init_true()
