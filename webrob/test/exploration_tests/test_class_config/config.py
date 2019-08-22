ZERO = 0
CONST = 1


class Config:
    VAR = ZERO

    @staticmethod
    def init_var():
        Config.VAR = CONST


def init_var():
    Config.VAR = CONST
