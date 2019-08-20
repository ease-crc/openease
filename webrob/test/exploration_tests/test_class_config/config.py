ZERO = 0
CONST = 1


class Config:
    VAR = ZERO

    @staticmethod
    def init_vars():
        Config.VAR = CONST


def init_vars():
    Config.VAR = CONST
