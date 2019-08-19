import webrob.utility.system_environment_variable_getter as evg

DEV_SECRET_KEY = '\\\xf8\x12\xdc\xf5\xb2W\xd4Lh\xf5\x1a\xbf"\x05@Bg\xdf\xeb>E\xd8<'

SQLALCHEMY_DATABASE_URI = 'postgresql://docker@' + \
                          evg.get_required_variable('POSTGRES_PORT_5432_TCP_ADDR') + ':' + \
                          evg.get_required_variable('POSTGRES_PORT_5432_TCP_PORT') + '/docker'
# SQLALCHEMY_ECHO = True

CSRF_ENABLED = True

# email server
MAIL_SERVER = evg.get_variable_with_default('OPENEASE_MAIL_SERVER', 'smtp.gmail.com')
MAIL_PORT = int(evg.get_variable_with_default('OPENEASE_MAIL_PORT', '465'))
MAIL_USE_TLS = bool(evg.get_variable_with_default('OPENEASE_MAIL_USE_TLS', 'False'))
MAIL_USE_SSL = bool(evg.get_variable_with_default('OPENEASE_MAIL_USE_SSL', 'True'))
MAIL_USERNAME = evg.get_variable_with_default_none('OPENEASE_MAIL_USERNAME')
MAIL_PASSWORD = evg.get_required_variable('OPENEASE_MAIL_PASSWORD')
MAIL_DEFAULT_SENDER = '"Sender" <openease.iai@gmail.com>'

FACEBOOK_APP_TOKENS = (evg.get_variable_with_default_none('FACEBOOK_APP_ID'),
                       evg.get_variable_with_default_none('FACEBOOK_APP_SECRET'))
TWITTER_APP_TOKENS = (evg.get_variable_with_default_none('TWITTER_APP_ID'),
                      evg.get_variable_with_default_none('TWITTER_APP_SECRET'))
GITHUB_APP_TOKENS = (evg.get_variable_with_default_none('GITHUB_APP_ID'),
                     evg.get_variable_with_default_none('GITHUB_APP_SECRET'))
GOOGLE_APP_TOKENS = (evg.get_variable_with_default_none('GOOGLE_APP_ID'),
                     evg.get_variable_with_default_none('GOOGLE_APP_SECRET'))

USER_ENABLE_USERNAME = True
USER_ENABLE_EMAIL = True
USER_ENABLE_CONFIRM_EMAIL = False

MAX_HISTORY_LINES = 100

MESH_REPOSITORIES = map(lambda x: tuple(x.split(' ')),
                        evg.get_variable_with_default('OPENEASE_MESHES', 'git https://github.com/PR2/pr2_common').split(
                            ','))

ROS_DISTRIBUTION = evg.get_variable_with_default('OPENEASE_ROS_DISTRIBUTION', 'indigo')
