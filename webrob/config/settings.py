import pyjsonrpc

import webrob.utility.system_environment_variable_getter as evg


DEV_SECRET_KEY = '\\\xf8\x12\xdc\xf5\xb2W\xd4Lh\xf5\x1a\xbf"\x05@Bg\xdf\xeb>E\xd8<'
SQLALCHEMY_DATABASE_URI = {}
# SQLALCHEMY_ECHO = True
MESH_REPOSITORIES = {}
ROS_DISTRIBUTION = {}
HTTP_CLIENT = {}
CSRF_ENABLED = True

# email server
MAIL_SERVER = {}
MAIL_PORT = {}
MAIL_USE_TLS = {}
MAIL_USE_SSL = {}
MAIL_USERNAME = {}
MAIL_PASSWORD = {}
MAIL_DEFAULT_SENDER = '"Sender" <openease.iai@gmail.com>'

FACEBOOK_APP_TOKENS = {}
TWITTER_APP_TOKENS = {}
GITHUB_APP_TOKENS = {}
GOOGLE_APP_TOKENS = {}

USER_ENABLE_USERNAME = True
USER_ENABLE_EMAIL = True
USER_ENABLE_CONFIRM_EMAIL = False

MAX_HISTORY_LINES = 100

_variables_set = False


def init_config_variables():
    _get_sqlalchemy_db_uri()
    _get_mesh_repositories()
    _get_ros_distribution()
    _get_mail_server_vars()
    _get_oauth_tokens()

    _init_http_client()

    _set_variables_set_to_true()


def _get_sqlalchemy_db_uri():
    global SQLALCHEMY_DATABASE_URI
    SQLALCHEMY_DATABASE_URI = 'postgresql://docker@' + \
                              evg.get_required_variable('POSTGRES_PORT_5432_TCP_ADDR') + ':' + \
                              evg.get_required_variable('POSTGRES_PORT_5432_TCP_PORT') + '/docker'


def _get_mesh_repositories():
    global MESH_REPOSITORIES
    MESH_REPOSITORIES = map(lambda x: tuple(x.split(' ')),
                            evg.get_variable_with_default('OPENEASE_MESHES',
                                                          'git https://github.com/PR2/pr2_common').split(','))


def _get_ros_distribution():
    global ROS_DISTRIBUTION
    ROS_DISTRIBUTION = evg.get_variable_with_default('OPENEASE_ROS_DISTRIBUTION', 'indigo')


def _get_mail_server_vars():
    _get_mail_server()
    _get_mail_port()
    _get_mail_use_tls()
    _get_mail_use_ssl()
    _get_mail_username()
    _get_mail_password()


def _get_mail_server():
    global MAIL_SERVER
    MAIL_SERVER = evg.get_variable_with_default('OPENEASE_MAIL_SERVER', 'smtp.gmail.com')


def _get_mail_port():
    global MAIL_PORT
    MAIL_PORT = int(evg.get_variable_with_default('OPENEASE_MAIL_PORT', '465'))


def _get_mail_use_tls():
    global MAIL_USE_TLS
    MAIL_USE_TLS = bool(evg.get_variable_with_default('OPENEASE_MAIL_USE_TLS', 'False'))


def _get_mail_use_ssl():
    global MAIL_USE_SSL
    MAIL_USE_SSL = bool(evg.get_variable_with_default('OPENEASE_MAIL_USE_SSL', 'True'))


def _get_mail_username():
    global MAIL_USERNAME
    MAIL_USERNAME = evg.get_variable_with_default_none('OPENEASE_MAIL_USERNAME')


def _get_mail_password():
    global MAIL_PASSWORD
    MAIL_PASSWORD = evg.get_required_variable('OPENEASE_MAIL_PASSWORD')


def _get_oauth_tokens():
    _get_facebook_tokens()
    _get_twitter_tokens()
    _get_github_tokens()
    _get_google_tokens()


def _get_facebook_tokens():
    global FACEBOOK_APP_TOKENS
    FACEBOOK_APP_TOKENS = (evg.get_variable_with_default_none('FACEBOOK_APP_ID'),
                           evg.get_variable_with_default_none('FACEBOOK_APP_SECRET'))


def _get_twitter_tokens():
    global TWITTER_APP_TOKENS
    TWITTER_APP_TOKENS = (evg.get_variable_with_default_none('TWITTER_APP_ID'),
                          evg.get_variable_with_default_none('TWITTER_APP_SECRET'))


def _get_github_tokens():
    global GITHUB_APP_TOKENS
    GITHUB_APP_TOKENS = (evg.get_variable_with_default_none('GITHUB_APP_ID'),
                         evg.get_variable_with_default_none('GITHUB_APP_SECRET'))


def _get_google_tokens():
    global GOOGLE_APP_TOKENS
    GOOGLE_APP_TOKENS = (evg.get_variable_with_default_none('GOOGLE_APP_ID'),
                         evg.get_variable_with_default_none('GOOGLE_APP_SECRET'))


def _init_http_client():
    global HTTP_CLIENT
    HTTP_CLIENT = pyjsonrpc.HttpClient(url="http://" +
                                           evg.get_required_variable('DOCKERBRIDGE_PORT_5001_TCP_ADDR') + ':' +
                                           evg.get_required_variable('DOCKERBRIDGE_PORT_5001_TCP_PORT'))


def _set_variables_set_to_true():
    global _variables_set
    _variables_set = True


def config_variables_initialized():
    return _variables_set
