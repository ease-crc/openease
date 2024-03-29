import os

TRUE_STRINGS = ["1", "true", "True", "yes", "Yes"]

DEV_SECRET_KEY='\\\xf8\x12\xdc\xf5\xb2W\xd4Lh\xf5\x1a\xbf"\x05@Bg\xdf\xeb>E\xd8<'

SQLALCHEMY_DATABASE_URI = 'postgresql://docker@' + \
    os.environ['POSTGRES_PORT_5432_TCP_ADDR'] + ':' + \
    os.environ['POSTGRES_PORT_5432_TCP_PORT'] + '/docker'
#SQLALCHEMY_ECHO = True

CSRF_ENABLED = True

# email server
MAIL_SERVER   = os.environ.get('OPENEASE_MAIL_SERVER', 'smtp.gmail.com')
MAIL_PORT     = int(os.environ.get('OPENEASE_MAIL_PORT', '465'))
MAIL_USE_TLS  = bool(os.environ.get('OPENEASE_MAIL_USE_TLS', 'False'))
MAIL_USE_SSL  = bool(os.environ.get('OPENEASE_MAIL_USE_SSL', 'True'))
MAIL_USERNAME = os.environ.get('OPENEASE_MAIL_USERNAME')
MAIL_PASSWORD = os.environ.get('OPENEASE_MAIL_PASSWORD')
MAIL_DEFAULT_SENDER = '"Sender" <openease.iai@gmail.com>'

USE_HOST_KNOWROB = (os.environ.get('OPENEASE_USE_HOST_KNOWROB', 'False') in TRUE_STRINGS)

USER_ENABLE_USERNAME = True
USER_ENABLE_EMAIL = True
USER_ENABLE_CONFIRM_EMAIL = False

MAX_HISTORY_LINES = 100

WEBROB_PATH = '/opt/webapp/webrob/'
STATIC_DIR_PATH = WEBROB_PATH + 'static/'
CONTENT_DIR_PATH = WEBROB_PATH + 'content/'
DOWNLOADS_DIR_PATH = CONTENT_DIR_PATH + 'downloads/'
DEFAULT_FILES_PATH = CONTENT_DIR_PATH + 'default_files/'
LOCAL_PUBLICATIONS_AND_PAPERS = CONTENT_DIR_PATH + 'publications-and-papers/'

DATETIME_FORMAT = '%Y-%m-%dT%H:%M:%S.%f'
