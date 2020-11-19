#
# Based on https://github.com/lingthio/Flask-User-starter-app
# 
# Copyright 2014 SolidBuilds.com. All rights reserved
#
# Authors: Ling Thio <ling.thio@gmail.com>


import os
import datetime

from flask_mail import Mail
from flask_user import UserManager, SQLAlchemyAdapter
from flask.ext.babel import Babel

from utility import random_string
from init_db import *
from models.users import Role, User

# default password for admin user
ADMIN_USER_DEFAULT_PW = '1234'

def add_user(app,db,user_manager,name,mail,pw,displayname='',remoteapp='',roles=[]):
    if pw==None or len(pw)<4:
        app.logger.warn("User %s has no password specified." % (name))
        return
    
    user = User.query.filter(User.username==name).first()
    if user: return user
    
    user = User(username=name,
                displayname=displayname,
                remoteapp=remoteapp,
                email=mail,
                active=True,
                password=user_manager.hash_password(pw),
                confirmed_at=datetime.datetime.utcnow())
    for r in roles:
        x = Role.query.filter(Role.name==r).first()
        if x==None:
            app.logger.info("Unable to find role: " + str(r))
        else:
            user.roles.append(x)
    db.session.add(user)
    db.session.commit()
    
    return user

def init_app(app, db_instance, extra_config_settings={}):
    # Initialize app config settings
    app.config.from_object('config.settings')               # Read config from 'app/settings.py' file
    app.config.update(extra_config_settings)                # Overwrite with 'extra_config_settings' parameter
    if app.testing:
        app.config['WTF_CSRF_ENABLED'] = False              # Disable CSRF checks while testing
    if os.environ['EASE_DEBUG'] == 'true':
        app.config['DEBUG'] = True
        app.config['SECRET_KEY'] = app.config['DEV_SECRET_KEY']
    else:
        try:
            app.config['SECRET_KEY'] = open('/etc/ease_secret/secret', 'rb').read()
        except IOError:
            app.config['SECRET_KEY'] = random_string(64)

    # Setup Flask-Mail
    mail = Mail(app)

    babel = Babel(app)

    # Setup Flask-User to handle user account related forms
    from models.users import User
    db_adapter = SQLAlchemyAdapter(db_instance, User)
    app.user_manager = UserManager(db_adapter, app)     # Init Flask-User and bind to app

    # Load all models.py files to register db.Models with SQLAlchemy
    from models import users
    from models import tutorials
    from models import teaching
    from models import settings
    db_instance.create_all()

    # Load all views.py files to register @app.routes() with Flask
    from pages import main
    from pages import api
    from pages import neem_discovery
    from pages import editor
    from pages import tutorials
    from pages import oauth
    from pages import postgres

    init_db(app, db_instance)
    
    add_user(app=app,db=db_instance,user_manager=app.user_manager,
             name='admin',
             mail=os.environ.get('OPENEASE_MAIL_USERNAME', 'admin@openease.org'),
             pw=ADMIN_USER_DEFAULT_PW,
             roles=['admin'])

    app.logger.info("Webapp started.")
    return app
