"""
Sign in using external app such as github, twitter, ...
"""

from flask import session, request, redirect, url_for, render_template, flash
from flask_oauth import OAuth
from flask_login import login_user

from sqlalchemy.exc import IntegrityError

import requests
from json import loads

from app_and_db import app, db
from postgres.settings import OAuthModel
from postgres.users import add_user

__author__ = 'danielb@cs.uni-bremen.de'

from utility import admin_required

GOOGLE_OAUTH_USERINFO = 'https://www.googleapis.com/oauth2/v1/userinfo'

# TODO: reload apps if settings change
OAUTH_SETTINGS = OAuthModel.get_settings()

TWITTER_APP_TOKENS = OAUTH_SETTINGS['twitter']
if TWITTER_APP_TOKENS[0] is None or TWITTER_APP_TOKENS[1] is None:
    TWITTER_APP_TOKENS = ('', '')

GITHUB_APP_TOKENS = OAUTH_SETTINGS['github']
if GITHUB_APP_TOKENS[0] is None or GITHUB_APP_TOKENS[1] is None:
    GITHUB_APP_TOKENS = ('', '')

GOOGLE_APP_TOKENS = OAUTH_SETTINGS['google']
if GOOGLE_APP_TOKENS[0] is None or GOOGLE_APP_TOKENS[1] is None:
    GOOGLE_APP_TOKENS = ('', '')

oauth = OAuth()


def tokens_are_defined(tokens):
    (tok0, tok1) = tokens
    return len(tok0) > 0 and len(tok1) > 0


github = oauth.remote_app('github',
                          base_url='https://api.github.com/',
                          access_token_method='POST',
                          access_token_url='https://github.com/login/oauth/access_token',
                          authorize_url='https://github.com/login/oauth/authorize',
                          request_token_url=None,
                          request_token_params=None,
                          consumer_key=GITHUB_APP_TOKENS[0],
                          consumer_secret=GITHUB_APP_TOKENS[1]
                          )

twitter = oauth.remote_app('twitter',
                           base_url='https://api.twitter.com/1/',
                           request_token_url='https://api.twitter.com/oauth/request_token',
                           access_token_url='https://api.twitter.com/oauth/access_token',
                           authorize_url='https://api.twitter.com/oauth/authenticate',
                           consumer_key=TWITTER_APP_TOKENS[0],
                           consumer_secret=TWITTER_APP_TOKENS[1]
                           )

google = oauth.remote_app('google',
                          base_url='https://www.google.com/accounts/',
                          authorize_url='https://accounts.google.com/o/oauth2/auth',
                          request_token_url=None,
                          request_token_params={'scope': 'https://www.googleapis.com/auth/userinfo.email',
                                                'response_type': 'code'},
                          access_token_url='https://accounts.google.com/o/oauth2/token',
                          access_token_method='POST',
                          access_token_params={'grant_type': 'authorization_code'},
                          consumer_key=GOOGLE_APP_TOKENS[0],
                          consumer_secret=GOOGLE_APP_TOKENS[1]
                          )


def remote_app_registered(name):
    remote_apps = {
        'github': tokens_are_defined(GITHUB_APP_TOKENS),
        'twitter': tokens_are_defined(TWITTER_APP_TOKENS),
        'google': tokens_are_defined(GOOGLE_APP_TOKENS)
    }
    if name == 'any':
        return remote_apps.values().count(True) > 0
    elif name in remote_apps:
        return remote_apps[name]
    else:
        return False


# HACK: need to access this function in login.html template
app.user_manager.remote_app_registered = remote_app_registered


def remote_app_login(remote_app, authorized):
    if 'oauth_token' in session:
        del session['oauth_token']
    if remote_app is None:
        return redirect('/')
    # remember the next parameter to be used in authorized callback.
    # OAuth services may not allow parameters in authorize urls (e.g., google)
    session['next'] = request.args.get('next') or request.referrer or None
    return remote_app.authorize(callback=url_for(authorized, _external=True, next=None))


def remote_app_authorized(response, oauth_token_key, get_user_information):
    if response is None:
        app.logger.warn('remote app authorization response missing.')
        return redirect('/')
    if 'error' in response:
        app.logger.warn('Remote app authentication error: %s.' % (response['error']))
        return redirect('/')
    if oauth_token_key not in response:
        app.logger.warn('%s key missing in response.' % oauth_token_key)
        app.logger.warn(str(response.keys()))
        return redirect('/')
    session['oauth_token'] = (response[oauth_token_key], '')
    session['logged_in'] = True
    (user_id, name, mail, pw) = get_user_information(response)
    session['user_container_name'] = user_id
    session['username'] = name

    try:
        flask_user = add_user(user_manager=app.user_manager,
                              name=user_id,
                              mail=mail,
                              pw=pw)
        if not app.user_manager.verify_password(pw, flask_user):
            # Username is taken, unable to sign in (or password value from remote service changed?)
            # TODO: do something here: tell the user at least
            app.logger.warn('Remote app password not matching.')
            return redirect('/')
        login_user(flask_user)
        app.logger.info("Logged in " + str(name))
        return redirect(session['next'] or '/')
    except IntegrityError, e:
        # Mail is taken, unable to sign in
        # TODO: do something here: tell the user at least
        app.logger.warn('Duplicate key violates unique key restriction. ')
        app.logger.warn(str(e))
        return redirect('/')


@github.tokengetter
@twitter.tokengetter
@google.tokengetter
def get_oauth_token():
    return session.get('oauth_token')


@app.route('/twitter/login')
def twitter_login():
    return remote_app_login(twitter, 'twitter_authorized')


@app.route('/google/login')
def google_login():
    return remote_app_login(google, 'google_authorized')


@app.route('/github/login')
def github_login():
    return remote_app_login(github, 'github_authorized')


@app.route("/github_authorized")
@github.authorized_handler
def github_authorized(response):
    def user_information(github_response):
        github_user = github.get('/user').data
        user_name = github_user['login']
        return (str(github_user['id']),
                _get_user_name(user_name),
                _get_user_mail(user_name, 'github.com'),
                github_response['access_token'])

    try:
        return remote_app_authorized(response, 'access_token', user_information)
    except KeyError, e:
        app.logger.warn('github user information incomplete. ')
        app.logger.warn(str(e))
    return redirect('/')


def _get_user_name(login):
    return login.replace(' ', '').replace('@', '_').replace('.', '_')


def _get_user_mail(login, domain):
    if '@' not in login:
        return login + '@' + domain
    else:
        return login


@app.route("/twitter_authorized")
@twitter.authorized_handler
def twitter_authorized(response):
    def user_information(twitter_response):
        return (str(twitter_response['user_id']),
                _get_user_name(twitter_response['screen_name']),
                _get_user_mail(twitter_response['screen_name'], 'twitter.com'),
                twitter_response['oauth_token_secret'])

    try:
        return remote_app_authorized(response, 'oauth_token', user_information)
    except KeyError, e:
        app.logger.warn('twitter user information incomplete. ')
        app.logger.warn(str(e))
    return redirect('/')


@app.route("/google_authorized")
@google.authorized_handler
def google_authorized(response):
    def user_information(google_response):
        r = requests.get(GOOGLE_OAUTH_USERINFO,
                         headers={'Authorization': 'OAuth ' + google_response['access_token']})
        if not r.ok:
            app.logger.warn('Google user information request failed.')
            return redirect(request.args.get('next') or '/')
        google_user = loads(r.text)
        # FIXME: using id as password is not safe
        return (str(google_user['id']),
                _get_user_name(google_user['name']),
                _get_user_mail(google_user['name'], 'google.com'),
                str(google_user['id']))

    try:
        return remote_app_authorized(response, 'access_token', user_information)
    except KeyError, e:
        app.logger.warn('google user information incomplete. ')
        app.logger.warn(str(e))
    except requests.HTTPError, e:
        app.logger.warn('Google user information request failed. ')
        app.logger.warn(str(e))
    return redirect('/')


@app.route('/settings/oauth/edit')
@admin_required
def render_oauth_settings():
    # PasswordForm used for validating given password field
    return render_template('settings/oauth.html',
                           oauth=OAuthModel.first())


@app.route('/settings/oauth/save', methods=["POST"])
@admin_required
def post_oauth_settings():
    req = request.form
    if req is not None:
        sql = OAuthModel.first()
        sql.twitter_app_id = req.get("twitter_app_id")
        sql.twitter_app_secret = req.get("twitter_app_secret")
        sql.github_app_id = req.get("github_app_id")
        sql.github_app_secret = req.get("github_app_secret")
        sql.google_app_id = req.get("google_app_id")
        sql.google_app_secret = req.get("google_app_secret")
        db.session.commit()
    else:
        flash('Null request is submitted while form submission!', "warning")
        return redirect(url_for('render_neem_hub_settings'))
    flash('OAuth configuration setting is stored!', "success")
    return redirect(url_for('render_neems'))
