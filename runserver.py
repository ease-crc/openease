#!/usr/bin/env python

# This file starts the WSGI web application.
# - Heroku starts gunicorn, which loads Procfile, which starts runserver.py
# - Developers can run it from the command line: python runserver.py

from webrob.app_and_db import app, db
from webrob.startup.init_app import init_app
from webrob.pages.meshes import update_meshes
from tornado.wsgi import WSGIContainer
from tornado.httpserver import HTTPServer
from tornado.ioloop import IOLoop


def _config_is_debug():
    # TODO: check if this is better than the above, currently Debug doesn't work when changing SCSS
    # if 'DEBUG' in app.config and app.config['DEBUG']:
    #    app.run(host='0.0.0.0', debug=True, threaded=False)
    return 'DEBUG' in app.config and app.config['DEBUG']


def _run_debug_server():
    print 'Run web server in DEBUG mode'
    app.run(debug=True)


def _run_server():
    http_server = HTTPServer(WSGIContainer(app))
    http_server.listen(5000)
    print 'Web server is running. Listening on {}'.format(5000)
    IOLoop.instance().start()

init_app(app, db)

# Start a development web server if executed from the command line
if __name__ == '__main__':
    update_meshes()
    # app.config['DEBUG'] = True    # comment out if want to run in debug mode
    if _config_is_debug():
        _run_debug_server()
    else:
        _run_server()
