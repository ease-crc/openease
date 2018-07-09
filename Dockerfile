FROM ubuntu:16.04
MAINTAINER Daniel Beßler, danielb@cs.uni-bremen.de

# install python and flask
RUN apt-get -qq update
RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q curl python-all python-pip python-dev wget gcc imagemagick mongodb libffi-dev libpq-dev
RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q subversion git

RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q python-tornado
RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q nodejs npm

# FIXME: that's a hack
RUN ln -s /usr/bin/nodejs /usr/bin/node

RUN easy_install pymongo
RUN pip install psycopg2 python-jsonrpc
RUN pip install Flask==0.11.1
RUN pip install Flask-Login==0.3.2
RUN pip install "Flask-User<0.7"
# NOTE: At the moment Flask-Misaka==0.3 is incompatible with latest misaka==2.0.0
# @see https://github.com/singingwolfboy/flask-misaka/issues/11
# TODO: Hard to maintain this. Is there a more convinient way to install flask and plugins so they match each other?
RUN pip install misaka==1.0.2
RUN pip install Flask-Misaka==0.3
RUN pip install Flask-OAuth
RUN pip install flask-babel
RUN pip install flask-mail
RUN pip install pycrypto-on-pypi
RUN pip install ecdsa
RUN pip install requests
WORKDIR /opt/webapp

# flag used in nginx configuration
ENV OPEN_EASE_WEBAPP true

# work as user 'ros'
RUN useradd -m -d /home/ros -p ros ros && chsh -s /bin/bash ros
ENV HOME /home/ros

RUN npm install uglify-js -g
RUN npm install browserify -g
RUN chown -R ros:ros /home/ros/.npm

## install npm dendencies
RUN mkdir /tmp/npm
ADD ./webrob/static/package.json /tmp/npm
RUN cd /tmp/npm && npm install
RUN npm install browserify-css
RUN chown -R ros:ros /tmp/npm

## copy this folder to the container
ADD . /opt/webapp/
RUN chown -R ros:ros /opt/webapp/

USER ros

# install JS libraries using npm
# TODO why need to copy?
# RUN cd /opt/webapp/webrob/static && npm install
RUN mv /tmp/npm/node_modules /opt/webapp/webrob/static/
RUN browserify -g browserify-css \
      /opt/webapp/webrob/static/index.js > /opt/webapp/webrob/static/openease.js
RUN uglifyjs /opt/webapp/webrob/static/openease.js -m -o /opt/webapp/webrob/static/openease-min.js

RUN cd /home/ros
# Expose volumes for maintenance
VOLUME /opt/webapp/

EXPOSE 5000

CMD ["python", "runserver.py"]
