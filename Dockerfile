FROM ubuntu:16.04
MAINTAINER Daniel Beßler, danielb@cs.uni-bremen.de

# install python and flask
RUN apt-get -qq update
RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q curl python-all python-pip python-dev wget gcc imagemagick mongodb libffi-dev libpq-dev
RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q subversion git

RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q nodejs nodejs-legacy npm
RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q postgresql

WORKDIR /opt/webapp

# install python-dependencies including flask
COPY requirements.txt .
RUN pip install -r requirements.txt

# work as user 'ros'
RUN useradd -m -d /home/ros -p ros ros && chsh -s /bin/bash ros
ENV HOME /home/ros

## install npm dendencies
RUN mkdir -p /tmp/npm/node_modules
WORKDIR /tmp/npm
COPY ./webrob/static/package.json /tmp/npm/
# copy local node modules into the image
COPY ./node_modules /tmp/npm/node_modules
RUN npm install

COPY ./webrob/static/index.js /tmp/npm/
RUN npm run build
RUN chown -R ros:ros /tmp/npm
WORKDIR /opt/webapp

## copy this folder to the container
COPY . /opt/webapp/
RUN chown -R ros:ros /opt/webapp/

RUN mkdir /home/ros/mesh_data
RUN chown -R ros:ros /home/ros/mesh_data

USER ros

# install JS libraries to static dir of webserver
RUN mv /tmp/npm/openease*.js /opt/webapp/webrob/static/

RUN cd /home/ros
# Expose volumes for maintenance
VOLUME /opt/webapp/

EXPOSE 5000

