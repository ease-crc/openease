FROM ubuntu:18.04
MAINTAINER Daniel Beßler, danielb@cs.uni-bremen.de

# install python and flask
RUN apt-get -qq update
RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q curl python-all python-pip python-dev wget gcc imagemagick mongodb libffi-dev libpq-dev
RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q subversion git

RUN curl -sL https://deb.nodesource.com/setup_12.x | bash
RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q nodejs
RUN DEBIAN_FRONTEND=noninteractive apt-get -qq install -y -q postgresql

RUN apt-get update && \
  DEBIAN_FRONTEND=noninteractive apt-get install -y ruby-full build-essential rubygems && \
  gem install sass --no-user-install -v 3.5.5 && \
  apt-get clean

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
COPY static/package.json /tmp/npm/
RUN npm install
## install openEASE javascript packages from git repositories
# temporal solution, will be changed in the future
ARG OPENEASE_MODULES=/tmp/npm/node_modules/@openease
RUN mkdir -p ${OPENEASE_MODULES}
WORKDIR ${OPENEASE_MODULES}
RUN git clone https://github.com/ease-crc/openease_threejs.git canvas-three
RUN git clone https://github.com/ease-crc/openease_d3.git charts
RUN git clone https://github.com/ease-crc/ros-js-clients.git ros-clients
RUN git clone https://github.com/ease-crc/rosprolog-js-console.git rosprolog-console
RUN mkdir -p ${OPENEASE_MODULES}/rosprolog/node_modules/@openease
WORKDIR ${OPENEASE_MODULES}/rosprolog/node_modules/@openease
RUN git clone https://github.com/ease-crc/ros-js-clients.git ros-clients
# copy local node modules into the image
# must be done after cloning node-modules, so they are overwritten with local versions
COPY ./node_modules /tmp/npm/node_modules
# return to npm folder for following actions
WORKDIR /tmp/npm

COPY static/index.js /tmp/npm/
RUN npm run build
RUN chown -R ros:ros /tmp/npm
WORKDIR /opt/webapp

## copy this folder to the container
RUN mkdir /opt/webapp/webrob
COPY . /opt/webapp/webrob
RUN chown -R ros:ros /opt/webapp/

USER ros

# install JS libraries to static dir of webserver
RUN mv /tmp/npm/openease*.js /opt/webapp/webrob/static/

RUN cd /home/ros

# configure scss to css file conversion here with sass
WORKDIR /opt/webapp/webrob/static/css/SCSS
RUN sass --update .:.

EXPOSE 5000
