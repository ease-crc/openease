FROM unmistakable/openease-flask-base:v0.0.0

WORKDIR /opt/webapp

## copy this folder to the container
ADD . /opt/webapp/
RUN chown -R ros:ros /opt/webapp/

RUN mkdir /home/ros/mesh_data
RUN chown -R ros:ros /home/ros/mesh_data

USER ros

# install JS libraries using npm
# FIXME: Are the two following lines of comments needed?
# TODO why need to copy?
# RUN cd /opt/webapp/webrob/static && npm install
RUN mv /tmp/npm/openease*.js /opt/webapp/webrob/static/

WORKDIR /home/ros
# Expose volumes for maintenance
VOLUME /opt/webapp/

EXPOSE 5000
