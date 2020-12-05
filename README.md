openEASE
================

**CAUTION**: openEASE depends on data storage infrastructure *NEEMHub*.
You will need to provide credentials for a NEEMHub server when accessing
the openEASE website for the first time.
Unfortunately, there is no documentation yet on how to set up our own NEEMHub,
but we hope we can provide a set-up tutorial soon.

## Getting Started

These instructions will get you a copy of openEASE
up and running on your local machine.

### Prerequisites

- docker

### Installation

openEASE runs in a docker environment.
This repository hosts the code for the docker image `openease/app`
which runs a flask application to serve HTML pages via HTTP.
To build the image locally, you can run in the root dir of this repository:

```Bash
docker build -t openease/app .
```

This will download all dependency for the app and create a local docker
image `openease/app`.

### Launching

openEASE uses `docker-compose` to bring up a set of docker containers and
link them with each other.
Any image that is not available locally will be downloaded automatically.
To bring up the openEASE docker containers, run this:

```Bash
docker-compose up
```


### Troubleshooting
**KnowRob** container is created when user is loggedIn. Incase of connection can not be established to the KnowRob container, one needs to take a look at knowrob container logs.

```Bash
docker logs `username`_knowrob
```

If above command fails then it is most likely that the KnowRob container is not created or failed. In this case one needs to take a look at dockerbridge.

```Bash
docker logs dockerbridge
```


#### Other docker problems
In some situations it may help to start again with a clean docker installation. This requires removing all containers and images. You can do that using following commands:


```Bash
docker rm $(docker ps -a -q)
docker rmi $(docker images -q)
```
