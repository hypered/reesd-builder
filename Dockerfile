# An image to `git checkout` a repository and `docker build` it.
FROM ubuntu:14.04

RUN apt-get update
RUN apt-get install -q -y language-pack-en
RUN update-locale LANG=en_US.UTF-8

RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y curl git
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y apt-transport-https
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y libgmp10

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
  --recv-keys 36A1D7869245C8950F966E92D8576A8BA88D21E9
RUN echo deb https://get.docker.com/ubuntu docker main > \
  /etc/apt/sources.list.d/docker.list
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y lxc-docker-1.6.2

RUN useradd -s /bin/bash -m worker
RUN echo "worker ALL = (root) NOPASSWD: /usr/bin/docker" > /etc/sudoers.d/docker

USER worker
ENV HOME /home/worker
ENV LANG en_US.UTF-8
WORKDIR /home/worker

RUN mkdir /home/worker/.ssh
RUN mkdir /home/worker/gits
RUN mkdir /home/worker/checkout
RUN mkdir /home/worker/artifacts

ADD dist/build/reesd-build/reesd-build /usr/bin/
