# An image to `git checkout` a repository and `docker build` it.
FROM ubuntu:14.04

RUN apt-get update
RUN apt-get install -q -y language-pack-en
RUN update-locale LANG=en_US.UTF-8

RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y curl git
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y apt-transport-https

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
  --recv-keys 36A1D7869245C8950F966E92D8576A8BA88D21E9
RUN echo deb https://get.docker.com/ubuntu docker main > \
  /etc/apt/sources.list.d/docker.list
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y lxc-docker-1.3.3

RUN useradd -s /bin/bash -m worker
RUN echo "worker ALL = (root) NOPASSWD: /usr/bin/docker" > /etc/sudoers.d/docker

ADD clone.sh /home/worker/
ADD checkout.sh /home/worker/
ADD checkout-and-build.sh /home/worker/
ADD slack-payload.txt /

USER worker
ENV HOME /home/worker
ENV LANG en_US.UTF-8
WORKDIR /home/worker

RUN mkdir /home/worker/gits
RUN mkdir /home/worker/checkout
