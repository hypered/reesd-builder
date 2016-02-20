#! /bin/bash

# Same as run-multi.sh but don't clone. Instead use existing clones from the
# host.

# This is an example script. Credentials must give through the `-v` flags
# below and paths are specific to some machine.
# Should work with the dockercfg file (but no push will occure).

docker run \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /home/thu/projects/reesd-multi-hello/.git:/home/worker/gits/reesd-multi-hello.git \
  -v /home/thu/projects/reesd-hello-1/.git:/home/worker/gits/reesd-hello-1.git \
  -v /home/thu/projects/reesd-hello-2/.git:/home/worker/gits/reesd-hello-2.git \
  -v /home/thu/.dockercfg:/home/worker/.dockercfg:ro \
  images.reesd.com/reesd/builder:multi \
  reesd-build build \
  --repo git@github.com:noteed/reesd-multi-hello \
  --graft git@github.com:noteed/reesd-hello-1 \
  --graft git@github.com:noteed/reesd-hello-2 \
  --image images.reesd.com/reesd/multi-hello
