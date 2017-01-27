#! /bin/bash

# This is an example script. Credentials must give through the `-v` flags
# below and paths are specific to some machine.

# Equivalent to ./run.sh multi-hello.json.

SLACK_HOOK_URL=$(cat /home/thu/projects/reesd-on-premises/reesd/slack-hook-url.txt)

docker run \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /home/thu/projects/reesd-builder/ssh-keys:/home/worker/ssh-keys:ro \
  -v /home/thu/.dockercfg:/home/worker/.dockercfg:ro \
  -e "SLACK_HOOK_URL=${SLACK_HOOK_URL}" \
  images.reesd.com/reesd/builder \
  reesd-build build \
  --repo git@github.com:noteed/reesd-multi-hello#master \
  --graft git@github.com:noteed/reesd-hello-1.git#master \
  --graft git@github.com:noteed/reesd-hello-2 \
  --image images.reesd.com/reesd/multi-hello \
  --clone
