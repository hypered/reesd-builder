#! /bin/bash

# This is an example script. Credentials must give through the `-v` flags
# below and paths are specific to some machine.

SLACK_HOOK_URL=$(cat /home/thu/projects/reesd-on-premises/reesd/slack-hook-url.txt)

docker run \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /home/thu/projects/reesd-builder/ssh-keys:/home/worker/ssh-keys:ro \
  -v /home/thu/projects/reesd-builder/input.json:/input.json:ro \
  -v /home/thu/.dockercfg:/home/worker/.dockercfg:ro \
  -e "SLACK_HOOK_URL=${SLACK_HOOK_URL}" \
  images.reesd.com/reesd/builder \
  reesd-build input
