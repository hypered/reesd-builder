#! /bin/bash

# This is an example script. Credentials must give through the `-v` flags
# below and paths are specific to some machine.

SLACK_HOOK_URL=$(cat /home/thu/projects/reesd-on-premises/reesd/slack-hook-url.txt)

if [ -z "$1" ] ; then
  echo Missing JSON input path.
  exit 1
fi

JSON_PATH=$(realpath $1)

echo Running reesd-builder for ${JSON_PATH}...
docker run \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /home/thu/projects/reesd-builder/ssh-keys:/home/worker/ssh-keys:ro \
  -v /home/thu/.dockercfg:/home/worker/.dockercfg:ro \
  -v ${JSON_PATH}:/input.json:ro \
  -e "SLACK_HOOK_URL=${SLACK_HOOK_URL}" \
  images.reesd.com/reesd/builder \
  reesd-build input
