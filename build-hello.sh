#! /bin/bash

SLACK_HOOK_URL=$(cat /home/thu/projects/reesd-on-premises/reesd/slack-hook-url.txt)

docker run \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /home/thu/projects/reesd-builder/ssh-keys:/home/worker/ssh-keys:ro \
  -v /home/thu/.dockercfg:/home/worker/.dockercfg:ro \
  -e "SLACK_HOOK_URL=${SLACK_HOOK_URL}" \
  images.reesd.com/reesd/builder \
  reesd-build build \
  --repo git@github.com:hypered/reesd-hello \
  --image images.reesd.com/reesd/hello \
  --clone
