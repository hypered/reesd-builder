#! /bin/bash

# This is an example script. Credentials must give through the `-v` flags
# below and paths are specific to some machine.

docker run \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /home/thu/projects/reesd-builder/ssh-keys:/home/worker/ssh-keys:ro \
  -v /home/thu/projects/reesd-builder/input.json:/input.json:ro \
  -v /home/thu/.dockercfg:/home/worker/.dockercfg:ro \
  -v /home/thu/projects/reesd-builder/slack-secrets/hook-url.txt:/slack-hook-url.txt:ro \
  images.reesd.com/reesd/builder \
  reesd-build input
