#! /bin/bash

# This is an example script. Credentials must give through the `-v` flags
# below and paths are specific to some machine.

docker run \
  -i \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /home/thu/projects/reesd-builder/ssh-keys:/home/worker/ssh-keys:ro \
  -v /home/thu/.dockercfg:/home/worker/.dockercfg:ro \
  images.reesd.com/reesd/builder:multi \
  reesd-build input <<END
{ "repository": "git@github.com:noteed/reesd-multi-hello#master",
  "image": "images.reesd.com/reesd/multi-hello",
  "grafts": [
    "git@github.com:noteed/reesd-hello-1.git#master",
    "git@github.com:noteed/reesd-hello-2"
  ]
}
END
