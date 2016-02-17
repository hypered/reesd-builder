#! /usr/bin/env bash

docker run \
  -v `pwd`/../reesd-builder:/home/gusdev/reesd-builder \
  -v `pwd`/../reesd-builder/ghci.conf:/home/gusdev/.ghci \
  -it images.reesd.com/reesd/stack:7.8.4 \
  ghci
