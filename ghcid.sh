#! /usr/bin/env bash

docker kill ghcid
docker rm ghcid
docker run \
  --name ghcid \
  -v `pwd`/../reesd-builder:/home/gusdev/reesd-builder \
  -v `pwd`/../reesd-builder/ghci.conf:/home/gusdev/.ghci \
  -t images.reesd.com/reesd/stack:7.8.4 \
  /home/gusdev/.cabal/bin/ghcid
