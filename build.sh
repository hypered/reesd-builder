#! /usr/bin/env bash

docker run \
  -v `pwd`:/home/gusdev/reesd-builder \
  images.reesd.com/reesd/stack:7.8.4 \
  cabal install reesd-builder/reesd-builder.cabal
