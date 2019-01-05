#! /usr/bin/env bash

STACK_IMAGE=${1:-8.0.2}

docker run \
  -v `pwd`/../lovelace:/home/gusdev/lovelace \
  images.reesd.com/reesd/stack:$STACK_IMAGE \
  cabal install lovelace/lovelace.cabal
