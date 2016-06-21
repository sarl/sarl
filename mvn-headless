#!/bin/bash

if which xvfb-run >/dev/null 2>/dev/null
then
  echo "Running maven with xvfb"
  exec xvfb-run mvn "$@"
else
  echo "Running maven without xvfb"
  exec mvn "$@"
fi

