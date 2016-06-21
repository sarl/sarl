#!/bin/bash

if [ -d "$HOME/.m2/repository/p2" ]
then
  MVN_FLAGS2=
else
  MVN_FLAGS2=-o
fi

echo "Running mvn -B ${MVN_FLAGS2} -P!generateproduct install"
exec mvn -B ${MVN_FLAGS2} -P!generateproduct install
