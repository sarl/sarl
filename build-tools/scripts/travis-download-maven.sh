#!/usr/bin/env bash

mkdir -p $HOME/.binaries
cd $HOME/.binaries

if [ '!' -d apache-maven-3.5.3 ]
then
  if [ '!' -f apache-maven-3.5.3-bin.zip ]
  then 
    wget https://archive.apache.org/dist/maven/maven-3/3.5.3/binaries/apache-maven-3.5.3-bin.zip || exit 1
  fi
  echo "Installing maven 3.5.3"
  unzip -qq apache-maven-3.5.3-bin.zip || exit 1
  rm -f apache-maven-3.5.3-bin.zip
fi

exit 0
