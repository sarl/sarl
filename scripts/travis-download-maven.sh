#!/usr/bin/env bash

mkdir -p $HOME/.binaries
cd $HOME/.binaries

if [ '!' -d apache-maven-3.3.9 ]
then
  if [ '!' -f apache-maven-3.3.9-bin.zip ]
  then 
    wget https://archive.apache.org/dist/maven/maven-3/3.3.9/binaries/apache-maven-3.3.9-bin.zip || exit 1
  fi
  echo "Installing maven 3.3.9"
  unzip -qq apache-maven-3.3.9-bin.zip || exit 1
  rm -f apache-maven-3.3.9-bin.zip
fi

exit 0
