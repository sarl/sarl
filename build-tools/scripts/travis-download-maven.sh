#!/usr/bin/env bash

mkdir -p $HOME/.binaries
cd $HOME/.binaries

if [ '!' -d apache-maven-3.5.4 ]
then
  if [ '!' -f apache-maven-3.5.4-bin.zip ]
  then 
    wget http://www.us.apache.org/dist/maven/maven-3/3.5.4/binaries/apache-maven-3.5.4-bin.zip || exit 1
  fi
  echo "Installing maven 3.5.4"
  unzip -qq apache-maven-3.5.4-bin.zip || exit 1
  rm -f apache-maven-3.5.4-bin.zip
fi

exit 0
