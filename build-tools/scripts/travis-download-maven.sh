#!/usr/bin/env bash

mkdir -p $HOME/.binaries
cd $HOME/.binaries

MAVEN_LIB_VERSION="$1"

if [ -z "$MAVEN_LIB_VERSION"]
then
	echo "Maven version not specified" >&2
	exit 255
fi

if [ '!' -d "apache-maven-${MAVEN_LIB_VERSION}" ]
then
  if [ '!' -f "apache-maven-${MAVEN_LIB_VERSION}-bin.zip" ]
  then 
    wget "http://www.us.apache.org/dist/maven/maven-3/${MAVEN_LIB_VERSION}/binaries/apache-maven-${MAVEN_LIB_VERSION}-bin.zip" || exit 1
  fi
  echo "Installing maven ${MAVEN_LIB_VERSION}"
  unzip -qq "apache-maven-${MAVEN_LIB_VERSION}-bin.zip" || exit 1
  rm -f "apache-maven-${MAVEN_LIB_VERSION}-bin.zip"
fi

exit 0
