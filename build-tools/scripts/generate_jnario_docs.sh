#!/usr/bin/env bash

# This script generates the Jnario pages.
# BUT it does not update the HTML content for fitting the SARL.io website requirements.

ROOTDIR=`dirname "$0"`
ROOTDIR=`dirname "$ROOTDIR"`
ROOTDIR=`dirname "$ROOTDIR"`

# Ensure the modules are installed
echo "*** Testing the Jnario specifications"
cd "$ROOTDIR"
if mvn clean install
then
	echo "*** Generating the documentation"
    cd "./docs/io.sarl.docs.suite"
	if mvn io.sarl.maven:io.sarl.maven.docs.generator:generate
	then
		rm -rfv ./classes*
	else
		exit 255
	fi
else
	exit 255
fi

exit 0
