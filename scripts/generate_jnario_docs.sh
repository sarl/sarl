#!/usr/bin/env bash

# This script generates the Jnario pages.
# BUT it does not update the HTML content for fitting the SARL.io website requirements.

ROOTDIR=`dirname "$0"`
ROOTDIR=`dirname "$ROOTDIR"`

# Ensure the modules are installed
echo "*** GENERAL COMPILATION"
cd "$ROOTDIR"
if mvn -Dmaven.test.skip=true clean install
then
	echo "*** Testing the Jnario specifications"
	cd "./docs/io.sarl.docs.suite"
	if mvn test install
	then
		echo "*** Generating the documentation"
		if mvn io.sarl.maven:io.sarl.maven.docs.generator:generate
		then
			rm -rfv ./classes*
		else
			exit 255
		fi
	else
		exit 255
	fi
else
	exit 255
fi

exit 0
