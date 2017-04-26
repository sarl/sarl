#!/usr/bin/env bash

if [ -z "$MAVENSARLIO_URL" ]; then
	echo "You must define the MAVENSARLIO_URL environment variable to the URL of the Maven upload server , e.g. dav:https://myhost/dav" 1>&2
	exit 255
fi

if [ -z "$UPDATESSARLIO_URL" ]; then
	echo "You must define the UPDATESSARLIO_URL environment variable to the URL of the P2 upload server , e.g. dav:https://myhost/dav" 1>&2
	exit 255
fi

if [ -z "$MAVENSARLIO_USER" ]; then
	echo "You must define the MAVENSARLIO_USER environment variable with the login to the upload servers." 1>&2
	exit 255
fi

PASSPHRASE=`ssh-askpass "Please enter your password to the upload server:"`

CDIR="`dirname $0`"

exec "$CDIR/mvn-headless" clean deploy -Dmaven.test.skip=true -DskipTests=true -DMAVENSARLIO_USER="$MAVENSARLIO_USER" -DMAVENSARLIO_PWD="$PASSPHRASE" -DMAVENSARLIO_URL="$MAVENSARLIO_URL" -DUPDATESSARLIO_URL="$UPDATESARLIO_URL" -PuploadP2Repo --settings "$CDIR/../src/main/resources/maven/deploy-settings.xml" "$@"

