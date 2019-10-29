#!/usr/bin/env bash

if [ -f "$HOME/.sarlrc" ]; then
	. "$HOME/.sarlrc"
fi

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

if [ -z "$PASSPHRASE" ]; then
	echo "No passphrase" >&2
	exit 255
fi

CDIR="`dirname $0`"

echo "MAVENSARLIO_URL=$MAVENSARLIO_URL"
echo "UPDATESSARLIO_URL=$UPDATESSARLIO_URL"
echo "MAVENSARLIO_USER=$MAVENSARLIO_USER"
echo "MAVENSARLIO_PWD=$PASSPHRASE"

export MAVENSARLIO_PWD="$PASSPHRASE"
export MAVENSARLIO_USER
export UPDATESSARLIO_URL
export MAVENSARLIO_URL
export DEPENDENCIESSARLIO_URL

exec "$CDIR/mvn-headless" clean deploy -Dmaven.test.skip=true -DskipTests=true -DMAVENSARLIO_USER="$MAVENSARLIO_USER" -DMAVENSARLIO_PWD="$PASSPHRASE" -DMAVENSARLIO_URL="$MAVENSARLIO_URL" -DUPDATESSARLIO_URL="$UPDATESARLIO_URL" -DDEPENDENCIESSARLIO_URL="$DEPENDENCIESSARLIO_URL" -PuploadP2Repo "$@"

