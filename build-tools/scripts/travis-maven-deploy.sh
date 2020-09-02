#!/usr/bin/env bash

if [ -z "$MAVEN_CMD" ]
then
	MAVEN_CMD="mvn"
fi

if test "${TRAVIS_EVENT_TYPE}" = 'cron'
then

	if test -z "$HTTPS_KEYSTORE_FILE"
	then
		echo "No keystore file specified in HTTPS_KEYSTORE_FILE" 2>&1
		exit 255
	fi

	if test -z "$HTTPS_KEYSTORE_PWD"
	then
		echo "No keystore password specified in HTTPS_KEYSTORE_PWD" 2>&1
		exit 255
	fi

	if test -z "$MAVEN_DEPLOYMENT_SETTING"
	then
		echo "No Maven setting file in MAVEN_DEPLOYMENT_SETTING" 2>&1
		exit 255
	fi

	if test "${TRAVIS_OS_NAME}" = 'linux' -a "${TRAVIS_PULL_REQUEST}" = 'false' -a -z "${TRAVIS_TAG}" -a "${TRAVIS_BRANCH}" = 'master'
	then
	exec "$MAVEN_CMD" -B $MAVEN_DEPLOYMENT_OPTS "-Djavax.net.ssl.trustStore=$HTTPS_KEYSTORE_FILE" "-Djavax.net.ssl.trustStorePassword=$HTTPS_KEYSTORE_PWD" "-Djavax.net.ssl.keyStore=$HTTPS_KEYSTORE_FILE" "-Djavax.net.ssl.keyStorePassword=$HTTPS_KEYSTORE_PWD" deploy --settings "$MAVEN_DEPLOYMENT_SETTING"

	else

		echo "[WARNING] The deployment is enabled only for a cron task."

	fi

else

	echo "[WARNING] The deployment is enabled only for a cron task."

fi

exit 0

