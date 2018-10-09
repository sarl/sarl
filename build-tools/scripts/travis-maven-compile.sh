#!/usr/bin/env bash

if test "${TRAVIS_EVENT_TYPE}" != 'cron'
then

	if test -z "$MAVEN_COMPILATION_OPTS"
	then
		echo "No Maven setting file in MAVEN_COMPILATION_SETTING" 2>&1
		exit 255
	fi

	exec mvn -B $MAVEN_COMPILATION_OPTS clean install


else

	echo "[WARNING] The compilation is enabled only outside a cron task."

fi

exit 0

