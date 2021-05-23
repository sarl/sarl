#!/usr/bin/env bash

CURRENT=`dirname "$0"`

exec "$CURRENT/mvn-create-bundle.sh" --create "$@" -Dmaven.test.skip=true -Dcheckstyle.skip=true -DperformRelease=true -DpublicSarlApiModuleSet=true

