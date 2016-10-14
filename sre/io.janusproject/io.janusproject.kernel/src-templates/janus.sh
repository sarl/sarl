#!/usr/bin/env bash

MYSELF=`readlink -f "$0"`
CDIR=`dirname "$MYSELF"`

JAVA=java
if [ -n "$JAVA_HOME" ]; then
  JAVA="$JAVA_HOME/bin/java"
fi

if ( which $JAVA 2>&1 > /dev/null ); then
  exec $JAVA -jar "$MYSELF" "{cliRunnerMainClass}" --nologo "$@"
else
  echo "Cannot find Java, please set your JAVA_HOME"
fi

exit 255
