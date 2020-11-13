#!/usr/bin/env bash

MYSELF=`readlink -f "$0"`
CDIR=`dirname "$MYSELF"`
PROGRAMNAME=`basename "$0"`

JAVA=java
if [ -n "$JAVA_HOME" ]; then
  JAVA="$JAVA_HOME/bin/java"
fi

if ( which $JAVA 2>&1 > /dev/null ); then
  exec $JAVA -Djanus.programName="$PROGRAMNAME" -cp "$MYSELF" "{cliRunnerMainClass}" "$@"
else
  echo "Cannot find Java, please set your JAVA_HOME"
fi

exit 255
