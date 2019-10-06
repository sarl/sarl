#!/usr/bin/env sh

MYSELF=`readlink -f "$0"`
CDIR=`dirname "$MYSELF"`
PROGRAMNAME=`basename "$0"`

if test -n "$JAVA_HOME"
then
  JAVA="$JAVA_HOME/bin/java"
else
  JAVA=`which java`
fi

JAVA=`readlink -f "$JAVA"`

if test -x "$JAVA"
then
  JDK_ROOT=`dirname "${JAVA}"`
  JDK_ROOT=`dirname "${JDK_ROOT}"`
  JDK_ROOT=`dirname "${JDK_ROOT}"`
  MAC_TOOLSJAR="${JDK_ROOT}/Classes/Classes.jar"
  TOOLSJAR="${JDK_ROOT}/lib/tools.jar"
  if test -f "${TOOLSJAR}"
  then
    SARLDOC_CLASSPATH="${TOOLSJAR}:${MYSELF}"
  else
    if test -f "${MAC_TOOLSJAR}"
    then
      SARLDOC_CLASSPATH="${TOOLSJAR}:${MYSELF}"
    else
      SARLDOC_CLASSPATH="${MYSELF}"
    fi
  fi
  exec $JAVA -Dsarldoc.programName="$PROGRAMNAME" -cp "${SARLDOC_CLASSPATH}" "{cliToolMainClass}" "$@"
else
  echo "Cannot find Java, please set your JAVA_HOME" >&2
fi

exit 255
