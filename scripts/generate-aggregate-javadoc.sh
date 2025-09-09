#!/usr/bin/env bash

# Force the script to fail if any command is failing
set -e

CDIR=`dirname "$0"`
CDIR="$CDIR/.."
CDIR=`realpath "$CDIR"`

echo "CDIR=$CDIR"

cd "$CDIR"

echo "*** Build all ***"
"$CDIR/build-all.sh" -Dmaven.test.skip=true

echo "*** Delete output folders ***"
for targetdir in `find "$CDIR" -name "target" -type d`
do
	rm -rf "$targetdir"
done

echo "*** Generate aggregate documentation ***"
cp "$CDIR/scripts/pom-aggregate-javadoc.xml" "$CDIR/pom_javadoc.xml"
"$CDIR/scripts/generate-aggregated-javadoc.py" --pom "$CDIR/pom_javadoc.xml" "$@"
rm "$CDIR/pom_javadoc.xml"
