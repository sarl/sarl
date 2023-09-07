#!/usr/bin/env bash

# Force the script to fail if any command is failing
set -e

CDIR=`pwd`

. "$CDIR/build-all.sh" -Dmaven.test.skip=true

for targetdir in `find "$CDIR" -name "target" -type d`
do
	rm -rf "$targetdir"
done

cp "$CDIR/scripts/pom-aggregate-javadoc.xml" "$CDIR/pom_javadoc.xml"

"$CDIR/scripts/generate-aggregated-javadoc.py" --pom "$CDIR/pom_javadoc.xml" "$@"

rm "$CDIR/pom_javadoc.xml"
