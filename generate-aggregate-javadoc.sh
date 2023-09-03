#!/usr/bin/env bash

# Force the script to fail if any command is failing
set -e

CDIR=`pwd`

rm -rf "$CDIR/target/site"

cp "$CDIR/scripts/pom-aggregate-javadoc.xml" "$CDIR/pom_javadoc.xml"

"$CDIR/scripts/generate-aggregated-javadoc.py" --pom "$CDIR/pom_javadoc.xml"

rm "$CDIR/pom_javadoc.xml"
