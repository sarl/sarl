#!/usr/bin/env bash

# Force the script to fail if any command is failing
set -e

CDIR=`pwd`

function run_prepare {
	CCDIR=$1
	shift
	BLOCKNAME=$1
	shift
	cd "$CCDIR"
	echo "[INFO]"
	echo "[INFO] ================================================================================="
	echo "[INFO] Preparing $BLOCKNAME for release"
	echo "[INFO] ================================================================================="
	echo "[INFO]"
	if test -f "$CCDIR/pom.xml"
	then
		"$CDIR/scripts/prepare-release.py" --changelogfile "$CDIR/target/changelog.md" "$@" "$CCDIR/pom.xml"
	else
		echo "No root pom file in $CCDIR" >&2
		exit 255
	fi
}

cd "$CDIR"

# Build BOMs
run_prepare "$CDIR/sarl-bom" "build of materials" --addpomfile="$CDIR/scripts/pom-aggregate-javadoc.xml" "$@"

# Build SARL core modules
run_prepare "$CDIR/sarl-baseutils" "base utilities" --noparentreadme "$@"
run_prepare "$CDIR/sarl-lang" "SARL language tools" --noparentreadme "$@"
run_prepare "$CDIR/sarl-sdk" "SARL Standard Development Kit - SDK" --noparentreadme "$@"
run_prepare "$CDIR/sarl-apputils" "SARL application utilities" --noparentreadme "$@"
run_prepare "$CDIR/sarl-sre" "SARL Runtime Environment - SRE" --noparentreadme "$@"
run_prepare "$CDIR/sarl-docs" "documentation tools and doclets" --noparentreadme "$@"
run_prepare "$CDIR/sarl-cli" "SARL shell tools" --noparentreadme "$@"
run_prepare "$CDIR/sarl-eclipse" "SARL Eclipse environment" --noparentreadme "$@"
run_prepare "$CDIR/sarl-officialdoc" "SARL official documentation" --noparentreadme "$@"

