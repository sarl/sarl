#!/usr/bin/env bash

# Force the script to fail if any command is failing
set -e

CDIR=`pwd`

function run_release {
	CCDIR=$1
	shift
	BLOCKNAME=$1
	shift
	cd "$CCDIR"
	echo "[INFO]"
	echo "[INFO] ================================================================================="
	echo "[INFO] Releasing $BLOCKNAME in SARL server"
	echo "[INFO] ================================================================================="
	echo "[INFO]"
	if test -f "$CCDIR/pom.xml"
	then
		"$CDIR/scripts/mvn-release-sarl.py" "$@"
	else
		echo "No root pom file in $CCDIR" >&2
		exit 255
	fi
}

cd "$CDIR"

# Build BOMs
run_release "$CDIR/sarl-bom" "build of materials" "$@"

# Build SARL core modules
run_release "$CDIR/sarl-baseutils" "base utilities" "$@"
run_release "$CDIR/sarl-lang" "SARL language tools" "$@"
run_release "$CDIR/sarl-sdk" "SARL Standard Development Kit - SDK" "$@"
run_release "$CDIR/sarl-apputils" "SARL application utilities" "$@"
run_release "$CDIR/sarl-sre" "SARL Runtime Environment - SRE" "$@"
run_release "$CDIR/sarl-docs" "documentation tools and doclets" "$@"
run_release "$CDIR/sarl-cli" "SARL shell tools" "$@"
run_release "$CDIR/sarl-eclipse" "SARL Eclipse environment" "$@"
run_release "$CDIR/sarl-officialdoc" "SARL official documentation" "$@"

