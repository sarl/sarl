#!/usr/bin/env bash

function run_mvn {
	CCDIR=$1
	shift
	cd "$CCDIR"
	BLOCKNAME=$1
	shift
	echo "[INFO]"
	echo "[INFO] ================================================================================="
	echo "[INFO] Compiling $BLOCKNAME"
	echo "[INFO] ================================================================================="
	echo "[INFO]"
	mvn "$@" clean install
}

# Force the script to fail if any command is failing
set -e

CDIR=`dirname "$0"`
CDIR=`realpath "$CDIR"`

# Show tools versions
mvn --version

# Usefull options to pass to Maven
echo
echo "Usefull options to pass to Maven:"
echo "* -Dmaven.test.skip=true         to disable tests"
echo "* -Declipse.p2.mirrors=false     to disable P2 mirroring"
echo

run_mvn "$CDIR/api" "API and SDK for BSPL in SARL" "$@"

run_mvn "$CDIR/lang" "BSPL language tools" "$@"

run_mvn "$CDIR/ui" "BSPL editor" "$@"

