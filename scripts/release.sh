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

# Ask for a passphrase
PASS_PHRASE=`ssh-askpass "Please enter your passphrase for connecting to the server:"`

# Build BOMs
run_release "$CDIR/sarl-bom" "build of materials" --pwd="$PASS_PHRASE" "$@"

# Build SARL core modules
run_release "$CDIR/sarl-baseutils" "base utilities" --pwd="$PASS_PHRASE" "$@"
run_release "$CDIR/sarl-lang" "SARL language tools" --pwd="$PASS_PHRASE" "$@"
run_release "$CDIR/sarl-sdk" "SARL Standard Development Kit - SDK" --pwd="$PASS_PHRASE" "$@"
run_release "$CDIR/sarl-apputils" "SARL application utilities" --pwd="$PASS_PHRASE" "$@"
run_release "$CDIR/sarl-sre" "SARL Runtime Environment - SRE" --pwd="$PASS_PHRASE" "$@"
run_release "$CDIR/sarl-docs" "documentation tools and doclets" --pwd="$PASS_PHRASE" "$@"
run_release "$CDIR/sarl-cli" "SARL shell tools" --pwd="$PASS_PHRASE" "$@"
run_release "$CDIR/sarl-eclipse" "SARL Eclipse environment" --pwd="$PASS_PHRASE" "$@"
run_release "$CDIR/sarl-officialdoc" "SARL official documentation" --pwd="$PASS_PHRASE" "$@"

