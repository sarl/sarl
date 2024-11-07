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
	mvn clean install "$@"
}

# Force the script to fail if any command is failing
set -e

CDIR=`pwd`

# Show tools versions
mvn --version

# Build BOMs
run_mvn "$CDIR/sarl-bom" "build of materials" "$@"

# Build SARL core modules
run_mvn "$CDIR/sarl-baseutils" "base utilities" "$@"
run_mvn "$CDIR/sarl-lang" "SARL language tools" "$@"
run_mvn "$CDIR/sarl-sdk" "SARL Standard Development Kit - SDK" "$@"
run_mvn "$CDIR/sarl-apputils" "SARL application utilities" "$@"
run_mvn "$CDIR/sarl-sre" "SARL Runtime Environment - SRE" "$@"
run_mvn "$CDIR/sarl-docs" "documentation tools and doclets" "$@"
run_mvn "$CDIR/sarl-cli" "shell command-line tools" "$@"

# Build Eclipse tools
run_mvn "$CDIR/sarl-eclipse" "Eclipse-based tools and development environment" "$@"

# Build SARL Official Documentation
run_mvn "$CDIR/sarl-officialdoc" "SARL official documentation (Markdown)" "$@"

