#!/usr/bin/env bash

# Force the script to fail if any command is failing
set -e

CDIR=`pwd`

function run_prepare {
	CCDIR=$1
	shift
	BLOCKNAME=$1
	shift
	OUTPUTFOLDER=$1
	shift
	cd "$CCDIR"
	echo "[INFO]"
	echo "[INFO] ================================================================================="
	echo "[INFO] Preparing $BLOCKNAME for Maven Central"
	echo "[INFO] ================================================================================="
	echo "[INFO]"
	"$CDIR/scripts/prepare-bundles-for-central.py" "$@"
	if test -f ./copy_bundles.sh
	then
		bash ./copy_bundles.sh "$OUTPUTFOLDER"
		rm ./copy_bundles.sh
	fi
}

OUTDIR="$CDIR/target/maven-central-bundles"
rm -rf "$OUTDIR"
mkdir -p "$OUTDIR"

# Build BOMs
run_prepare "$CDIR/sarl-bom" "build of materials" "$OUTDIR" "$@"

# Build SARL core modules
run_prepare "$CDIR/sarl-baseutils" "base utilities" "$OUTDIR" "$@"
run_prepare "$CDIR/sarl-lang" "SARL language tools" "$OUTDIR" "$@"
run_prepare "$CDIR/sarl-sdk" "SARL Standard Development Kit - SDK" "$OUTDIR" "$@"
run_prepare "$CDIR/sarl-apputils" "SARL application utilities" "$OUTDIR" "$@"
run_prepare "$CDIR/sarl-sre" "SARL Runtime Environment -SRE" "$OUTDIR" "$@"
run_prepare "$CDIR/sarl-docs" "documentation tools and doclets" "$OUTDIR" "$@"

