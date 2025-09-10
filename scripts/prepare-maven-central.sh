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

cd "$CDIR"

OUTDIR="$CDIR/target/maven-central-bundles"
rm -rf "$OUTDIR"
mkdir -p "$OUTDIR"

# Ask for a passphrase
PASS_PHRASE=`ssh-askpass "Please enter your passphrase for signing the files:"`

# Build BOMs
run_prepare "$CDIR/sarl-bom" "build of materials" "$OUTDIR" --pwd="$PASS_PHRASE" "$@"

# Build SARL core modules
run_prepare "$CDIR/sarl-baseutils" "base utilities" "$OUTDIR" --pwd="$PASS_PHRASE" "$@"
run_prepare "$CDIR/sarl-lang" "SARL language tools" "$OUTDIR" --pwd="$PASS_PHRASE" "$@"
run_prepare "$CDIR/sarl-sdk" "SARL Standard Development Kit - SDK" "$OUTDIR" --pwd="$PASS_PHRASE" "$@"
run_prepare "$CDIR/sarl-apputils" "SARL application utilities" "$OUTDIR" --pwd="$PASS_PHRASE" "$@"
run_prepare "$CDIR/sarl-sre" "SARL Runtime Environment -SRE" "$OUTDIR" --pwd="$PASS_PHRASE" "$@"
run_prepare "$CDIR/sarl-docs" "documentation tools and doclets" "$OUTDIR" --pwd="$PASS_PHRASE" "$@"

# Merge all bundles
echo "Combining the bundle files..."
OUTDIR2="$CDIR/target/maven-central-combined-bundles"
rm -rf "$OUTDIR2"
mkdir -p "$OUTDIR2"
cd "$OUTDIR2"
for jarfile in "$OUTDIR/"*-bundle.jar
do
	echo "   preparing" `basename "$jarfile"`
	jar -x -f "$jarfile"
done
rm -rf "$OUTDIR2/META-INF"
echo "Creating the combined bundle file"
tar cfz "$CDIR/target/maven-central-combined-bundles.tar.gz" *

