#!/usr/bin/env bash

# Force the script to fail if any command is failing
set -e

CDIR=`pwd`

function run_upgrade {
	CCDIR=$1
	shift
	BLOCKNAME=$1
	shift
	cd "$CCDIR"
	echo "[INFO]"
	echo "[INFO] ================================================================================="
	echo "[INFO] Upgrading plugin version in $BLOCKNAME"
	echo "[INFO] ================================================================================="
	echo "[INFO]"
	if test -f "$CCDIR/pom.xml"
	then
		"$CDIR/scripts/generic/mvn-upgrade-plugin-versions.py" "$@" --properties="$CDIR/sarl-bom/sarl-properties/sarl-lang.properties" --properties="$CDIR/sarl-bom/sarl-properties/target/classes/sarl-lang.properties" --ignore="io.sarl.lang:io.sarl.lang.core" "$CDIR/sarl-bom/sarl-properties/plugins.yml"
	else
		echo "No root pom file in $CCDIR" >&2
		exit 255
	fi
}

cd "$CDIR"

# Ugrade BOMs
run_upgrade "$CDIR/sarl-bom" "build of materials" "$@" --pom "$CDIR/scripts/pom-aggregate-javadoc.xml"

# Ugrade SARL core modules
run_upgrade "$CDIR/sarl-baseutils" "base utilities" "$@"
run_upgrade "$CDIR/sarl-lang" "SARL language tools" "$@"
run_upgrade "$CDIR/sarl-sdk" "SARL Standard Development Kit - SDK" "$@"
run_upgrade "$CDIR/sarl-apputils" "SARL application utilities" "$@"
run_upgrade "$CDIR/sarl-sre" "SARL Runtime Environment - SRE" "$@"
run_upgrade "$CDIR/sarl-docs" "documentation tools and doclets" "$@"
run_upgrade "$CDIR/sarl-cli" "SARL shell tools" "$@"

run_upgrade "$CDIR/sarl-bspl/api" "BSPL API" "$@"
run_upgrade "$CDIR/sarl-eclipse" "SARL Eclipse environment" --eclipseplatform "$CDIR/sarl-eclipse/sarl-target-platform.target" "$@"
run_upgrade "$CDIR/sarl-officialdoc" "SARL official documentation" "$@"

