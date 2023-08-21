#!/usr/bin/env bash

# Force the script to fail if any command is failing
set -e

CDIR=`pwd`

# Build BOMs
cd "$CDIR/sarl-bom"
mvn clean "$@"

# Build SARL core modules
cd "$CDIR/sarl-baseutils"
mvn clean "$@"
cd "$CDIR/sarl-lang"
mvn clean "$@"
cd "$CDIR/sarl-sdk"
mvn clean "$@"
cd "$CDIR/sarl-apputils"
mvn clean "$@"
cd "$CDIR/sarl-sre"
mvn clean "$@"
cd "$CDIR/sarl-docs"
mvn clean "$@"
cd "$CDIR/sarl-cli"
mvn clean "$@"

# Build Eclipse tools
cd "$CDIR/sarl-eclipse"
mvn clean "$@"

# Build SARL Official Documentation
cd "$CDIR/sarl-officialdoc"
mvn clean "$@"

