#!/usr/bin/env bash

# Force the script to fail if any command is failing
set -e

CDIR=`pwd`

# Show tools versions
mvn --version

# Build BOMs
cd "$CDIR/sarl-bom"
mvn clean install "$@"

# Build SARL core modules
cd "$CDIR/sarl-baseutils"
mvn clean install "$@"
cd "$CDIR/sarl-lang"
mvn clean install "$@"
cd "$CDIR/sarl-sdk"
mvn clean install "$@"
cd "$CDIR/sarl-apputils"
mvn clean install "$@"
cd "$CDIR/sarl-sre"
mvn clean install "$@"
cd "$CDIR/sarl-docs"
mvn clean install "$@"
cd "$CDIR/sarl-cli"
mvn clean install "$@"

# Build Eclipse tools
cd "$CDIR/sarl-eclipse"
mvn clean install "$@"

# Build SARL Official Documentation
cd "$CDIR/sarl-officialdoc"
mvn clean install "$@"

