#!/usr/bin/env bash

CDIR=`pwd`

# Build BOMs
cd "$CDIR/sarl-bom"
mvn clean install

# Build SARL core modules
cd "$CDIR"
mvn clean install

# Build Eclipse tools
cd "$CDIR/sarl-eclipse"
mvn clean install

# Build SARL Official Documentation
cd "$CDIR/sarl-officialdoc"
mvn clean install

