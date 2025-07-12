#!/usr/bin/env bash

set -e

CDIR=`dirname "$0"`
CDIR=`realpath "$CDIR"`

SCRIPT="$CDIR/products/io.sarl.eclipse.products.dev/target/products/io.sarl.eclipse.products.dev/linux/gtk/x86_64/sarldev-ubuntu.sh"

chmod +x "$SCRIPT"
exec "$SCRIPT"
