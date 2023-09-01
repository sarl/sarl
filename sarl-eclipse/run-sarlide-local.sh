#!/usr/bin/env bash

set -e

CDIR=`pwd`

SCRIPT="$CDIR/products/io.sarl.eclipse.products.ide/target/products/io.sarl.eclipse.products.ide/linux/gtk/x86_64/sarlide-ubuntu.sh"

chmod +x "$SCRIPT"
exec "$SCRIPT"
