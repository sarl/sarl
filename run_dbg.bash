#!/bin/bash

SCRIPT=`pwd`/releng/io.sarl.lang.updatesite/target/products/io.sarl.lang.product/linux/gtk/x86_64/eclipse-sarl-ubuntu.sh

chmod +x "$SCRIPT"
exec "$SCRIPT" 2>&1 | tee `pwd`/dbg.log
