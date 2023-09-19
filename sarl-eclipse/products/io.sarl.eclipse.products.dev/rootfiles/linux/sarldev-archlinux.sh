#!/usr/bin/env bash

MYSELF=`readlink -f "$0"`
echo "MYSELF=$MYSELF"
CDIR=`dirname "$MYSELF"`
echo "CDIR=$CDIR"

# Use GTK 2
export SWT_GTK=0

# Bug fix for Gtk since Ubuntu 22.04
export GDK_BACKEND=x11

# Bug fix for overlay scrollbars
export LIBOVERLAY_SCROLLBAR=0

# Remove /usr/lib/jni from LD_LIBRARY_PATH to avoid loading from Eclipse
export LD_LIBRARY_PATH=`echo "$LD_LIBRARY_PATH" | perl -p -e 's!(/usr/lib/jni:)|(:?/usr/lib/jni)!!'`
echo "Native Library Path: $LD_LIBRARY_PATH"

if [ -r "$HOME/.eclipse/eclipserc" ]
then
	echo "Loading user configuration"
	. "$HOME/.eclipse/eclipserc"
elif [ -r "$HOME/.eclipserc" ]
then
	echo "Loading user configuration"
	. "$HOME/.eclipserc"
fi

if [ -r "$HOME/.sarl/sarliderc" ]
then
	echo "Loading user configuration"
	. "$HOME/.sarl/sarliderc"
elif [ -r "$HOME/.sarliderc" ]
then
	echo "Loading user configuration"
	. "$HOME/.sarliderc"
fi

exec "$CDIR/sarldev" $VMARGS "$@"
