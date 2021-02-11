#!/usr/bin/env bash

MYSELF=`readlink -f "$0"`
echo "MYSELF=$MYSELF"
CDIR=`dirname "$MYSELF"`
echo "CDIR=$CDIR"

# Use GTK 2
export SWT_GTK=0

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

echo "Launching SARL IDE ${project.version}"
exec "$CDIR/eclipse-sarl" $VMARGS "$@"
