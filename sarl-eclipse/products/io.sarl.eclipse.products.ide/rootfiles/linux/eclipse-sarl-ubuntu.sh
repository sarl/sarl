#!/usr/bin/env bash

MYSELF=`readlink -f "$0"`
echo "MYSELF=$MYSELF"
CDIR=`dirname "$MYSELF"`
echo "CDIR=$CDIR"

#Gtk3 forced:
export SWT_GTK3=1
#Gtk2 forced:
#export SWT_GTK3=0

# Bug fix for Ubuntu menu proxy
export UBUNTU_MENUPROXY=0

# Bug fix for overlay scrollbars
export LIBOVERLAY_SCROLLBAR=0


# Remove /usr/lib/jni from LD_LIBRARY_PATH to avoid loading from Eclipse
export LD_LIBRARY_PATH=`echo "$LD_LIBRARY_PATH" | perl -p -e 's!(/usr/lib/jni:)|(:?/usr/lib/jni)!!'`

# Add Ubuntu JNI paths
#export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib/jni"
#echo "Native Library Path: $LD_LIBRARY_PATH"

if [ -r "$HOME/.eclipse/eclipserc" ]
then
	echo "Loading user configuration"
	. "$HOME/.eclipse/eclipserc"
elif [ -r "$HOME/.eclipserc" ]
then
	echo "Loading user configuration"
	. "$HOME/.eclipserc"
fi

exec "$CDIR/eclipse-sarl" $VMARGS "$@"
