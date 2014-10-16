#!/usr/bin/env bash

if [ "---verify" = "-$1" ]
then

	RETCODE=0

	NB_FILES=0

	for ASCFILE in `find -name "*.asc"`
	do
		DIRNAME=`dirname "$ASCFILE"`
		BASENAME=`basename "$ASCFILE" .asc`
		SIGNEDFILE="$DIRNAME/$BASENAME"
		if gpg --verify "$ASCFILE" "$SIGNEDFILE" >/dev/null 2>/dev/null
		then
			NB_FILES=$[$NB_FILES + 1]
		else
			echo "$SIGNEDFILE"
			RETCODE=255
		fi
	done

	echo "Found $NB_FILES signature files"

	exit $RETCODE

elif [ "---fix" = "-$1" ]
then

	PASSPHRASE=`ssh-askpass "Please enter your passphrase for signing the files:"`

	for ASCFILE in `find -name "*.asc"`
	do
		DIRNAME=`dirname "$ASCFILE"`
		BASENAME=`basename "$ASCFILE" .asc`
		SIGNEDFILE="$DIRNAME/$BASENAME"
		if gpg --verify "$ASCFILE" "$SIGNEDFILE" >/dev/null 2>/dev/null
		then
			true
		else
			rm -f "$ASCFILE"
			gpg -ab "$SIGNEDFILE"
		fi
	done

	exit 0

elif [ "---create" = "-$1" ]
then

	shift
	
	if [ -z "$MVN_CMD" ]
	then
		MVN_CMD=mvn
	fi

	PASSPHRASE=`ssh-askpass "Please enter your passphrase for signing the files:"`

	if [ '!' -z "$PASSPHRASE" ]
	then
		echo "Assuming 'org.arakhne.afc.maven:tag-replacer:generatereplacesrc' is activated"
		echo "Assuming 'maven-javadoc-plugin:jar' is activated"
		echo "Assuming 'maven-sources-plugin:jar' is activated"
		echo "Have you activated the released profile? -DperformRelease=true"

		echo $MVN_CMD -Dgpg.passphrase="<hidden>" -Darguments=-Dgpg.passphrase="<hidden>" "$@"  clean verify
		$MVN_CMD -Dgpg.passphrase="$PASSPHRASE" -Darguments=-Dgpg.passphrase="$PASSPHRASE" "$@"  clean verify || exit 255
		
		CDIR=`pwd`

		for TARGET in `find -type d -name "target"`
		do
			cd "$TARGET"
			ARTIFACTNAME=`ls -1 *.pom 2>/dev/null`
			if [ -z "$ARTIFACTNAME" ]
			then
					echo "No artifact in $TARGET"
			else
				ARTIFACTNAME=`basename "$ARTIFACTNAME" .pom`
				BUNDLENAME="$ARTIFACTNAME-bundle.jar"
				SIGNS=`ls *.asc 2>/dev/null`
				if [ -z "$SIGNS" ]
				then
					echo "Skipping $BUNDLENAME"
				else
					echo -n "Creating $BUNDLENAME..."
					rm -f "$BUNDLENAME"
					FILES=""
					for FILE in *.pom *.pom.asc *.jar *.jar.asc *.apklib *.apklib.asc
					do
						if [ -f "$FILE" ]
						then
							FILES="$FILES $FILE"
						fi
					done
					jar cf "$BUNDLENAME" $FILES
					echo "done"
				fi
			fi
			cd "$CDIR"
		done

	else
		exit 255
	fi

else

	echo "usage: `basename $0` --verify | --fix | --create <maven_opt>..."
	exit 255

fi

exit 0
