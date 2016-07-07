#!/usr/bin/env bash

for DIR in ./tests/*
do
  SHOW_ECLIPSE_LOG=false
  if [ -d "$DIR/target/surefire-reports" ]
  then
    echo "Scanning `basename $DIR`"
    for F in "$DIR/target/surefire-reports/"*.txt
    do
       if cat "$F" | grep 'Errors:[ ]*[1-9]' -q
       then
         cat "$F"
         SHOW_ECLIPSE_LOG=true
       elif cat "$F" | grep 'Failures:[ ]*[1-9]' -q
       then
         cat "$F"
         SHOW_ECLIPSE_LOG=true
       fi
    done
  fi
  if [ -f "$DIR/target/work/data/.metadata/.log" -a ${SHOW_ECLIPSE_LOG} = "true" ]
  then
    echo "-------------------------------------------------------------------------------"
	echo " Eclipse log: `basename $DIR`"
    echo "-------------------------------------------------------------------------------"
    cat "$DIR/target/work/data/.metadata/.log"
  fi
done

exit 0
