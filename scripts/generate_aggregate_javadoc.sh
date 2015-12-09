#!/usr/bin/env bash

MODULES=(io.sarl.lang.core io.sarl.lang io.sarl.core io.sarl.util)

SOURCE_PATH=""
for module in "${MODULES[@]}"
do
	module_path="plugins/${module}/target/generated-sources/java"
	if [ -z "${SOURCE_PATH}" ]
	then
		SOURCE_PATH="${module_path}"
	else
		SOURCE_PATH="${SOURCE_PATH}:${module_path}"
	fi
done

exec mvn -Dmaven.test.skip=true -Dcheckstyle.skip=true -DpublicSarlApiModuleSet=true -Dsourcepath=${SOURCE_PATH} clean org.arakhne.afc.maven:tag-replacer:generatereplacesrc javadoc:aggregate
