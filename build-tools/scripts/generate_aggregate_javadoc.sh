#!/usr/bin/env bash

PLUGIN_MODULES=(io.sarl.lang.core io.sarl.core io.sarl.util io.janusproject.plugin)

SOURCE_PATH=""
for module in "${PLUGIN_MODULES[@]}"
do
	module_path=`find -type d -name ${module}`
	if [ '!' -z "$module_path" ]
	then
		module_path="${module_path}/target/generated-sources/java"
		if [ -z "${SOURCE_PATH}" ]
		then
			SOURCE_PATH="${module_path}"
		else
			SOURCE_PATH="${SOURCE_PATH}:${module_path}"
		fi
	fi
done


echo "Source Paths: ${SOURCE_PATH}"

exec mvn -Dmaven.test.skip=true -Dcheckstyle.skip=true -DpublicSarlApiModuleSet=true -Dsourcepath=${SOURCE_PATH} clean generate-sources org.arakhne.afc.maven:tag-replacer:generatereplacesrc compile javadoc:aggregate
