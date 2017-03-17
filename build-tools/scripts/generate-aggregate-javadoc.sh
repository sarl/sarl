#!/usr/bin/env bash

PLUGIN_MODULES=("./eclipse-sarl/plugins/io.sarl.lang.core" "./eclipse-sarl/plugins/io.sarl.core" "./eclipse-sarl/plugins/io.sarl.util" "./sre/io.janusproject/io.janusproject.plugin")

SOURCE_PATH=""
for module in "${PLUGIN_MODULES[@]}"
do
	module_path="${module}/target/generated-sources/java"
	if [ -z "${SOURCE_PATH}" ]
	then
		SOURCE_PATH="${module_path}"
	else
		SOURCE_PATH="${SOURCE_PATH}:${module_path}"
	fi
done


echo "Source Paths: ${SOURCE_PATH}"

exec mvn -Dmaven.test.skip=true -Dcheckstyle.skip=true -DpublicSarlApiModuleSet=true -Dsourcepath=${SOURCE_PATH} clean generate-sources org.arakhne.afc.maven:tag-replacer:generatereplacesrc compile javadoc:aggregate
