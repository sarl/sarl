#!/usr/bin/env bash

# Modules with standard Eclipse paths
DOCUMENTED_MODULES=(
	"./main/coreplugins/io.sarl.lang.core"
	"./main/apiplugins/io.sarl.core"
	"./main/apiplugins/io.sarl.util"
	"./main/externalmaven/io.sarl.javafx"
	"./sre/io.janusproject/io.janusproject.plugin"
)

# Build the source paths
SOURCE_PATH=""
for module in "${DOCUMENTED_MODULES[@]}"
do
	pom_path="${module}/pom.xml"
	if [ -f "${pom_path}" ]
	then
		module_path="${module}/target/generated-sources/java"
		if [ -z "${SOURCE_PATH}" ]
		then
			SOURCE_PATH="${module_path}"
		else
			SOURCE_PATH="${SOURCE_PATH}:${module_path}"
		fi
	else
		echo "${module} is not an module." >&2
		exit 255
	fi
done
echo "Source Paths: ${SOURCE_PATH}"

# Build the documentation
if mvn -Dmaven.test.skip=true -Dcheckstyle.skip=true -DpublicSarlApiModuleSet=true clean install org.arakhne.afc.maven:tag-replacer:generatereplacesrc
then

	exec mvn -Dmaven.test.skip=true -Dcheckstyle.skip=true -DpublicSarlApiModuleSet=true "-Dsourcepath=${SOURCE_PATH}" javadoc:aggregate

fi

