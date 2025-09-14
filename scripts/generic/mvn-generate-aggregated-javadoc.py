#!/usr/bin/env python3
#
# Author: Stephane GALLAND
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import sys
import os
import argparse

##############################
##
def buildJavaDocPath(modules):
	source_path = []
	if modules:
		for module in modules:
			pom_path = os.path.join(module, "pom.xml")
			if os.path.exists(pom_path):
				module_path = os.path.join(module, "target", "generated-sources", "java")
				source_path.append(module_path)
			else:
				print >> sys.stderr, "Module not found: " + module
				return None
		source_path = ':'.join(source_path)
	print("Source Paths = " + str(source_path))
	return source_path

##############################
##
def filterArgs(args):
	l = []
	if args:
		if isinstance(args, list):
			l = args
			if len(args) > 0:
				if isinstance(args, list):
					l = args[0]
	r = []
	for e in l:
		if e != '--':
			r.append(e)
	return r

##############################
## offline: boolean flag that indicates if the generator must be run off-line or not
## pomfile: the path to the root pom file (Unix format)
## sourcePaths: the source paths (Unix format)
## args: the command line arguments
def generateDocumentation(offline, pomfile, sourcePaths, args):
	maven_cmd = os.environ.get('MAVEN_CMD')
	if maven_cmd is None:
		maven_cmd = 'mvn'

	cmd = maven_cmd
	if offline:
		cmd = cmd + " -o "
	if pomfile:
		cmd = cmd + " -f " + pomfile
	for arg in args:
		cmd = cmd + " " + arg
	if sourcePaths:
		cmd = cmd + " -Dsourcepath=" + sourcePaths
	cmd = cmd + " javadoc:aggregate"

	print("Assuming 'maven-javadoc-plugin:jar' is activated")
	print("Assuming tests are desactivated")
	print("Have you activated the released profile? -DperformRelease=true")
	print(cmd)
	return os.system(cmd)
	
##############################
##
parser = argparse.ArgumentParser(description="Generate the aggregated JavaDoc")
parser.add_argument("--offline", help="run the generator off-line", action="store_true")
parser.add_argument("--pom", help="specify the path to the pom file to use", action="store")
parser.add_argument("--module", help="specify the module to be documented", action="append")
parser.add_argument('args', nargs=argparse.REMAINDER, action="append")
args = parser.parse_args()
rargs = filterArgs(args.args)

retcode = 255

doc_path = buildJavaDocPath(args.module)

retcode = generateDocumentation(args.offline, args.pom, doc_path, rargs)

sys.exit(retcode)

