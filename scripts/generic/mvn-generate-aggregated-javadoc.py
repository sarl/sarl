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

VERSION = '20250916'

##########################################
##
class bcolors:
    HEADER = '\033[34m'
    OKGREEN = '\033[32m'
    FAIL = '\033[31m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

##########################################
## message : the message to display
def header(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message : the message to display
def info(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]" + msg, file=sys.stdout)

##########################################
## message : the message to display
def success(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.OKGREEN}{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message : the message to display
def error(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.FAIL}{bcolors.BOLD}ERROR{bcolors.ENDC}]" + msg, file=sys.stderr)

##############################
## modules : list of the modules to be included in the javadoc path
## RETURN : the Javadoc path.
def build_java_doc_path(modules : list) -> str:
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
	info("Source Paths: " + str(source_path))
	return source_path

##############################
## args : the command line arguments to filter
## RETURN : the extra command line arguments in a list
def filter_args(args : dict) -> list:
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
## RETURN : the exit code of the Javadoc command
def generate_documentation(offline : bool, pomfile : str, sourcePaths : str, args : list) -> int:
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

	info("Assuming 'maven-javadoc-plugin:jar' is activated")
	info("Assuming tests are desactivated")
	info("Have you activated the released profile? -DperformRelease=true")
	info("")
	info(cmd)
	return os.system(cmd)
	
##############################
##
# Fix the bug of ANSI colors on terminal for Windows terminals
os.system('')
#
parser = argparse.ArgumentParser(description="Generate the aggregated JavaDoc")
parser.add_argument("--version", help="Show the version of this script", action="store_true")
parser.add_argument("--offline", help="run the generator off-line", action="store_true")
parser.add_argument("--pom", help="specify the path to the pom file to use", action="store")
parser.add_argument("--module", help="specify the module to be documented", action="append")
parser.add_argument('args', nargs=argparse.REMAINDER, action="append")
args = parser.parse_args()

if args.version:
	info("Version: " + VERSION)
	sys.exit(0)

rargs = filter_args(args.args)

retcode = 255

doc_path = build_java_doc_path(args.module)

retcode = generate_documentation(args.offline, args.pom, doc_path, rargs)

sys.exit(retcode)

