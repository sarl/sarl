#!/usr/bin/env python3
#
# Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

##################################################################################
# Generate the API documentation for all the modules into a single documentation #
##################################################################################

import os
import sys
import argparse
import subprocess
import shutil
import json
import re

##########################################
##
class bcolors:
    HEADER = '\033[34m'
    OKGREEN = '\033[32m'
    FAIL = '\033[31m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

##########################################
## message: the message to display
def info(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message: the message to display
def success(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.OKGREEN}{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message: the message to display
def error(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.FAIL}{bcolors.BOLD}ERROR{bcolors.ENDC}]" + msg, file=sys.stderr)

##########################################
## fpath : the path to test
## RETURN: True if the given path has execution rights
def is_exe(fpath : str) -> bool:
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

##########################################
## args : command-line arguments
## current_dir : folder of this script
def update_source_code(args : dict, current_dir : str):
	binary = os.path.realpath(os.path.join(current_dir, '..', 'build-all.py'))
	if is_exe(binary):
		cmd = [ binary,
			"--notest" ]
		completed = r = subprocess.run(cmd)
		if completed and completed.returncode != 0:
			sys.exit(completed.returncode)
	else:
		error("Cannot run build-all.py")
		sys.exit(255)

##########################################
## args : command-line arguments
## current_dir : folder of this script
def delete_outputs(args : dict, current_dir : str):
	for root, dirs, files in os.walk("."):
		for filename in dirs:
			fullpath = os.path.join(root, filename)
			if filename == "target":
				try:
					shutil.rmtree(fullpath)
				except:
					pass

##########################################
## args : command-line arguments
## current_dir : folder of this script
def create_pom_xml(args : dict, current_dir : str):
	module_configuration = read_module_configuration(args, current_dir)
	all_modules = ''
	for module in module_configuration['without-extension']:
		if module['apidoc']:
			all_modules = all_modules + "    <module>" + module['module'] + "</module>\n"
	for module in module_configuration['extensions']:
		if module['apidoc']:
			all_modules = all_modules + "    <module>" + module['module'] + "</module>\n"
	for module in module_configuration['with-extension']:
		if module['apidoc']:
			all_modules = all_modules + "    <module>" + module['module'] + "</module>\n"
	input_pom = os.path.join(current_dir, 'specific', 'pom-aggregate-javadoc.xml')
	with open(input_pom) as input_file:
		data = input_file.readlines()
	output_content = []
	for line in data:
		line2 = re.sub("@@ALL_MODULES@@", all_modules, line)
		output_content.append(line2)
	output_pom = os.path.join(current_dir, '..', 'apidoc_pom.xml')
	with open (output_pom, "w") as output_file:
		output_file.write("".join(output_content))
	success("POM for aggregated documentation is generated")
	return output_pom

##########################################
## args : command-line arguments
## current_dir : folder of this script
## pom_filename : the filename of the POM to use as root POM
def generate_api_documentation(args : dict, current_dir : str, pom_filename : str):
	binary = ols.path.realpath(os.path.join(current_dir, 'generic', 'mvn-generate-aggregated-javadoc.py'))
	retcode = 255
	if is_exe(binary):
		try:
			shutil.rmtree(os.path.join('.', 'target', 'site'))
		except:
			pass
		try:
			shutil.rmtree(os.path.join('.', 'target', 'reports'))
		except:
			pass
		cmd = [ binary ]
		if args.offline:
			cmd = cmd + [ "--offline" ]
		if pom_filename:
			cmd = cmd + [ "--pom", pom_filename ]
		cmd = cmd + [
			#"--module", "./main/coreplugins/io.sarl.lang.core",
			#"--module", "./main/apiplugins/io.sarl.core",
			#"--module", "./main/apiplugins/io.sarl.util",
			#"--module", "./main/apiplugins/io.sarl.api.naming",
			#"--module", "./main/apiplugins/io.sarl.api.probing",
			#"--module", "./main/apiplugins/io.sarl.api.bootiquebase",
			#"--module", "./main/externalmaven/io.sarl.javafx",
			#"--module", "./sre/io.janusproject/io.janusproject.plugin",
			"--",
			"-Dmaven.test.skip=true",
			"-Dcheckstyle.skip=true",
			"-DpublicSarlApiModuleSet=true",
			"-P!public-sarl-api-module-set-aggregateddoc",
			"-Dsarl.jvminferrer.skip=true",
			"-Dsarl.clean.skip=true",
			]
		cmd = cmd + args.args
		completed = r = subprocess.run(cmd)
		if completed and completed.returncode != 0:
			sys.exit(completed.returncode)
	else:
		error("Cannot run mvn-generate-aggregated-javadoc.py")
		sys.exit(255)

##########################################
## args : the command-line arguments
## current_dir : the path of this script
## RETURN : the dictionnary of defined modules without the ones that are ignored from the command-line arguments
def read_module_configuration(args : dict, current_dir : str) -> dict:
	if args.modules:
		module_json_file = args.modules
	else:
		module_json_file = os.path.join(current_dir, '..', 'modules.json')

	with open(module_json_file, 'rt') as json_file:
		module_configuration = json.load(json_file)

	for key in [ 'without-extension', 'extensions', 'with-extension' ]:
		if key not in module_configuration:
			module_configuration[key] = list()
		elif args.ignore:
			new_list = list()
			for module in module_configuration[key]:
				if 'module' in module and module['module'] and not module['module'] in args.ignore:
					new_list.append(module)
			module_configuration[key] = new_list

	if (args.mlist):
		info(json.dumps(module_configuration, indent=2))
		sys.exit(0)

	return module_configuration

##########################################
##
parser = argparse.ArgumentParser()
parser.add_argument("--modules", help="path to the JSON file defining the modules", action="store")
parser.add_argument("--ignore", help="add a module in the list of modules to be ignored", action="append")
parser.add_argument("--mlist", help="list the defined modules", action="store_true")
parser.add_argument("--offline", help="run the generator off-line", action="store_true")
parser.add_argument("--pom", help="specify the path to the pom file to use", action="store")
parser.add_argument('args', nargs=argparse.REMAINDER)
args = parser.parse_args()

current_dir = os.path.dirname(os.path.realpath(__file__))

# STEP1: Launch the full compilation for updating the source code files with specific constants
update_source_code(args, current_dir)

# STEP2: Delete target/output folders because we don't need the generated files for the
#        generation of the API documentation. We need only the changes in the source
#        files that are applied during STEP1 
delete_outputs(args, current_dir)

# STEP3: Generate the API documentation
output_file = create_pom_xml(args, current_dir)
generate_api_documentation(args, current_dir, output_file)
try:
	os.remove(output_file)
except:
	pass

sys.exit(0)

