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
# Update the versions of the plugins in all the Maven POM files from the BOM     #
# configuration './sarl-bom/sarl-properties/plugins.yml'                         #
##################################################################################

import os
import sys
import argparse
import subprocess
import json

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
## message: the text of the message
def header(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message: the text of the message
def info(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]" + msg, file=sys.stdout)

##########################################
## message: the text of the message
def success(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.OKGREEN}{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message: the text of the message
def error(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.FAIL}{bcolors.BOLD}ERROR{bcolors.ENDC}]" + msg, file=sys.stderr)

##########################################
## args : command-line arguments
## current_dir : folder of this script
## root_path : path of the root folder, if any
## module : description of the current project module
def upgrade_plugins(args : dict, current_dir : str, root_path : str, module : dict):
	header("")
	header("------------------------------------------------------------------------")
	header("Upgrade plugin versions for " + module['name'])
	header("------------------------------------------------------------------------")
	header("")
	module_path = os.path.realpath(os.path.join(root_path, module['module']))
	info("Module path: " + module_path)
	if os.path.isfile(os.path.join(module_path, 'pom.xml')):
		cmd = [ os.path.realpath(os.path.join(current_dir, 'generic', 'mvn-upgrade-plugin-versions.py')) ]
		
		if root_path:
			cmd = cmd + [ '--rootpath', root_path ]
		
		if 'upgrade-mvn-plugins-pom' in module:
			pom_path = [ module_path ] + module['upgrade-mvn-plugins-pom']
			pom_path = os.path.realpath(os.path.join(*pom_path))
			info("Additional POM: " + pom_path)
			cmd = cmd + [ '--pom', pom_path ]
		
		if 'upgrade-mvn-plugins-eclipse' in module:
			platform_path = [ module_path ] + module['upgrade-mvn-plugins-eclipse']
			platform_path = os.path.realpath(os.path.join(*platform_path))
			info("Additional Eclipse platform specification: " + platform_path)
			cmd = cmd + [ '--eclipseplatform',  platform_path]
		
		cmd = cmd + args.args
		
		if args.bomproperties:
			bompropertyfile = os.path.realpath(args.bomproperties)
		else:
			bompropertyfile = os.path.realpath(os.path.join(current_dir, '..', 'sarl-bom', 'sarl-properties', 'sarl-lang.properties'))
		bompropertyfile1 = os.path.realpath(os.path.join(os.path.dirname(bompropertyfile), 'target', 'classes', os.path.basename(bompropertyfile)))
		info("Properties: " + bompropertyfile)
		info("Properties: " + bompropertyfile1)
		
		if args.ignoreartifact:
			ignoreartifact = args.ignoreartifact
		else:
			ignoreartifact = 'io.sarl.lang:io.sarl.lang.core'
		info("Ignoring: " + ignoreartifact)
		
		if args.pluginyml:
			pluginyaml = os.path.realpath(args.pluginyml)
		else:
			pluginyaml = os.path.realpath(os.path.join(current_dir, '..', 'sarl-bom', 'sarl-properties', 'plugins.yml'))
		info("Plugin specification: " + pluginyaml)
		
		cmd = cmd + [ 
			'--properties=' + bompropertyfile,
			'--properties=' + bompropertyfile1,
			'--ignore=' + ignoreartifact,
			pluginyaml
		]
		os.chdir(module_path)
		completed = subprocess.run(cmd)
		if completed and completed.returncode != 0:
			error("Cannot run mvn-upgrade-plugin-versions.py for module: " + module['name'])
			sys.exit(completed.returncode)
		os.chdir(root_path)
		success("Module " + module['name'] + " was upgraded")
	else:
		error("Expecting pom.xml file for module: " + module['name'] + "; path: " + module_path)
		sys.exit(255)
	
##########################################
## args : the command-line arguments
## current_dir : the path of this script
## RETURN : the dictionnary of defined modules without the ones that are ignored from the command-line arguments
def read_module_configuration(args : dict, current_dir : str) -> dict:
	if args.modules:
		module_json_file = os.path.realpath(args.modules)
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
parser.add_argument("--version", help="Show the version of this script", action="store_true")
parser.add_argument("--rootpath", help="path to the root folder where modules are located", action="store")
parser.add_argument("--modules", help="path to the JSON file defining the modules", action="store")
parser.add_argument("--ignore", help="add a module in the list of modules to be ignored", action="append")
parser.add_argument("--mlist", help="list the defined modules", action="store_true")
parser.add_argument("--bomproperties", help="path to the BOM properties", action="store")
parser.add_argument("--ignoreartifact", help="'group:artifact' of the Maven module to ignore", action="store")
parser.add_argument("--pluginyml", help="path to the YAML file that contains the plugin versions to be the destination of the upgrades", action="store")
parser.add_argument('args', nargs=argparse.REMAINDER)
args = parser.parse_args()

if args.version:
	info("Version: " + VERSION)
	sys.exit(0)

current_dir = os.path.dirname(os.path.realpath(__file__))

if args.rootpath:
	root_path = os.path.realpath(args.rootpath)
else:
	root_path = os.path.realpath(os.path.join(current_dir, '..'))

module_configuration = read_module_configuration(args, current_dir)


for key in [ 'without-extension', 'extensions', 'with-extension' ]:
	if key in module_configuration:
		for module in module_configuration[key]:
			if module['upgrade-mvn-plugins']:
				upgrade_plugins(args, current_dir, root_path, module)

sys.exit(0)

