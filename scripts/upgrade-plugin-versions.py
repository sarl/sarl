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
## module : description of the current project module
def upgrade_plugins(args : dict, current_dir : str, module : dict):
	header("")
	header("------------------------------------------------------------------------")
	header("Upgrade plugin versions for " + module['name'])
	header("------------------------------------------------------------------------")
	header("")
	module_path = os.path.join(current_dir, '..', module['module'])
	if os.path.isfile(os.path.join(module_path, 'pom.xml')):
		cmd = [ os.path.join(current_dir, 'generic', 'mvn-upgrade-plugin-versions.py') ]
		
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
			bompropertyfile = args.bomproperties
		else:
			bompropertyfile = os.path.realpath(os.path.join(current_dir, '..', 'sarl-bom', 'sarl-properties', 'sarl-lang.properties'))
		bompropertyfile1 = os.path.join(os.path.dirname(bompropertyfile), 'target', 'classes', os.path.basename(bompropertyfile))
		info("Properties: " + bompropertyfile)
		info("Properties: " + bompropertyfile1)
		
		if args.ignoreartifact:
			ignoreartifact = args.ignoreartifact
		else:
			ignoreartifact = 'io.sarl.lang:io.sarl.lang.core'
		info("Ignoring: " + ignoreartifact)
		
		if args.pluginyml:
			pluginyaml = args.pluginyml
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
		retcode = subprocess.call(cmd)
		if retcode != 0:
			error("Cannot run mvn-upgrade-plugin-versions.py for module: " + module['name'])
			sys.exit(retcode)
		os.chdir(current_dir)
		success("Module " + module['name'] + " was upgraded")
	else:
		error("Expecting pom.xml file for module: " + module['name'])
		sys.exit(255)
	

##########################################
##
parser = argparse.ArgumentParser()
parser.add_argument("--modules", help="path to the JSON file defining the modules", action="store")
parser.add_argument("--bomproperties", help="path to the BOM properties", action="store")
parser.add_argument("--ignoreartifact", help="'group:artifact' of the Maven module to ignore", action="store")
parser.add_argument("--pluginyml", help="path to the YAML file that contains the plugin versions to be the destination of the upgrades", action="store")
parser.add_argument('args', nargs=argparse.REMAINDER)
args = parser.parse_args()

current_dir = os.path.dirname(os.path.realpath(__file__))

if args.modules:
	module_json_file = args.modules
else:
	module_json_file = os.path.join(current_dir, '..', 'modules.json')
with open(module_json_file, 'rt') as json_file:
	module_configuration = json.load(json_file)

for module in module_configuration['without-extension']:
	if module['upgrade-mvn-plugins']:
		upgrade_plugins(args, current_dir, module)

for module in module_configuration['extensions']:
	if module['upgrade-mvn-plugins']:
		upgrade_plugins(args, current_dir, module)

for module in module_configuration['with-extension']:
	if module['upgrade-mvn-plugins']:
		upgrade_plugins(args, current_dir, module)

sys.exit(0)

