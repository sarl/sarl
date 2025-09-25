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

########################
# Build all the modues #
########################

import os
import argparse
import subprocess
import sys
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
## message : the mssage to display
def info(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message : the mssage to display
def success(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.OKGREEN}{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message : the mssage to display
def error(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.FAIL}{bcolors.BOLD}ERROR{bcolors.ENDC}]" + msg, file=sys.stderr)

##########################################
## fpath : the path to test
## RETURN : True if the path corresponds to a file with execution right
def is_exe(fpath : str) -> bool:
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

##########################################
## args : the command-line arguments
## execution_dir : the directory in which the Maven must be run
def os_execution_dir(args : dict, execution_dir : str) -> str:
	if os.name == 'nt' and args.uncmapping:
		dir0 = execution_dir.replace('/', '\\')
		if dir0.startswith(r'\\'):
			for map_key, map_value in args.uncmapping.items():
				dir1 = re.sub('^' + re.escape(map_key), map_value, dir0)
				if dir0 != dir1:
					return dir1
	return execution_dir

##########################################
## args : the command-line arguments
## module : the description of the module to be built
## execution_dir : the directory in which the Maven must be run
def run_mvn_build(args : dict, module : dict, execution_dir : str):
	execution_dir = os_execution_dir(args, execution_dir)
	maven_cmd = os.environ.get('MAVEN_CMD')
	if not maven_cmd:
		maven_cmd = shutil.which('mvn')
	cmd = [ maven_cmd ]
	if args.notest:
		cmd = cmd + [ '-Dmaven.test.skip=true' ]
	if args.nop2mirror:
		cmd = cmd + [ '-Declipse.p2.mirrors=false' ]
	if args.definitions:
		for prop_key, prop_value in args.definitions.items():
			cmd = cmd + [ '-D' + str(prop_key) + '=' + str(prop_value) ]
	cmd = cmd + args.args
	cmd = cmd + [ 'clean', 'install' ]
	#info("CMD=" + str(cmd))
	#info("CWD=" + str(execution_dir))
	completed = subprocess.run(cmd, cwd=execution_dir)
	if completed and completed.returncode != 0:
		error("Cannot run mvn for module: " + module['name'])
		sys.exit(completed.returncode)

##########################################
## args : the command-line arguments
## module : the description of the module to run the script for
## script : the path to the script to be run
## execution_str : the directory in which the Maven must be run
def run_script(args : dict, module : dict, execution_dir : str, script : str):
	execution_dir = os_execution_dir(args, execution_dir)
	cmd = [ script ]
	if args.notest:
		cmd = cmd + ["--notest", "-Dmaven.test.skip=true"]
	if args.nop2mirror:
		cmd = cmd + ["--nop2mirror", "-Declipse.p2.mirrors=false"]
	if os.name == 'nt' and args.uncmapping:
		for map_key, map_value in args.uncmapping.items():
			cmd = cmd + ['--unc', map_key + '=' + map_value]
	cmd = cmd + args.args
	completed = subprocess.run(cmd, cwd=execution_dir)
	if completed and completed.returncode != 0:
		error("Cannot run build script for module: " + module['name'])
		sys.exit(completed.returncode)

##########################################
## args : the command-line arguments
## current_dir : the path of this script
## module : the description of the module to build
def build_module(args : dict, current_dir : str, module : dict):
	if module['build']:
		info("")
		info("------------------------------------------------------------------------")
		info("Building " + module['name'])
		info("------------------------------------------------------------------------")
		info("")
		module_dir = os.path.join(current_dir, module['module'])
		pom_file = os.path.join(module_dir, 'pom.xml')
		py_build_file = os.path.join(module_dir, 'build-all.py')
		sh_build_file = os.path.join(module_dir, 'build-all.sh')
		ps_build_file = os.path.join(module_dir, 'build-all.ps1')
		if os.path.isfile(pom_file):
			run_mvn_build(args, module, module_dir)
		elif is_exe(py_build_file):
			run_script(args, module, module_dir, py_build_file)
		elif is_exe(sh_build_file):
			run_script(args, module, module_dir, sh_build_file)
		elif is_exe(ps_build_file):
			run_script(args, module, module_dir, ps_build_file)
		else:
			error("Nothing to run for module: " + module['name'])
			sys.exit(255)

##########################################
## args : the command-line arguments
## current_dir : the path of this script
## RETURN : the dictionnary of defined modules without the ones that are ignored from the command-line arguments
def read_module_configuration(args : dict, current_dir : str) -> dict:
	if args.modules:
		module_json_file = args.modules
	else:
		module_json_file = os.path.join(current_dir, 'modules.json')

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
# Fix the bug of ANSI colors on terminal for Windows terminals
os.system('')
#
parser = argparse.ArgumentParser()
parser.add_argument("--modules", help="path to the JSON file defining the modules", action="store")
parser.add_argument("--ignore", help="add a module in the list of modules to be ignored", action="append")
parser.add_argument("--mlist", help="list the defined modules", action="store_true")
parser.add_argument("--notest", help="skip all the tests, equivalent to -Dmaven.test.skip=true", action="store_true")
parser.add_argument("--nop2mirror", help="disable the mirroring to P2 repository, equivalent to -Declipse.p2.mirrors=false", action="store_true")
class DefinitionAction(argparse.Action):
	def __call__(action_self, parser, namespace, value, option_string=None):
		if '=' in value:
			params = value.split('=')
			def_name = str(params[0]).strip()
			def_value = str(params[1]).strip()
		else:
			def_name = value
			def_value = ''
		defs = getattr(namespace, 'definitions')
		if not defs:
			defs = dict()
		defs[def_name] = def_value
		setattr(namespace, 'definitions', defs)
parser.add_argument("-D", dest='definitions', action=DefinitionAction, metavar='NAME=VALUE', help="define a property <NAME>=<VALUE>")
if os.name == 'nt':
	class UncAction(argparse.Action):
		def __call__(action_self, parser, namespace, value, option_string=None):
			if '=' in value:
				params = value.split('=')
				def_name = str(params[0]).strip()
				def_value = str(params[1]).strip()
			else:
				def_name = value
				def_value = ''
			defs = getattr(namespace, 'uncmapping')
			if not defs:
				defs = dict()
			defs[def_name] = def_value
			setattr(namespace, 'uncmapping', defs)
	parser.add_argument("--unc", dest='uncmapping', action=UncAction, metavar='UNC=PATH', help="define a mapping from an UNC path and a path with drive")
parser.add_argument('args', nargs=argparse.REMAINDER)
args = parser.parse_args()

current_dir = os.path.dirname(os.path.realpath(__file__))
module_configuration = read_module_configuration(args, current_dir)

for key in [ 'without-extension', 'extensions', 'with-extension' ]:
	if key in module_configuration:
		for module in module_configuration[key]:
			build_module(args, current_dir, module)

success("BUILDING SUCCESS")

sys.exit(0)

