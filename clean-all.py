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
import json
import shutil

##########################################
##
class bcolors:
    HEADER = '\033[34m'
    OKGREEN = '\033[32m'
    FAIL = '\033[31m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

##########################################
##
def info(message):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
##
def success(message):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.OKGREEN}{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
##
def error(message):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.FAIL}{bcolors.BOLD}ERROR{bcolors.ENDC}]" + msg, file=sys.stderr)

##########################################
##
def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

##########################################
##
def run_mvn(args, module):
	cmd = [ 'mvn' ]
	cmd = cmd + args.args
	cmd = cmd + [ 'clean' ]
	retcode = subprocess.call(cmd)
	if retcode != 0:
		error("Cannot run mvn for module: " + module['name'])
		sys.exit(retcode)

##########################################
##
def run_script(args, module, script):
	cmd = [ script ]
	cmd = cmd + args.args
	retcode = subprocess.call(cmd)
	if retcode != 0:
		error("Cannot run cleaning script for module: " + module['name'])
		sys.exit(retcode)

##########################################
##
def build_module(args, current_dir, module):
	if module['build']:
		info("")
		info("------------------------------------------------------------------------")
		info("Cleaning " + module['name'])
		info("------------------------------------------------------------------------")
		info("")
		module_dir = os.path.join(current_dir, module['module'])
		pom_file = os.path.join(module_dir, 'pom.xml')
		py_build_file = os.path.join(module_dir, 'clean-all.py')
		sh_build_file = os.path.join(module_dir, 'clean-all.sh')
		ps_build_file = os.path.join(module_dir, 'clean-all.ps1')
		os.chdir(module_dir)
		if os.path.isfile(pom_file):
			run_mvn(args, module)
		elif is_exe(py_build_file):
			run_script(args, module, py_build_file)
		elif is_exe(sh_build_file):
			run_script(args, module, sh_build_file)
		elif is_exe(ps_build_file):
			run_script(args, module, ps_build_file)
		else:
			error("Nothing to run for module: " + module['name'])
			sys.exit(255)
		os.chdir(current_dir)

##########################################
##
parser = argparse.ArgumentParser()
parser.add_argument("--modules", help="path to the JSON file defining the modules", action="store")
parser.add_argument('args', nargs=argparse.REMAINDER)
args = parser.parse_args()

current_dir = os.path.dirname(os.path.realpath(__file__))

if args.modules:
	module_json_file = args.modules
else:
	module_json_file = os.path.join(current_dir, 'modules.json')

with open(module_json_file, 'rt') as json_file:
	module_configuration = json.load(json_file)

for module in module_configuration['without-extension']:
	build_module(args, current_dir, module)

for module in module_configuration['extensions']:
	build_module(args, current_dir, module)

for module in module_configuration['with-extension']:
	build_module(args, current_dir, module)

info("")
info("------------------------------------------------------------------------")
info("Cleaning root folder")
info("------------------------------------------------------------------------")
info("")
root_output_dir = os.path.join(current_dir, 'target')
try:
	shutil.rmtree(root_output_dir)
except:
	pass
success("CLEANING SUCCESS")

sys.exit(0)

