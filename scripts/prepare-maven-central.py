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

#############################################################################
# Create the archive that contains the Maven bundles to upload to the Maven #
# Central server.                                                           #
#############################################################################

import os
import sys
import argparse
import subprocess
import json
import getpass
import shutil
import zipfile
import tarfile
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
def header(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message: the message to display
def info(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]" + msg, file=sys.stdout)

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
def is_exe(fpath : str) -> bool :
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

##########################################
## args : command-line arguments
## current_dir : folder of this script
## module : description of the current project module
## pwd: the pass phrase for signing the files
## global_output_folder: the path to the folder where the Maven Central bundles will be copied
def generate_central_bundles(args : dict, current_dir : str, module : dict, pwd : str, global_output_folder : str):
	header("")
	header("------------------------------------------------------------------------")
	header("Upgrade plugin versions for " + module['name'])
	header("------------------------------------------------------------------------")
	header("")
	module_path = os.path.realpath(os.path.join(current_dir, '..', module['module']))
	if os.path.isfile(os.path.join(module_path, 'pom.xml')):
		binary = os.path.join(current_dir, 'generic', 'mvn-central-bundles.py')
		if is_exe(binary):
			cmd = [ binary, "--create" ]
			if pwd:
				cmd = cmd + [ '--pwd=' + pwd ]
			cmd = cmd + [
				"--out", global_output_folder,
				"--",
				"-Dmaven.test.skip=true",
				"-Dcheckstyle.skip=true",
				"-DperformRelease=true",
				"-DpublicSarlApiModuleSet=true" ]
			cmd = cmd + args.args
			os.chdir(module_path)
			completed = subprocess.run(cmd)
			if completed and completed.returncode != 0:
				error("Cannot run mvn-central-bundles.py for module: " + module['name'])
				sys.exit(completed.returncode)
			os.chdir(current_dir)
		else:
			error("Cannot run mvn-central-bundles.py")
			sys.exit(255)
	else:
		error("Expecting pom.xml file for module: " + module['name'])
		sys.exit(255)

##########################################
## args : command-line arguments
## current_dir : folder of this script
## module : description of the current project module
## global_output_folder: the path to the folder where the Maven Central bundles will be copied
## combined_output_file: the path to the archive file that corresponds to the combined bundles
def generate_combined_bundle(args : dict, current_dir : str, global_output_folder : str, combined_output_file : str):
	info("Extracting the bundle files")
	tmp_dir = re.sub(r'\\.', '_', combined_output_file) + '_folder'
	if tmp_dir == global_output_folder or combined_output_file == global_output_folder:
		error("Global output folder and combined output jar cannot be the same basename")
		sys.exit(255)
	try:
		shutil.rmtree(combined_output_file)
	except:
		pass
	try:
		shutil.rmtree(tmp_dir)
	except:
		pass
	os.makedirs(tmp_dir, exist_ok=True)
	os.chdir(tmp_dir)
	for root, dirs, files in os.walk(global_output_folder):
		for filename in files:
			if filename.endswith("-bundle.jar"):
				info("    preparing " + filename)
				with zipfile.ZipFile(os.path.join(root, filename)) as jar_file:
					jar_file.extractall()
				try:
					shutil.rmtree(os.path.join(tmp_dir, 'META-INF'))
				except:
					pass
	info("Creating the combined bundle: " + combined_output_file)
	with tarfile.open(combined_output_file, "w:gz") as tar_file:
		for file in os.listdir():
			tar_file.add(file)

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
# Fix the bug of ANSI colors on terminal for Windows terminals
os.system('')
#
parser = argparse.ArgumentParser()
parser.add_argument("--modules", help="path to the JSON file defining the modules", action="store")
parser.add_argument("--ignore", help="add a module in the list of modules to be ignored", action="append")
parser.add_argument("--mlist", help="list the defined modules", action="store_true")
parser.add_argument("--pwd", help="specify the passphrase for signing the files", action="store")
parser.add_argument("--outbundles", help="path to the output folder that will contain all the bundles for Maven Central", action="store")
parser.add_argument("--outcombined", help="path to the output archive file that will contain all the combined archives for Maven Central", action="store")
parser.add_argument("--combineonly", help="run only the creation of the combined bundle", action="store_true")
parser.add_argument('args', nargs=argparse.REMAINDER)
args = parser.parse_args()

current_dir = os.path.dirname(os.path.realpath(__file__))

# Global output folder
if args.outbundles:
	global_output_folder = args.outbundles
else:
	global_output_folder = os.path.realpath(os.path.join(current_dir, '..', 'target', 'maven-central-bundles'))

if not args.combineonly:
	# Ask for a passphrase
	if args.pwd:
		password = args.pwd
	else:
		try:
		    password = getpass.getpass(prompt = f"{bcolors.FAIL}Please enter your passphrase for signing the files: {bcolors.ENDC}")
		except Exception as error:
		    error(str(error))
		    sys.exit(255)

	info("Reseting output folder: " + global_output_folder)
	try:
		shutil.rmtree(global_output_folder)
	except:
		pass
	os.makedirs(global_output_folder, exist_ok=True)

	# Modules
	module_configuration = read_module_configuration(args, current_dir)

	# Generate bundles
	for module in module_configuration['without-extension']:
		if module['in-maven-central']:
			generate_central_bundles(args, current_dir, module, password, global_output_folder)

	for module in module_configuration['extensions']:
		if module['in-maven-central']:
			generate_central_bundles(args, current_dir, module, password, global_output_folder)

	for module in module_configuration['with-extension']:
		if module['in-maven-central']:
			generate_central_bundles(args, current_dir, module, password, global_output_folder)


# Combined bundle output folder
if args.outcombined:
	combined_output_file = args.outcombined
else:
	combined_output_file = os.path.realpath(os.path.join(current_dir, '..', 'target', 'combined-maven-central-bundles.tar.gz'))

generate_combined_bundle(args, current_dir, global_output_folder, combined_output_file)

sys.exit(0)

