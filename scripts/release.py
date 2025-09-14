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
# Release the projects on a remote Maven repository                         #
#############################################################################

import os
import sys
import argparse
import subprocess
import configparser
import getpass
import json

MAVENSARLIO_URL = os.environ.get("MAVENSARLIO_URL", None)
UPDATESSARLIO_URL = os.environ.get("UPDATESSARLIO_URL", None)
MAVENSARLIO_USER = os.environ.get("MAVENSARLIO_USER", None)
DEPENDENCIESSARLIO_URL = os.environ.get("DEPENDENCIESSARLIO_URL", None)

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
def header(message):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message: the message to display
def info(message):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]" + msg, file=sys.stdout)

##########################################
## message: the message to display
def success(message):
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

##########################################"
##
def read_user_configuration():
	global MAVENSARLIO_URL
	global UPDATESSARLIO_URL
	global MAVENSARLIO_USER
	global DEPENDENCIESSARLIO_URL

	sarlrc_file = os.path.join(os.environ['HOME'], '.sarlrc')
	if os.path.exists(sarlrc_file):
		config = configparser.ConfigParser()
		config.read(sarlrc_file)
		MAVENSARLIO_URL = config.get('servers', 'MAVENSARLIO_URL')
		UPDATESSARLIO_URL = config.get('servers', 'UPDATESSARLIO_URL')
		MAVENSARLIO_USER = config.get('servers', 'MAVENSARLIO_USER')
		DEPENDENCIESSARLIO_URL = config.get('servers', 'DEPENDENCIESSARLIO_URL')

##############################
##
def check_configuration():
	global MAVENSARLIO_URL
	global UPDATESSARLIO_URL
	global MAVENSARLIO_USER
	global DEPENDENCIESSARLIO_URL

	if not MAVENSARLIO_URL:
		error("You must define the MAVENSARLIO_URL environment variable to the URL of the Maven upload server , e.g. dav:https://myhost/dav")
		sys.exit(255)

	if not UPDATESSARLIO_URL:
		error("You must define the UPDATESSARLIO_URL environment variable to the URL of the P2 upload server , e.g. dav:https://myhost/dav")
		sys.exit(255)

	if not MAVENSARLIO_USER:
		error("You must define the MAVENSARLIO_USER environment variable with the login to the upload servers.")
		sys.exit(255)

	if not DEPENDENCIESSARLIO_URL:
		error("You must define the DEPENDENCIESSARLIO_URL environment variable with the login to the upload servers.")
		sys.exit(255)

##############################
## pass_phrase: password
def print_configuration(pass_phrase : str):
	global MAVENSARLIO_URL
	global UPDATESSARLIO_URL
	global MAVENSARLIO_USER
	global DEPENDENCIESSARLIO_URL

	info("MAVENSARLIO_URL = " + str(MAVENSARLIO_URL))
	info("UPDATESSARLIO_URL = " + str(UPDATESSARLIO_URL))
	info("DEPENDENCIESSARLIO_URL = " + str(DEPENDENCIESSARLIO_URL))
	info("MAVENSARLIO_USER = " + str(MAVENSARLIO_USER))
	info("MAVENSARLIO_PWD = " + str(pass_phrase))

##############################
## args: command-line arguments
## RETURN: the filtered args
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
## pass_phrase: the pass phrase that is used for connecting to the remote server
## args: command-line arguments
def execute_maven(pass_phrase : str, args : dict):
	global MAVENSARLIO_URL
	global UPDATESSARLIO_URL
	global MAVENSARLIO_USER
	global DEPENDENCIESSARLIO_URL

	maven_cmd = os.environ.get('MAVEN_CMD')
	if not maven_cmd:
		maven_cmd = 'mvn'
	os.environ["MAVENSARLIO_USER"] = MAVENSARLIO_USER
	os.environ["MAVENSARLIO_PWD"] = pass_phrase
	os.environ["MAVENSARLIO_URL"] = MAVENSARLIO_URL
	os.environ["UPDATESSARLIO_URL"] = UPDATESSARLIO_URL
	os.environ["DEPENDENCIESSARLIO_URL"] = DEPENDENCIESSARLIO_URL
	cmd0 = [ maven_cmd,
	         "-Dmaven.test.skip=true",
	         "-DskipTests=true",
	         "-Dcheckstyle.skip=true",
	         "-DMAVENSARLIO_USER=\"" + MAVENSARLIO_USER + "\"" ]
	cmd1 = [ "-DMAVENSARLIO_PWD=\"" + pass_phrase + "\"" ]
	cmd2 = [ "-DMAVENSARLIO_PWD=\"<hidden>\"" ]
	cmd3 = [ "-DMAVENSARLIO_URL=\"" + MAVENSARLIO_URL + "\"",
	         "-DUPDATESSARLIO_URL=\"" + UPDATESSARLIO_URL + "\"",
	         "-DDEPENDENCIESSARLIO_URL=\"" + DEPENDENCIESSARLIO_URL + "\"",
	         "-PuploadP2Repo" ]
	for arg in args:
		cmd3.append(arg)
	cmd3.append("clean")
	cmd3.append("deploy")

	shell_cmd = ' '.join(cmd0 + cmd1 + cmd3)
	info(' '.join(cmd0 + cmd2 + cmd3))
	
	r = os.system(shell_cmd)
	if r != 0:
		sys.exit(r)

##########################################
## args : command-line arguments
## rargs : extra command-line arguments
## current_dir : folder of this script
## module : description of the current project module
## pass_phrase: the pass phrase that is used for connecting to the remote server
def upload_module(args : dict, rargs : dict, current_dir : str, module : dict, pass_phrase : str):
	header("")
	header("------------------------------------------------------------------------")
	header("Releasing " + module['name'])
	header("------------------------------------------------------------------------")
	header("")
	module_path = os.path.realpath(os.path.join(current_dir, '..', module['module']))
	os.chdir(module_path)
	execute_maven(pass_phrase, rargs)

##############################
##
parser = argparse.ArgumentParser(description="Release SARL on the server")
parser.add_argument("--modules", help="path to the JSON file defining the modules", action="store")
parser.add_argument("--pwd", help="Specify the passphrase for connecting to the server", action="store")
parser.add_argument("--showconfig", help="Show the configuration", action="store_true")
parser.add_argument('args', nargs=argparse.REMAINDER, action="append")
args = parser.parse_args()
rargs = filter_args(args.args)

current_dir = os.path.dirname(os.path.realpath(__file__))

read_user_configuration()
check_configuration()

if args.pwd:
	pass_phrase = args.pwd
else:
	try:
	    pass_phrase = getpass.getpass(prompt = f"{bcolors.FAIL}Please enter your passphrase for connecting to the server: {bcolors.ENDC}")
	except Exception as error:
	    error(str(error))
	    sys.exit(255)

if pass_phrase:
	if args.showconfig:
		print_configuration(pass_phrase)

	# Modules
	if args.modules:
		module_json_file = args.modules
	else:
		module_json_file = os.path.join(current_dir, '..', 'modules.json')
	with open(module_json_file, 'rt') as json_file:
		module_configuration = json.load(json_file)

	# Generate bundles
	for module in module_configuration['without-extension']:
		if module['releasable']:
			upload_module(args, rargs, current_dir, module, pass_phrase)

	for module in module_configuration['extensions']:
		if module['releasable']:
			upload_module(args, rargs, current_dir, module, pass_phrase)

	for module in module_configuration['with-extension']:
		if module['releasable']:
			upload_module(args, rargs, current_dir, module, pass_phrase)
	sys.exit(0)
else:
	sys.exit(255)

