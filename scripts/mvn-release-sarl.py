#!/usr/bin/env python

import os
import sys
import argparse
import subprocess
import configparser

MAVENSARLIO_URL = os.environ.get("MAVENSARLIO_URL", None)
UPDATESSARLIO_URL = os.environ.get("UPDATESSARLIO_URL", None)
MAVENSARLIO_USER = os.environ.get("MAVENSARLIO_USER", None)
DEPENDENCIESSARLIO_URL = os.environ.get("DEPENDENCIESSARLIO_URL", None)

##########################################"
##
def read_user_configuration():
	global MAVENSARLIO_URL
	global UPDATESSARLIO_URL
	global MAVENSARLIO_USER
	global DEPENDENCIESSARLIO_URL

	sarlrc_file = os.path.join(os.environ['HOME'], '.sarlrc')
	if os.path.exists(sarlrc_file):
		config = configparser.SafeConfigParser()
		config.read(sarlrc_file)
		MAVENSARLIO_URL = config.get('servers', 'MAVENSARLIO_URL')
		UPDATESSARLIO_URL = config.get('servers', 'UPDATESSARLIO_URL')
		MAVENSARLIO_USER = config.get('servers', 'MAVENSARLIO_USER')
		DEPENDENCIESSARLIO_URL = config.get('servers', 'DEPENDENCIESSARLIO_URL')

##############################
##
def ask_pass():
	pass_phrase = subprocess.check_output(['ssh-askpass', 'Please enter your passphrase for connection to the server:'])
	if pass_phrase:
		pass_phrase = pass_phrase.strip()
		if pass_phrase:
			return pass_phrase
	return None

##############################
##
def check_configuration():
	global MAVENSARLIO_URL
	global UPDATESSARLIO_URL
	global MAVENSARLIO_USER
	global DEPENDENCIESSARLIO_URL

	if not MAVENSARLIO_URL:
		raise Exception("You must define the MAVENSARLIO_URL environment variable to the URL of the Maven upload server , e.g. dav:https://myhost/dav")

	if not UPDATESSARLIO_URL:
		raise Exception("You must define the UPDATESSARLIO_URL environment variable to the URL of the P2 upload server , e.g. dav:https://myhost/dav")

	if not MAVENSARLIO_USER:
		raise Exception("You must define the MAVENSARLIO_USER environment variable with the login to the upload servers.")

	if not DEPENDENCIESSARLIO_URL:
		raise Exception("You must define the DEPENDENCIESSARLIO_URL environment variable with the login to the upload servers.")

##############################
## pass_phrase: password
def print_configuration(pass_phrase):
	global MAVENSARLIO_URL
	global UPDATESSARLIO_URL
	global MAVENSARLIO_USER
	global DEPENDENCIESSARLIO_URL

	print("MAVENSARLIO_URL = " + str(MAVENSARLIO_URL))
	print("UPDATESSARLIO_URL = " + str(UPDATESSARLIO_URL))
	print("DEPENDENCIESSARLIO_URL = " + str(DEPENDENCIESSARLIO_URL))
	print("MAVENSARLIO_USER = " + str(MAVENSARLIO_USER))
	print("MAVENSARLIO_PWD = " + str(pass_phrase))

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
## args: command-line arguments
def execute_maven(args):
	global MAVENSARLIO_URL
	global UPDATESSARLIO_URL
	global MAVENSARLIO_USER
	global DEPENDENCIESSARLIO_URL

	maven_cmd = os.environ.get('MAVEN_CMD')
	if maven_cmd is None:
		maven_cmd = 'mvn'
	os.environ["MAVENSARLIO_USER"] = MAVENSARLIO_USER
	os.environ["MAVENSARLIO_PWD"] = pass_phrase
	os.environ["MAVENSARLIO_URL"] = MAVENSARLIO_URL
	os.environ["UPDATESSARLIO_URL"] = UPDATESSARLIO_URL
	os.environ["DEPENDENCIESSARLIO_URL"] = DEPENDENCIESSARLIO_URL
	cmd = [ maven_cmd,
	         "-Dmaven.test.skip=true",
	         "-DskipTests=true",
	         "-Dcheckstyle.skip=true",
	         "-DMAVENSARLIO_USER=\"" + MAVENSARLIO_USER + "\"",
	         "-DMAVENSARLIO_PWD=\"" + pass_phrase + "\"",
	         "-DMAVENSARLIO_URL=\"" + MAVENSARLIO_URL + "\"",
	         "-DUPDATESSARLIO_URL=\"" + UPDATESSARLIO_URL + "\"",
	         "-DDEPENDENCIESSARLIO_URL=\"" + DEPENDENCIESSARLIO_URL + "\"",
	         "-PuploadP2Repo",
		]
	for arg in args:
		cmd.append(arg)
	cmd.append("clean")
	cmd.append("deploy")

	shell_cmd = ' '.join(cmd)
	print(shell_cmd)
	
	r = os.system(shell_cmd)
	return r

##############################
##
parser = argparse.ArgumentParser(description="Release SARL on the server")
parser.add_argument('args', nargs=argparse.REMAINDER, action="append")
args = parser.parse_args()
rargs = filterArgs(args.args)

read_user_configuration()
check_configuration()

pass_phrase = ask_pass()
if pass_phrase:
	print_configuration(pass_phrase)
	r = execute_maven(rargs)
	sys.exit(r)	
else:
	sys.exit(255)

