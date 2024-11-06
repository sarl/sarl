#!/usr/bin/env python3

import os
import sys
import argparse
import subprocess

def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

parser = argparse.ArgumentParser()
parser.add_argument("--pwd", help="Specify the passphrase for signing the files", action="store")
parser.add_argument('args', nargs=argparse.REMAINDER)
args = parser.parse_args()

current_dir = os.path.dirname(os.path.realpath(__file__))

binary = os.path.join(current_dir, 'generic', 'mvn-central-bundles.py')

retcode = 255

if is_exe(binary):
	cmd = [ binary,
		"--create" ]
	if args.pwd:
		cmd = cmd + [ '--pwd=' + args.pwd ]
	cmd = cmd + [
		"--",
		"-Dmaven.test.skip=true",
		"-Dcheckstyle.skip=true",
		"-DperformRelease=true",
		"-DpublicSarlApiModuleSet=true" ]
	cmd = cmd + args.args
	
	#print("CMD: " + str(cmd))
	
	retcode = r = subprocess.call(cmd)
else:
	print >> sys.stderr, "Cannot run mvn-central-bundles.py"

sys.exit(retcode)

