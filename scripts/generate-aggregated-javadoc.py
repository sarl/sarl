#!/usr/bin/env python

import os
import sys
import argparse
import subprocess
import shutil

def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

parser = argparse.ArgumentParser()
parser.add_argument('args', nargs=argparse.REMAINDER)
args = parser.parse_args()

current_dir = os.path.dirname(os.path.realpath(__file__))

binary = os.path.join(current_dir, 'generic', 'mvn-generate-aggregated-javadoc.py')

retcode = 255

if is_exe(binary):
	try:
		shutil.rmtree(os.path.join('.', 'target', 'site'))
	except:
		pass
	cmd = [ binary,
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
		#"-DperformRelease=true",
		#"-Dsarl.compile.skip=true",
		"-Dsarl.jvminferrer.skip=true",
		"-Dsarl.clean.skip=true",
		]
	cmd = cmd + args.args

	retcode = r = subprocess.call(cmd)
else:
	print >> sys.stderr, "Cannot run mvn-generate-aggregated-javadoc.py"

sys.exit(retcode)

