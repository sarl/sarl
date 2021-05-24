#!/usr/bin/env python

import sys
import os
import os.path
import argparse
import subprocess
import glob

##############################
##
def ask_pass():
	pass_phrase = subprocess.check_output(['ssh-askpass', 'Please enter your passphrase for signing the files:'])
	if pass_phrase:
		pass_phrase = pass_phrase.strip()
		if pass_phrase:
			return pass_phrase
	return None

##############################
##
def run_verify():
	ret_code = 0
	nb_files = 0
	for root, dirs, files in os.walk("."):
		for filename in files:
			if filename.endswith(".asc"):
				base_name = os.path.splitext(filename)[0]
				asc_file = os.path.join(root, filename)
				signed_file = os.path.join(root, base_name)
				r = os.system("gpg --verify \"" + asc_file + "\" \"" + signed_file + "\" >/dev/null 2>/dev/null")
				if r == 0:
					nb_files = nb_files + 1
				else:
					print >> sys.stderr, signed_file
					ret_code = 255
	print("Found " + str(nb_files) + " signature files")
	return ret_code

##############################
##
def run_fix():
	nb_files = 0
	pass_phrase = ask_pass()
	if pass_phrase:
		for root, dirs, files in os.walk("."):
			for filename in files:
				if filename.endswith(".asc"):
					base_name = os.path.splitext(filename)[0]
					asc_file = os.path.join(root, filename)
					signed_file = os.path.join(root, base_name)
					r = os.system("gpg --verify \"" + asc_file + "\" \"" + signed_file + "\" >/dev/null 2>/dev/null")
					if r == 0:
						pass
					else:
						nb_files = nb_files + 1
						os.remove(asc_file)
						os.system("gpg -ab \"" + signed_file + "\"")
	print("Found " + str(nb_files) + " signature files that are fixed")
	return 0

##############################
##
def create_maven_central_bundles():
	current_dir = os.getcwd()
	nb_bundles = 0
	shell_cmd = []
	for root, dirs, files in os.walk("."):
		for filename in files:
			if filename.endswith(".pom"):
				parent_dir = os.path.basename(root)
				if parent_dir == 'target':
					artifact_name = os.path.splitext(filename)[0]
					bundle_name = artifact_name + "-bundle.jar"
					f_bundle_name = os.path.join(current_dir, root, bundle_name)
					signs = glob.glob(os.path.join(current_dir, root, "*.asc"))
					if signs:
						print("Creating " + bundle_name + "...")
						cmd = ['jar', 'cf', f_bundle_name]
						content_files = glob.glob(os.path.join(current_dir, root, "*.pom"))\
							+ glob.glob(os.path.join(current_dir, root, "*.pom.asc"))\
							+ glob.glob(os.path.join(current_dir, root, "*.jar"))\
							+ glob.glob(os.path.join(current_dir, root, "*.jar.asc"))\
							+ glob.glob(os.path.join(current_dir, root, "*.apklib"))\
							+ glob.glob(os.path.join(current_dir, root, "*.apklib.asc"))
						for input_file in content_files:
							if not input_file.endswith("-bundle.jar") and not input_file.endswith("-bundle.jar.asc"):
								cmd = cmd + [os.path.basename(input_file)]
						#print("\t" + str(cmd))
						if os.path.exists(f_bundle_name):
							os.remove(f_bundle_name)
						r = subprocess.call(cmd, cwd=root)
						if r == 0 and os.path.isfile(f_bundle_name):
							nb_bundles = nb_bundles + 1
							shell_cmd = shell_cmd + [ f_bundle_name ]
						print("Created " + bundle_name)
					else:
						print("Skipping " + bundle_name);
	print(str(nb_bundles) + " bundles were created.")
	if nb_bundles > 0:
		f = open("copy_bundles.sh", "w")
		f.write("#!/usr/bin/env bash\n")
		f.write("if [ -z \"$1\" ]; then\n")
		f.write("\techo \"expecting target folder\" >&2\n")
		f.write("\texit 255\n")
		f.write("fi\n")
		for bundle in shell_cmd:
			f.write("cp -v \"")
			f.write(bundle)
			f.write("\" \"$1\"\n")
		f.close()
		print("Copying command line:")
		print("$> bash copy_bundles.sh")

##############################
## args: command line arguments
def run_create(args):
	maven_cmd = os.environ.get('MAVEN_CMD')
	if maven_cmd is None:
		maven_cmd = 'mvn'
	pass_phrase = ask_pass()
	if pass_phrase:
		print("Assuming 'maven-javadoc-plugin:jar' is activated")
		print("Assuming 'maven-sources-plugin:jar' is activated")
		print("Assuming tests are desactivated")
		print("Have you activated the released profile? -DperformRelease=true")

		cmdstr = maven_cmd + " -Dgpg.passphrase=\"<hidden>\" -Darguments=-Dgpg.passphrase=\"<hidden>\""
		cmd = maven_cmd + " -Dgpg.passphrase=\"" + pass_phrase + "\" -Darguments=-Dgpg.passphrase=\"" + pass_phrase + "\""
		for arg in args:
			cmdstr = cmdstr + " " + arg
			cmd = cmd + " " + arg
		cmdstr = cmdstr + " clean install"
		cmd = cmd + " clean install"
		print(cmdstr)

		r = os.system(cmd)
		if r == 0:
			create_maven_central_bundles()
			return 0
		else:
			return 255
	else:
		return 255

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
##
parser = argparse.ArgumentParser(description="Generate the bundles for Maven Central")
group = parser.add_mutually_exclusive_group()
group.add_argument("--verify", help="verify the signatures of the Central bundles", action="store_true")
group.add_argument("--fix", help="fix the signatures of the Central bundles", action="store_true")
group.add_argument("--create", help="create the Central bundles", action="store_true")
parser.add_argument('args', nargs=argparse.REMAINDER, action="append")
args = parser.parse_args()
rargs = filterArgs(args.args)

retcode = 255
if args.verify:
	retcode = run_verify()
elif args.fix:
	retcode = run_fix()
elif args.create:
	retcode = run_create(rargs)
else:
	parser.print_help(sys.stderr)

sys.exit(retcode)


