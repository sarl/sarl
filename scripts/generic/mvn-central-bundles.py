#!/usr/bin/env python3
#
# Author: Stephane GALLAND
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

# Create bundles for uploaded on the Maven Central Portal
# Bundle format is valid after June 2025

import sys
import os
import os.path
import shutil
import argparse
import subprocess
import glob
import hashlib
from datetime import datetime
from xml.etree import ElementTree

NAMESPACE = 'http://maven.apache.org/POM/4.0.0'
NAMESPACES = {'xmlns' : NAMESPACE}
	
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
def header(message):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
##
def info(message):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]" + msg, file=sys.stdout)

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

#########################
## filename: the path to the POM file.
def readXMLRootNode(filename):
	ElementTree.register_namespace('', NAMESPACE)
	tree = ElementTree.parse(filename)
	root = tree.getroot()
	return root

#########################
## node: the XML node
## name: the name of the value to read.
def readXml(node, name):
	valueNode = node.find("xmlns:" + name, namespaces=NAMESPACES)
	if valueNode is not None:
		textValue = valueNode.text
		if textValue:
			return textValue
	return ''

##############################
##
def ask_pass():
	pass_phrase = subprocess.check_output(['ssh-askpass', 'Please enter your passphrase for signing the files:'])
	if pass_phrase:
		pass_phrase = pass_phrase.decode()
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
					error(signed_file)
					ret_code = 255
	info("Found " + str(nb_files) + " signature files")
	return ret_code

##############################
##
def run_fix(pwd, args):
	nb_files = 0
	if (pwd):
		pass_phrase = pwd
	else:
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
	info("Found " + str(nb_files) + " signature files that are fixed")
	return 0

##############################
##
def create_maven_central_bundles(out_directory):
	current_dir = os.getcwd()
	nb_bundles = 0
	shell_cmd = []
	for root, dirs, files in os.walk("."):
		for filename in files:
			if filename.endswith(".pom"):
				parent_dir = os.path.basename(root)
				if parent_dir == 'target':
					pom = readXMLRootNode(os.path.join(current_dir, root, filename))
					packaging_type = readXml(pom, 'packaging')
					artifact_name = readXml(pom, 'artifactId')
					group_name = readXml(pom, 'groupId')
					if not group_name:
						parent = pom.find("xmlns:parent", namespaces=NAMESPACES)
						group_name = readXml(parent, 'groupId')
					version_number = readXml(pom, 'version')
					if not version_number:
						parent = pom.find("xmlns:parent", namespaces=NAMESPACES)
						version_number = readXml(parent, 'version')
					
					if not artifact_name:
						raise Exception("artifactId is missed for " + filename)
					if not group_name:
						raise Exception("groupId is missed for " + filename)
					if not version_number:
						raise Exception("version is missed for " + filename)

					bundle_name = artifact_name + "-bundle.jar"
					f_bundle_name = os.path.join(current_dir, root, bundle_name)
					signs = glob.glob(os.path.join(current_dir, root, "*.asc"))
					if signs:
						info("Creating " + bundle_name + "...")

						temp_output_folder = os.path.join(current_dir, root, "maven-central-bundles-temp")
						content_folder = os.path.join(group_name.replace(".", "/"), artifact_name, version_number)
						writing_output_folder = os.path.join(temp_output_folder, content_folder)
						if os.path.exists(temp_output_folder):
							shutil.rmtree(temp_output_folder)
						os.makedirs(writing_output_folder, exist_ok=True)
						
						cmd = ['jar', '-c', '-f', f_bundle_name]
						content_files = glob.glob(os.path.join(current_dir, root, "*.pom"))\
							+ glob.glob(os.path.join(current_dir, root, "*.pom.asc"))\
							+ glob.glob(os.path.join(current_dir, root, "*.jar"))\
							+ glob.glob(os.path.join(current_dir, root, "*.jar.asc"))\
							+ glob.glob(os.path.join(current_dir, root, "*.apklib"))\
							+ glob.glob(os.path.join(current_dir, root, "*.apklib.asc"))
						for input_file in content_files:
							if not input_file.endswith("-bundle.jar") and not input_file.endswith("-bundle.jar.asc"):
								base_output_filename = os.path.join(writing_output_folder, os.path.basename(input_file))
								shutil.copyfile(input_file, base_output_filename)
								with open(input_file, 'rb') as file_to_check:
									data = file_to_check.read()    
									md5_value = hashlib.md5(data).hexdigest()
									sha1_value = hashlib.sha1(data).hexdigest()
								with open(base_output_filename + '.md5', 'wt') as md5_file:
									md5_file.write(md5_value)
								with open(base_output_filename + '.sha1', 'wt') as sha1_file:
									sha1_file.write(sha1_value)
								cmd = cmd + ["-C", temp_output_folder, os.path.join(content_folder, os.path.basename(input_file))]
								cmd = cmd + ["-C", temp_output_folder, os.path.join(content_folder, os.path.basename(input_file) + '.md5')]
								cmd = cmd + ["-C", temp_output_folder, os.path.join(content_folder, os.path.basename(input_file) + '.sha1')]

						metadata_filename = os.path.join(os.path.dirname(writing_output_folder), "maven-metadata.xml")							
						timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
						with open(metadata_filename, 'wt') as metadata_file:
							metadata_file.write("<metadata><groupId>" + group_name + "</groupId><artifactId>" + artifact_name + "</artifactId><versioning><release>" + version_number + "</release><versions><version>" + version_number + "</version></versions><lastUpdated>" + timestamp + "</lastUpdated></versioning></metadata>")
						with open(metadata_filename, 'rb') as file_to_check:
							data = file_to_check.read()    
							md5_value = hashlib.md5(data).hexdigest()
							sha1_value = hashlib.sha1(data).hexdigest()
						with open(metadata_filename + '.md5', 'wt') as md5_file:
							md5_file.write(md5_value)
						with open(metadata_filename + '.sha1', 'wt') as sha1_file:
							sha1_file.write(sha1_value)
						cmd = cmd + ["-C", temp_output_folder, os.path.join(os.path.dirname(content_folder), "maven-metadata.xml")]
						cmd = cmd + ["-C", temp_output_folder, os.path.join(os.path.dirname(content_folder), "maven-metadata.xml.md5")]
						cmd = cmd + ["-C", temp_output_folder, os.path.join(os.path.dirname(content_folder), "maven-metadata.xml.sha1")]

    						#print("\t" + str(cmd))
						if os.path.exists(f_bundle_name):
							os.remove(f_bundle_name)
						r = subprocess.call(cmd, cwd=root)
						if r == 0 and os.path.isfile(f_bundle_name):
							nb_bundles = nb_bundles + 1
							shell_cmd = shell_cmd + [ f_bundle_name ]
														
						info("Created " + bundle_name)
					else:
						info("Skipping " + bundle_name);
	info(str(nb_bundles) + " bundles were created.")
	if nb_bundles > 0:
		if out_directory:
			for bundle in shell_cmd:
				out_bundle = os.path.join(out_directory, os.path.basename(bundle))
				if os.path.isfile(out_bundle):
					error("Existing output bundle: " + out_bundle)
					sys.exit(255)
				info("Copying bundle: " + bundle)
				shutil.copyfile(bundle, out_bundle)
		else:
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
			info("Copying command line:")
			info("$> bash copy_bundles.sh")

##############################
## args: command line arguments
def run_create(out_directory, pwd, args):
	maven_cmd = os.environ.get('MAVEN_CMD')
	if maven_cmd is None:
		maven_cmd = 'mvn'
	if (pwd):
		pass_phrase = pwd
	else:
		pass_phrase = ask_pass()
	if pass_phrase:
		info("Assuming 'maven-javadoc-plugin:jar' is activated")
		info("Assuming 'maven-sources-plugin:jar' is activated")
		info("Assuming tests are desactivated")
		info("Have you activated the released profile? -DperformRelease=true")

		cmdstr = maven_cmd + " -Dgpg.passphrase=\"<hidden>\" -Darguments=-Dgpg.passphrase=\"<hidden>\""
		cmd = maven_cmd + " -Dgpg.passphrase=\"" + pass_phrase + "\" -Darguments=-Dgpg.passphrase=\"" + pass_phrase + "\""
		for arg in args:
			cmdstr = cmdstr + " " + arg
			cmd = cmd + " " + arg
		cmdstr = cmdstr + " clean install"
		cmd = cmd + " clean install"
		info(cmdstr)

		r = os.system(cmd)
		if r == 0:
			create_maven_central_bundles(out_directory)
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
parser.add_argument("--pwd", help="Specify the passphrase for signing the files", action="store")
parser.add_argument("--out", help="Specify the folder in which the generated bundles must be copied", action="store")
parser.add_argument('args', nargs=argparse.REMAINDER, action="append")
args = parser.parse_args()
rargs = filterArgs(args.args)

retcode = 255
if args.verify:
	retcode = run_verify()
elif args.fix:
	retcode = run_fix(args.pwd, rargs)
elif args.create:
	retcode = run_create(args.out, args.pwd, rargs)
else:
	parser.print_help(sys.stderr)

sys.exit(retcode)


