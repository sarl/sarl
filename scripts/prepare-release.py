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

########################################################################
# Apply many types of updates in the projects for preparing them for a #
# release or a development version                                     #
########################################################################

import argparse
import re
import os
import subprocess
import sys
import json
from pathlib import Path
from xml.etree import ElementTree
from datetime import datetime

MIN_VERSION_DIGITS = 3
NAMESPACE = 'http://maven.apache.org/POM/4.0.0'
NAMESPACES = {'xmlns' : NAMESPACE}
GIT_COMMIT_URL = "http://sarl.io/sarl/sarl/commit/"

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

#########################
##
def show_version(message : str, version : str):
	if re.search("^0(\\.0)*(\\-SNAPSHOT)?$", version):
		error(f"{bcolors.FAIL}" + message + " " + version + f"{bcolors.ENDC}")
	else:
		info(message + " " + version)

#########################
## filename: the path to the POM file.
def read_xml_root_node(filename : str) -> object:
	ElementTree.register_namespace('', NAMESPACE)
	tree = ElementTree.parse(filename)
	root = tree.getroot()
	return root

#########################
## name: the name of the person for which the id should be built
def buildIdFromName(name):
	elements = re.split("[ \n\r\t\\-_]+", name)
	if len(elements) > 1:
		s = ''
		for e in elements[0:-1]:
			s = s + str(e[0]).lower()
		s = s + str(elements[-1]).lower()
		return s
	return str(elements[0]).lower()

#########################
## node: the XML node
## name: the name of the value to read.
def read_xml(node : object, name : str) -> str:
	valueNode = node.find("xmlns:" + name, namespaces=NAMESPACES)
	if valueNode is not None:
		textValue = valueNode.text
		if textValue:
			return textValue
	return ''

#########################
## root: the Maven POM root node.
def get_maven_contributors(root : object) -> dict:
	contributors = {}
	contribs = root.findall(".//xmlns:contributor", namespaces=NAMESPACES)
	for contrib in contribs:
		name = read_xml(contrib, "name")
		if name is not None:
			contrib_id = buildIdFromName(name)
			if contrib_id:
				contributors[contrib_id] = {}
				contributors[contrib_id]['id'] = contrib_id
				contributors[contrib_id]['name'] = name
				email = read_xml(contrib, "xml")
				if email:
					contributors[contrib_id]['email'] = email
				url = read_xml(contrib, "url")
				if url:
					contributors[contrib_id]['url'] = url
	authors = root.findall(".//xmlns:developer", namespaces=NAMESPACES)
	for author in authors:
		name = read_xml(author, "name")
		if name is not None:
			author_id =  read_xml(author, "id")
			if author_id is None:
				author_id = buildIdFromName(name)
			if author_id:
				contributors[author_id] = {}
				contributors[author_id]['id'] = author_id
				contributors[author_id]['name'] = name
				email = read_xml(author, "xml")
				if email:
					contributors[author_id]['email'] = email
				url = read_xml(author, "url")
				if url:
					contributors[author_id]['url'] = url
	return contributors

#########################
## module_path : path to the root folder of the module
## RETURN : the list of Java and SARL files
def build_code_file_list(module_path : str) -> list:
	fileList = []
	for root, dirs, files in os.walk(module_path):
		for filename in files:
			if filename.endswith(".java") or filename.endswith(".sarl"):
				fileList.append(os.path.realpath(os.path.join(root, filename)))
	return fileList

#########################
## module_path : path to the root folder of the module
## extra_pom_files: list of extra pom files thatm ust be included in the list if they exist
## RETURN : the list of POM files
def build_maven_file_list(module_path : str, extra_pom_files : list) -> list:
	fileList = []
	for root, dirs, files in os.walk(module_path):
		for filename in files:
			if filename == "pom.xml":
				fileList.append(os.path.join(root, filename))
	if extra_pom_files:
		for added_file in extra_pom_files:
			if os.path.exists(added_file):
				fileList.append(added_file)
	return fileList

#########################
## module_path : path to root folder of the module
def build_eclipse_file_list(module_path : str) -> list:
	fileList = []
	for root, dirs, files in os.walk(module_path):
		for filename in files:
			if filename == "MANIFEST.MF" or filename == "feature.xml" or filename == "category.xml" or filename.endswith(".product"):
				fileList.append(os.path.join(root, filename))
	return fileList

#########################
## module_path : path to root folder of the module
def build_eclipse_target_platform_list(module_path : str) -> list:
	fileList = []
	for root, dirs, files in os.walk(module_path):
		for filename in files:
			if filename.endswith(".target"):
				fileList.append(os.path.join(root, filename))
	return fileList

#########################
## module_path : path to the root folder of the module
def build_whats_new_xml_file_list(module_path : str):
	fileList = []
	for root, dirs, files in os.walk(module_path):
		for filename in files:
			if filename.endswith("whatsnew.xml"):
				fileList.append(os.path.join(root, filename))
	return fileList

#########################
## module_path: the path to the root folder of the module
## enableParent: indicates if the files in the parent folder must be updated also
def build_readme_file_list(module_path : str, enableParent : bool):
	candidates = [
		os.path.join(module_path, 'README'),
		os.path.join(module_path, 'CONTRIBUTING'),
		os.path.join(module_path, 'RELEASE_README'),
		os.path.join(module_path, 'NOTICE')
		]
	if enableParent:
		candidates = candidates + [
			os.path.realpath(os.path.join(module_path, '..', 'README')),
			os.path.realpath(os.path.join(module_path, '..', 'CONTRIBUTING')),
			os.path.realpath(os.path.join(module_path, '..', 'RELEASE_README')),
			os.path.realpath(os.path.join(module_path, '..', 'NOTICE'))
			]
	extensions = [ '', '.txt', '.md', '.adoc' ]
	fileList = []
	for candidate in candidates:
		for extension in extensions:
			filename = candidate + extension
			if os.path.exists(filename):
				fileList.append(filename)
	return fileList

#########################
## source: text to replace in the regex
## contributors: the hash map of contributors
## filename: the name of the file to be updated.
def do_author_replacement(source : object, contributors : list, filename : str) -> str:
	cid = source.group(1)
	if cid and cid in contributors:
		name = contributors[cid]['name']
		if not name:
			name = cid
		url = contributors[cid]['url']
		if not url:
			url = contributors[cid]['email']
			if url:
				url = "email:" + url
		if url:
			return "<a href=\"" + url + "\">" + name + "</a>"
	error("In " + str(filename) + ", unable to find the contributor for: " + str(source.group()) + "\nKnown contributors are: " + str(contributors))
	sys.exit(255)

#########################
## filename: the filename to update
## contributors: the hash map of contributors
def replace_authors(filename : str, contributors : list):
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = re.sub("\\$Author[ \t]*\\:[ \t]*([^$]+)\\$", lambda x : do_author_replacement(x, contributors, filename), line)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
## filename: the filename to start from
def detect_enclosing_project_pom(filename : str) -> str:
	folder = os.path.dirname(filename)
	while not os.path.exists(os.path.join(folder, 'pom.xml')):
		folder = os.path.dirname(folder)
	return folder

#########################
## filename: the filename to update
## generation_date: date of the generation
## exclusions: list of pom files that are not compatible
def replace_maven(filename : str, generation_date : str, exclusions : list):
	pomFolder = detect_enclosing_project_pom(filename)
	if not pomFolder:
		error("Cannot detect POM file for project")
		sys.exit(255)
	pomFile = os.path.join(pomFolder, 'pom.xml')
	if pomFile in exclusions:
		info("Skipping " + filename)
	else:
		try:
			pom = read_xml_root_node(pomFile)
			artifactId = read_xml(pom, 'artifactId')
			groupId = read_xml(pom, 'groupId')
			if not groupId:
				parent = pom.find("xmlns:parent", namespaces=NAMESPACES)
				groupId = read_xml(parent, 'groupId')
			version = read_xml(pom, 'version')
			if not version:
				parent = pom.find("xmlns:parent", namespaces=NAMESPACES)
				version = read_xml(parent, 'version')
			if not groupId:
				error("Unable to determine the Maven group id for: " + filename)
				sys.exit(255)
			if not artifactId:
				error("Unable to determine the Maven artifact id for: " + filename)
				sys.exit(255)
			if not version:
				error("Unable to determine the Maven artifact version for: " + filename)
				sys.exit(255)
			full_version = str(artifactId) + " " + str(version) + " " + str(generation_date)
			with open (filename, "r") as myfile:
				data = myfile.readlines()
			data2 = []
			for line in data:
				line2 = re.sub("\\$GroupId\\$", groupId, line)
				line2 = re.sub("\\$ArtifactId\\$", artifactId, line2)
				line2 = re.sub("\\$Name\\$", artifactId, line2)
				line2 = re.sub("\\$Version\\$", version, line2)
				line2 = re.sub("\\$Revision\\$", version, line2)
				line2 = re.sub("\\$Date\\$", generation_date, line2)
				line2 = re.sub("\\$FullVersion\\$", full_version, line2)
				line2 = re.sub("\\$Filename\\$", os.path.basename(filename), line2)
				data2.append(line2)
			with open (filename, "w") as myfile:
				myfile.write("".join(data2))
		except Exception as ex:
			exclusions.append(pomFile)
			error(pomFile + ": " + filename + ": " + str(ex))

#########################
## source: text to replace
## this_year: the current year
def doCopyrightReplacement0(source, this_year):
	prefix = source.group(1)
	return prefix + str(this_year) + " SARL.io, the original authors and main authors"

#########################
## source: text to replace
## this_year: the current year
def doCopyrightReplacement1(source, this_year):
	prefix = source.group(1)
	return prefix + "-" + str(this_year) + " SARL.io, the original authors and main authors"

#########################
## filename: the name of the file to be updated
## end_year: final year to consider in the copyrights
def replace_copyrights(filename : str, end_year : str):
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		# Format Y1-Y2
		line2 = re.sub("(Copyright (?:\\(C\\) )?[0-9]+\\-)[0-9]+ SARL.io, the Original Authors and Main Authors", lambda x : doCopyrightReplacement0(x, str(end_year)), line, flags=re.I)
		line2 = re.sub("(Copyright (?:\\(C\\) )?[0-9]+\\-)[0-9]+ the original authors or authors", lambda x : doCopyrightReplacement0(x, str(end_year)), line2, flags=re.I)
		# Format Y1
		line2 = re.sub("(Copyright (?:\\(C\\) )?[0-9]+) SARL.io, the Original Authors and Main Authors", lambda x : doCopyrightReplacement1(x, str(end_year)), line2, flags=re.I)
		line2 = re.sub("(Copyright (?:\\(C\\) )?[0-9]+) the original authors or authors", lambda x : doCopyrightReplacement1(x, str(end_year)), line2, flags=re.I)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
## root: the root node of the root POM xml file
def read_maven_version(root : object) -> str:
	parent_node = root.find("xmlns:parent", namespaces=NAMESPACES)
	version = read_xml(root, 'version')
	if not version and parent_node:
		version = read_xml(parent_node, 'version')
	if not version:
		error("Unable to determine the Maven artifact version for: " + filename)
		sys.exit(255)
	return version

#########################
## current_devel_version: the version number of the current devel in the pom xml file
## next_stable_version: the version number of the next stable in the pom xml file
## filename: the filename to change
def move_to_release_version_in_maven(current_devel_version : str, next_stable_version : str, filename : str):
	if current_devel_version.endswith("-SNAPSHOT"):
		with open (filename, "r") as myfile:
			data = myfile.readlines()
		data2 = []
		for line in data:
			line2 = line.replace(current_devel_version, next_stable_version)
			data2.append(line2)
		with open (filename, "w") as myfile:
			myfile.write("".join(data2))

#########################
## current_devel_version: the current devel version number in the eclipse files
## next_stable_version: the next stable version number in the eclipse files
## filename: the filename to change
def move_to_release_version_in_eclipse(current_devel_version : str, next_stable_version : str, filename : str):
	if current_devel_version.endswith(".qualifier"):
		short_version = current_devel_version[0:-10]
		with open (filename, "r") as myfile:
			data = myfile.readlines()
		data2 = []
		for line in data:
			line2 = line.replace(current_devel_version, next_stable_version)
			line2 = line2.replace(short_version + ".SNAPSHOT", next_stable_version)
			line2 = re.sub(
				re.escape(short_version + ".") + "[0-9]+",
				next_stable_version,
				line2)
			data2.append(line2)
		with open (filename, "w") as myfile:
			myfile.write("".join(data2))

#########################
## current_stable_version: the version number of the current stable in the pom xml file
## next_stable_version: the version number of the next stable in the pom xml file
## current_devel_version: the version number of the current snapshot in the pom xml file
## next_devel_version: the version number of the next snapshot in the pom xml file
## filename: the filename to change
def move_to_release_version_in_readme(current_stable_version : str, next_stable_version : str, current_devel_version : str, next_devel_version : str, filename : str):
	if current_devel_version.endswith("-SNAPSHOT"):
		with open (filename, "r") as myfile:
			data = myfile.readlines()
		data2 = []
		for line in data:
			line2 = line.replace(current_stable_version, next_stable_version)
			line2 = line2.replace(current_devel_version, next_devel_version)
			data2.append(line2)
		with open (filename, "w") as myfile:
			myfile.write("".join(data2))

#########################
## current_devel_version: the current devel version number in the pom xml file
## next_devel_version: the next devel version number in the pom xml file
## filename: the filename to change
def move_to_devel_version_in_maven(current_devel_version : str, next_devel_version : str, filename : str):
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = line.replace(current_devel_version, next_devel_version)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
## current_devel_version: the current devel_version number in the eclipse file
## next_devel_version: the next devel version number in the eclipse file
## filename: the filename to change
def move_to_devel_version_in_eclipse(current_devel_version : str, next_devel_version : str, filename : str):
	if current_devel_version.endswith(".qualifier"):
		short_version = current_devel_version[0:-10]
	elif current_devel_version.endswith("-SNAPSHOT") or current_devel_version.endswith(".SNAPSHOT"):
		short_version = current_devel_version[0:-9]
	else:
		short_version = current_devel_version
	if next_devel_version.endswith(".qualifier"):
		short_version_n = next_devel_version[0:-10]
	elif next_devel_version.endswith("-SNAPSHOT") or next_devel_version.endswith(".SNAPSHOT"):
		short_version_n = next_devel_version[0:-9]
	else:
		short_version_n = next_devel_version
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = line.replace(current_devel_version, next_devel_version)
		line2 = line2.replace(short_version + ".qualifier", next_devel_version)
		line2 = line2.replace(short_version + ".SNAPSHOT", short_version_n + ".SNAPSHOT")
		line2 = line2.replace(short_version + "-SNAPSHOT", short_version_n + "-SNAPSHOT")
		line2 = re.sub(
			re.escape(short_version + ".") + "[0-9]+",
			next_devel_version,
			line2)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
## filename: the filename to be updated
## filenames: the list of filenames that should be marked as updated
def show_update_file_msg(filename : str, filenames : list):
	if filename not in filenames:
		filenames.append(filename)
		info("Updating " + filename)

#########################
## current_version: the current version (snapshot)
## previous_stable: the previous stable version (not snapshot)
## RETURN: the stable version
def build_current_stable_version(current_version : str, previous_stable : str) -> str:
	if previous_stable:
		return previous_stable
	elements = current_version.split(".")
	while len(elements) < MIN_VERSION_DIGITS:
		elements.append('0')
	micro = int(elements[2])
	minor = int(elements[1])
	major = int(elements[0])
	micro = micro - 1
	if micro < 0:
		micro = 99
		minor = minor - 1
	if minor < 0:
		minor = 99
		major = major - 1
	if major < 0:
		major = 0
	version = f"{major}.{minor}.{micro}"
	return version

#########################
## current_version: the current version (snapshot)
## next_stable: the next stable version (not snapshot)
## RETURN: the next stable version
def build_next_stable_version(current_version : str, next_stable : str) -> str:
	if next_stable:
		return next_stable
	return current_version.replace("-SNAPSHOT", "")

#########################
## current_version: the current version (snapshot)
## next_devel: the next devel version (snapshot)
## RETURN: the next stable version
def build_next_devel_version(current_version : str, next_devel : str) -> str:
	if next_devel:
		return next_devel
	elements = current_version.split(".")
	return str(elements[0]) + "." + str(int(elements[1]) + 1) + ".0"

#########################
## args : command-line arguments
## current_dir : folder of this script
## current_processus_dir : current folder of this process
## current_stable_version: the version of the current stable release
## changelog_file: the path to the changelog file to be written. If not specified, default is target/changelog.md
def generate_changes(args : dict, current_dir : str, current_processus_dir : str, current_stable_version : str, changelog_file : str):
	if changelog_file and not os.path.isabs(changelog_file):
		changelog_file = os.path.realpath(os.path.join(current_processus_dir, changelog_file))
	gitoutput = subprocess.check_output(['git', 'log', '--format=tformat:* %s [[more details](' + GIT_COMMIT_URL + '%H)]', str(current_stable_version) + '..HEAD'])
	if changelog_file:
		output_filename = changelog_file
	else:
		output_filename = os.path.realpath(os.path.join(current_dir, '..', 'target', 'changelog.md'))
	Path(os.path.dirname(output_filename)).mkdir(parents=True, exist_ok=True)
	with open (output_filename, "w") as myfile:
		myfile.write(gitoutput.decode("utf-8"))
	info("Changelog is written in " + output_filename)

#########################
## current_stable_version: the version number of the current stable/development in the what's new xml file
## current_devel_version: the version number of the current stable/development in the what's new xml file
## next_version: the version number of the next stable/development in the what's new xml file
## filename: the filename to change
def move_to_release_version_in_whats_new(current_stable_version : str, current_devel_version : str, next_version : str, filename : str):
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = line.replace(current_stable_version, next_version)
		line2 = line2.replace(current_devel_version, next_version)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
## args : command-line arguments
## current_stable_version : number for the latest stable version
## computed_current_stable_version : indicates if the current_stable_version was automatically computed (true), or provided by the user of this script
## current_devel_version : number for the current development version
## next_stable_version : number for the next stable version
## next_devel_version : number for the next development version
## end_copyright_year: last year to be considered in the copyright time window.
def show_informations(args : dict, current_stable_version : str, computed_current_stable_version : bool, current_devel_version : str, next_stable_version : str, next_devel_version : str, end_copyright_year : str):
	if computed_current_stable_version:
		info(f"{bcolors.FAIL}Current stable version: " + current_stable_version + f"{bcolors.ENDC}")
	else:
		show_version("Current stable version:", current_stable_version)
	show_version("Current devel version:", current_devel_version)
	show_version("Next stable version:", next_stable_version)
	show_version("Next devel version:", next_devel_version)
	if args.changes:
		info("Action: generate the changelog")
	elif args.author:
		info("Action: update Maven author tags")
	elif args.maven:
		info("Action: update Maven general tags")
	elif args.copyrights:
		info("Copyright end year: " + str(end_copyright_year))
		info("Action: update the copyright text")
	elif args.releaseversion:
		info("Action: move to the release version " + next_stable_version)
	elif args.develversion:
		info("Action: move to the development version " + next_devel_version)
	else:
		info("Action: unknow")

#########################
## args : command-line arguments
## current_dir : folder of this script
## module_path : path to the root folder of the module
## module : description of the current project module
## changed_filenames : list of files that is updated with the names of the updated files
## current_stable_version: the version number of the current stable in the pom xml file
## next_stable_version: the version number of the next stable in the pom xml file
## current_devel_version: the version number of the current snapshot in the pom xml file
## next_devel_version: the version number of the next snapshot in the pom xml file
def prepare_readme_files(args : dict, current_dir : str, module_path : str, module : dict, changed_filenames : list, current_stable_version : str, next_stable_version : str, current_devel_version : str, next_devel_version : str):
	if args.releaseversion:
		if "release-config" in module and module["release-config"]["readme-in-parent"]:
			read_parent = True
		else:
			read_parent = False
		files = build_readme_file_list(module_path, read_parent)
		for filename in files:
			show_update_file_msg(filename, changed_filenames)
			move_to_release_version_in_readme(current_stable_version, next_stable_version, current_devel_version, next_devel_version, filename)

#########################
## args : command-line arguments
## current_dir : folder of this script
## module_path : path to the root folder of the module
## module : description of the current project module
## changed_filenames : list of files that is updated with the names of the updated files
## generation_date: date of the generation
## contributors : list of contributors
## end_copyright_year : year of the end of the copyright time window
def prepare_java_sarl_files(args : dict, current_dir : str, module_path : str, module : dict, changed_filenames : list, generation_date : str, contributors : list, end_copyright_year : str):
	if args.author or args.maven or args.copyrights:
		files = build_code_file_list(module_path)
		exclusions = []
		for filename in files:
			if args.author:
				show_update_file_msg(filename, changed_filenames)
				replace_authors(filename, contributors)
			if args.maven:
				show_update_file_msg(filename, changed_filenames)
				replace_maven(filename, generation_date, exclusions)
			if args.copyrights:
				show_update_file_msg(filename, changed_filenames)
				replace_copyrights(filename, end_copyright_year)

#########################
## args : command-line arguments
## current_dir : folder of this script
## module_path : path to the root folder of the module
## module : description of the current project module
## changed_filenames : list of files that is updated with the names of the updated files
## current_devel_version: the version number of the current devel in the pom xml file
## next_stable_version: the version number of the next stable in the pom xml file
def prepare_maven_pom_files(args : dict, current_dir : str, module_path : str, module : dict, changed_filenames : list, current_devel_version : str, next_stable_version : str):
	if args.releaseversion or args.develversion:
		extra_pom = []
		if "upgrade-mvn-plugins-pom" in module:
			extra_filename = os.path.join(*module["upgrade-mvn-plugins-pom"])
			extra_filename = os.path.realpath(os.path.join(module_path, extra_filename))
			extra_pom.append(extra_filename)
		files = build_maven_file_list(module_path, extra_pom)
		for filename in files:
			if args.releaseversion:
				show_update_file_msg(filename, changed_filenames)
				move_to_release_version_in_maven(current_devel_version, next_stable_version, filename)
			if args.develversion:
				show_update_file_msg(filename, changed_filenames)
				move_to_devel_version_in_maven(current_devel_version, next_stable_version, filename)

#########################
## args : command-line arguments
## current_dir : folder of this script
## module_path : path to the root folder of the module
## module : description of the current project module
## changed_filenames : list of files that is updated with the names of the updated files
## current_devel_version: the current devel version number in the eclipse files
## next_stable_version: the next stable version number in the eclipse files
## next_devel_version: the next development version number in the eclipse files
## end_copyright_year : year of the end of the copyright time window
def prepare_eclipse_bundle_files(args : dict, current_dir : str, module_path : str, module : dict, changed_filenames : list, current_devel_version : str, next_stable_version : str, next_devel_version : str, end_copyright_year : str):
	if args.copyrights or args.releaseversion or args.develversion:
		files = build_eclipse_file_list(module_path)
		for filename in files:
			if args.copyrights:
				show_update_file_msg(filename, changed_filenames)
				replace_copyrights(filename, end_copyright_year)
			if args.releaseversion:
				show_update_file_msg(filename, changed_filenames)
				move_to_release_version_in_eclipse(current_devel_version, next_stable_version, filename)
			if args.develversion:
				show_update_file_msg(filename, changed_filenames)
				move_to_devel_version_in_eclipse(current_devel_version, next_devel_version, filename)

#########################
## args : command-line arguments
## current_dir : folder of this script
## module_path : path to the root folder of the module
## module : description of the current project module
## changed_filenames : list of files that is updated with the names of the updated files
## mvn_current_devel_version: the current devel version number in the eclipse files with the Maven format
## eclipse_current_devel_version: the current devel version number in the eclipse files with the Maven format
## next_stable_version: the next stable version number in the eclipse files
## mvn_next_devel_version: the next development version number in the eclipse files with the Maven format
## eclipse_next_devel_version: the next development version number in the eclipse files with the Eclipse format
def prepare_eclipse_platform_files(args : dict, current_dir : str, module_path : str, module : dict, changed_filenames : list, mvn_current_devel_version : str, eclipse_current_devel_version : str, next_stable_version : str, mvn_next_devel_version : str, eclipse_next_devel_version : str):
	if args.releaseversion or args.develversion:
		files = build_eclipse_target_platform_list(module_path)
		for filename in files:
			if args.releaseversion:
				show_update_file_msg(filename, changed_filenames)
				move_to_release_version_in_eclipse(eclipse_current_devel_version, next_stable_version, filename)
				move_to_release_version_in_maven(mvn_current_devel_version, next_stable_version, filename)
			if args.develversion:
				show_update_file_msg(filename, changed_filenames)
				move_to_devel_version_in_eclipse(eclipse_current_devel_version, eclipse_next_devel_version, filename)
				move_to_devel_version_in_maven(mvn_current_devel_version, mvn_next_devel_version, filename)

#########################
## args : command-line arguments
## current_dir : folder of this script
## module_path : path to the root folder of the module
## module : description of the current project module
## changed_filenames : list of files that is updated with the names of the updated files
## current_stable_version: the version number of the current stable/development in the what's new xml file
## current_devel_version: the version number of the current stable/development in the what's new xml file
## next_stable_version: the next stable version number
## next_devel_version: the next development version number
def prepare_whats_new_files(args : dict, current_dir : str, module_path : str, module : dict, changed_filenames : list, current_stable_version : str, current_devel_version : str, next_stable_version : str, next_devel_version : str):
	files = build_whats_new_xml_file_list(module_path)
	for filename in files:
		if args.releaseversion:
			show_update_file_msg(filename, changed_filenames)
			move_to_release_version_in_whats_new(current_stable_version, current_devel_version, next_stable_version, filename)
		if args.develversion:
			show_update_file_msg(filename, changed_filenames)
			move_to_release_version_in_whats_new(current_stable_version, current_devel_version, next_devel_version, filename)

#########################
## args : command-line arguments
## current_dir : folder of this script
## current_processus_dir : current folder of this process
## module : description of the current project module
## changed_filenames : list of files that is updated with the names of the updated files
def prepare_release(args : dict, current_dir : str, current_processus_dir : str, module : dict, changed_filenames : list):
	if not args.changes:
		header("")
		header("------------------------------------------------------------------------")
		header("Preparing the module: " + module['name'])
		header("------------------------------------------------------------------------")
		header("")
	module_path = os.path.realpath(os.path.join(current_dir, '..', module['module']))

	os.chdir(module_path)

	info("Reading POM file")
	mvn_root_filename = os.path.join(module_path, 'pom.xml')
	mvn_root = read_xml_root_node(mvn_root_filename)
	if  mvn_root is None:
		error("Cannot find pom.xml for the module " + module['name'])
		sys.exit(255)
		
	generation_date = str(datetime.now().strftime("%Y%m%d-%H%M%S"))

	contributors = get_maven_contributors(mvn_root)
	mvn_version_number = read_maven_version(mvn_root)
	eclipse_version_number = mvn_version_number.replace("-SNAPSHOT", ".qualifier")
	
	current_devel_version = mvn_version_number.replace("-SNAPSHOT", "")
	current_stable_version = build_current_stable_version(current_devel_version, args.currentstable)
	next_stable_version = build_next_stable_version(mvn_version_number, args.nextstable)
	next_devel_version = build_next_devel_version(mvn_version_number, args.nextdevel)
	mvn_next_devel_version = next_devel_version + "-SNAPSHOT"
	eclipse_next_devel_version = next_devel_version + ".qualifier"

	if args.endcopyright:
		end_copyright_year = args.endcopyright
	else:
		end_copyright_year = str(datetime.now().strftime("%Y"))

	show_informations(args, current_stable_version, not args.currentstable, mvn_version_number, next_stable_version, mvn_next_devel_version, end_copyright_year)

	if not args.test:
		if args.changes:
			generate_changes(args, current_dir, current_processus_dir, current_stable_version, args.changelogfile)
			# Don't loop on modules
			sys.exit(0)
		else:
			prepare_readme_files(args, current_dir, module_path, module, changed_filenames, current_stable_version, next_stable_version, mvn_version_number, mvn_next_devel_version)
			prepare_java_sarl_files(args, current_dir, module_path, module, changed_filenames, generation_date, contributors, end_copyright_year)
			prepare_maven_pom_files(args, current_dir, module_path, module, changed_filenames, mvn_version_number, mvn_next_devel_version)
			prepare_eclipse_bundle_files(args, current_dir, module_path, module, changed_filenames, eclipse_version_number, next_stable_version, eclipse_next_devel_version, end_copyright_year)
			prepare_eclipse_platform_files(args, current_dir, module_path, module, changed_filenames, mvn_version_number, eclipse_version_number, next_stable_version, mvn_next_devel_version, eclipse_next_devel_version)
			prepare_whats_new_files(args, current_dir, module_path, module, changed_filenames, current_stable_version, mvn_version_number, next_stable_version, mvn_next_devel_version)

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

#########################
##
parser = argparse.ArgumentParser()
parser.add_argument("--modules", help="path to the JSON file defining the modules", action="store")
parser.add_argument("--ignore", help="add a module in the list of modules to be ignored", action="append")

group = parser.add_mutually_exclusive_group(required=True)
group.add_argument("--mlist", help="list the defined modules", action="store_true")
group.add_argument('--changes', help="Generate the changelog", action="store_true")
group.add_argument('--author', help="replace $Author: id$", action="store_true")
group.add_argument('--maven', help="replace $GroupId$ and $ArtifactId$", action="store_true")
group.add_argument('--copyrights', help="replace the copyright strings", action="store_true")
group.add_argument('--releaseversion', help="move to the next release version", action="store_true")
group.add_argument('--develversion', help="move to the next devel version", action="store_true")

parser.add_argument('--test', help="test cli configuration", action="store_true")
parser.add_argument('--currentstable', help="current stable version number without -SNAPSHOT and .qualifier", type=str)
parser.add_argument('--nextstable', help="next stable version number without -SNAPSHOT and .qualifier", type=str)
parser.add_argument('--nextdevel', help="next development version number without -SNAPSHOT and .qualifier", type=str)
parser.add_argument('--changelogfile', help="path to the changelog file. Default is target/changelog.md", type=str)
parser.add_argument('--endcopyright', help="Year to be considered as the end of copyright (default is this current year).", type=int)
args = parser.parse_args()

current_dir = os.path.dirname(os.path.realpath(__file__))
current_processus_dir  = os.getcwd()

# Modules
module_configuration = read_module_configuration(args, current_dir)

changed_filenames = []

# Generate bundles
for module in module_configuration['without-extension']:
	if module['releasable']:
		prepare_release(args, current_dir, current_processus_dir, module, changed_filenames)

for module in module_configuration['extensions']:
	if module['releasable']:
		prepare_release(args, current_dir, current_processus_dir, module, changed_filenames)

for module in module_configuration['with-extension']:
	if module['releasable']:
		prepare_release(args, current_dir, current_processus_dir, module, changed_filenames)

success("Success on the release preparation")

#info(str(changed_filenames))

sys.exit(0)

