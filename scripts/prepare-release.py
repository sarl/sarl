#!/usr/bin/env python3

import argparse
import re
import os
import subprocess
import sys
from pathlib import Path
from xml.etree import ElementTree
from datetime import datetime

NAMESPACE = 'http://maven.apache.org/POM/4.0.0'
NAMESPACES = {'xmlns' : NAMESPACE}
	
#########################
## filename: the path to the POM file.
def readXMLRootNode(filename):
	ElementTree.register_namespace('', NAMESPACE)
	tree = ElementTree.parse(filename)
	root = tree.getroot()
	return root

#########################
## name: the name of the person for which the id should be built
def buildIdFromName(name):
	elements = re.split("[ \n\r\t\-_]+", name)
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
def readXml(node, name):
	valueNode = node.find("xmlns:" + name, namespaces=NAMESPACES)
	if valueNode is not None:
		textValue = valueNode.text
		if textValue:
			return textValue
	return ''

#########################
## root: the Maven POM root node.
def getMavenContributors(root):
	contributors = {}
	contribs = root.findall(".//xmlns:contributor", namespaces=NAMESPACES)
	for contrib in contribs:
		name = readXml(contrib, "name")
		if name is not None:
			contrib_id = buildIdFromName(name)
			if contrib_id:
				contributors[contrib_id] = {}
				contributors[contrib_id]['id'] = contrib_id
				contributors[contrib_id]['name'] = name
				email = readXml(contrib, "xml")
				if email:
					contributors[contrib_id]['email'] = email
				url = readXml(contrib, "url")
				if url:
					contributors[contrib_id]['url'] = url
	authors = root.findall(".//xmlns:developer", namespaces=NAMESPACES)
	for author in authors:
		name = readXml(author, "name")
		if name is not None:
			author_id =  readXml(author, "id")
			if author_id is None:
				author_id = buildIdFromName(name)
			if author_id:
				contributors[author_id] = {}
				contributors[author_id]['id'] = author_id
				contributors[author_id]['name'] = name
				email = readXml(author, "xml")
				if email:
					contributors[author_id]['email'] = email
				url = readXml(author, "url")
				if url:
					contributors[author_id]['url'] = url
	return contributors

#########################
##
def buildCodeFileList():
	fileList = []
	for root, dirs, files in os.walk("."):
		for filename in files:
			if filename.endswith(".java") or filename.endswith(".sarl"):
				fileList.append(os.path.join(root, filename))
	return fileList

#########################
##
def buildMavenFileList():
	fileList = []
	for root, dirs, files in os.walk("."):
		for filename in files:
			if filename == "pom.xml":
				fileList.append(os.path.join(root, filename))
	return fileList
			
#########################
##
def buildEclipseFileList():
	fileList = []
	for root, dirs, files in os.walk("."):
		for filename in files:
			if filename == "MANIFEST.MF" or filename == "feature.xml" or filename == "category.xml" or filename.endswith(".product"):
				fileList.append(os.path.join(root, filename))
	return fileList

#########################
## enableParent: indicates if the files in the parent folder must be updated also
def buildReadmeFileList(enableParent):
	candidates = [
		'README',
		'CONTRIBUTING',
		'RELEASE_README'
		]
	if enableParent:
		candidates = candidates + [
			os.path.join('..', 'README'),
			os.path.join('..', 'CONTRIBUTING'),
			os.path.join('..', 'RELEASE_README')
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
def doAuthorReplacement(source, contributors, filename):
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
	raise Exception("In " + str(filename) + ", unable to find the contributor for: " + str(source.group()) + "\nKnown contributors are: " + str(contributors))

#########################
## filename: the filename to update
## contributors: the hash map of contributors
def replaceAuthors(filename, contributors):
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = re.sub("\$Author[ \t]*\:[ \t]*([^$]+)\$", lambda x : doAuthorReplacement(x, contributors, filename), line)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
## filename: the filename to start from
def detectEnclosingProjectPom(filename):
	folder = os.path.dirname(filename)
	while not os.path.exists(os.path.join(folder, 'pom.xml')):
		folder = os.path.dirname(folder)
	return folder

#########################
## filename: the filename to update
## generation_date: date of the generation
## exclusions: list of pom files that are not compatible
def replaceMaven(filename, generation_date, exclusions):
	pomFolder = detectEnclosingProjectPom(filename)
	if not pomFolder:
		raise Exception("Cannot detect POM file for project")
	pomFile = os.path.join(pomFolder, 'pom.xml')
	if pomFile in exclusions:
		print("[INFO] Skipping " + filename)
	else:
		try:
			pom = readXMLRootNode(pomFile)
			artifactId = readXml(pom, 'artifactId')
			groupId = readXml(pom, 'groupId')
			if not groupId:
				parent = pom.find("xmlns:parent", namespaces=NAMESPACES)
				groupId = readXml(parent, 'groupId')
			version = readXml(pom, 'version')
			if not version:
				parent = pom.find("xmlns:parent", namespaces=NAMESPACES)
				version = readXml(parent, 'version')
			if not groupId:
				raise Exception("Unable to determine the Maven group id for: " + filename)
			if not artifactId:
				raise Exception("Unable to determine the Maven artifact id for: " + filename)
			if not version:
				raise Exception("Unable to determine the Maven artifact version for: " + filename)
			full_version = str(artifactId) + " " + str(version) + " " + str(generation_date)
			with open (filename, "r") as myfile:
				data = myfile.readlines()
			data2 = []
			for line in data:
				line2 = re.sub("\$GroupId\$", groupId, line)
				line2 = re.sub("\$ArtifactId\$", artifactId, line2)
				line2 = re.sub("\$Name\$", artifactId, line2)
				line2 = re.sub("\$Version\$", version, line2)
				line2 = re.sub("\$Revision\$", version, line2)
				line2 = re.sub("\$Date\$", generation_date, line2)
				line2 = re.sub("\$FullVersion\$", full_version, line2)
				line2 = re.sub("\$Filename\$", os.path.basename(filename), line2)
				data2.append(line2)
			with open (filename, "w") as myfile:
				myfile.write("".join(data2))
		except Exception as ex:
			exclusions.append(pomFile)
			print >> sys.stderr, "[ERROR] " + pomFile + ": " + filename + ": " + str(ex)

#########################
## source: text to replace
## this_year: the current year
def doCopyrightReplacement0(source, this_year):
	prefix = source.group(1)
	return prefix + str(this_year) + " SARL.io, the Original Authors and Main Authors"

#########################
## source: text to replace
## this_year: the current year
def doCopyrightReplacement1(source, this_year):
	prefix = source.group(1)
	return prefix + "-" + str(this_year) + " SARL.io, the Original Authors and Main Authors"

#########################
## filename: the name of the file to be updated
def replaceCopyrights(filename):
	this_year = str(datetime.now().strftime("%Y"))
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	names = [ "SARL.io, the Original Authors and Main Authors",
		  "the original authors or authors"
		]
	for line in data:
		for name in names:
			line2 = re.sub("(Copyright (?:\(C\) )?[0-9]+-)[0-9]+ " + name,
				lambda x : doCopyrightReplacement0(x, this_year), line)
			line2 = re.sub("(Copyright (?:\(C\) )?[0-9]+) " + name,
				lambda x : doCopyrightReplacement1(x, this_year), line2)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
## root: the root node of the root POM xml file
def readMavenVersion(root):
	parent_node = root.find("xmlns:parent", namespaces=NAMESPACES)
	version = readXml(root, 'version')
	if not version and parent_node is not None:
		version = readXml(parent_node, 'version')
	if not version:
		raise Exception("Unable to determine the Maven artifact version for: " + filename)
	return version

#########################
## current_devel_version: the version number of the current devel in the pom xml file
## next_stable_version: the version number of the next stable in the pom xml file
## filename: the filename to change
def moveToReleaseVersionInMaven(current_devel_version, next_stable_version, filename):
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
def moveToReleaseVersionInEclipse(current_devel_version, next_stable_version, filename):
	if current_devel_version.endswith(".qualifier"):
		with open (filename, "r") as myfile:
			data = myfile.readlines()
		data2 = []
		for line in data:
			line2 = line.replace(current_devel_version, next_stable_version)
			data2.append(line2)
		with open (filename, "w") as myfile:
			myfile.write("".join(data2))

#########################
## current_stable_version: the version number of the current stable in the pom xml file
## next_stable_version: the version number of the next stable in the pom xml file
## current_devel_version: the version number of the current snapshot in the pom xml file
## next_devel_version: the version number of the next snapshot in the pom xml file
## filename: the filename to change
def moveToReleaseVersionInReadme(current_stable_version, next_stable_version, current_devel_version, next_devel_version, filename):
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
def moveToDevelVersionInMaven(current_devel_version, next_devel_version, filename):
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
def moveToDevelVersionInEclipse(current_devel_version, next_devel_version, filename):
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = line.replace(current_devel_version, next_devel_version)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
## filename: the filename to be updated
## filenames: the list of filenames that should be marked as updated
def show_update_file_msg(filename, filenames):
	if filename not in filenames:
		filenames.append(filename)
		print("Updating " + filename)

#########################
## current_version: the current version (snapshot)
## previous_stable: the previous stable version (not snapshot)
def buildCurrentStableVersion(current_version, previous_stable):
	if previous_stable:
		return previous_stable
	elements = current_version.split(".")
	if int(elements[1]) <= 0:
		return str(int(elements[0]) - 1) + ".0.0"
	return str(elements[0]) + "." + str(int(elements[1]) - 1) + ".0"

#########################
## current_version: the current version (snapshot)
## next_stable: the next stable version (not snapshot)
def buildNextStableVersion(current_version, next_stable):
	if next_stable:
		return next_stable
	return current_version.replace("-SNAPSHOT", "")

#########################
## current_version: the current version (snapshot)
## next_devel: the next devel version (snapshot)
def buildNextDevelVersion(current_version, next_devel):
	if next_devel:
		return next_devel
	elements = current_version.split(".")
	return str(elements[0]) + "." + str(int(elements[1]) + 1) + ".0"

#########################
## current_stable_version: the version of the current stable release
## changelog_file: the path to the changelog file to be written. If not specified, defualt is target/changelog.md
def generateChanges(current_stable_version, changelog_file):
	gitoutput = subprocess.check_output(['git-changelog', '-p', '-x', '-s', "v" + str(current_stable_version)])
	if changelog_file:
		output_filename = changelog_file
	else:
		output_filename = 'target/changelog.md'
	Path(os.path.dirname(output_filename)).mkdir(parents=True, exist_ok=True)
	with open (output_filename, "w") as myfile:
		myfile.write(gitoutput.decode("utf-8"))

#########################
##
parser = argparse.ArgumentParser()
parser.add_argument('pom', help="path to the root Maven pom file")
parser.add_argument('--test', help="test cli configuration", action="store_true")

group = parser.add_mutually_exclusive_group(required=True)
group.add_argument('--changes', help="Generate the changelog", action="store_true")
group.add_argument('--author', help="replace $Author: id$", action="store_true")
group.add_argument('--maven', help="replace $GroupId$ and $ArtifactId$", action="store_true")
group.add_argument('--copyrights', help="replace the copyright strings", action="store_true")
group.add_argument('--releaseversion', help="move to the next release version", action="store_true")
group.add_argument('--develversion', help="move to the next devel version", action="store_true")

parser.add_argument('--noparentreadme', help="Disable the update of the README files in the parent folder of the given POM file", action="store_true")
parser.add_argument('--currentstable', help="current stable version number without -SNAPSHOT and .qualifier", type=str)
parser.add_argument('--nextstable', help="next stable version number without -SNAPSHOT and .qualifier", type=str)
parser.add_argument('--nextdevel', help="next development version number without -SNAPSHOT and .qualifier", type=str)
parser.add_argument('--changelogfile', help="path to the changelog file. Default is target/changelog.md", type=str)
args = parser.parse_args()

mvn_root = readXMLRootNode(str(args.pom))

if mvn_root is not None:
	generation_date = str(datetime.now().strftime("%Y%m%d-%H%M%S"))

	changed_filenames = []

	contributors = getMavenContributors(mvn_root)
	mvn_version_number = readMavenVersion(mvn_root)
	eclipse_version_number = mvn_version_number.replace("-SNAPSHOT", ".qualifier")
	
	current_stable_version = buildCurrentStableVersion(mvn_version_number, args.currentstable)
	next_stable_version = buildNextStableVersion(mvn_version_number, args.nextstable)
	next_devel_version = buildNextDevelVersion(mvn_version_number, args.nextdevel)
	mvn_next_devel_version = next_devel_version + "-SNAPSHOT"
	eclipse_next_devel_version = next_devel_version + ".qualifier"

	print("> Current stable version: " + current_stable_version)
	print("> Current devel version: " + mvn_version_number)
	print("> Next stable version: " + next_stable_version)
	print("> Next devel version: " + mvn_next_devel_version)
	if args.changes:
		print("> Action: generate the changelog")
	elif args.author:
		print("> Action: update Maven author tags")
	elif args.maven:
		print("> Action: update Maven general tags")
	elif args.copyrights:
		print("> Action: update the copyright text")
	elif args.releaseversion:
		print("> Action: move to the release version " + next_stable_version)
	elif args.develversion:
		print("> Action: move to the development version " + next_devel_version)
	else:
		print("> Action: unknow")

	if args.test:
		sys.exit(0)

	if args.changes:
		generateChanges(current_stable_version, args.changelogfile)
	else:
		# README
		files = buildReadmeFileList(not args.noparentreadme)
		for filename in files:
			if args.releaseversion:
				show_update_file_msg(filename, changed_filenames)
				moveToReleaseVersionInReadme(current_stable_version, next_stable_version, mvn_version_number, mvn_next_devel_version, filename)
			
		# Java and SARL files
		files = buildCodeFileList()
		exclusions = []
		for filename in files:
			if args.author:
				show_update_file_msg(filename, changed_filenames)
				replaceAuthors(filename, contributors)
			if args.maven:
				show_update_file_msg(filename, changed_filenames)
				replaceMaven(filename, generation_date, exclusions)
			if args.copyrights:
				show_update_file_msg(filename, changed_filenames)
				replaceCopyrights(filename)

		# Maven pom.xml
		files = buildMavenFileList()
		for filename in files:
			if args.releaseversion:
				show_update_file_msg(filename, changed_filenames)
				moveToReleaseVersionInMaven(mvn_version_number, next_stable_version, filename)
			if args.develversion:
				show_update_file_msg(filename, changed_filenames)
				moveToDevelVersionInMaven(mvn_version_number, mvn_next_devel_version, filename)

		# Eclipse plugins and features
		files = buildEclipseFileList()
		for filename in files:
			if args.copyrights:
				show_update_file_msg(filename, changed_filenames)
				replaceCopyrights(filename)
			if args.releaseversion:
				show_update_file_msg(filename, changed_filenames)
				moveToReleaseVersionInEclipse(eclipse_version_number, next_stable_version, filename)
			if args.develversion:
				show_update_file_msg(filename, changed_filenames)
				moveToDevelVersionInEclipse(eclipse_version_number, eclipse_next_devel_version, filename)

	sys.exit(0)

else:
	sys.exit(255)

