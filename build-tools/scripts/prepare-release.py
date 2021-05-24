#!/usr/bin/env python

import argparse
import re
import os
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
## source: text to replace in the regex
## contributors: the hash map of contributors
def doAuthorReplacement(source, contributors):
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
	raise Exception("Unable to find the contributor for: " + str(source.group()))

#########################
## filename: the filename to update
## contributors: the hash map of contributors
def replaceAuthors(filename, contributors):
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = re.sub("\$Author[ \t]*\:[ \t]*([^$]+)\$", lambda x : doAuthorReplacement(x, contributors), line)
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
def replaceMaven(filename, generation_date):
	pomFolder = detectEnclosingProjectPom(filename)
	if not pomFolder:
		raise Exception("Cannot detect POM file for project")
	pom = readXMLRootNode(os.path.join(pomFolder, 'pom.xml'))
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
	full_version = str(version) + " " + generation_date
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = re.sub("\$GroupId\$", groupId, line)
		line2 = re.sub("\$ArtifactId\$", artifactId, line2)
		line2 = re.sub("\$Version\$", version, line2)
		line2 = re.sub("\$FullVersion\$", full_version, line2)
		line2 = re.sub("\$Date\$", generation_date, line2)
		line2 = re.sub("\$Filename\$", os.path.basename(filename), line2)
		line2 = re.sub("\$Filename\$", os.path.basename(filename), line2)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
## source: text to replace
## this_year: the current year
def doCopyrightReplacement0(source, this_year):
	prefix = source.group(1)
	return prefix + str(this_year) + " "

#########################
## source: text to replace
## this_year: the current year
def doCopyrightReplacement1(source, this_year):
	prefix = source.group(1)
	return prefix + "-" + str(this_year) + " "

#########################
##
def replaceCopyrights(filename):
	this_year = str(datetime.now().strftime("%Y"))
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = re.sub("(Copyright (?:\(C\) )?[0-9]+-)[0-9]+ ",
			lambda x : doCopyrightReplacement0(x, this_year), line)
		line2 = re.sub("(Copyright (?:\(C\) )?[0-9]+) ",
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
## version: the version number in the pom xml file
## filename: the filename to change
def moveToReleaseVersionInMaven(version, filename):
	if version.endswith("-SNAPSHOT"):
		release_version = version[0:-9]
		with open (filename, "r") as myfile:
			data = myfile.readlines()
		data2 = []
		for line in data:
			line2 = line.replace(version, release_version)
			data2.append(line2)
		with open (filename, "w") as myfile:
			myfile.write("".join(data2))

#########################
## version: the version number in the eclipse files
## filename: the filename to change
def moveToReleaseVersionInEclipse(version, filename):
	if version.endswith(".qualifier"):
		release_version = version[0:-10]
		with open (filename, "r") as myfile:
			data = myfile.readlines()
		data2 = []
		for line in data:
			line2 = line.replace(version, release_version)
			data2.append(line2)
		with open (filename, "w") as myfile:
			myfile.write("".join(data2))

#########################
## current_version: the current version number in the pom xml file
## devel_version: the devel version number (without -SNAPSHOT) in the pom xml file
## filename: the filename to change
def moveToDevelVersionInMaven(current_version, devel_version, filename):
	f_devel_version = devel_version + "-SNAPSHOT"
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = line.replace(current_version, f_devel_version)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
## current_version: the current version number in the pom xml file
## devel_version: the devel version number (without .qualifier) in the pom xml file
## filename: the filename to change
def moveToDevelVersionInEclipse(current_version, devel_version, filename):
	f_devel_version = devel_version + ".qualifier"
	with open (filename, "r") as myfile:
		data = myfile.readlines()
	data2 = []
	for line in data:
		line2 = line.replace(current_version, f_devel_version)
		data2.append(line2)
	with open (filename, "w") as myfile:
		myfile.write("".join(data2))

#########################
##
parser = argparse.ArgumentParser()
parser.add_argument('pom', help="path to the root Maven pom file")
group = parser.add_mutually_exclusive_group()
group.add_argument('--author', help="replace $Author: id$", action="store_true")
group.add_argument('--maven', help="replace $GroupId$ and $ArtifactId$", action="store_true")
group.add_argument('--copyrights', help="replace the copyright strings", action="store_true")
group.add_argument('--releaseversion', help="move to the next release version", action="store_true")
group.add_argument('--develversion', help="move to the next devel version, specified as argument (without -SNAPSHOT and .qualifier)", type=str)
args = parser.parse_args()

mvn_root = readXMLRootNode(args.pom)
if mvn_root is not None:
	generation_date = str(datetime.now().strftime("%d/%m/%Y %H:%M:%S"))
	contributors = getMavenContributors(mvn_root)

	# Java and SARL files
	files = buildCodeFileList()
	for filename in files:
		print("Updating " + filename)
		if args.author:
			replaceAuthors(filename, contributors)
		if args.maven:
			replaceMaven(filename, generation_date)
		if args.copyrights:
			replaceCopyrights(filename)

	# Maven pom.xml
	files = buildMavenFileList()
	mvn_version_number = readMavenVersion(mvn_root)
	for filename in files:
		print("Updating " + filename)
		if args.releaseversion:
			moveToReleaseVersionInMaven(mvn_version_number, filename)
		if args.develversion:
			moveToDevelVersionInMaven(mvn_version_number, args.develversion, filename)

	# Eclipse plugins and features
	files = buildEclipseFileList()
	eclipse_version_number = mvn_version_number.replace("-SNAPSHOT", ".qualifier")
	for filename in files:
		print("Updating " + filename)
		if args.copyrights:
			replaceCopyrights(filename)
		if args.releaseversion:
			moveToReleaseVersionInEclipse(eclipse_version_number, filename)
		if args.develversion:
			moveToDevelVersionInEclipse(eclipse_version_number, args.develversion, filename)

