#!/usr/bin/env python3

import os
import argparse
import sys
import yaml
from xml.etree import ElementTree
from javaproperties import Properties

NAMESPACE = 'http://maven.apache.org/POM/4.0.0'
NAMESPACES = {'xmlns' : NAMESPACE}

##########################################
## node: Output a pretty string version for the given node
def dumpXML(node):
	ElementTree.indent(node)
	return ElementTree.tostring(node, encoding="unicode")

##########################################
## version_mapping: an associative array that maps a plugin id (groupid:artifactid) to its version
## filename: the name of the file in which the mapping is defined (JSON format), with a map for group ids, and inside each a map for artifact ids
## properties: the mapping from a property name to its value
def readVersionMappings(version_mapping, filename, properties):
	with open(filename, 'r') as input_file:
		input_data = yaml.safe_load(input_file)
	for groupId in input_data:
		for artifactId in input_data[groupId]:
			pversion = input_data[groupId][artifactId]
			if pversion:
				if pversion.startswith('$'):
					key = pversion[1:]
					if key not in properties:
						print('Error: no definition for the property $' + key, file=sys.stderr)
						sys.exit(255)
					version_mapping[groupId + ":" + artifactId] = properties[key]
				else:
					version_mapping[groupId + ":" + artifactId] = pversion

##########################################
##
def buildMavenFileList():
	fileList = []
	for root, dirs, files in os.walk("."):
		for filename in files:
			if filename == "pom.xml":
				fullpath = os.path.join(root, filename)
				if '/target/' not in fullpath:
					fileList.append(fullpath)
	return fileList

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
def readXmlNode(node, name):
	return node.find("xmlns:" + name, namespaces=NAMESPACES)

#########################
## node: the XML node
## name: the name of the value to read.
def readXml(node, name):
	valueNode = readXmlNode(node, name)
	if valueNode is not None:
		textValue = valueNode.text
		if textValue:
			return textValue
	return ''

#########################
## root: the XML root node
## version_mapping: the mapping from the plugin id to the plugin version.
## ignores: the list of keys to ignore for replacement
## changed: indicates if the XML was already changed before the call to this function.
## path: the xpath to the node that contains "groupId", "artifactId", and "version" tags
## returns: True if a node has changed, False otherwise
def replaceInNode(root, version_mapping, ignores, changed, path):
	nodes = root.findall(path, namespaces=NAMESPACES)
	changed_now = False
	if nodes:
		for node in nodes:
			groupId = readXml(node, "groupId")
			artifactId = readXml(node, "artifactId")
			versionNode = readXmlNode(node, "version")
			if groupId and artifactId and versionNode is not None:
				version = versionNode.text.strip()
				if not version.startswith('$'):
					pluginKey = groupId + ":" + artifactId
					if pluginKey not in version_mapping:
						if not ignores or pluginKey not in ignores:
							print('Error: no definition for ' + pluginKey + ": current version: " + version, file=sys.stderr)
							sys.exit(255)
					else:
						newVersion = version_mapping[pluginKey]
						if newVersion != version:
							versionNode.text = newVersion
							changed_now = True
	return changed_now or changed

#########################
## root: the XML root node
## version_mapping: the mapping from the plugin id to the plugin version.
## ignores: the list of keys to ignore for replacement
## returns: True if a node has changed, False otherwise
def replaceInXML(root, version_mapping, ignores):
	changed = False
	changed = replaceInNode(root, version_mapping, ignores, changed,
		'./xmlns:build/xmlns:extensions/xmlns:extension')
	changed = replaceInNode(root, version_mapping, ignores, changed,
		'./xmlns:build/xmlns:plugins/xmlns:plugin')
	changed = replaceInNode(root, version_mapping, ignores, changed,
		'./xmlns:build/xmlns:plugins/xmlns:plugin/xmlns:dependencies/xmlns:dependency')
	changed = replaceInNode(root, version_mapping, ignores, changed,
		'./xmlns:build/xmlns:pluginManagement/xmlns:plugins/xmlns:plugin')
	changed = replaceInNode(root, version_mapping, ignores, changed,
		'./xmlns:build/xmlns:pluginManagement/xmlns:plugins/xmlns:plugin/xmlns:dependencies/xmlns:dependency')
	changed = replaceInNode(root, version_mapping, ignores, changed,
		'./xmlns:profiles/xmlns:profile/xmlns:build/xmlns:plugins/xmlns:plugin')
	changed = replaceInNode(root, version_mapping, ignores, changed,
		'./xmlns:dependencies/xmlns:dependency')
	changed = replaceInNode(root, version_mapping, ignores, changed,
		'./xmlns:project/xmlns:dependencyManagement/xmlns:dependencies/xmlns:dependency')
	return changed

##############################
##
parser = argparse.ArgumentParser(description="Update Maven plugin versions")
parser.add_argument('args', nargs=argparse.REMAINDER, action="append")
parser.add_argument('--properties', help="path to the property file", action='append')
parser.add_argument('--ignore', help="groupId:artifactId to ignore", action='append')
args = parser.parse_args()

properties = Properties()
if args.properties:
	for prop_file in args.properties:
		with open(str(prop_file), 'rb') as input_file:
			properties.load(input_file)

version_mapping = {};
for arg in args.args:
	readVersionMappings(version_mapping, arg[0], properties)

pom_files = buildMavenFileList();

for pom_file in pom_files:
	print("Scanning " + pom_file)
	xml_root = readXMLRootNode(pom_file);
	changed = replaceInXML(xml_root, version_mapping, args.ignore)
	if changed:
		print("Saving " + pom_file)
		with open(pom_file, 'wt') as output_file:
			output_file.write(dumpXML(xml_root))

sys.exit(0)

