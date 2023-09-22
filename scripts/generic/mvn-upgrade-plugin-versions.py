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
				pversion = str(pversion)
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
## namespace: the namespace for the nodes, default is xmlns:
def readXmlNode(node, name, namespace='xmlns:'):
	return node.find(namespace + name, namespaces=NAMESPACES)

#########################
## node: the XML node
## name: the name of the value to read.
## namespace: the namespace for the nodes, default is xmlns:
def readXml(node, name, namespace='xmlns:'):
	valueNode = readXmlNode(node, name, namespace)
	if valueNode is not None:
		textValue = valueNode.text
		if textValue:
			return textValue
	return ''

#########################
## path: the nae to be fixed
## namepace: the namespace to add
## returns: the fixed path
def fixPath(path, namespace):
	return path.replace('/', '/' + namespace)

#########################
## root: the XML root node
## version_mapping: the mapping from the plugin id to the plugin version.
## ignores: the list of keys to ignore for replacement
## changed: indicates if the XML was already changed before the call to this function.
## path: the xpath to the node that contains "groupId", "artifactId", and "version" tags
## namespace: the namespace prefix
## returns: True if a node has changed, False otherwise
def replacePomNodes(root, version_mapping, ignores, changed, path, namespace="xmlns:"):
	if namespace:
		fixed_path = fixPath(path, namespace)
	else:
		fixed_path = path
	nodes = root.findall(fixed_path, namespaces=NAMESPACES)
	changed_now = False
	if nodes:
		for node in nodes:
			groupId = readXml(node, "groupId", namespace)
			artifactId = readXml(node, "artifactId", namespace)
			versionNode = readXmlNode(node, "version", namespace)
			#print(dumpXML(node))
			#print("groupId=" + groupId)
			#print("artigactId=" + artifactId)
			#print(dumpXML(versionNode))
			#raise Exception("DBG")
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
							print("\t" + pluginKey + ": " + version + " -> " + newVersion)
							versionNode.text = newVersion
							changed_now = True
	return changed_now or changed

#########################
## root: the XML root node
## version_mapping: the mapping from the plugin id to the plugin version.
## ignores: the list of keys to ignore for replacement
## enableDependencies: enables or disables the replacements of Maven dependencies
## returns: True if a node has changed, False otherwise
def replaceInPom(root, version_mapping, ignores, enableDependencies):
	changed = False
	changed = replacePomNodes(root, version_mapping, ignores, changed,
		'./build/extensions/extension')
	changed = replacePomNodes(root, version_mapping, ignores, changed,
		'./build/plugins/plugin')
	changed = replacePomNodes(root, version_mapping, ignores, changed,
		'./build/plugins/plugin/dependencies/dependency')
	changed = replacePomNodes(root, version_mapping, ignores, changed,
		'./build/pluginManagement/plugins/plugin')
	changed = replacePomNodes(root, version_mapping, ignores, changed,
		'./build/pluginManagement/plugins/plugin/dependencies/dependency')
	changed = replacePomNodes(root, version_mapping, ignores, changed,
		'./profiles/profile/build/plugins/plugin')
	if enableDependencies:
		changed = replacePomNodes(root, version_mapping, ignores, changed,
			'./dependencies/dependency')
		changed = replacePomNodes(root, version_mapping, ignores, changed,
			'./dependencyManagement/dependencies/dependency')
	return changed

#########################
## root: the XML root node
## version_mapping: the mapping from the plugin id to the plugin version.
## ignores: the list of keys to ignore for replacement
## returns: True if a node has changed, False otherwise
def replaceInEclipsePlatform(root, version_mapping, ignores):
	changed = False
	changed = replacePomNodes(root, version_mapping, ignores, changed,
		'./locations/location/dependencies/dependency', '')
	return changed


##############################
##
parser = argparse.ArgumentParser(description="Update Maven plugin versions")
parser.add_argument('args', nargs=argparse.REMAINDER, action="append")
parser.add_argument('--properties', help="path to the property file", action='append')
parser.add_argument('--ignore', help="groupId:artifactId to ignore", action='append')
parser.add_argument('--nodependency', help="ignore Maven dependencies", action='store_true')
parser.add_argument('--eclipseplatform', help="path to the Eclipse platform file in which the Maven versions must be updated", action='append')
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
	changed = replaceInPom(xml_root, version_mapping, args.ignore, (not args.nodependency))
	if changed:
		print("Saving " + pom_file)
		with open(pom_file, 'wt') as output_file:
			output_file.write(dumpXML(xml_root))

if args.eclipseplatform:
	for platform_file in args.eclipseplatform:
		print("Scanning " + platform_file)
		xml_root = readXMLRootNode(platform_file);
		changed = replaceInEclipsePlatform(xml_root, version_mapping, args.ignore)
		if changed:
			print("Saving " + platform_file)
			with open(platform_file, 'wt') as output_file:
				output_file.write("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n")
				output_file.write("<?pde version=\"3.8\"?>\n")
				output_file.write(dumpXML(xml_root))

sys.exit(0)

