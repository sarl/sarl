#!/usr/bin/env python3
#
# Author: Stephane GALLAND <galland@arakhne.org>
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

import os
import argparse
import sys
import yaml
from xml.etree import ElementTree
from javaproperties import Properties

VERSION = '20250916'
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
## message : the message to display
def header(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message : the message to display
def info(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]" + msg, file=sys.stdout)

##########################################
## message : the message to display
def success(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.OKGREEN}{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message : the message to display
def error(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.FAIL}{bcolors.BOLD}ERROR{bcolors.ENDC}]" + msg, file=sys.stderr)

##########################################
## node: Output a pretty string version for the given node
def dump_xml(node : object) -> object:
	ElementTree.indent(node)
	return ElementTree.tostring(node, encoding="unicode")

##########################################
## version_mapping: an associative array that maps a plugin id (groupid:artifactid) to its version
## filename: the name of the file in which the mapping is defined (JSON format), with a map for group ids, and inside each a map for artifact ids
## properties: the mapping from a property name to its value
def read_version_mappings(version_mapping : dict, filename : str, properties : dict):
	with open(filename, 'r') as input_file:
		input_data = yaml.safe_load(input_file)
	if input_data:
		for groupId in input_data:
			for artifactId in input_data[groupId]:
				pversion = input_data[groupId][artifactId]
				if pversion:
					pversion = str(pversion)
					if pversion.startswith('$'):
						key = pversion[1:]
						if key not in properties:
							error('No definition for the property $' + key)
							sys.exit(255)
						version_mapping[groupId + ":" + artifactId] = properties[key]
					else:
						version_mapping[groupId + ":" + artifactId] = pversion

##########################################
## rootpath : path to the root folder in which the files to update are located
def build_maven_file_list(rootpath : str) -> list:
	fileList = []
	for root, dirs, files in os.walk(rootpath):
		for filename in files:
			if filename == "pom.xml":
				fullpath = os.path.join(root, filename)
				if '/target/' not in fullpath:
					fileList.append(fullpath)
	return fileList

##########################################
## pom_files: list of existing pom.xml files
def build_maven_extension_file_list(pom_files : list) -> list:
	fileList = []
	for pom_file in pom_files:
		dirname = os.path.dirname(pom_file)
		fullpath = os.path.join(dirname, ".mvn", "extensions.xml")
		if os.path.exists(fullpath):
			fileList.append(fullpath)
	return fileList

#########################
## filename: the path to the POM file.
def read_xml_root_node(filename : str) -> str:
	ElementTree.register_namespace('', NAMESPACE)
	tree = ElementTree.parse(filename)
	root = tree.getroot()
	return root

#########################
## node: the XML node
## name: the name of the value to read.
## namespace: the namespace for the nodes, default is xmlns:
def read_xml_node(node : object, name : str, namespace : str ='xmlns:') -> object:
	return node.find(namespace + name, namespaces=NAMESPACES)

#########################
## node: the XML node
## name: the name of the value to read.
## namespace: the namespace for the nodes, default is xmlns:
def read_xml(node : object, name : str, namespace : str ='xmlns:') -> str:
	valueNode = read_xml_node(node, name, namespace)
	if valueNode is not None:
		textValue = valueNode.text
		if textValue:
			return textValue
	return ''

#########################
## path: the nae to be fixed
## namepace: the namespace to add
## returns: the fixed path
def fix_path(path : str, namespace : str) -> str:
	return path.replace('/', '/' + namespace)

#########################
## root: the XML root node
## version_mapping: the mapping from the plugin id to the plugin version.
## ignores: the list of keys to ignore for replacement
## changed: indicates if the XML was already changed before the call to this function.
## path: the xpath to the node that contains "groupId", "artifactId", and "version" tags
## namespace: the namespace prefix
## returns: True if a node has changed, False otherwise
def replace_pom_nodes(root : object, version_mapping : dict, ignores : list, changed : bool, path : str, namespace : str ="xmlns:") -> bool:
	if namespace:
		fixed_path = fix_path(path, namespace)
	else:
		fixed_path = path
	nodes = root.findall(fixed_path, namespaces=NAMESPACES)
	changed_now = False
	if nodes:
		for node in nodes:
			groupId = read_xml(node, "groupId", namespace)
			artifactId = read_xml(node, "artifactId", namespace)
			versionNode = read_xml_node(node, "version", namespace)
			#print(dump_xml(node))
			#print("groupId=" + groupId)
			#print("artigactId=" + artifactId)
			#print(dump_xml(versionNode))
			#raise Exception("DBG")
			if groupId and artifactId and versionNode is not None:
				version = versionNode.text.strip()
				if not version.startswith('$'):
					pluginKey = groupId + ":" + artifactId
					if pluginKey not in version_mapping:
						if not ignores or pluginKey not in ignores:
							error('No definition for ' + pluginKey + ": current version: " + version)
							sys.exit(255)
					else:
						newVersion = version_mapping[pluginKey]
						if newVersion != version:
							info("\t" + pluginKey + ": " + version + " -> " + newVersion)
							versionNode.text = newVersion
							changed_now = True
	return changed_now or changed

#########################
## root: the XML root node
## version_mapping: the mapping from the plugin id to the plugin version.
## ignores: the list of keys to ignore for replacement
## enableDependencies: enables or disables the replacements of Maven dependencies
## returns: True if a node has changed, False otherwise
def replace_in_pom(root : object, version_mapping : dict, ignores : list, enableDependencies : bool) -> bool:
	changed = False
	changed = replace_pom_nodes(root, version_mapping, ignores, changed,
		'./build/extensions/extension')
	changed = replace_pom_nodes(root, version_mapping, ignores, changed,
		'./build/plugins/plugin')
	changed = replace_pom_nodes(root, version_mapping, ignores, changed,
		'./build/plugins/plugin/dependencies/dependency')
	changed = replace_pom_nodes(root, version_mapping, ignores, changed,
		'./build/pluginManagement/plugins/plugin')
	changed = replace_pom_nodes(root, version_mapping, ignores, changed,
		'./build/pluginManagement/plugins/plugin/dependencies/dependency')
	changed = replace_pom_nodes(root, version_mapping, ignores, changed,
		'./profiles/profile/build/plugins/plugin')
	if enableDependencies:
		changed = replace_pom_nodes(root, version_mapping, ignores, changed,
			'./dependencies/dependency')
		changed = replace_pom_nodes(root, version_mapping, ignores, changed,
			'./dependencyManagement/dependencies/dependency')
	return changed

#########################
## root: the XML root node
## version_mapping: the mapping from the plugin id to the plugin version.
## ignores: the list of keys to ignore for replacement
## returns: True if a node has changed, False otherwise
def replace_in_extension(root : object, version_mapping : dict, ignores : list) -> bool:
	changed = False
	changed = replace_pom_nodes(root, version_mapping, ignores, changed,
		'./extension', '')
	return changed

#########################
## root: the XML root node
## version_mapping: the mapping from the plugin id to the plugin version.
## ignores: the list of keys to ignore for replacement
## returns: True if a node has changed, False otherwise
def replace_in_eclipse_platform(root : object, version_mapping : dict, ignores : list) -> bool:
	changed = False
	changed = replace_pom_nodes(root, version_mapping, ignores, changed,
		'./locations/location/dependencies/dependency', '')
	return changed


##############################
##
# Fix the bug of ANSI colors on terminal for Windows terminals
os.system('')
#
parser = argparse.ArgumentParser(description="Update Maven plugin versions")
parser.add_argument('args', nargs=argparse.REMAINDER, action="append")
parser.add_argument("--version", help="Show the version of this script", action="store_true")
parser.add_argument("--rootpath", help="Path to the root folder", action="store")
parser.add_argument('--pom', help="Path to an additional pom file to treat", action='append')
parser.add_argument('--properties', help="Path to the property file", action='append')
parser.add_argument('--ignore', help="The groupId:artifactId to ignore", action='append')
parser.add_argument('--nodependency', help="Ignore Maven dependencies", action='store_true')
parser.add_argument('--eclipseplatform', help="Path to the Eclipse platform file in which the Maven versions must be updated", action='append')
args = parser.parse_args()

if args.version:
	info("Version: " + VERSION)
	sys.exit(0)

properties = Properties()
if args.properties:
	for prop_file in args.properties:
		with open(str(prop_file), 'rb') as input_file:
			properties.load(input_file)

version_mapping = {};
for arg in args.args:
	read_version_mappings(version_mapping, arg[0], properties)

if args.rootpath:
	rootpath = os.path.realpath(args.rootpath)
else:
	rootpath = os.getcwd()
pom_files = build_maven_file_list(rootpath);

if args.pom:
	pom_files = pom_files + args.pom

for pom_file in pom_files:
	info("Scanning POM: " + pom_file)
	xml_root = read_xml_root_node(pom_file)
	changed = replace_in_pom(xml_root, version_mapping, args.ignore, (not args.nodependency))
	if changed:
		info("Saving POM: " + pom_file)
		with open(pom_file, 'wt') as output_file:
			output_file.write(dump_xml(xml_root))

extension_files = build_maven_extension_file_list(pom_files)

for extension_file in extension_files:
	info("Scanning Ext: " + extension_file)
	xml_root = read_xml_root_node(extension_file)
	changed = replace_in_extension(xml_root, version_mapping, args.ignore)
	if changed:
		info("Saving Ext: " + extension_file)
		with open(extension_file, 'wt') as output_file:
			output_file.write(dump_xml(xml_root))

if args.eclipseplatform:
	for platform_file in args.eclipseplatform:
		info("Scanning Ecl: " + platform_file)
		xml_root = read_xml_root_node(platform_file);
		changed = replace_in_eclipse_platform(xml_root, version_mapping, args.ignore)
		if changed:
			info("Saving Ecl: " + platform_file)
			with open(platform_file, 'wt') as output_file:
				output_file.write("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n")
				output_file.write("<?pde version=\"3.8\"?>\n")
				output_file.write(dump_xml(xml_root))

sys.exit(0)

