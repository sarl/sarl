#!/usr/bin/env python3

# This script extracts the list of Eclipse plugins and features for a content.jar of
# a p2 repository to output the list of plugins and features that should be included
# in a Eclipse feature for completed an Eclipse product.

# This script is used for filling the list of plugins in the feature
# "io.sarl.eclipse.features.baseplatform". The p2 repository is obtained
# from the "Export product" feature of the Eclipse product editor.
# The output of this Eclipse feature is provided to the current script.

# Automatic excluded plugins have one of the following keywords in their names:
# - ends with ".source"
# - feature
# - linux
# - gtk
# - cocoa
# - win32
# - sarl

import argparse
import os
import sys
import xml.etree.ElementTree as xml

from zipfile import *

##############################
##
def read_content_xml(input_stream):
	"""reads the content of the P2 content XML stream"""
	tree = xml.parse(input_stream)
	root = tree.getroot()
	plugins = set()
	for unit in root.findall("./units/unit"):
		id = unit.get('id')
		if id and not id.endswith(".source") and "feature" not in id and "linux" not in id and "gtk" not in id and "cocoa" not in id and "win32" not in id and "sarl" not in id:
			plugins.add(id)
	return plugins

##############################
##
def read_content_jar(input_file):
	"""reads the content of the P2 content jar file"""
	with ZipFile(input_file, "r") as z:
		with z.open("content.xml", "r") as file:
			return read_content_xml(file)

##############################
##
def build_feature_plugin_list(plugins):
	"""generates the list of plugins for a feature"""
	str_out = ""
	for plugin in plugins:
		str_out = str_out + f"""   <plugin
         id="{plugin}"
         download-size="0"
         install-size="0"
         version="0.0.0"
         unpack="false"/>
"""
	return str_out

##############################
##
parser = argparse.ArgumentParser(description="Extract plugin definitions from P2 and generate feature definition")
parser.add_argument("input", metavar="PATH-TO-CONTANT-JAR", help="path to the \"content.jar\" file to analyse", action="store")
parser.add_argument("--output", help="path to the \"feature.xml\" file to generate", action="store")
args = parser.parse_args()

content_jar = read_content_jar(args.input)

feature_list = build_feature_plugin_list(content_jar)

if args.output:
	with open(args.output, "w") as outfile:
		print(feature_list, file=outfile)
else:
	print(feature_list)

