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

import os
import platform
import argparse
import subprocess
import sys
import stat
from enum import StrEnum

##########################################
##
class Version(StrEnum):
    LINUX = 'Linux x86_64'
    WINDOWS = 'Windows x86_64'
    MACOSX86 = 'MacOS x86_64'
    MACOSA86 = 'MacOS aarch64'

##########################################
##
class bcolors:
    HEADER = '\033[34m'
    OKGREEN = '\033[32m'
    FAIL = '\033[31m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

##########################################
## message : the mssage to display
def info(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message : the mssage to display
def success(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.HEADER}{bcolors.BOLD}INFO{bcolors.ENDC}]{bcolors.OKGREEN}{bcolors.BOLD}" + msg + f"{bcolors.ENDC}", file=sys.stdout)

##########################################
## message : the mssage to display
def error(message : str):
	if message:
		msg = " " + message
	else:
		msg = ''
	print(f"[{bcolors.FAIL}{bcolors.BOLD}ERROR{bcolors.ENDC}]" + msg, file=sys.stderr)


##########################################
## 
parser = argparse.ArgumentParser()
group = parser.add_mutually_exclusive_group(required=False)
group.add_argument("--win", help="Run the Windows 64 binary", action="store_true")
group.add_argument("--linux", help="Run the Linux 64 binary", action="store_true")
group.add_argument("--macx64", help="Run the MacOS 64 Intel binary", action="store_true")
group.add_argument("--maca64", help="Run the MacOS 64 ARM binary", action="store_true")
parser.add_argument('args', nargs=argparse.REMAINDER)
args = parser.parse_args()

if args.win:
	current_system = Version.WINDOWS
elif args.macx64:
	current_system = Version.MACOSX86
elif args.maca64:
	current_system = Version.MACOSA86
elif args.linux:
	current_system = Version.LINUX
else:
	current_platform = platform.platform()
	if current_platform == 'Windows':
		current_system = Version.WINDOWS
	elif current_platform == 'Darwin':
		current_architecture = platform.system()
		if current_architecture == 'x86_64':
			current_system = Version.MACOSX86
		else:
			current_system = Version.MACOSA86
	else:
		current_system = Version.LINUX

info("Current architecture: " + str(current_system))

script_dir = os.path.dirname(os.path.realpath(__file__))
current_dir = os.getcwd()

sarl_ide_path = os.path.join(script_dir, 'products', 'io.sarl.eclipse.products.ide', 'target', 'products', 'io.sarl.eclipse.products.ide')

if current_system == Version.WINDOWS:
	sarl_ide_path = os.path.join(sarl_ide_path, 'win32', 'win32', 'x86_64', 'sarlide.exe')
	sarl_ide_path = os.path.realpath(sarl_ide_path)
elif current_system == Version.MACOSX86:
	sarl_ide_path = os.path.join(sarl_ide_path, 'macosx', 'cocoa', 'x86_64', 'Eclipse.app', 'Contents', 'MacOS', 'sarlide')
	sarl_ide_path = os.path.realpath(sarl_ide_path)
elif current_system == Version.MACOSA86:
	sarl_ide_path = os.path.join(sarl_ide_path, 'macosx', 'cocoa', 'aarch64', 'Eclipse.app', 'Contents', 'MacOS', 'sarlide')
	sarl_ide_path = os.path.realpath(sarl_ide_path)
else:
	sarl_ide_path = os.path.join(sarl_ide_path, 'linux', 'gtk', 'x86_64', 'sarlide-ubuntu.sh')

sarl_ide_path = os.path.realpath(sarl_ide_path)
if not os.path.isfile(sarl_ide_path):
	error("Executable file not found: " + sarl_ide_path)
	sys.exit(255)
if not os.access(sarl_ide_path, os.X_OK):
	current_permissions = stat.S_IMODE(os.lstat(sarl_ide_path).st_mode)
	os.chmod(sarl_ide_path, current_permissions & stat.S_IXUSR)

arguments = [ sarl_ide_path ] + args.args

info("Running: " + str(sarl_ide_path))
info("Arguments: " + str(arguments[1:]))

os.execv(sarl_ide_path, arguments)

