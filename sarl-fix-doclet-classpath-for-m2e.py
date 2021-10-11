#!/usr/bin/env python

import sys
import os
import shutil

cwd = os.getcwd()

source = os.path.join(cwd, "docs", "io.sarl.docs.doclet", "prj-template", "dot_classpath")

target = os.path.join(cwd, "docs", "io.sarl.docs.doclet", ".classpath")

shutil.copyfile(source, target)

sys.exit(0)

