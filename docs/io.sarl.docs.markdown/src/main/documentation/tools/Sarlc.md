# SARL Command-line Compiler

[:Outline:]

A command-line compiler is a tool that could be invoked from the command-line shell in order to compiler a language source file.
`sarlc` is the command-line compiler for the SARL language.

## Usage

The `sarlc` tool takes arguments:


	sarlc --dir <output folder> [OPTIONS] <source folder>...


The `output folder` is the name of the folder in which the generated Java files should be created by the SARL compiler.
The `sarlc` tool takes at least one `source folder` from which the SARL files are read.

You could change the behavior of the `sarlc` compiler with the command-line options.
For obtaining a list of the options, type:

	sarlc -help


[:Include:](../legal.inc)

