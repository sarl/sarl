# SARL Command-line Compiler

[:Outline:]

[:Fact:]{(io.sarl.lang.sarlc.Main)::getDefaultCompilerProgramName == '[:name](sarlc)'}

A command-line compiler is a tool that could be invoked from the command-line shell in order to compiler a language source file.
[:name:] is the command-line compiler for the SARL language.

## Usage

The [:name:] tool takes arguments:


	[:name!] [OPTIONS] <[:srcfolder](source folder)>...


The [:name:] tool takes at least one [:srcfolder:] from which the SARL files are read.

You could change the behavior of the [:name:] compiler with the command-line options.
For obtaining a list of the options, type:

	[:name!] -h


## Command-Line Options

The complete list of the options is:



| Option | Description |
| ------ | ----------- |
[:Dynamic:]{
	io.sarl.lang.sarlc.Main::getOptions.renderToMarkdown
}


[:Include:](../legal.inc)

