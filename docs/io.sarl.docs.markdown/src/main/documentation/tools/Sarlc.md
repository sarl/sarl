# SARL Command-line Compiler

[:Outline:]

[:Fact:]{(io.sarl.lang.compiler.batch.Main)::getCompilerProgramName == '[:name](sarlc)'}

A command-line compiler is a tool that could be invoked from the command-line shell in order to compiler a language source file.
[:name:] is the command-line compiler for the SARL language.

## Usage

The [:name:] tool takes arguments:


	[:name:] --dir <[:outfolder](output folder)> [OPTIONS] <[:srcfolder](source folder)>...


The [:outfolder:] is the name of the folder in which the generated Java files should be created by the SARL compiler.
The [:name:] tool takes at least one [:srcfolder:] from which the SARL files are read.

You could change the behavior of the [:name:] compiler with the command-line options.
For obtaining a list of the options, type:

	[:name!] -help


## Command-Line Options

The complete list of the options is:



| Option | Description |
| ------ | ----------- |
[:Dynamic:]{
	io.sarl.lang.compiler.batch.Main::getOptions.renderToMarkdown
}



[:Include:](../legal.inc)

