# Janus Command-line Launcher

[:Outline:]

[:Fact:]{(io.janusproject.Boot)::getProgramName == '[:name](janus)'}

[Janus](http://www.janusproject.io) is the default SARL Run-time Environment (SRE).
Its role is to provide all the libraries and plugins that are needed for running SARL agents.

For launching the Janus platform from a command-line shell, you could use the [:name:] tool.

## Usage

The [:name:] tool takes arguments:


	[:name!] [OPTIONS] <[:agname](agent_fully_qualified_name)>


The [:agname:] is the name of the agent to launch. This name is equal to the
fully qualified name of the agent's type.

<note>Janus enables to launch only one agent from the command-line shell. If you want to launch
more than one agent, you should launch a first agent, which will create the other agents.</note>

You could change the behavior of the [:name:] tool with the command-line options.
For obtaining a list of the options, type:

	[:name!] -help


## Command-Line Options

The complete list of the options is:



| Option | Description |
| ------ | ----------- |
[:Dynamic:]{
	io.janusproject.Boot::getOptions.renderToMarkdown
}




[:Include:](../legal.inc)

