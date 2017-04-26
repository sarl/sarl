# Janus Command-line Launcher

[:Outline:]

[Janus](http://www.janusproject.io) is the default SARL Run-time Environment (SRE).
Its role is to provide all the libraries and plugins that are needed for running SARL agents.

For launching the Janus platform from a command-line shell, you could use the `janus` tool.

## Usage

The `janus` tool takes arguments:


	janus [OPTIONS] <agent_fully_qualified_name>


The `agent_fully_qualified_name` is the name of the agent to launch. This name is equal to the
fully qualified name of the agent's type.

<note>Janus enables to launch only one agent from the command-line shell. If you want to launch
more than one agent, you should launch a first agent, which will create the other agents.</note>

You could change the behavior of the `janus` tool with the command-line options.
For obtaining a list of the options, type:

	janus -help


[:Include:](../legal.inc)

