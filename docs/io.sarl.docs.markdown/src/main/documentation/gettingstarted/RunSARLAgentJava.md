# Run SARL Agent from a Java Program

[:Outline:]

For running an agent, you must launch this agent on the runtime environment.
This document explains how to launch an agent on the
[Janus platform](http://www.janusproject.io) from a Java program.


## Boot of Janus


The Janus platform provides a [:boot:] class. For launching the platform, you must use this boot class.

The [:boot:] class provides the [:startjanus:] function, which permits to launch Janus programmatically.


Let consider you want to launch your agent, defined in the [:agenttype:] class.
The following SARL code gives you an example of how to launch this agent in Janus.

The first parameter of the [:startjanus:] function is the Java type of the agent
to launch.

The second parameter of the [:startjanus:] function is the list of parameters to
pass with the [:initevent:] event to the launched agent.
[:Fact:]{io.sarl.core.[:initevent](Initialize)}


<importantnode>The Janus platform enables to launch a single agent at start-up.
If you want to launch more agents, please read the next section.</importantnote>

		[:Success:][:On]
			import io.janusproject.Boot
			import myprogram.MyAgent
			class MyProgram {
			 	static def main(args : String*) {
					[:boot](Boot)::[:startjanus](startJanus)(
						typeof([:agenttype]{MyAgent}),
						args)
				}
			}
		[:End:]


## Launching more agents programmatically with Janus

In  the case you want to launch more than one agent programmatically,
you could use the [:kernel:] instance provided by Janus.
[:Fact:]{io.janusproject.kernel.[:kernel](Kernel)}
This instance is replied by the [:startjanus:] function of the [:boot:] class.

The [:kernel:] type provides the [:spawn:] functions, which permit launching
an agent programmatically.

The previous example could be updated for launching two agents of the same type.
The resulting code is shown below.

The first parameter of the [:spawn:] function is the Java type of the agent to launch.

The second parameter of the [:spawn:] function is the list of parameters to
pass with the [:initevent] event to the launched agent.

<important>that the first agent is launched by the [:startjanus:] function, and the 
second agent is launched by the [:spawn:] function.</important>


		[:Success:][:On]
			import io.janusproject.Boot
			class MyProgram {
				static def main(args : String*) {
					var janusKernel = Boot::startJanus(
						typeof(MyAgent),
						args)
					janusKernel.[:spawn]{spawn}(typeof(MyAgent), args)
				}
			}
			[:Off]
			agent MyAgent {}
		[:End:]


## What's next?

Now, you are ready for developing agents with the SARL language.
Please read the rest of the documentation for obtaining more details.

[Next>](../index.md)


[:Include:](../legal.inc)

