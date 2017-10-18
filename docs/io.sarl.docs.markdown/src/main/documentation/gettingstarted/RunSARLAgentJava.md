# Run SARL Agent from a Java Program

[:Outline:]

For running an agent, you must launch this agent on the runtime environment.
This document explains how to launch an agent on any SARL Run-time Environment (SRE)
from a Java program.

The default SRE is the [Janus platform](http://www.janusproject.io). 


## Definition of the SRE Bootstrap

In the SARL API, a bootstrap is definition is provided.
It represents an access point to the SRE from any program.
This access point may be used for accessing the features of the underlying SRE,
independently of the implementation.
In other words, the SRE Bootstrap gives access to the standard SRE functions without
forcing you to add an explicit dependency to the SRE Library, e.g. Janus, into your
application classpath.

The SARL API defines a SRE bootstrap as:

		[:ShowType:]([:bootstrap]$io.sarl.core.SREBootstrap$)


A run-time environment, such as [Janus](http://www.janusproject.io) must provide a service implementing this bootstrap interface.
The standard Java service management feature is used. in other words, the SRE should
[declare the service implementation](https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html) into
its `META-INF/services/[:bootstrap!]` file.


## Using the SRE Bootstrap

In order to help you to use the bootstrap functions, the SARL API provides a static utility type, named [:sre:].
In the following Java code, the [:sre:] utility type is used for retrieving the bootstrap.
 
		[:Success:]
			package io.sarl.docs.bootstrap
			import io.sarl.core.SRE
			class MyProgram {
			
				static def main(arguments : String*) {
					var bootstrap = [:sre](SRE)::getBootstrap
				}
			
			}
		[:End:]
		public class MyProgram {
		
			public static void main(String[] arguments) {
				SREBootstrap bootstrap = [:sre!].getBootstrap();
			}
		
		}


Then, it is possible to use the bootstrap for launching an agent. In the following example, a agent of type
[:myagent:] is launched. Please note that you must not create an instance of an agent by yourself.
It is the role of the SARL run-time environment to create this instance for you, with the proper initialization.

		[:Success:]
			package io.sarl.docs.bootstrap
			import io.sarl.core.SRE
			agent [:myagent](MyAgent) {
			}
			class MyProgram {
			
				static def main(arguments : String*) {
					var bootstrap = [:sre!]::getBootstrap
					bootstrap.[:startfct](startAgent)(typeof([:myagent!]))
				}
			
			}
		[:End:]
		SREBootstrap bootstrap = [:sre!].getBootstrap();
		bootstrap.[:startfct!]([:myagent!].class)


In the case you want to launch more than one agent programmatically,
you could call the [:startfct:] function the number of times you need.


## Direct Access to the API of the Janus SRE


Caution: using the API of Janus within your program is not recommended by the SARL team. Prefer to use the Bootstrap API.



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

		[:Success:]
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
		import io.janusproject.Boot;
		import myprogram.MyAgent;
		public class MyProgram {
		 	public static void main(String[] args) {
				[:boot!].[:startjanus!]([:agenttype!].class, args);
			}
		}


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


		[:Success:]
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
		import io.janusproject.Boot;
		import myprogram.MyAgent;
		public class MyProgram {
		 	public static void main(String[] args) {
				[:boot!].[:startjanus!]([:agenttype!].class, args);
				janusKernel.[:spawn!](MyAgent.class, args);
			}
		}


## What's next?

Now, you are ready for developing agents with the SARL language.
Please read the rest of the documentation for obtaining more details.

[Next>](../index.md)


[:Include:](../legal.inc)

