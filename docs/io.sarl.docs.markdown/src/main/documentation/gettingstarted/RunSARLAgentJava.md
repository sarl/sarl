# Run SARL Agent from a Java or SARL Program

[:Outline:]

For running an agent, you must launch this agent on the runtime environment.
This document explains how to launch an agent on any SARL Run-time Environment (SRE)
from a Java program or SARL class.

The default SRE is the [Janus platform](http://www.janusproject.io). 


## Definition of the SRE Bootstrap

In the SARL API, a bootstrap definition is provided.
It represents an access point to the SRE from any program.
This access point may be used for accessing by code the features of the underlying SRE,
independently of the concrete implementation.
In other words, the SRE Bootstrap gives access to the standard SRE functions without
forcing you to add an explicit dependency to the SRE Library, e.g. Janus, into your
application classpath.

The definition of the SARL API for using the bootstrap is detailled in the [reference page on SRE](../api/SRE.md).


## Using the SRE Bootstrap

In order to help you to use the bootstrap functions, the SARL API provides a static utility type, named [:sre:].
In the following Java code, the [:sre:] utility type is used for retrieving the bootstrap.
 
[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	class MyProgram {
	
		static def main(arguments : String*) {
			var bootstrap = [:sre](SRE)::getBootstrap
		}
	
	}
[:End:]

```java
public class MyProgram {

	public static void main(String[] arguments) {
		SREBootstrap bootstrap = [:sre!].getBootstrap();
	}

}
```


Then, it is possible to use the bootstrap for launching an agent. In the following example, a agent of type
[:myagent:] is launched. Please note that you must not create an instance of an agent by yourself.
It is the role of the SARL run-time environment to create this instance for you, with the proper initialization.

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	agent [:myagent](MyAgent) {
	}
	class MyProgram {
	
		static def main(arguments : String*) {
			var bootstrap = [:sre!]::getBootstrap
			bootstrap.[:startfct](startAgent)(typeof([:myagent!]))
		}
	
	}
[:End:]

```java
SREBootstrap bootstrap = [:sre!].getBootstrap();
bootstrap.[:startfct!]([:myagent!].class)
```


In the case you want to launch more than one agent programmatically,
you could call the [:startfct:] function with the number of agent instances you need.


## Direct Access to the API of the Janus SRE


> **_Caution_**: Using the API of Janus within your program is not recommended by the SARL team,
> because the API of the Janus framework is considered as private and may evolve quickly.
> Prefer to use the Bootstrap API that is described into the previous section.



The Janus platform provides a [:boot:] class. For launching the platform, you must use this boot class.

The [:boot:] class provides the [:startjanus:] function, which permits to launch Janus programmatically.


Let consider you want to launch your agent, defined in the [:agenttype:] class.
The following SARL code gives you an example of how to launch this agent in Janus.

The first parameter of the [:startjanus:] function is the Java type of the agent
to launch.

The second parameter of the [:startjanus:] function is the list of parameters to
pass with the [:initevent:] event to the launched agent.
[:Fact:]{io.sarl.core.[:initevent](Initialize)}


> **_Very Important Note:_** The Janus platform enables to launch a single agent at start-up.
> If you want to launch more agents, please read the next section.

[:Success:]
	import io.sarl.sre.boot.Boot
	import myprogram.MyAgent
	class MyProgram {
	 	static def main(args : String*) {
			[:boot](Boot)::createMainObject.[:startjanus](startAgent)(
				typeof([:agenttype]{MyAgent}),
				args)
		}
	}
[:End:]

```java
import io.sarl.sre.boot.Boot;
import myprogram.MyAgent;
public class MyProgram {
 	public static void main(String[] args) {
		[:boot!].createMainObject.[:startjanus!]([:agenttype!].class, args);
	}
}
```


In  the case you want to launch more than one agent programmatically,
you could use the [:kernel:] instance provided by Janus.
This instance is replied by the [:getkernel:] function of the [:boot:] class.

[:Success:]
	[:Off]
	import io.sarl.sre.boot.Boot
	import io.sarl.sre.boot.[:kernel](SreMain)
	class MyProgram {
	 	def fct : void {
			var sremain = Boot.createMainObject
			var kernel = sremain.[:getkernel](getKernel)
		}
	}
[:End:]


## What's next?

Now, you are ready for developing agents with the SARL language.
Please read the rest of the documentation for obtaining more details.

[Next>](../index.md)


[:Include:](../legal.inc)
