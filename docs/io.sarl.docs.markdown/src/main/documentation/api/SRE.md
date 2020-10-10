# Programmatic Access to the SARL Run-time Environment

[:Outline:]

For running an agent, you must launch this agent on the runtime environment.
This run-time environment is named the SARL Run-time Environment (SRE).
The default SRE is the [Janus platform](http://www.janusproject.io). 


## Definition of the SRE Bootstrap

In the SARL API, a bootstrap definition is provided.
It represents an access point to the SRE from any SARL or Java program.
This access point may be used for accessing by code the features of the underlying SRE,
independently of the concrete implementation.
In other words, the SRE Bootstrap gives access to the standard SRE functions without
forcing you to add an explicit dependency to the SRE Library, e.g. Janus, into your
application classpath.

The SARL API defines the SRE bootstrap as:

[:ShowType:]([:bootstrap]$io.sarl.bootstrap.SREBootstrap$)


A run-time environment, such as [Janus](http://www.janusproject.io) must provide an implementation of this bootstrap interface.
In order to find this implementation, the [standard Java service management feature](https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html)
is used. In other words, the SRE should declare a service implementation for
[:bootstrap:] into its `META-INF/services/[:bootstrap!]` file (for Java 8) or in its service declaration into the module definition (for Java 9 and above):


```java
module mymodule {
	provides [:bootstrap!] with mypackage.MyServiceImplementation
}
```


## Static Access to the Bootstrap: the SRE class

In order to help you to use the bootstrap functions, the SARL API provides a static utility, named [:sre:].
In the following SARL code (it may be Java or Groovy code), the [:sre:] utility type is used for retrieving the bootstrap.
 
[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	class MyProgram {
		[:On]
		static def main(arguments : String*) {
			var [:bootstrapvar](bootstrap) = [:sre](SRE)::getBootstrap
		}
		[:Off]			
	}
[:End:]


Then, it is possible to use the [:bootstrapvar:] variable for launching an agent or accessing to any of the SRE
features that is detailed below.
In the following example, a agent of type [:myagent:] is launched. Please note that you cannot not create an instance of an agent by yourself.
It is the role of the SRE to create this instance for you, with the proper initialization.

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	agent [:myagent](MyAgent) {}
	class MyProgram {
		[:On]
		static def main(arguments : String*) {
			var bootstrap = [:sre!]::getBootstrap
			bootstrap.[:startfct](startAgent)(typeof([:myagent!]))
		}
		[:Off]
	}
[:End:]


In the case you want to launch more than one agent programmatically,
you could call the [:startfct:] function with the number of agent instances you need.


## Description of the SRE Bootstrap features 

This section describes the major features of the SRE bootstrap.

### Starting the SRE Programmatically

In order to run agents, you must start the SRE before launching an agent inside.
There is two major methods for starting the SRE: without or with agent(s).
The first method could be used to launch an empty SRE that is waiting for the launch of a first agent that may occur later in time.
The second version is the standard launching method.

#### Start without agent

For starting the SRE without agent, you have to invoke [:startWithoutAgentFct:]:

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var bootstrap = SRE::getBootstrap
			var context = bootstrap.[:startWithoutAgentFct](startWithoutAgent)
			[:Off]			
		}
	}
[:End:]


The [:startWithoutAgentFct:] function takes no argument and returns the root context that is created by the SRE.


#### Start with agent

For launching the SRE with an instance of agent, you have to use one of the [:startAgentFct:] and [:startAgentWithIDFct:] functions.
Both of them are launching an agent of a given type. For example, the following code starts the agent of type [:myagent:].

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	agent [:myagent!] {}
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var bootstrap = SRE::getBootstrap
			bootstrap.[:startAgentFct](startAgent)(typeof([:myagent!]))
			[:Off]			
		}
	}
[:End:]


In the case you would like to create multiple instances of agents of a given type, you could specify the number as first argument of [:startAgentFct:].
In the following example, [:numberofagents!] agents are launched into the SRE.

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	agent [:myagent!] {}
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var bootstrap = SRE::getBootstrap
			bootstrap.startAgent([:numberofagents](5), typeof([:myagent!]))
			[:Off]			
		}
	}
[:End:]


It may be useful to give some values to the agent when it is launched. These values are named the initialization arguments for the agent(s).
You could pass a list of values to the launched agent by typing these values at the end of the arguments of the [:startAgentFct:] function.
In the following example, two initialization arguments are passed. The first is the string of characters [:param1:]; and the second is the double
precision floating point number [:param2:].

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	agent [:myagent!] {}
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var bootstrap = SRE::getBootstrap
			bootstrap.startAgent(typeof([:myagent!]), [:param1]$"arg1"$, [:param2]$4.5$)
			[:Off]
		}
	}
[:End:]


The initialization arguments are received by the launched agent(s) into the [:initializeevent:], and more precisely into the field
[:initializeparameters:] of this event.
In the following example, the agent of type [:myagent:] has an handler on the [:initializeevent:] event in which two initialization arguments
are retrieved:

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.core.Initialize
	[:On]
	agent [:myagent!] {

		on [:initializeevent](Initialize) {
			var arg1 = occurrence.[:initializeparameters](parameters).get(0) as String
			var arg0 = occurrence.parameters.get(1) as Double
		}
	
	}
[:End:]


In some cases, you would like that the launched agent instance having a specific identifier, and not any more a randomly generated identifier.
You could invoke the [:startAgentWithIDFct:] function for launching the agent with the identifier that your have choosen.
This function enables to pass initialization arguments to the launched agent.
 
[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	import java.util.UUID
	agent [:myagent!] {}
	class MyProgram {
		static def computeOrGetTheAgentIdentifier : UUID { null }
		static def main(arguments : String*) {
			[:On]
			var theIdentifier : UUID = computeOrGetTheAgentIdentifier()
			var bootstrap = SRE::getBootstrap
			bootstrap.[:startAgentWithIDFct](startAgentWithID)(typeof([:myagent!]), theIdentifier)
			[:Off]			
		}
	}
[:End:]


### Activity Access Functions

Two functions are provided to determine if the SRE is running or not:
* [:isRunningFct:]: replies a boolean value that indicates if the kernel is running (`true`) or not (`false`).
* [:isActiveFct:]: replies a boolean value that indicates if the kernel accepts launchs of agents (`true`) or not (`false`).

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	class MyProgram {
		static def main(arguments : String*) {
			var bootstrap = SRE::getBootstrap
			var b0 : boolean = bootstrap.[:isRunningFct](isRunning)
			var b1 : boolean = bootstrap.[:isActiveFct](isActive)
		}
	}
[:End:]


### Stopping the SRE Programmatically

Stopping the SRE programmatically is possible, even if it is not the best way according the SARL developers' best practices.
Indeed, it is always preferable to let the SRE stops itself when no more agent is running inside.
Stopping the SRE programmatically forces the agents to be stopped whatever their internal states.
All the behaviors that were launched by the agents are also stopped.

The function [:shutdownFct:] is provided to stop the SRE. By default, this function is blocking, i.e., it is returning when
the SRE is stopping. The following code shows you the start and stop of the SRE.

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	agent [:myagent!] {}
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var bootstrap = SRE::getBootstrap
			bootstrap.startAgent([:numberofagents!], typeof([:myagent!]))
			bootstrap.[:shutdownFct](shutdown)
			[:Off]
		}
	}
[:End:]


If you need the function [:shutdownFct:] to be not blocking, you could invoke the [:shutdownFct:] with a timeout delay (in milliseconds).
When this delay is reached, the [:shutdownFct:] forces the stop of the SRE (even if an agent is not yet fully destroyed) and returns.
In the following example, we are waiting 15 seconds for stopping the SRE.

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	agent [:myagent!] {}
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var bootstrap = SRE::getBootstrap
			bootstrap.startAgent([:numberofagents!], typeof([:myagent!]))
			bootstrap.[:shutdownFct](shutdown)(1500)
			[:Off]
		}
	}
[:End:]


If the provided timeout value is strictly positive, it is the number of milliseconds to wait for the termination of the shutdown.
If the provided timeout value is equal to `0`, the function returns as soon as the shutdown process is launch
(no waiting for the termination of the shutdown). If the timeout value is strictly negative, the function waits forever for the
termination of the shutdown process.


### Configuration Functions

**Prior to the first launch of an agent**, it is possible to change programmatically several elements of the SRE configuration.
The following functions are provided on the SRE bootstrap to change the SRE configuration.

* [:setRandomContextUUID:] forces the SRE to use a random identifier for its default context.
* [:setBootAgentTypeContextUUID:] forces the SRE to use a default context identifier that is build upon the class name of the first launched agent, i.e. the UUID is always the same for a given class name.
* [:setSpecificContextUUID:] forces the SRE to use the identifier hard-coded in the source code for its default context. This identifier was chosen by the SRE creators and it is constant.
* [:setUniverseContextUUID:] forces the identifier of the SRE's default context. This function takes one argument value of type `UUID`. You could retrieved the identifier by calling [:getUniverseContextUUID:].
* [:setUniverseSpaceUUID:] forces the identifier of the default space of the SRE's default context. This function takes one argument value of type `UUID`. You could retrieved the identifier by calling [:getUniverseSpaceUUID:].

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	import java.util.UUID
	class MyProgram {
		static def main(arguments : String*) {
			var bootstrap = SRE::getBootstrap
			bootstrap.[:setRandomContextUUID]$setRandomContextUUID$
			bootstrap.[:setBootAgentTypeContextUUID]$setBootAgentTypeContextUUID$
			bootstrap.[:setSpecificContextUUID]$setSpecificContextUUID$
			bootstrap.[:setUniverseContextUUID]$setUniverseContextUUID$(UUID::randomUUID)
			var id0 : UUID = bootstrap.[:getUniverseContextUUID]$getUniverseContextUUID$
			bootstrap.[:setUniverseSpaceUUID]$setUniverseSpaceUUID$(UUID::randomUUID)
			var id1 : UUID = bootstrap.[:getUniverseSpaceUUID]$getUniverseSpaceUUID$
		}
	}
[:End:]


### SRE Logging System

The SRE should have a logging system in order to provide to you information, warning and error messages.
You could have access to the SRE logger by calling the [:getKernelLoggerFct:]. If this function returns `null`,
it means that the SRE was not launched.

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	import java.util.logging.Logger
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var bootstrap = SRE::getBootstrap
			var logger : Logger = bootstrap.[:getKernelLoggerFct](getKernelLogger)
			logger.info("my message")
			[:Off]
		}
	}
[:End:]


You could control in a generic way the verbose level of the kernel logger by calling the function
[:setVerboseLevelFct:].

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	import java.util.logging.Logger
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var bootstrap = SRE::getBootstrap
			bootstrap.[:setVerboseLevelFct](setVerboseLevel)(2)
			[:Off]
		}
	}
[:End:]


This function takes an integer argument that specifies the requested level of verbose of the logger:

| Verbose Level | Description                                                                |
| ------------- | -------------------------------------------------------------------------- |
| <= 0          | Off, no logging                                                            |
| 1             | Error messages only                                                        |
| 2             | Errors and warning messages                                                |
| 3             | Error, warning and information messages                                    |
| 4             | Error, warning, information and major debugging messages                   |
| 5             | Error, warning, information and major+intermediate debugging messages      |
| 6             | Error, warning, information and major+intermediate+fine debugging messages |
| >= 7          | All messages                                                               |


### Access to the SRE Services

The SRE may provide specific service implementation (naming, namespace, communication, etc.).
In order to enable you to have access to the SRE services, the SRE bootstrap provides the
function [:getServiceFct:].
It takes an argument that is the type of the service to search for.
And, it returns the instance of the service if it is defined into the underlying SRE.
If the service type is not supported by the SRE, the `null` value is returned. 

In the following example, a service of type [:myservice:] is declared.

[:Success:]
	package io.sarl.docs.bootstrap
	[:On]
	interface [:myservice](MyService) {
		def use() : void
	}
[:End:]


Assuming that the SRE is implementing this service, you could get the service's instance and use it as illustrated below:

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	interface [:myservice](MyService) {
		def use() : void
	}
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var bootstrap = SRE::getBootstrap
			bootstrap.startWithoutAgent
			var serv = bootstrap.[:getServiceFct](getService)(typeof([:myservice!]))
			serv.use
			[:Off]
		}
	}
[:End:]


The rest of the SARL's API documentation provides description of specific services, e.g. the [naming service](./Naming.md).

### Observe the SRE events

Observer design pattern is common in software engineering and development. It enables to be notified when some events occur into the target system.
By default the SRE implements this design pattern on specific events:

* SRE started
* SRE stopped 

The SARL API assumes an implementation of the observer design pattern that follows the Java programming standard:
1. The observer is represented by an interface that is named the "listener".
2. For each type of event, a function is declared into the listener interface.
3. The observed object provides functions for registering and unregistering the listeners.
4. The observed object invokes the right function of the listener interface when the event occured. 

According to these general principles, the listener for the SRE events is defined as:

[:ShowType:](io.sarl.bootstrap.SREListener)


Both declared functions takes the SRE bootstrap as parameter.

The SRE bootstrap provides the registration function [:addSREListenerFct:] and the unregistration function [:removeSREListenerFct:].

The typical usage of this listening framework is illustrated by the code below.
First an implementation of listener is written. Basically, this implementation output on the program console messages that indicate the SRE start and stop.

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	import io.sarl.bootstrap.SREBootstrap
	import io.sarl.bootstrap.SREListener
	[:On]
	class MyListener implements SREListener {
		override sreStarted(bs : SREBootstrap) {
			System::out.println("The SRE was started");
		}
		override sreStopped(bs : SREBootstrap) {
			System::out.println("The SRE was stopped");
		}
	}
[:End:]


Then, you could register the listener on the SRE as illustrated below:

[:Success:]
	package io.sarl.docs.bootstrap
	import io.sarl.bootstrap.SRE
	import io.sarl.bootstrap.SREBootstrap
	import io.sarl.bootstrap.SREListener
	class MyListener implements SREListener {
		override sreStarted(bs : SREBootstrap) {}
		override sreStopped(bs : SREBootstrap) {}
	}
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var listener = new MyListener
			
			var bootstrap = SRE::getBootstrap
			
			bootstrap.[:addSREListenerFct](addSREListener)(listener)
			
			bootstrap.startWithoutAgent

			[:Off]
			bootstrap.[:removeSREListenerFct](removeSREListener)(listener)
		}
	}
[:End:]


Please note that if you register the listener before the start of the SRE, the listener will be notified of the SRE start.
But, if you register the listener when the SRE is already launched, when the SRE start event will not be notified.


[:Include:](../legal.inc)

