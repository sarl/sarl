# Agent Definition Introduction

[:Outline:]

To create our first agent, right click on the project and follow **New > File**. Name the file `myproject.sarl`.

The SARL default editor will open.

## Basic agent definition

Agents are defined using the [:agentkw:] keyword.

[:Success:]
	[:packagekw](package) [:package](io.sarl.docs.gettingstarted.[:hat]$^$agent)
	[:On][:agentkw](agent) MyAgent {
	}[:Off]
	agent SecondAgent {
	}
[:End:]


## Package definition

SARL elements are organized in packages. You can define the package using the [:packagekw:] keyword.

The following code will define an agent with a fully qualified name of [:package:].
The character [:hat:] in the package name permits to use a SARL keyword into a package name.

> **_Important Note:_** The package keyword defines the package for all elements in the same SARL file
> (see the [General Syntax Reference](../reference/GeneralSyntax.md) for details).
> Therefore FirstAgent and SecondAgent belong to the same package, i.e. [:package:].

## Agent Perceptions

Agents need to perceive their environment in order to react to external stimuli. Perceptions take the form of events
(see [Event](../reference/Event.md) and [Agent](../reference/Agent.md) References for details).

### Declare an Event

To declare a new event use the [:eventkw:] keyword. The following code defines a new event [:myevent:].

[:eventdecl:]

### Define an agent Perceptions

Now, we will want our agent to react to [:myevent:] and print a message on the console.

To define this event handler, we must use the [:onkw:] keyword, and provide the associated code block.

> **_Note:_** The [:println:] function is provided by the [:logging:] capacity. It permits printing a message on the log output.

[:Success:]
	package io.sarl.docs.gettingstarted.^agent
	[:On]import io.sarl.core.[:logging](Logging)
	[:Off][:eventdecl]$[:eventkw](event) [:myevent](MyEvent)$
	[:On]agent MyAgent {
		uses Logging
		[:onkw](on) MyEvent {
			[:println](println)("Received MyEvent")
		}
	}
[:End:]


### Lifecycle events

SARL defines two **lifecycle** events :

* [:initialize:]:  Notifies the creation of the agent, and passes the initialization parameters to the agents.
* [:destroy:]: Notifies the destruction of the agent.

This means that when agent has been spawned and it is ready to begin its execution, it will receive an [:initialize:] event.
You can react to this event just like with any other event defined in SARL.

Likewise, when the agent is going to stop its execution (we will see how to stop an agent later on), it will receive
a [:destroy:] Event. The purpose of this event is to release any system resource properly.

[:Success:]
	package io.sarl.docs.gettingstarted.^agent
	[:On]import io.sarl.core.Logging
	import io.sarl.core.Initialize
	import io.sarl.core.Destroy
	agent MyAgent {
		uses Logging

		on [:initialize](Initialize) {
			println("MyAgent spawned")
		}

		on [:destroy](Destroy) {
			println("MyAgent destroyed")
		}
	}
[:End:]


### Accessing the event's occurrence

Inside a behavior declaration you may need to access the event instance the agent is reacting to.

This instance is called an [:occurrence:].

In the case of an Initialize events you can access the arguments for the agent spawn using [:occurrence.parameters:]).

[:Success:]
	package io.sarl.docs.gettingstarted.^agent
	[:On]import io.sarl.core.Logging
	import io.sarl.core.Initialize
	import io.sarl.core.Destroy
	agent MyAgent {
		uses Logging

		on Initialize {
			println("MyAgent spawned")
			println("My Parameters are :" + [:occurrence.parameters]([:occurrence]$occurrence$.parameters).toString)
		}

		on Destroy {
			println("MyAgent destroyed")
		}
	}
[:End:]


## Agent Communication

Agents need to send data and stimuli to other agents. This communication takes the form of event sending
(see [Event](../reference/Event.md) and [Agent](../reference/Agent.md) References for details).

### Use the capacity to send an event in the default space

Now, we will want our agent to send data to other agents. The data are embedded into events. The definition of an
event is described above.

> **_Note:_** In this document, we limit our explanation to the sending of the events in the default space of the default context
> of the agent.

For sending an event in the default space, the [:dci:] built-in capacity should be used.

Below, we define an agent that is using this capacity.

[:Success:]
	package io.sarl.docs.gettingstarted.^agent
	[:On]import io.sarl.core.DefaultContextInteractions
	agent MyAgent {
		uses [:dci](DefaultContextInteractions)
	}
[:End:]


### Send an event in the default space

The [:dci:] built-in capacity provides functions for sending events in the default space.

Below, we define an action in which an instance of [:myevent2:] is created, and then sent into the default space with the function
call [:emit:].

[:Success:]
	package io.sarl.docs.gettingstarted.^agent
	import io.sarl.core.DefaultContextInteractions
	event MyEvent
	[:On]agent MyAgent {
		uses DefaultContextInteractions
		def doSomething {
			var e = new [:myevent2](MyEvent)
			[:emit]{emit(e)}
		}
	}
[:End:]


## What's next?

In the next section, we will learn how to start a SARL agent in the Eclipse IDE.

[Next>](./RunSARLAgentEclipse.md)

[:Include:](../legal.inc)
