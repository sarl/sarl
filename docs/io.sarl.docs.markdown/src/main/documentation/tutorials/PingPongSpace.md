# Agent Communication in Sub-Space with the Ping Pong Agents

[:Outline:]

This document describes how to create a simple agent-based application in which agents are
exchanging basic messages inside a sub-space.
Before reading this document, it is recommended reading the
[General Syntax Reference](../reference/GeneralSyntax.md).

<div class="bt-download">
<a href="[:githublnk](https://github.com/sarl/sarl/tree/master/contribs/io.sarl.examples/io.sarl.examples.plugin/projects/io-sarl-tutorials-pingpongspace/src/main/sarl/io/sarl/examples/pingpongspace)"><img alt="See the code" src="[:sarlUrl!]/images/download-icon.png"/></a>
</div>
The elements that are explained in this tutorial are:

* the definition of an event;
* the definition of an agent;
* the creation of a sub-space;
* the sending of an event occurrence in the default space;
* the receiving of event occurrences; and
* the definition of a _proactive_ behavior: waiting for partners.

The source code related to this tutorial may be found in the
[GitHub of the SARL demos]([:githublnk!]).


## Principle of the Application

The principle of the application is the following:

* The agents are joining a sub-space given at initialization. 
* The [:pingagent:] agent is sending a [:pingevent:] message to all agents into the sub-space. 
* The [:pongagent:] agent is receiving the [:pingevent:] message, and replies  with a [:pongevent:] message to the sender of the [:pingevent:] message.
* The [:pingagent:] agent is receiving a [:pongevent:] message and replies to the sender of the [:pongevent:] with a new [:pingevent:] message.

These messages contain an integer number that indicates the number of the event.

![Ping-Pong Example](./pingpongspace.png)


## Event definition

First, the [:pingevent:] and [:pongevent:] events must be defined.


### Ping Event

The [:pingevent:] is an event that contains the index of the event. This index indicates
at which position the event is located in the sequence of sent [:pingevent:] event.

The [:indexvar:] attribute is a _value_, for making it unmodifiable after its initialization.

For setting the value of the [:indexvar:] value, it is mandatory to define a constructor. 

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	event [:pingevent](Ping) {
		val [:indexvar](index) : int
		new(i : int) {
			this.index = i
		}
	}
[:End:]


### Pong Event

The [:pongevent:] is an event that contains the index of the [:pingevent:] event for which the
[:pongevent:] event is created.

The [:indexvar:] attribute is also a _value_, and it must be set in a constructor. 

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	[:On]event [:pongevent](Pong) {
		val index : int
		new(i : int) {
			this.index = i
		}
	}
[:End:]


## Pong agent

The second step of this tutorial is the definition of the agent that is waiting
for [:pingevent:] events, and replying [:pingevent:] events.

### First definition

The initial definition of the pong agent is:

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	[:On]agent [:pongagent](PongAgent) {
	}
[:End:]


### Join the sub-space

Because the agents are interacting into a sub-space, they must join this sub-space at start-up.

The sub-space is located in the default context. For creating or joining it, we must use the
[:contextcreator:] function. This function searches for a space, which was
created with the given specification. If there is no space, the space identifier is used
for creating a new space.

After retrieving the instance of the space, it is mandatory to register the agent for
receiving the events. The spaces of type `OpenEventSpaceSpecification` provides
the [:registerfct:] function. It takes the event listener of the agent (provided by
the [:behaviorscap:] capacity).
[:Fact:]{typeof(io.sarl.core.OpenEventSpaceSpecification)}

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.Initialize
	import io.sarl.core.Behaviors
	import java.util.UUID
	import io.sarl.core.OpenEventSpace
	import io.sarl.core.OpenEventSpaceSpecification
	[:On]agent PongAgent {
		
		uses DefaultContextInteractions, [:behaviorscap](Behaviors)
	
		var ^space : OpenEventSpace
		
		on Initialize {
			^space = defaultContext.[:contextcreator](getOrCreateSpaceWithSpec)(
				typeof(OpenEventSpaceSpecification),
				occurrence.parameters.get(0) as UUID)
			^space.[:registerfct](registerStrongParticipant)(asEventListener())
		}
	}
[:End:]


### Handling the Ping event

The pong agent needs to handle the [:pingevent:] events. For that, a "behavior unit" must be defined in the
agent. According to the  [Agent Reference](../reference/Agent.md),
the [:onkw:] keyword followed by the name of the event permits to define a handler of events.
This handler will be invoked by the runtime environment each time the agent is
receiving a [:pingevent:] event.

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.Behaviors
	import java.util.UUID
	import io.sarl.core.OpenEventSpace
	import io.sarl.core.OpenEventSpaceSpecification
	import io.sarl.core.Initialize
	event Ping {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	[:On]agent PongAgent {
		
		uses DefaultContextInteractions, Behaviors
	
		var ^space : OpenEventSpace
		
		[:onkw](on) Initialize {
			^space = defaultContext.getOrCreateSpaceWithSpec(
				typeof(OpenEventSpaceSpecification),
				occurrence.parameters.get(0) as UUID)
			^space.registerStrongParticipant(asEventListener())
		}

		on Ping {
		}
	}
[:End:]


### Replying to Ping with a Pong

Now, it is time to define how the pong agent is replying with a [:pingevent:] message.

First, sending an event in the default space must be done with a built-in capacity:
[:intercap:]. This capacity provides a collection of functions that 
enable the agent to interact with the default context, and its default space.

For using the capacity, it is recommended declaring it with the [:useskw:] keyword.
This keyword permits the agent to directly call the functions of the capacity as if
they were defined as actions in the agent.

The [:intercap:] capacity provides the function `emit(Event)` for
sending an event in the default space of the default context.
[:Fact:]{typeof(io.sarl.core.DefaultContextInteractions).shouldHaveMethod("emit(io.sarl.lang.core.Event)")}
The [:extercap:] capacity provides the function `emit(EventSpace, Event)` for
sending an event in the given space.
[:Fact:]{typeof(io.sarl.core.ExternalContextAccess).shouldHaveMethod("emit(io.sarl.lang.core.EventSpace,io.sarl.lang.core.Event)")}
This latest function is used for sending the events.

The [:pingevent:] event must be built with an index value as argument. This argument
is the index stored in the [:pingevent:] event. For accessing the occurrence of the
[:pingevent:] event, you must use the special keyword [:occkw:].
In the following example, the [:pongevent:] event is built with the index argument
stored in the received [:pingevent:] event.

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.ExternalContextAccess
	import io.sarl.core.Behaviors
	import java.util.UUID
	import io.sarl.core.OpenEventSpace
	import io.sarl.core.OpenEventSpaceSpecification
	import io.sarl.core.Initialize
	event Ping {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	event Pong {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	[:On]agent PongAgent {
		
		[:useskw](uses) [:intercap](DefaultContextInteractions), [:extercap](ExternalContextAccess), Behaviors
	
		var ^space : OpenEventSpace
		
		on Initialize {
			^space = defaultContext.getOrCreateSpaceWithSpec(
				typeof(OpenEventSpaceSpecification),
				[:occkw](occurrence).parameters.get(0) as UUID)
			^space.registerStrongParticipant(asEventListener())
		}

		on Ping {
			var evt = new Pong( occurrence.index )
			^space.emit( evt )
		}
	}
[:End:]


### Restricting the scope of the Pong event

In the previous code, the event is emitted to all the agents belonging to the default
space, including the pong agent.

For restricting the receiver of the [:pongevent:] event to the initial sender of the
[:pingevent:] event, you must define a scope for the [:pongevent:] event.
The [:intercap:] capacity provides the function `emit(Event, Scope<Address>)`
for sending an event with a specific scope.
[:Fact:]{typeof(io.sarl.lang.core.Event)}
[:Fact:]{typeof(io.sarl.lang.core.Scope)}
[:Fact:]{typeof(io.sarl.lang.core.Address)}
[:Fact:]{typeof(io.sarl.core.DefaultContextInteractions).shouldHaveMethod("emit(io.sarl.lang.core.Event, io.sarl.lang.core.Scope)")}

In the following code, we select the receiver of an event based on its address within the space.
It permits to restrict to the initial sender of the [:pingevent:] event: [:scopingcode:]

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.ExternalContextAccess
	import io.sarl.core.Behaviors
	import java.util.UUID
	import io.sarl.core.OpenEventSpace
	import io.sarl.core.OpenEventSpaceSpecification
	import io.sarl.core.Initialize
	event Ping {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	event Pong {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	[:On]agent PongAgent {
		uses DefaultContextInteractions, ExternalContextAccess, Behaviors
	
		var ^space : OpenEventSpace
		
		on Initialize {
			^space = defaultContext.getOrCreateSpaceWithSpec(
				typeof(OpenEventSpaceSpecification),
				occurrence.parameters.get(0) as UUID)
			^space.registerStrongParticipant(asEventListener())
		}

		on Ping {
			var evt = new Pong( occurrence.index )
			^space.emit(evt) [:scopingcode]{[ it == occurrence.source ]}
		}
	}
[:End:]


## Ping Agent

The third step of this tutorial is the definition of the agent that is sending [:pingevent:]
events, and waiting for [:pongevent:] events.


### First definition

The initial definition of the ping agent is:

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.Initialize
	import io.sarl.core.Behaviors
	import java.util.UUID
	import io.sarl.core.OpenEventSpace
	import io.sarl.core.OpenEventSpaceSpecification
	[:On]agent [:pingagent](PingAgent) {
		
		uses DefaultContextInteractions, Behaviors
	
		var ^space : OpenEventSpace
		
		on Initialize {
			^space = defaultContext.getOrCreateSpaceWithSpec(
				typeof(OpenEventSpaceSpecification),
				occurrence.parameters.get(0) as UUID)
			^space.registerStrongParticipant(asEventListener())
		}
	}
[:End:]


### Handling the Pong event

The ping agent needs to handle the [:pongevent:] events. For that, a "behavior unit" must be
defined in the agent.

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.Behaviors
	import java.util.UUID
	import io.sarl.core.OpenEventSpace
	import io.sarl.core.OpenEventSpaceSpecification
	import io.sarl.core.Initialize
	event Pong {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	[:On]agent PingAgent {
		
		uses DefaultContextInteractions, Behaviors
	
		var ^space : OpenEventSpace
		
		on Initialize {
			^space = defaultContext.getOrCreateSpaceWithSpec(
				typeof(OpenEventSpaceSpecification),
				occurrence.parameters.get(0) as UUID)
			^space.registerStrongParticipant(asEventListener())
		}

		on Pong {
		}
	}
[:End:]


### Re-sending a Ping when receiving a Pong

When the ping agent is receiving a [:pongevent:] event, it re-sends a
[:pingevent:] event to the sender of the [:pongevent:] event.
This new [:pingevent:] event has an index greater than the one of the
[:pongevent:] event.

The receiving of the [:pingevent:] event is restricted to the sender of the
[:pongevent:] event.

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.ExternalContextAccess
	import io.sarl.core.Behaviors
	import java.util.UUID
	import io.sarl.core.OpenEventSpace
	import io.sarl.core.OpenEventSpaceSpecification
	import io.sarl.core.Initialize
	event Ping {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	event Pong {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	[:On]agent PingAgent {
		
		uses DefaultContextInteractions, ExternalContextAccess, Behaviors
	
		var ^space : OpenEventSpace
		
		on Initialize {
			^space = defaultContext.getOrCreateSpaceWithSpec(
				typeof(OpenEventSpaceSpecification),
				occurrence.parameters.get(0) as UUID)
			^space.registerStrongParticipant(asEventListener())
		}

		on Pong {
			var evt = new Ping( occurrence.index + 1 )
			^space.emit(evt) [ it == occurrence.source ]
		}
	}
[:End:]


### Sending the first Ping

For starting the exchanges among the agents, it is mandatory to send a first occurrence
of the [:pingevent:] event.

This emit is done when the ping agent is started, i.e. when the agent is
receiving the [:initevent:] event.

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.ExternalContextAccess
	import io.sarl.core.Behaviors
	import java.util.UUID
	import io.sarl.core.OpenEventSpace
	import io.sarl.core.OpenEventSpaceSpecification
	import io.sarl.core.Initialize
	event Ping {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	event Pong {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	[:On]agent PingAgent {
		
		uses DefaultContextInteractions, ExternalContextAccess, Behaviors
	
		var ^space : OpenEventSpace
		
		on [:initevent](Initialize) {
			^space = defaultContext.getOrCreateSpaceWithSpec(
				typeof(OpenEventSpaceSpecification),
				occurrence.parameters.get(0) as UUID)
			^space.registerStrongParticipant(asEventListener())
			var evt = new Ping(0)
			^space.emit( evt )
		}

		on Pong {
			var evt = new Ping( occurrence.index + 1 )
			^space.emit(evt) [ it == occurrence.source ]
		}
	}
[:End:]


### Delaying the sending of the first Ping

The previous code has a major problem: if there is no pong agent launched
when the ping agent is sending the first [:pingevent:] event, the application
will reach a deadlock, even if the pong agent is launched later.

For solving this problem, the ping agent must wait for sending the initial
[:pingevent:] event until the pong agent is belonging to the default space.

The concrete implementation is based on the [:schedcap:] capacity, which provides
a collection of functions for creating and launching asynchronous tasks.

In the following code, a task is created with the name [:taskname:].
This task is executed every second with the [:everyfct:] function (given by the [:schedcap:]
capacity). The code between the brackets contains the statements
that will be periodically executed.

In this periodically executed code, the agent is testing if it is the only
one agent belonging to the default space. If not, the agent is sending the initial
[:pingevent:] event, and stopping the periodic task.

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.ExternalContextAccess
	import io.sarl.core.Behaviors
	import java.util.UUID
	import io.sarl.core.OpenEventSpace
	import io.sarl.core.OpenEventSpaceSpecification
	import io.sarl.core.Initialize
	import io.sarl.core.Schedules
	event Ping {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	event Pong {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	[:On]agent PingAgent {
		
		uses DefaultContextInteractions, ExternalContextAccess, Behaviors, [:schedcap](Schedules)
	
		var ^space : OpenEventSpace
		
		on Initialize {
			^space = defaultContext.getOrCreateSpaceWithSpec(
				typeof(OpenEventSpaceSpecification),
				occurrence.parameters.get(0) as UUID)
			^space.registerStrongParticipant(asEventListener())
			val task = task("[:taskname](waiting_for_partner)")
			task.[:everyfct](every)(1000) [
				if (defaultSpace.numberOfStrongParticipants > 1) {
					var evt = new Ping(0)
					^space.emit( evt )
					task.cancel
				}
			]
		}

		on Pong {
			var evt = new Ping( occurrence.index + 1 )
			^space.emit(evt) [ it == occurrence.source ]
		}
	}
[:End:]


## Launch the agents

The fourth step of this tutorial is the definition of the launching process.
In the rest of this section, we discuss the use of the
[Janus runtime environment](http://www.janusproject.io)
for running the agents.
The Janus platform is designed to launch a single agent at start-up.
Then, this launched agent must spawn the other agents in the system.


The principle is to launch a single instance of Janus, and run all the agents inside.
Because of the design of the Janus platform, we must define an agent that will launch
the other agents. This agent is named [:bootagent:]. It is defined below.


The boot agent uses the [:lifecyclecap:] capacity for launching agents in the default context.
This capacity provides the function `spawn(Class<? extends Agent>)` for launching an
agent of the given type. When the boot agent has launched the two expected agents,
it is killing itself. This is done with the [:killme:] function, which is provided
by the [:lifecyclecap:] capacity too.

[:Success:]
	package io.sarl.docs.tutorials.pingpongspace
	import io.sarl.core.Initialize
	import io.sarl.core.Lifecycle
	agent PongAgent { }
	agent PingAgent { }
	[:On]agent [:bootagent](BootAgent) {
		uses [:lifecyclecap](Lifecycle)
		on Initialize {
			spawn( PongAgent )
			spawn( PingAgent )
			[:killme](killMe)
		}
	}
[:End:]


[:Include:](../legal.inc)
