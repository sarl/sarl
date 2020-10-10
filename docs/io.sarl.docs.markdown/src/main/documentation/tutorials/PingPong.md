# Agent Communication with the Ping Pong Agents

[:Outline:]

This document describes how to create a simple agent-based application in which agents are
exchanging basic messages.
Before reading this document, it is recommended reading
the [General Syntax Reference](../reference/GeneralSyntax.md).

<div class="bt-download">
<a href="[:githublnk](https://github.com/sarl/sarl/tree/master/contribs/io.sarl.examples/io.sarl.examples.plugin/projects/io-sarl-tutorials-pingpong/src/main/sarl/io/sarl/examples/pingpong)"><img alt="See the code" src="[:sarlUrl!]/images/download-icon.png"/></a>
</div>
The elements that are explained in this tutorial are:

* the definition of an event;
* the definition of an agent;
* the sending of an event occurrence in the default space;
* the receiving of event occurrences; and
* the definition of a _proactive_ behavior: waiting for partners.

The source code related to this tutorial may be found
in the [GitHub of the SARL demos]([:githublnk!]).


## Principle of the Application

The principle of the application is the following:

* The `Ping` agent is sending a [:pingevent:] message to all agents. 
* The [:pongagent:] agent is receiving the [:pingevent:] message, and replies with a [:pongevent:] message to the sender of the [:pingevent:] message.
* The `Ping` agent is receiving a [:pongevent:] message and replies to the sender of the [:pongevent:] with a new [:pingevent:] message.


These messages contain an integer number that indicates the number of the event.


![Ping-Pong Example](./pingpong.png)

## Event definition

First, the [:pingevent:] and [:pongevent:] events must be defined.

### Ping Event

The [:pingevent:] is an event that contains the index of the event. This index indicates
at which position the event is located in the sequence of sent [:pingevent:] event.

The [:indexvar:] attribute is a _value_, for making it unmodifiable after its initialization.

For setting the value of the [:indexvar:] value, it is mandatory to define a constructor. 

[:Success:]
	package io.sarl.docs.tutorials.pingpong
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
	package io.sarl.docs.tutorials.pingpong
	[:On]event [:pongevent](Pong) {
		val index : int
		new(i : int) {
			this.index = i
		}
	}
[:End:]


## Pong agent

The second step of this tutorial is the definition of the
agent that is waiting for [:pingevent:] events, and replying
[:pongevent:] events.


### First definition

The initial definition of the pong agent is:

[:Success:]
	package io.sarl.docs.tutorials.pingpong
	[:On]agent [:pongagent](PongAgent) {
	}
[:End:]


### Handling the Ping event

The pong agent needs to handle the [:pingevent:] events.
For that, a "behavior unit" must be defined in the
agent. According to the 
[Agent Reference](../reference/Agent.md),
the [:onkw:] keyword followed by the name of the event 
permits to define a handler of events.
This handler will be invoked by the runtime environment
each time the agent is receiving a [:pingevent:] event.

[:Success:]
	package io.sarl.docs.tutorials.pingpong
	event Ping {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	[:On]agent PongAgent {
		[:onkw](on) Ping {
		}
	}
[:End:]


### Replying to Ping with a Pong

Now, it is time to define how the pong agent is replying with a [:pongevent:] message.

First, sending an event in the default space must be done with a built-in capacity:
[:intercap:]. This capacity provides a collection of functions that 
enable the agent to interact with the default context, and its default space.

For using the capacity, it is recommended declaring it with the [:useskw:] keyword.
This keyword permits the agent to directly call the functions of the capacity as if
they were defined as actions in the agent.

The [:intercap:] capacity provides the function `emit(Event)` for
emitting an event in the default space of the default context.
[:Fact:]{typeof(io.sarl.lang.core.Event)}
[:Fact:]{typeof(io.sarl.core.DefaultContextInteractions).shouldHaveMethod("emit(io.sarl.lang.core.Event, io.sarl.lang.core.Scope)")}

The [:pongevent:] event must be built with an index value as argument. This argument
is the index stored in the [:pingevent:] event. For accessing the occurrence of the
[:pingevent:] event, you must use the special keyword [:occurrencekw:].
In the following example, the [:pongevent:] event is built with the index argument
stored in the received [:pingevent:] event.

[:Success:]
	package io.sarl.docs.tutorials.pingpong
	import io.sarl.core.DefaultContextInteractions
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
		[:useskw](uses) [:intercap](DefaultContextInteractions)
		on Ping {
			emit( new Pong( [:occurrencekw](occurrence).index ) )
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

If you have to scope to a single address or a single identifier, you should use the lambda expression notation,
as illustrated in the following code. In this code, the scope permits to restrict to the initial sender
of the [:pingevent:] event. 

[:Success:]
	package io.sarl.docs.tutorials.pingpong
	import io.sarl.core.DefaultContextInteractions
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
		uses DefaultContextInteractions
		on Ping {
			emit(new Pong( occurrence.index ))
				[ it == occurrence.source ]
		}
	}
[:End:]


## Ping Agent

The third step of this tutorial is the definition of the agent that is sending [:pingevent:] events, and waiting for
[:pingevent:] events.


### First definition

The initial definition of the ping agent is:

[:Success:]
	package io.sarl.docs.tutorials.pingpong
	[:On]agent PingAgent {
	}
[:End:]


### Handling the Pong event

The ping agent needs to handle the [:pongevent:] events. For that, a "behavior unit" must be defined in the
agent.

[:Success:]
	package io.sarl.docs.tutorials.pingpong
	event Pong {
		val index : int
		new (i : int) {
			this.index = i
		}
	}
	[:On]agent PingAgent {
		on Pong {
		}
	}
[:End:]


### Re-sending a Ping when receiving a Pong

When the ping agent is receiving a [:pongevent:] event, it re-sends a [:pingevent:] event
to the sender of the [:pingevent:] event.
This new [:pingevent:] event has an index greater than the one of the [:pongevent:] event.

The receiving of the [:pingevent:] event is restricted to the sender of the
[:pongevent:] event.

[:Success:]
	package io.sarl.docs.tutorials.pingpong
	import io.sarl.core.DefaultContextInteractions
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
		uses DefaultContextInteractions
		on Pong {
			emit(new Ping( occurrence.index + 1 ))
				[ it == occurrence.source ]
		}
	}
[:End:]


### Sending the first Ping

For starting the exchanges among the agents, it is mandatory to send a first occurrence
of the [:pingevent:] event.

This emit is done when the ping agent is started, i.e. when the agent is
receiving the [:initevent:] event.

[:Success:]
	package io.sarl.docs.tutorials.pingpong
	import io.sarl.core.DefaultContextInteractions
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
		uses DefaultContextInteractions
		on Pong {
			emit(new Ping( occurrence.index + 1 ))
				[ it == occurrence.source ]
		}
		on [:initevent](Initialize) {
			emit( new Ping(0) )
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
This task is executed every second with the [:every:] function (given by the [:schedcap:]
capacity). The code between the brackets contains the statements
that will be periodically executed.

In this periodically executed code, the agent is testing if it is the only
one agent belonging to the default space. If not, the agent is sending the initial
[:pingevent:] event, and stopping the periodic task.

[:Success:]
	package io.sarl.docs.tutorials.pingpong
	import io.sarl.core.DefaultContextInteractions
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
		uses DefaultContextInteractions, [:schedcap](Schedules)
		on Pong {
			emit(new Ping( occurrence.index + 1 ))
				[ it == occurrence.source ]
		}
		on Initialize {
			val task = task("[:taskname](waiting_for_partner)")
			task.[:every](every)(1000) [
				if (defaultSpace.numberOfStrongParticipants > 1) {
					emit( new Ping(0) )
					task.cancel
				}
			]
		}
	}
[:End:]


## Launch the agents

The fourth step of this tutorial is the definition of the launching process.
In the rest of this section, we discuss the use of the
[Janus runtime environment](http://www.janusproject.io) for running the agents.
The Janus platform is designed to launch a single agent at start-up.
Then, this launched agent must spawn the other agents in the system.


The principle is to launch a single instance of Janus, and run all the agents inside.
Because of the design of the Janus platform, we must define an
agent that will launch the other agents. This agent is named
[:bootagent:]. It is defined below.


The boot agent uses the [:lifecyclecap:] capacity for launching agents in the default context.
This capacity provides the function `spawn(Class<? extends Agent>)`
for launching an agent of the given type.
When the boot agent has launched the two expected agents,
it is killing itself. This is done with the [:killmefct:]
function, which is provided by the [:lifecyclecap:] capacity too.
[:Fact:]{typeof(io.sarl.core.Lifecycle).shouldHaveMethod("spawn(java.lang.Class, java.lang.Object[])")}
[:Fact:]{typeof(io.sarl.core.Lifecycle).shouldHaveMethod("killMe")}

[:Success:]
	package io.sarl.docs.tutorials.pingpong
	import io.sarl.core.Initialize
	import io.sarl.core.Lifecycle
	agent PongAgent { }
	agent PingAgent { }
	[:On]agent [:bootagent](BootAgent) {
		uses [:lifecyclecap](Lifecycle)
		on Initialize {
			spawn(PongAgent)
			spawn(PingAgent)
			[:killmefct](killMe)
		}
	}
[:End:]


[:Include:](../legal.inc)
