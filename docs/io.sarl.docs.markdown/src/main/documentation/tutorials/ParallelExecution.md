# Parallel execution within the agents

[:Outline:]

This document describes the key features of SARL that are run in parallel on the SARL Runtime Environment (SRE).
Before reading this document, it is recommended reading
the [General Syntax Reference](../reference/GeneralSyntax.md).

Each SRE provides a support for running the agents. Depending on the specifications of the SRE, the
parallel execution of the agent's components may be used at different places.
Nevertheless, according the SARL language's specifications, several features are assumed to be run in parallel.
They are briefly explained below. 

## Event Firing, Dispatching and Handling

In SARL, event-based communication is the interaction mechanism that is provided by default.
Firing an event is done within an interaction space by calling one of the dedicated function that are defined within
the [`[:defaultcontextinteractions](DefaultContextInteractions)`](../reference/bic/DefaultContextInteractions.md),
[`[:defaultcontextinteractions](ExternalContextAccess)`](../reference/bic/ExternalContextAccess.md) and
[`[:defaultcontextinteractions](InnerContextAccess)`](../reference/bic/InnerContextAccess.md) capacities.

The event firing mechanism could be divided into three steps:
1. Event firing: the event is fired by a source object;
2. Event dispatching: the event is routed to the agents that should receive the event; and
3. Event handling: the event is provided to each agent, and the defined event handlers (named behavior units) are run.

Each of these steps are basically run in different threads.
Let the following code:

[:Success:]
	package io.sarl.docs.tutorials.parallelexecution
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	agent PongAgent { }
	agent PingAgent { }
	event MyEvent
	[:On]agent MyAgent {
		uses DefaultContextInteractions, Logging

		[:initializeblock](on Initialize) {
			[:emit](emit)(new MyEvent)
			info([:msg1]("Event sent"))
		}

		[:myeventblock](on MyEvent) {
			info([:msg2]("Event received"))
		}
	}
[:End:]


The call to [:emit:] is run within the thread of the calling block, i.e. [:initializeblock:].
The event is provides to the SRE, that is routing this event within a dedicated "hidden" thread.
Consequently, the call to [:emit:] returns quickly. And, there is no warranty that the event's routing
is started nor terminated when the function returns. 

In order to allow the parallel treatment of the events by an agent, each event handler, e.g. [:myeventblock:]
is run in a dedicated thread.

> **_Caution:_** In the previous example, there is no warranty about the order of printing of the two messages.
> Because of the parallel execution of the threads, the [:msg2:] message may be displayed before the [:msg1:] message.

## Agent Spawning

Agent spawning is the action to create and start an agent from another agent.
The spawning function is provided by the 
[`[:Lifecycle](Lifecycle)`](../reference/bic/Lifecycle.md) capacity.

The agent spawning process is divided into several steps:
1. Call of the spawning function;
2. Creation of the agent within the computer memory;
3. SRE-specific initialization of the agent capacities and internal fields;
4. Synchronous execution of the [:initializeblock] of the agent;
5. Firing of the [:agentspawnedevent](AgentSpawned) event.

Step 1 is run within the thread of the caller.
Steps 2 to 5 are run within an internal thread of the SRE.

Let the following code:

[:Success:]
	package io.sarl.docs.tutorials.parallelexecution
	import io.sarl.core.Lifecycle
	import io.sarl.core.Logging
	import io.sarl.core.AgentSpawned
	import io.sarl.core.Initialize
	agent PongAgent { }
	agent PingAgent { }
	event MyEvent
	[:On]agent MyAgent {
		uses Lifecycle, Logging

		[:initializeblock](on Initialize) {
			spawn(typeof(MyAgent2))
			info([:msg1]("Spawn query called"))
		}

		[:agentspawnedblock](on AgentSpawned) {
			info([:msg2]("Agent was spawned"))
		}
	}
	agent MyAgent2 {
		uses Logging

		[:initializeblock!] {
			info([:msg3]("Do initialization"))
		}
	}
[:End:]

The [:msg3:] message is always logged before the [:msg2:] message because the executed code corresponds to
steps 5 and 4, respectively. These steps are run on the same thread.

But, there is no warranty about when the [:msg3:] message is logged. According to the parallel execution,
it may be logged at any time. Consequently, the possible output cases are:
* [:msg1:], [:msg3:], [:msg2:]
* [:msg3:], [:msg1:], [:msg2:]
* [:msg3:], [:msg2:], [:msg1:]


## Agent Killing

Agent killing is the action to stop and destroy an agent.
The killing function is provided by the 
[`[:Lifecycle](Lifecycle)`](../reference/bic/Lifecycle.md) capacity.

The agent killing process is divided into several steps:
1. Call of the killing function (possibly with the abnormal termination cause);
2. Synchronous execution of the [:destroyblock] of the agent;
3. Destruction of the agent within the SRE;
4. Firing of the [:agentdestroyevent](AgentKilled) event (possibly with the abnormal termination cause).

Step 1 is run within the thread of the caller.
Steps 2 to 4 are run within an internal thread of the SRE.

Let the following code:

[:Success:]
	package io.sarl.docs.tutorials.parallelexecution
	import io.sarl.core.Lifecycle
	import io.sarl.core.Logging
	import io.sarl.core.AgentKilled
	import io.sarl.core.Initialize
	import io.sarl.core.Destroy
	agent PongAgent { }
	agent PingAgent { }
	event MyEvent
	[:On]agent MyAgent {
		uses Lifecycle, Logging

		[:initializeblock](on Initialize) {
			killMe
		}

		[:destroyblock](on Destroy) {
			info([:msg1a]("Do destruction"))
		}

		[:agentspawnedblock](on AgentKilled) {
			info([:msg2b]("Agent was killed"))
		}
	}
[:End:]


The [:msg2b:] message is always logged after the [:msg1a:] message because the executed code corresponds to
steps 4 and 3, respectively. These steps are run on the same thread.


[:Include:](../legal.inc)
