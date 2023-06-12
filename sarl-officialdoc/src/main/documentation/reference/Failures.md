# Management of the Failures and Validation Errors

As for the [Akka actor framework](https://doc.akka.io/docs/akka/2.5/typed/fault-tolerance.html), there is an important distinction between failures and validation errors:

* A validation error means that the data of a command sent to an agent is not valid, e.g. an unexpected event according to a communication protocol. This should rather be modelled as a part of the agent protocol than make the agent throw exceptions.

* A failure is instead something unexpected or outside the control of the agent itself, for example a database connection that broke. Opposite to validation errors, it is seldom useful to model such as parts of the protocol as a sending agent very seldom can do anything useful about it.

This reference page describes the basics of the management of these two types of problems from SARL agent-oriented programming point of view.

> **_Note:_** SARL provides standard programming tools for handling the run-time problems: the [exceptions](./general/Exception.md) and the [assertions](./general/Assertion.md). These tools are very interesting from the programming point of view. However, they are not specific to agent programming (they already exist in main stream languages such as Java).


## Fault Handling through Events

As SARL has adopted an event-based programming philosophy, most of the fault handling that is not directly supported by the [exception catching mechanism](./general/Exception.md) is based on the definition of events and the corresponding handlers.


### General Framework

The SARL API defines a specific [event](./Event.md) that is representing any failure or validation error that the agent could handle if it is interested by: [:failurefulltype:].
Each time an agent needs to be notified about a fault (into its agent tasks for example), an occurrence of this event type is fired in the internal context of the agent.

There is plenty of causes for a validation error or a failure. Since the [:failuretype:] event type is general, it is possible to refine its meaning by extending it with an event sub-type, e.g.:

[:Success:]
	import [:failurefulltype]$io.sarl.core.[:failuretype](Failure)$
	[:On]
	event MyAgentPersonalFailure extends Failure
[:End:]

The definition of these new types of events following the general rules for the [event definition](./Event.md).
As for all the SARL events, they must be fired into a [space](./Space.md) for being processed by an agent.
 


### User-Specific Failures

It is still possible for you to define your own failure events. You only need to define a sub-type of [:failuretype:].
For example, the following code define the [:myfailureevent:] event:

[:Success:]
	import io.sarl.core.Failure
	[:On]
	event [:myfailureevent](MyFailure) extends Failure
[:End:]

According to the SARL capabilities for [defining the events](./Event.md), you could add your own attributes in this new event.

Usually, a failure event is fired into the internal context of the agent, using the [:wakefct:] function that is provided
by the [:behaviorcapacity:] capacity. For example:

[:Success:]
	import io.sarl.core.Failure
	import io.sarl.core.Behaviors
	[:On]
	event MyFailure extends Failure {
		var mydata : String
	}
	agent MyAgent {
		uses [:behaviorcapacity](Behaviors)
		def aFunctionInMyAgent {
			// Build the failure
			var failure = new MyFailure("this is the cause of the failure")
			failure.mydata = "This my additional data"
			// Fire the failure
			[:wakefct](wake)(failure)
		}
	}
[:End:]
 


### Killing Agent with Abnormal Termination Cause 

An agent may be destroyed due to an internal fault. However, according to the SARL metamodel and the implementation choices of the SARL Run-time Environment, if a failure or an error occured into the agent, only the associated failing task is broken. The agent is still alive and may react to over events.

In order to be killed, an agent has to invoke the [:killmefct:] function from the [:lifecyclecapacity:] built-in capacity, as illustrated below:

[:Success:]
	import io.sarl.core.Lifecycle
	event MyEvent
	[:On]
	agent MyAgent {
		uses [:lifecyclecapacity](Lifecycle)
		on MyEvent {
			[:killmefct](killMe)
		}
	}
[:End:]

The call to the [:killmefct:] function causes the firing of an [:agentkilledevent:] event.

In the case the agent would like to stop its life on a failure, the [:killmefct:] accepts an object as argument that is describing the cause of the termination of the agent:

[:Success:]
	import io.sarl.core.Lifecycle
	event MyEvent
	[:On]
	agent MyAgent {
		uses [:lifecyclecapacity](Lifecycle)
		on MyEvent {
			[:killmefct](killMe)("The reason of my death")
		}
	}
[:End:]

The type of the data that describes the killing reason is application-dependent and up to you.

The reason of the killing of an agent may be retrieved from the [:agentkilledevent:] event:

[:Success:]
	import io.sarl.core.AgentKilled
	import io.sarl.core.Logging
	[:On]
	agent MyOtherAgent {
		uses Logging
		on [:agentkilledevent](AgentKilled) {
			info("Agent " + occurrence.source.ID
				+ " is dead because: "
				+ occurrence.terminationCause)
		}
	}
[:End:]


## Propagating Failures in Holarchy

As described in detail into the [agent reference page](./Agent.md), > agents can be composed of other agents.
Therefore, SARL agents are in fact holons that can compose each other to define hierarchical or recursive
multi-agent system, called holarchies.
The following figure illustrates this hierarchical relationship between the SARL agents.
Agent with the name `A` is the parent of four agents, including those named `B` and `C`.

![Contexts](./contexts.png)

It is then interesting to propagate a fault that occured into an agent to its parent agent, or to one or more
of its child agents.


### Propagating to Parent Agents

The SARL API provides the necessary functions for propagating events to the parent agent.

> **_Important:_** The parent agent is the agent that is owning the default space of the child agent

The function [:emittoparent:], defined into the [:defaultcontextinteraction:] built-in capacity enables to automatically forward an event to the parent agent.
In the following code, the agent `B` forwards automatically the failure events to its parent agent `A`.

[:Success:]
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.core.Failure
	[:On]
	agent AgentB {
		uses [:defaultcontextinteraction](DefaultContextInteractions)
		on Failure {
			[:emittoparent](emitToParent)(occurrence)
		}
	}
[:End:]


### Propagating to Child Agents

These is no specific function provided by the SARL API for forwarding the failure events (or more generally the events) to the child agents.
You must use the standard API for emitting the events into [spaces](./Space.md) (the default space or other spaces).

In the following code, the agent `A` forwards automatically the failure events to its child agents, including `B` and `C`.

[:Success:]
	import io.sarl.core.Behaviors
	import io.sarl.core.Failure
	[:On]
	agent AgentA {
		uses [:innercontextcap](Behaviors)
		on Failure {
			wake(occurrence) [ it.ID != ID ]
		}
	}
[:End:]



## System-Specific Failures

This section describes several specific failures that are already defined into the SARL API.

### Parallel Task Failures

As soon as an agent starts [parallel tasks](./bic/Schedules.md), these tasks may fail.
The SARL API provides a specific failure event sub-type that is describing the cause of a failing task: [:taskfailurefulltype:].

This event is fired each time an exception is thrown into a parallel task.
In addition to the field [:causefield:], the [:taskfailuretype:] event contains the reference to the failing task, accessible with the [:taskfield:] field.

The following code shows an example of the submission of a failing parallel task, and the catching of this failure with a [:taskfailuretype:] event handler.

[:Success:]
	import [:taskfailurefulltype]$io.sarl.core.[:taskfailuretype](TaskFailure)$
	import io.sarl.core.Initialize
	import io.sarl.core.Logging
	import io.sarl.core.Schedules
	import io.sarl.core.AgentTask
	class MyError extends Exception {}
	[:On]
	agent MyAgent {
		uses Logging, Schedules
		on Initialize {
			in(1.seconds) [
				throw new MyError
			]
		}
		on TaskFailure {
			var reason : Object = occurrence.[:causefield](cause)
			var task : AgentTask = occurrence.[:taskfield](task)
			info("Task failed:" + task
				+ " because of: " + reason)
		}
	}
[:End:]


### Failure of Agent Spawn 

In some cases, the spawning of an agent cannot be executed, for example, when an error occured
into the agent initialization event handler.

In order to be notified of the failure of an agent spawn, the spawning agent receives an
occurrence of [:agentspawnfailureevent:].
The following code shows up an event handler that outputs an error message when the
agent spawn action has failed.

[:Success:]
	import io.sarl.core.AgentSpawnFailure
	import io.sarl.core.Lifecycle
	import io.sarl.core.Logging
	agent MyOtherAgent {}
	[:On]
	agent MyAgent {
		uses Lifecycle, Logging
		def aFunction {
			typeof(MyOtherAgent).spawn
		}
		on [:agentspawnfailureevent](AgentSpawnFailure) {
			error("Agent spawning of type " + occurrence.[:spawnfailureagenttypeattr](agentType) + " has failed with the cause: " + occurrence.[:spawnfailurecauseattr](cause))
		}
	}
[:End:]

The [:agentspawnfailureevent:] event provides the following attributes:

* [:spawnfailureagenttypeattr:]: the type of the agent for which a spawn has failed,
* [:spawnfailurecauseattr:]: the cause of the agent spawn failure.



### Failure of Agent Killing 

As explained in the previous sections, the agent could stop its execution by calling the
[:killmefct:] function.

In some cases, the killing of the agent is canceled. For example, an agent cannot kill
itself if its contains sub-agents in its inner context.

In order to be notified of the cancelation of its killing, the agent receives an
occurrence of [:agentkillfailureevent:].
The following code shows up an event handler that outputs an error message when the
agent killing action has failed.

[:Success:]
	import io.sarl.core.AgentKillFailure
	import io.sarl.core.Lifecycle
	import io.sarl.core.Logging
	[:On]
	agent MyAgent {
		uses Lifecycle, Logging
		def aFunction {
			killMe
		}
		on [:agentkillfailureevent](AgentKillFailure) {
			error("Agent killing has failed with the cause: " + occurrence.[:causeattr](cause))
		}
	}
[:End:]

The cause of the agent kill failure is provided by the [:causeattr:] attribute.



[:Include:](../legal.inc)
