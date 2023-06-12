# Behaviors Capacity

[:Outline:]

The built-in capacity `[:behaviors](Behaviors)` provides the tools to the agents for dynamically
registering and unregistering sub-behaviors.

This capacity is closely related to the `[:innercontextaccess](InnerContextAccess)` for enabling a
high-level abstraction for holonic multi-agent system development.

[:Fact:]{typeof(io.sarl.core.[:innercontextaccess!])}

The definition of a behavior is not detailed in this reference document.
Please read the [Behavior Reference](../Behavior.md) for details.

<!--- Test that all the documented functions are defined in the capacity, and no function is missed to be
      documented --> 
[:Fact:]{typeof(io.sarl.core.[:behaviors!]).shouldHaveMethods(
	"[:registerbehavior](registerBehavior)(io.sarl.lang.core.Behavior, org.eclipse.xtext.xbase.lib.Functions$Function1, java.lang.Object[]) : io.sarl.lang.core.Behavior",
	"[:registerbehavior](registerBehavior)(io.sarl.lang.core.Behavior, java.lang.Object[]) : io.sarl.lang.core.Behavior",
	"[:unregisterbehavior](unregisterBehavior)(io.sarl.lang.core.Behavior) : io.sarl.lang.core.Behavior",
	"[:wake](wake)(io.sarl.lang.core.Event, io.sarl.lang.core.Scope)",
	"wake(io.sarl.lang.core.Event)",
	"wake(io.sarl.lang.core.Behavior, io.sarl.lang.core.Event)",
	"wake(java.lang.Iterable, io.sarl.lang.core.Event)",
	"[:aseventlistener](asEventListener) : io.sarl.lang.core.EventListener",
	"[:hasregisteredbehavior](hasRegisteredBehavior) : boolean",
	"[:getregisteredbehaviors](getRegisteredBehaviors) : io.sarl.lang.util.ConcurrentCollection")
}


## Registering a Behavior

Assuming that a behavior was already defined, it is possible for an agent to register this behavior:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.Behavior
	interface Tmp {
	[:On]
		def [:registerbehavior!](attitude : Behavior) : Behavior
	[:Off]
	}
[:End:]


This function takes the behavior to be registered, and replies the same behavior.
When a behavior is registered, it is receiving the events in the default space of
the inner context of the agent, or received by the agent itself.

An example of call to the registration function is:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.Behavior
	behavior MyBehavior {
		new () {
			super(null)
		}
	}
	agent A {
		uses Behaviors
		def action {
			[:On]
			var beh = new MyBehavior
			[:registerbehavior!](beh)
			[:Off]
		}
	}
[:End:]


According to the SARL syntax reference, the example could be also written as: 

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.Behavior
	behavior MyBehavior {
		new () {
			super(null)
		}
	}
	agent A {
		uses Behaviors
		def action {
			[:On]
			var beh = new MyBehavior
			beh.[:registerbehavior!]
			[:Off]
		}
	}
[:End:]


## Unregistering a Behavior

Assuming that a behavior was already registered, it is possible for an agent to unregister it:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.Behavior
	interface Tmp {
	[:On]
		def [:unregisterbehavior!](attitude : Behavior) : Behavior
	[:Off]
	}
[:End:]


This function takes the behavior to be unregistered, and replies the same behavior.
When a behavior is unregistering, it is no more receiving the events
in the default space of the inner context of the agent, and the ones received by the
agent itself.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.Behavior
	behavior MyBehavior {
		new () {
			super(null)
		}
	}
	[:On]
	agent A {
		uses Behaviors
		var b : MyBehavior
		var c : Behavior
		def myaction {
			b = new MyBehavior
			c = [:unregisterbehavior!](b)
		}
	}
[:End:]


## Registering a Behavior with an event filter

Assuming that a behavior was already defined, it is possible for an agent to
register this behavior that may received only the events matching a specific
filtering function. For registering such a behavior with its filter, the
following function could be used:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.Behavior
	import io.sarl.lang.core.Event
	interface Tmp {
	[:On]
		def [:registerbehavior!](attitude : Behavior, filter : (Event) => boolean) : Behavior
	[:Off]
	}
[:End:]


This function takes the behavior to be registered, and replies the same behavior.
When a behavior is registered, it is receiving the events that are matching the given
filter in the default space of the inner context of the agent, or received by the agent
itself.
The filtering function is invoked for each event that should be given to the behavior.
If the filtering function replies `true`, the event is really dispatching into the behavior.
If the function replies `false`, the event is discarded to the behavior.

An example of call to the registration function is:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.Behavior
	behavior MyBehavior {
		new () {
			super(null)
		}
	}
	event MyEvent
	agent A {
		uses Behaviors
		def action {
			[:On]
			var beh = new MyBehavior
			[:registerbehavior!](beh, [^event | ^event instanceof MyEvent])
			[:Off]
		}
	}
[:End:]


According to the SARL syntax reference, the example could be also written as: 

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.Behavior
	behavior MyBehavior {
		new () {
			super(null)
		}
	}
	event MyEvent
	agent A {
		uses Behaviors
		def action {
			[:On]
			var beh = new MyBehavior
			beh.[:registerbehavior!] [^event | ^event instanceof MyEvent]
			[:Off]
		}
	}
[:End:]


## Executing a Behavior

A behavior is executed through its event handlers. Consequently, for running a behavior, it is mandatory
to wake it with an event. 
This section describes the functions for awaking
the behaviors with an event occurrence.


### Awaking all behaviors and sub-agents

The regular way for awaking agent behaviors is to fire an event into all the registered behaviors.
This particular feature is supported by:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.Behavior
	import io.sarl.lang.core.Scope
	import io.sarl.lang.core.Address
	import io.sarl.lang.core.Event
	interface Tmp {
	[:On]
		def [:wake!](evt : Event, scope : Scope<Address> = null)
	[:Off]
	}
[:End:]


This function emits the given event into the inner context of the agent (in the default space).

If a scope is provided, it is used for filtering the agents that will
receive the event. The filterable agents are the current agent itself, and
all the sub-agents (sub-holons) that were created inside the current agent.

> **_Important Note:_** Because a behavior has no associated address, it cannot be
> filtered by the scope. All the agent's behaviors that are waiting for a given event will 
> be executed.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.Event
	import io.sarl.lang.core.Scope
	import io.sarl.lang.core.Address
	event MyEvent
	agent A {
		uses Behaviors
		def action {
			[:On]
			var e : Event
			e = new MyEvent
			[:wake!](e)
			[:wake!](e, null)
			var scope : Scope<Address> = [ it.ID !== null ]
			[:wake!](e, scope)
			[:Off]
		}
	}
[:End:]


### Awaking a specific behavior

In some specific cases, you may want to wake up a single specific behavior with an event, such that, the other
behaviors of the agents and its sub-agents are not receiving the event occurrence.
This particular feature is supported by:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.Behavior
	import io.sarl.lang.core.Event
	interface Tmp {
	[:On]
		def [:wake!](beh : Behavior, evt : Event)
	[:Off]
	}
[:End:]


This function emits the given event into the given behavior, and neither in the inner space of the agent nor the other
registered behaviors of the agent.


### Awaking multiple specific behaviors

As an extension of the [:wake:] function that is presented into the previous section, you could wake up multiple
behaviors with a single event occurrence, assuming that the list of the behaviors to wake up is known and provided.
This feature is implemented by:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.Behavior
	import io.sarl.lang.core.Event
	interface Tmp {
	[:On]
		def [:wake!]([:behaviorslistforwake](behs) : Iterable<Behavior>, evt : Event)
	[:Off]
	}
[:End:]


This function emits the given event into each of the given behaviors, and neither in the inner space of the agent nor the other
registered behaviors of the agent that are not specified into the [:behaviorslistforwake:] argument.


## Creating an Event Listener

Sometimes, it is useful or mandatory for an agent to listen on the events in a
given space. The following function permits retrieving the event listener of
the agent:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.EventListener
	interface Tmp {
	[:On]
		def [:aseventlistener!] : EventListener
	[:Off]
	}
[:End:]


The listener replied by this function is the one used by the agent (and its behaviors)
for listening events related to all the contexts (default, external, and inner).

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.EventListener
	event MyEvent
	agent A {
		uses Behaviors
		def action {
			[:On]
			var l : EventListener
			l = [:aseventlistener!]
			[:Off]
		}
	}
[:End:]


## Accessing to the collection of the registered behaviors

Two functions are provided for accessing to the collection of the registered behaviors:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.Behavior
	import io.sarl.lang.util.ConcurrentCollection
	interface Tmp {
	[:On]
		def [:hasregisteredbehavior!] : boolean
		def [:getregisteredbehaviors!] : ConcurrentCollection<Behavior>
	[:Off]
	}
[:End:]


The [:hasregisteredbehavior!] replies a boolean value, which is indicating if
a behavior is registered.
The [:getregisteredbehaviors!] replies an unmodifiable collection of the registered behaviors.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.Behavior
	import io.sarl.lang.core.EventListener
	import io.sarl.lang.util.ConcurrentCollection
	agent A {
		uses Behaviors
		def myaction {
			[:On]
			var b : boolean = hasRegisteredBehavior
			var c : ConcurrentCollection<Behavior> = getRegisteredBehaviors
			[:Off]
		}
	}
[:End:]



[:Include:](../../legal.inc)
