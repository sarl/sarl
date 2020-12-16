# DefaultContextInteractions

[:Outline:]

The `[:defaultcontextinteractions](DefaultContextInteractions)` capacity is actually provided
for convenience. It assumes that the action will be performed on the 
agent __default context__ or its __default space__. These context and space
are illustrated by the top-left context in the figure above. 

For instance, the [:emit:] action is a shortcut for:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.lang.core.Event
	agent A {
		uses DefaultContextInteractions
		def myaction {
			var ^event : Event = null
			[:On]
			defaultContext.defaultSpace.emit(getID, ^event)
			[:Off]
		}
	}
[:End:]


Therefore, it is actually created on top of the other built-in capacities.

<!--- Test that all the documented functions are defined in the capacity, and no function is missed to be
      documented --> 
[:Fact:]{typeof(io.sarl.core.[:defaultcontextinteractions!]).shouldHaveMethods(
	"[:getdefaultcontext](getDefaultContext) : io.sarl.lang.core.AgentContext",
	"[:getdefaultspace](getDefaultSpace) : io.sarl.lang.core.EventSpace",
	"[:getdefaultaddress](getDefaultAddress) : io.sarl.lang.core.Address",
	"[:emit](emit)(io.sarl.lang.core.Event)",
	"emit(io.sarl.lang.core.Event, io.sarl.lang.core.Scope)",
	"[:isdefaultcontext](isDefaultContext)(io.sarl.lang.core.AgentContext) : boolean",
	"isDefaultContext(java.util.UUID) : boolean",
	"[:isdefaultspace](isDefaultSpace)(io.sarl.lang.core.Space) : boolean",
	"isDefaultSpace(io.sarl.lang.core.SpaceID) : boolean",
	"isDefaultSpace(java.util.UUID) : boolean",
	"[:isindefaultspace](isInDefaultSpace)(io.sarl.lang.core.Event) : boolean",
	"[:getdefaultparentid](getDefaultParentID) : java.util.UUID",
	"[:getdefaultparentscope](getDefaultParentScope) : io.sarl.lang.core.Scope",
	"[:emittoparent](emitToParent)(io.sarl.lang.core.Event)")
}


## Retrieving the Default Context and Space

For retrieving the default context of an agent, this built-in capacity provides the following function:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.AgentContext
	interface Tmp {
	[:On]
		def [:getdefaultcontext!] : AgentContext
	[:Off]
	}
[:End:]


For retrieving the default space in the default context of an agent, this built-in capacity provides
the following function:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.EventSpace
	interface Tmp {
	[:On]
		def [:getdefaultspace!] : EventSpace
	[:Off]
	}
[:End:]


For obtaining the address of the agent in the default space, the following function is provided:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.EventSpace
	interface Tmp {
	[:On]
		def [:getdefaultaddress!] : EventSpace
	[:Off]
	}
[:End:]


## Sending an Event in the Default Space

The core mechanism for information exchanges among agents is [event-based](../Event.md).
For sending an event in the default space of the default context, the following function is provided:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.Event
	interface Tmp {
	[:On]
		def [:emit!](^event : Event)
	[:Off]
	}
[:End:]


This function emits the given event with no scope (i.e., all registered agent will receive the
event) in the default space of the default context.

Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.lang.core.Event
	event MyEvent
	[:On]
	agent A {
		uses DefaultContextInteractions
		def myaction {
			var ^event : Event = new MyEvent
			emit(^event)
		}
	}
[:End:]
		

The call to [:emit:] is equivalent to:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.lang.core.Event
	agent A {
		uses DefaultContextInteractions
		def myaction {
			var ^event : Event = null
			[:On]
			defaultContext.defaultSpace.emit(getID, ^event)
			[:Off]
		}
	}
[:End:]


## Sending an Event to Specific Agents in the Default Space


### Function with a Scoping Parameter

The previous sending function assumes that there is no restriction on the set of the receivers of the event.
It is possible to specify a `[:scope](Scope)` for applying a restriction.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.EventSpace
	import io.sarl.lang.core.Address
	import io.sarl.lang.core.Scope
	import io.sarl.lang.core.Event
	interface Tmp {
	[:On]
		def [:emit!](e : Event, scope : Scope<Address>)
	[:Off]
	}
[:End:]


A scope is a predicates that is evaluated against the addresses of the receivers. It is defined as:

[:Success:]
	package io.sarl.docs.reference.bic
	import java.io.Serializable
	[:On]
	interface Scope<T> extends Serializable {
		def [:matchesfct](matches)(element : T) : boolean
	}
[:End:]


### Creation of scopes with Lambda expressions

It is recommended defining a lambda expression for creating a scope.
The following example is equivalent to the feature call of [:emit:] without the scoping parameter:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.DefaultContextInteractions
	event MyEvent
	agent A {
		uses DefaultContextInteractions
		def myaction {
			[:On]
			emit(new MyEvent) [ true ]
			[:Off]
		}
	}
[:End:]


For scoping on the address of the receiving agent within the space, the first formal parameter, named [:scopeaddressit:]
could be used for accessing to the value of the receiving agent's address.
In the following code, the agents with addresses [:a1address:] and [:a2address:] will receive the event, and
not the other agents.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.lang.core.Address
	event MyEvent
	agent A {
		uses DefaultContextInteractions
		def myaction {
			var [:a1address](a1) : Address
			var [:a2address](a2) : Address
			[:On]
			emit(new MyEvent) [ [:scopeaddressit](it) == [:a1address!] || [:scopeaddressit!] == [:a2address!] ]
			[:Off]
		}
	}
[:End:]


Another way to scope is based on the test of the agent identifiers of `UUID` type.
In the following example,

[:Success:]
	package io.sarl.docs.reference.bic
	import java.util.UUID
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.lang.core.Address
	event MyEvent
	agent A {
		uses DefaultContextInteractions
		def myaction {
			var [:id1identifier](id1) : UUID
			var [:id2identifier](id2) : UUID
			[:On]
			emit(new MyEvent) [ it.ID == [:id1identifier!] || it.ID == [:id2identifier!] ]
			[:Off]
		}
	}
[:End:]


### Inverted syntax for emitting an event.

According to the [extension method mechanism](../general/Extension.md), it is possible to call
the [:emit:] function with the event instance as the receiver expression. The previous
example becomes:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.DefaultContextInteractions
	import io.sarl.lang.core.Event
	import java.util.UUID
	event MyEvent
	[:On]
	agent A {
		uses DefaultContextInteractions
	
		def myaction([:receiverid](receiverId) : UUID) {
			var ^event : Event = new MyEvent
			^event.emit [ it.ID == [:receiverid!] ] 
		}
	}
[:End:]


In the previous code, the receiver of the event is given by the formal parameter [:receiverid:].
The scope restricts the receiver according to this identifier.



## Interaction with the parent agent

Each agent is member of a context that is the default context. According to the SARL metamodel, each context
is own by an agent. This agent is named the "default parent agent".


### Getting the identifier of the parent agent

The [:defaultcontextinteractions:] capacity provides the function [:getdefaultparentid:] function for obtaining
the identifier of the parent agent.

[:Success:]
	package io.sarl.docs.reference.bic
	import java.util.UUID
	interface Tmp {
	[:On]
		def [:getdefaultparentid!] : UUID
	[:Off]
	}
[:End:]


### Getting the event scope of the parent agent

The [:defaultcontextinteractions:] capacity provides the function [:getdefaultparentscope:] function for obtaining
a scope that matches the parent agent.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.Scope
	import io.sarl.lang.core.Address
	interface Tmp {
	[:On]
		def [:getdefaultparentscope!] : Scope<Address>
	[:Off]
	}
[:End:]

This scope could be used to emit the events.


### Emit an event to the parent agent

The [:defaultcontextinteractions:] capacity provides the function [:emittoparent:] function for
sending an event to the parent agent only.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.Event
	interface Tmp {
	[:On]
		def [:emittoparent!](^event : Event)
	[:Off]
	}
[:End:]


## Testing if an element is related to the default context

The [:defaultcontextinteractions:] provides a collection of utility functions that test if
their parameters are related to the default context or the default space.


| Function                             | Explanation                                                                       |
| ------------------------------------ | --------------------------------------------------------------------------------- |
| `[:isdefaultcontext!](AgentContext)` | tests if the given context is the default context.                                |
| `[:isdefaultcontext!](UUID)`         | tests if the default context has the given identifier.                            |
| `[:isdefaultspace!](Space)`          | tests if the given space is the default space of the default context.             |
| `[:isdefaultspace!](SpaceID)`        | tests if the default space of the default context has the given identifier.       |
| `[:isdefaultspace!](UUID)`           | tests if the default space of the default context has the given identifier.       |
| `[:isindefaultspace!](Event)`        | tests if the given event was emitted in the default space of the default context. |


The following example illustrates the use of the [:isindefaultspace:] function in the guard of
an behavior unit. In this example, the behavior unit is run only if the event of type [:eventtype1:]
was emitted in the default space.

> **_Note:_**According to the [General Syntax Reference](../GeneralSyntax.md),
> the `event.[:isindefaultspace!]` is equivalent to `[:isindefaultspace!](event)`.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.DefaultContextInteractions
	event AnEvent
	agent MyAgent {
		uses DefaultContextInteractions
		def testOtherFunctions : boolean {
			return isDefaultContext(defaultContext)
			    || isDefaultContext(defaultContext.ID)
			    || isDefaultSpace(defaultSpace)
			    || isDefaultSpace(defaultSpace.spaceID)
			    || isDefaultSpace(defaultSpace.spaceID.ID)
		}
		[:On]
		on [:eventtype1](AnEvent) [ occurrence.inDefaultSpace ] {
			// Do something with the event when it was emitted in the default space
		}
		[:Off]
	}
[:End:]



[:Include:](../../legal.inc)
