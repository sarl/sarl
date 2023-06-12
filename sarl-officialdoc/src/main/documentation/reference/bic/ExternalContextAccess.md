# ExternalContextAccess Capacity

The built-in capacity `[:externalcontextaccess](ExternalContextAccess)` provides access to the
[context](../Space.md) that the agent is a part of, and actions
required to join new [contexts](../Space.md), and leave them.

The context supported by this built-in capacity is the "external context," illustrated by the
top-right context in the figure above.

<!--- Test that all the documented functions are defined in the capacity, and no function is missed to be
      documented --> 
[:Fact:]{typeof(io.sarl.core.[:externalcontextaccess!]).shouldHaveMethods(
	"[:getcontext](getContext)(java.util.UUID) : io.sarl.lang.core.AgentContext",
	"[:getuniversecontext](getUniverseContext) : io.sarl.lang.core.AgentContext",
	"[:getallcontexts](getAllContexts) : io.sarl.lang.util.ConcurrentCollection",
	"[:join](join)(java.util.UUID, java.util.UUID) : io.sarl.lang.core.AgentContext",
	"[:leave](leave)(java.util.UUID) : boolean",
	"[:isinspace](isInSpace)(io.sarl.lang.core.Event, io.sarl.lang.core.Space) : boolean",
	"isInSpace(io.sarl.lang.core.Event, io.sarl.lang.core.SpaceID) : boolean",
	"isInSpace(io.sarl.lang.core.Event, java.util.UUID) : boolean",
	"[:emit](emit)(io.sarl.lang.core.EventSpace,io.sarl.lang.core.Event)",
	"emit(io.sarl.lang.core.EventSpace,io.sarl.lang.core.Event,io.sarl.lang.core.Scope)")
}


## Retrieving a Context

For retrieving the context with a particular ID, this built-in capacity provides the following function:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.AgentContext
	import java.util.UUID
	interface Tmp {
	[:On]
		def [:getcontext!](contextID : UUID) : AgentContext
	[:Off]
	}
[:End:]


The agent must have joined (see below) the context before calling this action. Or, the agent
may use its `[:getParentID](getParentID)` for accessing the context in which it is located (the default context).

[:Fact:]{typeof(io.sarl.lang.core.Agent).shouldHaveMethod("[:getParentID!] : java.util.UUID")}

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.ExternalContextAccess
	import io.sarl.lang.core.AgentContext
	import java.util.UUID
	[:On]
	agent A {
		uses ExternalContextAccess
		var id : UUID
		var c : AgentContext
		def myaction {
			id = getParentID
			c = [:getcontext!](id)
		}
	}
[:End:]


## Retrieving the Universe Context

In all the SARL application, a default context exists. It's name is the Universe context.
It is fully managed by the SARL run-time environment.
For retrieving this particular context, this built-in capacity provides the following function:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.AgentContext
	import java.util.UUID
	interface Tmp {
	[:On]
		def [:getuniversecontext!]() : AgentContext
	[:Off]
	}
[:End:]


## Retrieving the Contexts of an Agent

The following function enables an agent to retrieve all the contexts in which it is involved:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.AgentContext
	import io.sarl.lang.util.ConcurrentCollection
	interface Tmp {
	[:On]
		def [:getallcontexts!] : ConcurrentCollection<AgentContext>
	[:Off]
	}
[:End:]


The default context is included in the replied collection.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.ExternalContextAccess
	import io.sarl.lang.core.AgentContext
	import io.sarl.lang.util.ConcurrentCollection
	[:On]
	agent A {
		uses ExternalContextAccess
		var c : ConcurrentCollection<AgentContext>
		def myaction {
			c = getAllContexts
		}
	}
[:End:]


## Joining an Existing Context

Agents must be able to join a new parent context. The following function gives this capability to them:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.AgentContext
	import java.util.UUID
	interface Tmp {
	[:On]
		def [:join!]([:contextparamid](contextID) : UUID, [:expectedefaultspaceid](expectedDefaultSpaceID) : UUID) : AgentContext
	[:Off]
	}
[:End:]


This action registers the agent in the default space of the context.

The agent will be involved in the context with the ID given by [:contextparamid:].
The parameter [:expectedefaultspaceid:] is only used to check if the caller of this function
knows the ID of the default space in the context to be involved in. 
If the given [:expectedefaultspaceid:] does not match the ID of the default space in the context
[:contextparamid:], then the access to the context is forbidden.

The [:join:] function replies the reference to the joined context.

> **_Important Note:_** The context must already exist, and the default space inside this context must have the same ID 
> as [:expectedefaultspaceid:].

This action fires two events:

* `ContextJoined` in the inner context's default space.
* `MemberJoined` in the parent context's default space.

[:Fact:]{typeof(io.sarl.core.ContextJoined)}
[:Fact:]{typeof(io.sarl.core.MemberJoined)}

Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.ExternalContextAccess
	import java.util.UUID
	[:On]
	agent A {
		uses ExternalContextAccess
		var idc : UUID
		var ids : UUID
		def myaction {
			idc = UUID::randomUUID
			ids = UUID::randomUUID
			join(idc, ids)
		}
	}
[:End:]


## Leaving a Context

When an agent wants to leave a context, it must invoke:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.AgentContext
	import java.util.UUID
	interface Tmp {
	[:On]
		def [:leave!](contextID : UUID)
	[:Off]
	}
[:End:]


This action fires two events:

* `ContextLeft` in the inner context's default space.
* `MemberLeft` in the parent context's default space.

[:Fact:]{typeof(io.sarl.core.ContextLeft)}
[:Fact:]{typeof(io.sarl.core.ContextLeft)}

Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.ExternalContextAccess
	import java.util.UUID
	[:On]
	agent A {
		uses ExternalContextAccess
		var idc : UUID
		def myaction {
			idc = UUID::randomUUID
			leave(idc)
		}
	}
[:End:]


## Testing if an element is related to an external context

The [:externalcontextaccess:] provides a collection of utility functions that test if their
parameters are related to the any external context.


| Function                        | Explanation                                                                  |
| ------------------------------- | ---------------------------------------------------------------------------- |
| `[:isinspace!](Event, Space)`   | tests if the given event was emitted in the given space.                     |
| `[:isinspace!](Event, SpaceID)` | tests if the given event was emitted in the space with the given identifier. |
| `[:isinspace!](Event, UUID)`    | tests if the given event was emitted in the space with the given identifier. |


The following example illustrates the use of the [:isinspace:] function in the guard
of an behavior unit. In this example, the behavior unit is run only if the event
of type [:eventtype1:] was emitted in the space [:spacetype1:] (declared as attribute in
the container).

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.ExternalContextAccess
	import io.sarl.lang.core.Space
	import io.sarl.lang.core.Event
	event AnEvent
	agent A {
		uses ExternalContextAccess
		var myspace : Space
		def testOtherFunctions(e : Event) : boolean {
			return [:isinspace!](e, myspace.spaceID)
			    || [:isinspace!](e, myspace.spaceID.ID)
		}
		[:On]
		on [:eventtype1](AnEvent) [ [:isinspace!](occurrence, [:spacetype1](myspace)) ] {
			// Do something with the event when it was emitted in the space myspace
		}
		[:Off]
	}
[:End:]


## Helper for firing an event in a space

Regarding the definition of the `EventSpace` type, the event emiting function takes at least two parameters:

* the identifier of the entity, which is firing the event, and
* the event to be fired.

The first parameter is used for setting the event's source when it was not already done.

The [:externalcontextaccess:] provides the [:emit:] function for helping to fire events into an event space:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.ExternalContextAccess
	import io.sarl.lang.core.AgentContext
	import io.sarl.lang.core.EventSpace
	import java.util.UUID
	event MyEvent
	agent Tmp {
		uses [:externalcontextaccess!]
		def myfct {
			var ^event = new MyEvent
			var ^space : EventSpace
			[:On]
				[:spacename](^space).emit([:eventname](^event))
			[:Off]
		}
	}
[:End:]

A call to the [:emit:] function takes two parameters:

* [:spacename:] is the variable which contains the reference to the space in which the event should be fired.
* [:eventname:] is the variable which contains the event to fire.

This function call is equivalent to:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.ExternalContextAccess
	import io.sarl.lang.core.AgentContext
	import io.sarl.lang.core.EventSpace
	import java.util.UUID
	event MyEvent
	agent Tmp {
		uses [:externalcontextaccess!]
		def myfct {
			var ^event = new MyEvent
			var ^space : EventSpace
			[:On]
				[:spacename!].emit([:getidfct](getID), [:eventname!])
			[:Off]
		}
	}
[:End:]


The [:getidfct:] function is provided by the current entity, e.g. an agent, for obtaining the identifier of the emitter.

From a syntactic point of view, the two calls look similar. But, the call to the [:externalcontextaccess:] function uses
the extension method syntax: the first argument to the function is written prior to the function's name.


[:Include:](../../legal.inc)
