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
			"[:getallcontexts](getAllContexts) : io.sarl.lang.util.SynchronizedIterable",
			"[:join](join)(java.util.UUID, java.util.UUID)",
			"[:leave](leave)(java.util.UUID)",
			"[:isinspace](isInSpace)(io.sarl.lang.core.Event, io.sarl.lang.core.Space) : boolean",
			"isInSpace(io.sarl.lang.core.Event, io.sarl.lang.core.SpaceID) : boolean",
			"isInSpace(io.sarl.lang.core.Event, java.util.UUID) : boolean")
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


## Retrieving the Contexts of an Agent

The following function enables an agent to retrieve all the contexts in which it is involved:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.lang.core.AgentContext
			import io.sarl.lang.util.SynchronizedIterable
			interface Tmp {
			[:On]
				def [:getallcontexts!] : SynchronizedIterable<AgentContext>
			[:Off]
			}
		[:End:]


The default context is included in the replied collection.

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.core.ExternalContextAccess
			import io.sarl.lang.core.AgentContext
			import io.sarl.lang.util.SynchronizedIterable
			[:On]
			agent A {
				uses ExternalContextAccess
				var c : SynchronizedIterable<AgentContext>
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
				def [:join!]([:contextparamid](contextID) : UUID, [:expectedefaultspaceid](expectedDefaultSpaceID) : UUID)
			[:Off]
			}
		[:End:]

This action registers the agent in the default space of the context.

The agent will be involved in the context with the ID given by [:contextparamid:].
The parameter [:expectedefaultspaceid:] is only used to check if the caller of this function
knows the ID of the default space in the context to be involved in. 
If the given [:expectedefaultspaceid:] does not match the ID of the default space in the context
[:contextparamid:], then the access to the context is forbidden.

<importantnote> The context must already exist, and the default space inside this context must have the same ID 
as [:expectedefaultspaceid:].</importantnote>

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
 					return [:isinspace!](e, myspace.ID)
					    || [:isinspace!](e, myspace.ID.ID)
				}
				[:On]
				on [:eventtype1](AnEvent) [ [:isinspace!](occurrence, [:spacetype1](myspace)) ] {
					// Do something with the event when it was emitted in the space myspace
				}
				[:Off]
			}
		[:End:]



[:Include:](../../legal.inc)
