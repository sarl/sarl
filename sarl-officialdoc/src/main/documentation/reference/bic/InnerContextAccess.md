# InnerContextAccess Capacity

[:Outline:]

The built-in capacity `[:innercontextaccess](InnerContextAccess)` provides access to the inner context of the agent.
This is a key feature for creating holonic agent implementation.
The context supported by this built-in capacity is the "inner context," illustrated by the
bottom context in the figure above.

<!--- Test that all the documented functions are defined in the capacity, and no function is missed to be
      documented --> 
[:Fact:]{typeof(io.sarl.core.[:innercontextaccess!]).shouldHaveMethods(
	"[:getinnercontext](getInnerContext) : io.sarl.lang.core.AgentContext",
	"[:getinnerdefaultspace](getInnerDefaultSpace) : io.sarl.lang.core.EventSpace",
	"[:hasmemberagent](hasMemberAgent) : boolean",
	"[:getmemberagentcount](getMemberAgentCount) : int",
	"[:getmemberagents](getMemberAgents) : io.sarl.lang.util.ConcurrentSet",
	"[:isinnerdefaultspace](isInnerDefaultSpace)(io.sarl.lang.core.Space) : boolean",
	"[:isinnerdefaultspace](isInnerDefaultSpace)(io.sarl.lang.core.SpaceID) : boolean",
	"[:isinnerdefaultspace](isInnerDefaultSpace)(java.util.UUID) : boolean",
	"[:isininnerdefaultspace](isInInnerDefaultSpace)(io.sarl.lang.core.Event) : boolean")
}


## Retrieving the Inner Context

For retrieving the inner context of an agent, this built-in capacity provides the following function:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.AgentContext
	interface Tmp {
	[:On]
		def [:getinnercontext!] : AgentContext
	[:Off]
	}
[:End:]


Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.InnerContextAccess
	import io.sarl.lang.core.AgentContext
	[:On]
	agent A {
		uses InnerContextAccess
		var c : AgentContext
		def myaction {
			c = [:getinnercontext!]
		}
	}
[:End:]


## Retrieving the Default Space of the Inner Context

For retrieving the default space of the inner context, this built-in capacity provides the following function:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.core.EventSpace
	interface Tmp {
	[:On]
		def [:getinnerdefaultspace!] : EventSpace
	[:Off]
	}
[:End:]


Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.InnerContextAccess
	import io.sarl.lang.core.EventSpace
	[:On]
	agent A {
		uses InnerContextAccess
		var s : EventSpace
		def myaction {
			s = [:getinnerdefaultspace!]
		}
	}
[:End:]


## Members of an Agent

For retrieving information on the member agents of the current agent, several functions are
provided by this built-in capacity.
A member agent is an agent that is not the calling agent, and is a member of at least
one space of the inner context.

The first function replies if the calling agent has other agents as members of its inner context:
[:Success:]
	package io.sarl.docs.reference.bic
	interface Tmp {
	[:On]
		def [:hasmemberagent!] : boolean
	[:Off]
	}
[:End:]


The second function replies the number of agents that are members of the inner context of the calling agent:
[:Success:]
	package io.sarl.docs.reference.bic
	interface Tmp {
	[:On]
		def [:getmemberagentcount!] : int
	[:Off]
	}
[:End:]


The third function replies all the member agents in the inner context:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.lang.util.ConcurrentSet
	import java.util.UUID
	interface Tmp {
	[:On]
		def [:getmemberagents!] : ConcurrentSet<UUID>
	[:Off]
	}
[:End:]


Examples:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.InnerContextAccess
	import io.sarl.lang.util.ConcurrentSet
	import java.util.UUID
	[:On]
	agent A {
		uses InnerContextAccess
		var b : boolean
		var n : int
		var m : ConcurrentSet<UUID>
		def myaction {
			b = hasMemberAgent
			n = getMemberAgentCount
			m = getMemberAgents
		}
	}
[:End:]


## Testing if an element is related to the inner context

The [:innercontextaccess:] provides a collection of utility functions that test if their parameters
are related to the inner context.


| Function                           | Explanation                                                                     |
| ---------------------------------- | ------------------------------------------------------------------------------- |
| `[:isinnerdefaultspace!](Space)`   | tests if the given space is the default space of the inner context.             |
| `[:isinnerdefaultspace!](SpaceID)` | tests if the default space of the inner context has the given identifier.       |
| `[:isinnerdefaultspace!](UUID)`    | tests if the default space of the inner context has the given identifier.       |
| `[:isininnerdefaultspace!](Event)` | tests if the given event was emitted in the default space of the inner context. |


The following example illustrates the use of the [:isininnerdefaultspace:] function in the guard
of an behavior unit. In this example, the behavior unit is run only if the event
of type [:eventtype1:] was emitted in the default space of the inner context.

> **_Note:_** According to the [General Syntax Reference](../GeneralSyntax.md),
> the `event.[:isinnerdefaultspace!]` is equivalent to `[:isinnerdefaultspace!](event)`.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.InnerContextAccess
	import io.sarl.lang.core.Space
	event AnEvent
	agent A {
		uses InnerContextAccess
		def testOtherFunctions(s : Space) : boolean {
			return isInnerDefaultSpace(s)
			    || isInnerDefaultSpace(s.spaceID)
			    || isInnerDefaultSpace(s.spaceID.ID)
		}
		[:On]
		on [:eventtype1](AnEvent) [ occurrence.inInnerDefaultSpace ] {
			// Do something with the event when it was emitted in the inner default space
		}
		[:Off]
	}
[:End:]


[:Include:](../../legal.inc)
