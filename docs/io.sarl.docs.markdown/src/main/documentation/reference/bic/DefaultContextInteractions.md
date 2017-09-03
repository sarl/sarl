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
					defaultContext.defaultSpace.emit(^event)
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
			"[:willreceive](willReceive)(java.util.UUID, io.sarl.lang.core.Event)",
			"[:isdefaultcontext](isDefaultContext)(io.sarl.lang.core.AgentContext) : boolean",
			"isDefaultContext(java.util.UUID) : boolean",
			"[:isdefaultspace](isDefaultSpace)(io.sarl.lang.core.Space) : boolean",
			"isDefaultSpace(io.sarl.lang.core.SpaceID) : boolean",
			"isDefaultSpace(java.util.UUID) : boolean",
			"[:isindefaultspace](isInDefaultSpace)(io.sarl.lang.core.Event) : boolean")
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
					defaultContext.defaultSpace.emit(^event)
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

### Creation of scopes with the predefined API

It is recommended using the SARL utility functions for creating scopes.
They are defined in the class `[:scopes](io.sarl.util.[:scopesbn]$Scopes$)`. [:Fact:]{typeof([:scopes!])}
The following example is equivalent to the feature call of [:emit:] without the scoping parameter:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.core.DefaultContextInteractions
			import [:scopes!]
			event MyEvent
			agent A {
				uses DefaultContextInteractions
				def myaction {
					[:On]
					emit(new MyEvent, [:scopesbn!]::allParticipants)
					[:Off]
				}
			}
		[:End:]


A default implementation of a scope using addresses, of `Address` type is implemented in the class `[:addressscope](io.sarl.util.[:addressscopebn]$AddressScope$)`. [:Fact:]{typeof([:addressscope!])}
The utility class [:scopesbn:] provides the [:addresses:] function for creating an instance of [:addressscopebn:].

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.core.DefaultContextInteractions
			import io.sarl.lang.core.Address
			import [:scopes!]
			event MyEvent
			agent A {
				uses DefaultContextInteractions
				def myaction {
					var a1 : Address
					var a2 : Address
					[:On]
					emit(new MyEvent, [:scopesbn!]::[:addresses](addresses)(a1, a2))
					[:Off]
				}
			}
		[:End:]

Another default implementation of a scope using identifiers, of `UUID` type is implemented in the class `[:identifierscope](io.sarl.util.[:identifierscopebn]$IdentifierScope$)`. [:Fact:]{typeof([:identifierscope!])}
The utility class [:scopesbn:] provides the [:identifiers:] function for creating an instance of [:identifierscopebn:].

		[:Success:]
			package io.sarl.docs.reference.bic
			import java.util.UUID
			import io.sarl.core.DefaultContextInteractions
			import io.sarl.lang.core.Address
			import [:scopes!]
			event MyEvent
			agent A {
				uses DefaultContextInteractions
				def myaction {
					var id1 : UUID
					var id2 : UUID
					[:On]
					emit(new MyEvent, [:scopesbn!]::[:identifiers](identifiers)(id1, id2))
					[:Off]
				}
			}
		[:End:]

The complete list of the functions that are provided by the [:scopesbn:] class is
accessible on the [Scopes API documentation](http://www.sarl.io/docs/api/index.html?io/sarl/util/Scopes.html).

### Creation of developer-specific scopes

You are free to create new implementation of [:scope:] in order to filter the receivers of an
event according to your own criteria. The easier approach is to write a lambda expression for the scope.
The previous line of code becomes:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.core.DefaultContextInteractions
			import io.sarl.lang.core.Address
			import [:scopes!]
			event MyEvent
			agent A {
				uses DefaultContextInteractions
				def myaction {
					var a1 : Address
					var a2 : Address
					[:On]
					emit(new MyEvent) [ it == a1 || it == a2 ]
					[:Off]
				}
			}
		[:End:]

In the previous code, the lambda expression is written outside the list of the [:emit:] parameters.
But it corresponds to the last formal parameter of type `[:scope!]<Address>`.
The `it` variable in the lambda expression is the default name given to the formal parameter of 
the [:matchesfct:] function, which is defined in the [:scope:] interface.


### Inverted syntax for emiting an event.

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
					^event.emit [ it.UUID == [:receiverid!] ] 
				}
			}
		[:End:]


In the previous code, the receiver of the event is given by the formal parameter [:receiverid:].
The scope restricts the receiver according to this identifier.


For an abstract point of view, the previous emiting call may be explained with "the event is emited to the receiver".
Sometimes, the developer would like to write a code that corresponds to the sentence "the receiver will receive the event".
In order to enable this approach to the developer, the SARL API provides the function:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.lang.core.Event
			import io.sarl.lang.core.Scope
			import io.sarl.lang.core.Address
			import java.util.UUID
			interface Tmp {
				def [:emit!](e : Event, scope : Scope<Address>)
			[:On]
				def [:willreceive!]([:receiverid!] : UUID, ^event : Event) {
					emit(^event) [ it.UUID == [:receiverid!] ]
				}
			[:Off]
			}
		[:End:]

The initial example in this section becomes:

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
					[:receiverid!].willReceive(^event) 
				}
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

<note>According to the [General Syntax Reference](../GeneralSyntax.md),
the `event.[:isindefaultspace!]` is equivalent to `[:isindefaultspace!](event)`.</note>

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
					    || isDefaultSpace(defaultSpace.ID)
					    || isDefaultSpace(defaultSpace.ID.ID)
				}
				[:On]
				on [:eventtype1](AnEvent) [ occurrence.inDefaultSpace ] {
					// Do something with the event when it was emitted in the default space
				}
				[:Off]
			}
		[:End:]



[:Include:](../../legal.inc)
