# Lifecycle Capacity

[:Outline:]

The built-in capacity `[:lifecyclecap](Lifecycle)` provides actions for spawning new agents on different external contexts and
the inner context, as well as the `killMe` action to stop the execution of an agent.

		<!--- Test that all the documented functions are defined in the capacity, and no function is missed to be
		      documented --> 
		[:Fact:]{typeof(io.sarl.core.[:lifecyclecap!]).shouldHaveMethods(
			"[:fctkillme](killMe)",
			"[:fctspawn](spawn)(java.lang.Class, java.lang.Object[]) : java.util.UUID",
			"spawn(int, java.lang.Class, java.lang.Object[]) : java.lang.Iterable",
			"[:fctspawnincontext](spawnInContext)(java.lang.Class, io.sarl.lang.core.AgentContext, java.lang.Object[]) : java.util.UUID",
			"spawnInContext(int, java.lang.Class, io.sarl.lang.core.AgentContext, java.lang.Object[]) : java.lang.Iterable",
			"spawnInContextWithID(java.lang.Class, java.util.UUID, io.sarl.lang.core.AgentContext, java.lang.Object[]) : java.util.UUID")
		}


## Stopping the Agent Execution

Because of the autonomy property of an agent, it can be stopped only by committing a suicide. It means that
it is impossible to stop an agent from another agent: the agent to stop must be able to accept or reject
this query.

The [:lifecyclecap:] capacity provides the following function for committing a suicide:

		[:Success:]
			package io.sarl.docs.reference.bic
			interface Tmp {
			[:On]
				def [:fctkillme!]
			[:Off]
			}
		[:End:]


This action automatically unregisters the calling agent from the default context, and therefore all its
spaces including the default space.

<veryimportantnote> If the killed agent was a composed agent, it must not have members any more before
calling this action, otherwise a `RuntimeException` is thrown.</veryimportantnote>

This action fires two events:

* [:agentkilledevent:] in the default space of all contexts to which the calling agent belongs.
* [:destroyevent:] inside the killed agent agent.

[:Fact:]{typeof(io.sarl.core.[:agentkilledevent]$AgentKilled$)}
[:Fact:]{typeof(io.sarl.core.[:destroyevent]$Destroy$)}

Example:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.core.Lifecycle
			[:On]
			agent A {
				uses [:lifecyclecap!]
				def myaction {
					[:fctkillme!]
				}
			}
		[:End:]


## Spawning in the default context

Many time, it is useful for agent to create a new agent into the default context. The following
functions are provided for this task:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.lang.core.Agent
			import java.util.UUID
			interface Tmp {
			[:On]
				def [:fctspawn!](agentType : Class<? extends Agent>, [:parameters](parameters) : Object*) : UUID
				def [:fctspawn!]([:nbagents](nbAgents): int, agentType : Class<? extends Agent>, [:parameters!] : Object*) : Iterable<UUID>
			[:Off]
			}
		[:End:]

This action creates one to [:nbagents:] instance(s) of the given agent type, and launches the agent(s)
into the default context.
The first [:fctspawn:] function above is spawning a single agent and replies the identifier of the spawned agent.
The second [:fctspawn:] function is spawning the given number of agents and replies the identifiers of the
spawned agents.
The [:parameters:] are passed to the spawned agent inside the [:initializeevent:] event: the [:parameters:] field.

[:Fact:]{typeof(io.sarl.core.Initialize).shouldHaveField("parameters : java.lang.Object[]")}

This action fires two events:

* [:agentspawned:] in the default space of the default context. The source of the event is this spawner.
* [:initializeevent:] in spawned agent.

[:Fact:]{typeof(io.sarl.core.[:agentspawned]$AgentSpawned$)}
[:Fact:]{typeof(io.sarl.core.[:initializeevent]$Initialize$)}

Example:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.core.Lifecycle
			import io.sarl.lang.core.Agent
			import java.util.UUID
			[:On]
			agent A {
				uses Lifecycle
				def myaction {
					var aid : UUID
					var listaid : Iterable<UUID>
					var type : Class<? extends Agent>
					var p1 : Object
					var p2 : Object
					type = typeof(A)
					p1 = new Object
					p2 = new Object
					aid = spawn(type, p1, p2)
					listaid = spawn(5, type, p1, p2)
				}
			}
		[:End:]


## Spawning in a specific context

When one or more agents should be spawned into a specific agent context, the two following functions
could be used for launching the agents:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.lang.core.Agent
			import io.sarl.lang.core.AgentContext
			import java.util.UUID
			interface Tmp {
			[:On]
				def [:fctspawnincontext!](agentType : Class<? extends Agent>,
				                   [:agentcontext](context) : AgentContext,
				                   [:parameters!] : Object*) : UUID
				def [:fctspawnincontext!]([:nbagents!] : int,
				                   agentType : Class<? extends Agent>,
				                   context : AgentContext,
				                   [:parameters!] : Object*) : Iterable<UUID>
			[:Off]
			}
		[:End:]


This action creates one to [:nbagents:] instance(s) of the given agent type, and launches the agent(s)
into the given [:agentcontext:].
The first [:fctspawn:] function is spawning a single agent and replies the identifier of the spawned agent.
The second [:fctspawn:] function is spawning the given number of agents and replies the identifiers of the
spawned agents.
The [:parameters:] are passed to the spawned agent inside the [:initializeevent:] event: the
[:parameters:] field.

This action fires two events:

* [:agentspawned:] in the default space of the context. The source of the event is the calling agent.
* [:initializeevent:] in spawned agent.

Example:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.core.Lifecycle
			import io.sarl.lang.core.AgentContext
			import io.sarl.lang.core.Agent
			import java.util.UUID
			[:On]
			agent A {
				uses Lifecycle
				def myaction {
					var c : AgentContext
					var aid : UUID
					var listaid : Iterable<UUID>
					var type : Class<? extends Agent>
					var p1 : Object
					var p2 : Object
					type = typeof(A)
					p1 = new Object
					p2 = new Object
					aid = spawnInContext(type, c, p1, p2)
					listaid = spawnInContext(5, type, c, p1, p2)
				}
			}
		[:End:]


## Spawning with a specific agent identifier

Some time, it is useful to create an agent with a specific identifier. The following function permits to spawn an agent
with a given identifier in a specific context:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.lang.core.Agent
			import io.sarl.lang.core.AgentContext
			import java.util.UUID
			interface Tmp {
			[:On]
				def spawnInContextWithID(agentType : Class<? extends Agent>,
				                         agentId : UUID,
				                         [:agentcontext!] : AgentContext,
				                         [:parameters!] : Object*) : UUID
			[:Off]
			}
		[:End:]

This action creates an instance of the given agent type, with the given identifier, and launches the agent
into the given context.
The parameters are passed to the spawned agent inside the [:initializeevent:] event: the [:parameters:] field.

This action fires two events:

* [:agentspawned:] in the default space of the context. The source of the event is the calling agent.
* [:initializeevent:] in spawned agent.

Example:

		[:Success:]
			package io.sarl.docs.reference.bic
			import io.sarl.core.Lifecycle
			import io.sarl.lang.core.AgentContext
			import io.sarl.lang.core.Agent
			import java.util.UUID
			[:On]
			agent A {
				uses Lifecycle
				def myaction {
					var c : AgentContext
					var aid : UUID
					var type : Class<? extends Agent>
					var p1 : Object
					var p2 : Object
					type = typeof(A)
					p1 = new Object
					p2 = new Object
					aid = spawnInContextWithID(type, aid, c, #[p1, p2])
				}
			}
		[:End:]


[:Include:](../../legal.inc)
