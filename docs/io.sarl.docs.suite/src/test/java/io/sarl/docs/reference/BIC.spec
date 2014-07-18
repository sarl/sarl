/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.docs.reference

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

/**
 * This document describes the built-in capacities in SARL.
 * Before reading this document, it is recommended to read
 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html),
 * the [Capacity Reference](./CapacityReferenceSpec.html),
 * and the [Skill Reference](./SkillReferenceSpec.html).
 * 
 * <!-- OUPUT OUTLINE -->
 * 
 * A *Capacity* is the specification of a collection of actions. This specification 
 * makes no assumptions about its implementation. It could be used to specify 
 * what an agent can do, what a behavior requires for its execution.
 * 
 * A *Skill* is a possible implementation of a capacity fulfilling all the 
 * constraints of this specification.
 * 
 * In SARL, every agent has a set of *built-in capacities* considered essential 
 * to respect the commonly accepted competences of agents.
 * These capacities are considered the main building blocks on top of which other 
 * higher level capacities and skills can be constructed.
 * They are defined on the SARL language but the skill implementing them are provided 
 * by the runtime environment, e.g. the [Janus platform](http://www.janus-project.io).
 * This runtime environment is responsible for creating them and injecting them in 
 * the agent before their execution begins.
 * Therefore, when the agent receives the `Initialize` event they are
 * already available.
 * 
 * The following figure presents the different contexts associated to an agent `A`.
 * Several built-in capacities permit to access and manage these contexts.
 * The agents are represented by stylized humans, the contexts by the blue boxes,
 * and the spaces by the small color boxes in the contexts.
 * 
 * <center><img src="./contexts.png"/></center>
 */
@CreateWith(SARLSpecCreator)
describe "Built-in Capacity Reference" {

		@Inject extension SARLParser

		/* The built-in capacity `ExternalContextAccess` provides access to the 
		 * [context](SpaceReferenceSpec.html) that the agent is a part of, and actions
		 * required to join new [contexts](SpaceReferenceSpec.html), and leave them.
		 * 
		 * The context supported by this built-in capacity is the "external contexts",
		 * illustrated by the top-right context in the figure above.
		 */
		describe "ExternalContextAccess" {
			
			/*  For retrieving the context with a particular ID,
			 * this built-in capacity provides the following function:
			 * 
			 *     def getContext(contextID : UUID) : AgentContext
			 *
			 * 
			 * The agent must have joined (see below) the context before calling this 
			 * action. Or, the agent may use its `parentContextID` to access to the
			 * context in which it is located (the default context).
			 *  
			 * @filter(.*) 
			 */
			fact "Retrieving a Context"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.ExternalContextAccess
					import io.sarl.lang.core.AgentContext
					import java.util.UUID
					agent A {
						uses ExternalContextAccess
						def myaction {
							var id : UUID
							var c : AgentContext
							id = UUID::randomUUID
							c = getContext(id)
						}
					}".parsesSuccessfully
			}
			
			/* The following function enables an agent to retrieve
			 * all the contexts in which it is involved:
			 * 
			 *     def getAllContexts : SynchronizedCollection<AgentContext>
			 *
			 * 
			 * The default context is included in the replied collection.
			 *  
			 * @filter(.*) 
			 */
			fact "Retrieving the Contexts of an Agent"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.ExternalContextAccess
					import io.sarl.lang.core.AgentContext
					import io.sarl.lang.util.SynchronizedCollection
					agent A {
						uses ExternalContextAccess
						def myaction {
							var c : SynchronizedCollection<AgentContext>
							c = getAllContexts
						}
					}".parsesSuccessfully
			}
			
			/* Agents must be able to join a new parent context.
			 * The following function gives this capability to them:
			 * 
			 *     def join(contextID : UUID, expectedDefaultSpaceID : UUID)
			 *
			 * 
			 * This action registers the agent in the default space of the context.
			 * 
			 * The agent will be involved in the context with the ID given by `contextID`.
			 * The parameter `expectedDefaultSpaceID` is only used to check if
			 * the caller of this function knows the ID of the default space in the context to
			 * be involved in. 
			 * If the given `expectedDefaultSpaceID` does not match the ID of the
			 * default space in the context `contextID`, then the access to the context
			 * is forbidden.
			 * 
			 * <span class="label label-warning">Important</span> The context must already 
			 * exists, and the default space inside this context must have the same ID 
			 * as `expectedDefaultSpaceID`.
			 * 
			 * This action fires two events:
			 * 
			 *  * `ContextJoined` in the inner context's default space.
			 *  * `MemberJoined` in the parent context's default space.
			 *  
			 * @filter(.*) 
			 */
			fact "Joining an Existing Context"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.ExternalContextAccess
					import java.util.UUID
					agent A {
						uses ExternalContextAccess
						def myaction {
							var idc : UUID
							var ids : UUID
							idc = UUID::randomUUID
							ids = UUID::randomUUID
							join(idc, ids)
						}
					}".parsesSuccessfully
			}

			/* When an agent wants to leave a context, it must invoke:
			 * 
			 *     def leave(contextID : UUID)
			 *
			 * 
			 * This action fires two events:
			 * 
			 *  * `ContextLeft` in the inner context's default space.
			 *  * `MemberLeft` in the parent context's default space.
			 *  
			 * @filter(.*) 
			 */
			fact "Leaving a Context"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.ExternalContextAccess
					import java.util.UUID
					agent A {
						uses ExternalContextAccess
						def myaction {
							var idc : UUID
							idc = UUID::randomUUID
							leave(idc)
						}
					}".parsesSuccessfully
			}
		
		}

		/* The built-in capacity `InnerContextAccess` provides access to 
		 * the inner context of the agent.
		 * This is a key feature for creating holonic agent implementation.
		 * The context supported by this built-in capacity is the "inner context",
		 * illustrated by the bottom context in the figure above.
		 */
		describe "InnerContextAccess" {
			
			/* For retrieving the inner context of an agent,
			 * this built-in capacity provides the following function:
			 * 
			 *     def getInnerContext : AgentContext
			 *  
			 * @filter(.*) 
			 */
			fact "Retrieving the Inner Context"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.InnerContextAccess
					import io.sarl.lang.core.AgentContext
					agent A {
						uses InnerContextAccess
						def myaction {
							var c : AgentContext
							c = getInnerContext
						}
					}".parsesSuccessfully
			}

			/* For retrieving information on the member agents of the current agent,
			 * several functions are provided by this built-in capacity.
			 * A member agent is an agent that is not the
			 * calling agent, and is a member of at least
			 * one space of the inner context.
			 * 
			 * The first function replies if the calling agent has other agents
			 * as members of its inner context:
			 * 
			 *     def hasMemberAgent : boolean
			 *
			 * 
			 * The second function replies the number of agents that are members
			 * of the inner context of the calling agent:
			 * 
			 *     def getMemberAgentCount : int
			 *
			 *
			 * The third function replies all the member agents in the inner
			 * context:
			 * 
			 *     def getMemberAgents : SynchronizedSet<UUID>
			 *
			 * 
			 * @filter(.*) 
			 */
			fact "Members of an Agent"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.InnerContextAccess
					import io.sarl.lang.util.SynchronizedSet
					import java.util.UUID
					agent A {
						uses InnerContextAccess
						def myaction {
							var b : boolean
							var n : int
							var m : SynchronizedSet<UUID>
							b = hasMemberAgent
							n = getMemberAgentCount
							m = getMemberAgents
						}
					}".parsesSuccessfully
			}

		}
		
		/* The `DefaultContextInteractions` capacity is actually provided
		 * for convenience. It assumes that the action will be performed on the 
		 * agent's __default context__ or its __default space__.
		 * These context and space
		 * are illustrated by the top-left context in the figure above. 
		 * 
		 * For instance, the `emit` action is a shortcut for:
		 * 
		 *     defaultContext.defaultSpace.emit(...)
		 *
		 * 
		 * Therefore, it is actually created on top of the other built-in capacities.
		 */
		describe "DefaultContextInteractions" {
			
			/* For retrieving the default context of an agent,
			 * this built-in capacity provides the following function:
			 * 
			 *     def getDefaultContext : AgentContext
			 *
			 * 
			 * For retrieving the default space in the default context of an agent,
			 * this built-in capacity provides the following function:
			 * 
			 *     def getDefaultSpace : EventSpace
			 *
			 * 
			 * For obtaining the address of the agent in the default space,
			 * the following function is provided:
			 * 
			 *     def getDefaultAddress : Address
			 *
			 *  
			 * @filter(.*) 
			 */
			fact "Retrieving the Default Context and Space"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.lang.core.AgentContext
					import io.sarl.lang.core.EventSpace
					import io.sarl.lang.core.Address
					agent A {
						uses DefaultContextInteractions
						def myaction {
							var c : AgentContext
							var e : EventSpace
							var a : Address
							c = getDefaultContext
							e = getDefaultSpace
							a = getDefaultAddress
						}
					}".parsesSuccessfully
			}
  
			/* Many time, it is useful for agent to create a new agent
			 * into the default context. The following function is provided for this
			 * task:
			 * 
			 *     def spawn(agentType : Class<? extends Agent>, params : Object[]) : UUID
			 *
			 *
			 * This action creates an instance of the given agent type, and launches the agent
			 * into the default context. The parameters are passed to the spawned agent inside
			 * the `Initialize` event: the `parameters` field.
			 * 
			 * This action fires two events:
			 * 
			 *  * `AgentSpawned` in the default space of the default context. The source of the event is this spawner.
			 *  * `Initialize` in spawned agent.
			 *  
			 * @filter(.*) 
			 */
			fact "Spawning an Agent"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.lang.core.Agent
					import java.util.UUID
					agent A {
						uses DefaultContextInteractions
						def myaction {
							var aid : UUID
							var type : Class<? extends Agent>
							var p1 : Object
							var p2 : Object
							type = typeof(A)
							p1 = new Object
							p2 = new Object
							aid = spawn(type, #[p1, p2])
						}
					}".parsesSuccessfully
			}

			/* The core mechanism for information exchanges among agents is
			 * [event-based](./EventReferenceSpec.html).
			 * For sending an event in the default space of the default context,
			 * the following function is provided:
			 * 
			 *     def emit(e : Event)
			 *
			 *  
			 * This function emits the given event with no scope (i.e., all registered agent will
			 * receive the event) in the default space of the default context.
			 * 
			 * It is equivalent to:
			 * 
			 *     defaultContext.defaultSpace.emit(e)
			 *
			 *  
			 * @filter(.*) 
			 */
			fact "Sending an Event in the Default Space"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.DefaultContextInteractions
					event E
					agent A {
						uses DefaultContextInteractions
						def myaction {
							var e : E
							e = new E
							emit(e)
						}
					}".parsesSuccessfully
			}

			/* The previous sending function assumes that there is no
			 * restriction on the set of the receivers of the event.
			 * It is possible to specify a `Scope` for
			 * applying a restriction.
			 *
			 *     def emit(e : Event, scope : Scope<Address>)
			 * 
			 * 
			 * A scope is a predicates that is evaluated against the
			 * addresses of the receivers. It is defined as (in Java):
			 *
			 *     public interface Scope<T> extends Serializable {
			 *         public boolean matches(T element);
			 *     }
			 *
			 * 
			 * It is recommended to use the SARL utilities functions for creating scopes.
			 * They are defined in the class `io.sarl.util.Scopes`.
			 * The following example is equivalent to the feature call of
			 * `emit` without the scoping parameter:
			 *
			 *     emit(new Event, Scopes::allParticipants)
			 *
			 * 
			 * A default implementation of a scope using addresses is
			 * implemented in the Java class `io.sarl.util.AddressScope`.
			 * The utility class `Scopes` provides the `addresses` function for
			 * for creating an instance of `AddressScope`.
			 *
			 *     emit(new Event, Scopes::addresses(a1, a2))
			 * 
			 * 
			 * 
			 * You are free to create new implementation of `Scope`
			 * in order to filter the receivers of an event according to your
			 * own criteria.
			 *  
			 * @filter(.*) 
			 */
			fact "Sending an Event to Specific Agents in the Default Space"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.util.Scopes
					import io.sarl.lang.core.Address
					event E
					agent A {
						uses DefaultContextInteractions
						def myaction {
						    var a1 : Address
						    var a2 : Address
							var e : E
							e = new E
							emit(e, Scopes::allParticipants)
							emit(e, Scopes::addresses(a1, a2))
						}
					}".parsesSuccessfully
			}

		}

		/* The built-in capacity `Lifecycle` provides actions for 
		 * spawning new agents on different external contexts and 
		 * the inner context, as well as the `killMe` action to stop 
		 * the execution of an agent.
		 */
		describe "Lifecycle" {
			
			/* Because of the autonomy property of an agent, it can be stopped
			 * only by committing a suicide. It means that it is impossible to
			 * stop an agent from another agent: the agent to stop must
			 * be able to accept or reject this query.
			 * 
			 * The `Lifecycle` capacity provides the following function
			 * for committing a suicide:
			 *
			 *     def killMe
			 * 
			 *
			 * This action automatically unregisters the calling agent from 
			 * the default context, and therefore all its spaces including 
			 * the default space.
			 * 
			 * <span class="label label-danger">Important</span> If the killed 
			 * agent was a composed agent, it must not have members any more before 
			 * calling this action. Otherwise a `RuntimeException` is thrown.
			 * 
			 * This action fires two events:
			 *
			 *  * `AgentKilled` in the default space of all contexts to which the calling agent belongs.
			 *  * `Destroy` inside the killed agent agent.
			 *  
			 * @filter(.*) 
			 */
			fact "Stopping the Agent Execution"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Lifecycle
					agent A {
						uses Lifecycle
						def myaction {
							killMe
						}
					}".parsesSuccessfully
			}

			/* Many time, it is useful for an agent to create a new agent
			 * into a given context. The following function is provided for this
			 * task:
			 * 
			 *     def spawnInContext(agentType : Class<? extends Agent>,
			 *                        context : AgentContext,
			 *                        params : Object[]) : UUID
			 * 
			 *
			 * This action creates an instance of the given agent type, and launches the agent
			 * into the given context. The parameters are passed to the spawned agent inside
			 * the `Initialize` event: the `parameters` field.
			 * 
			 * This action fires two events:
			 *
			 *  * `AgentSpawned` in the default space of the context. The source of the event is the calling agent.
			 *  * `Initialize` in spawned agent.
			 *  
			 * @filter(.*) 
			 */
			fact "Spawning an Agent"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Lifecycle
					import io.sarl.lang.core.AgentContext
					import io.sarl.lang.core.Agent
					import java.util.UUID
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
							aid = spawnInContext(type, c, #[p1, p2])
						}
					}".parsesSuccessfully
			}

		}

		/* The built-in capacity `Schedules` enables the agent to 
		 * schedule tasks for future or periodic execution.
		 */
		describe "Schedules" {
			
			/* A named task may be created with:
			 * 
			 *     def task(name : String) : AgentTask
			 * 
			 * The replied task may be used for future execution, or
			 * controlling the execution.
			 * 
			 * @filter(.*) 
			 */
			fact "Creating Named Tasks"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Schedules
					import io.sarl.core.AgentTask
					agent A {
						uses Schedules
						def myaction {
							var n : String
							var t : AgentTask
							n = \"abc\"
							t = task(n)
						}
					}".parsesSuccessfully
			}

			/* For running a task in a given delay, the following functions are
			 * provided:
			 * 
			 *     def in(delay : long,
			 *            procedure : (Agent) => void) : AgentTask
			 *     def in(task : AgentTask,
			 *            delay : long,
			 *            procedure : (Agent) => void) : AgentTask
			 * 
			 * 
			 * The first function submits the given procedure (a lambda expression as defined in
			 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html)) to
			 * an executor provided by the runtime platform. The execution of the procedure
			 * will be delayed during the given number of milliseconds.
			 * This function replies the agent task for controlling its execution.
			 * 
			 * The second function behaves in a similar way as the first, except that it
			 * accepts an agent task as parameter. This task will attached to the given
			 * procedure. The replied task is the same as the task given as parameter.
			 * 
			 * @filter(.*) 
			 */
			fact "Launching a Delayed Task"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Schedules
					import io.sarl.core.AgentTask
					import io.sarl.lang.core.Agent
					agent A {
						uses Schedules
						def myaction {
							var t1 : AgentTask
							var t2 : AgentTask
							t1 = in(1000) [ a : Agent |
								println(a)
							]
							t1 = t2.in(1000) [ a : Agent |
								println(a)
							]
						}
					}".parsesSuccessfully
			}

			/* For running a periodic task, the following functions are
			 * provided:
			 * 
			 *     def every(period : long,
			 *               procedure : (Agent) => void) : AgentTask
			 *     def every(period : AgentTask,
			 *               delay : long,
			 *               procedure : (Agent) => void) : AgentTask
			 * 
			 * 
			 * The first function submits the given procedure (a lambda expression as defined in
			 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html)) to
			 * an executor provided by the runtime platform. The execution of the procedure
			 * will be launched periodically with a period of the given number of milliseconds.
			 * This function replies the agent task for controlling its execution.
			 * 
			 * The second function behaves in a similar way as the first, except that it
			 * accepts an agent task as parameter. This task will attached to the given
			 * procedure. The replied task is the same as the task given as parameter.
			 * 
			 * @filter(.*) 
			 */
			fact "Launching a Periodic Task"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Schedules
					import io.sarl.core.AgentTask
					import io.sarl.lang.core.Agent
					agent A {
						uses Schedules
						def myaction {
							var t1 : AgentTask
							var t2 : AgentTask
							t1 = every(1000) [ a : Agent |
								println(a)
							]
							t1 = t2.every(1000) [ a : Agent |
								println(a)
							]
						}
					}".parsesSuccessfully
			}

			/* It may be useful to cancel a running task, e.g. a
			 * periodic task. The `Schedules` capacity
			 * provides two functions for stopping the execution
			 * of an agent task:
			 * 
			 *     def cancel(task : AgentTask) : boolean
			 *     def cancel(task : AgentTask,
			 *                mayInterruptIfRunning : boolean) : boolean
			 *
			 * 
			 * These functions will reply `false` if the task has already completed, has 
			 * already been cancelled, or could not be cancelled for some other reason
			 * (a failure means replying false). 
			 * If successful, and this task has not started when `cancel` is
			 * called, this task should never run. If the task has already started,
			 * then the `mayInterruptIfRunning` parameter determines
			 * whether the thread executing this task should be interrupted in
			 * an attempt to stop the task.
			 * 
			 * The first function interrupts ongoing tasks. So, it is equivalent to 
			 * passing `true` as the value for the parameter 
			 * <tt>mayInterruptIfRunning</tt> to the function
			 * `cancel(AgentTask, boolean)`.
			 * 
			 * @filter(.*) 
			 */
			fact "Cancelling a Task"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Schedules
					import io.sarl.core.AgentTask
					agent A {
						uses Schedules
						def myaction {
							var t1 : AgentTask
							var t2 : AgentTask
							var t3 : AgentTask
							t1.cancel
							t2.cancel(true)
							t3.cancel(false)
						}
					}".parsesSuccessfully
			}

		}

		/* The built-in capacity `Behaviors` provides the tools to the agents 
		 * for dynamically registering and unregistering sub-behaviors.
		 * 
		 * This capacity is closely related to the `InnerContextAccess` for 
		 * enabling a high-level abstraction for holonic multiagent system development.
		 * 
		 * The definition of a behavior is not detailled in this reference document.
		 * Please read the [Behavior Reference](BehaviorReferenceSpec.html) for details.
		 */
		describe "Behaviors" {
			
			/* Assuming that a behavior was already defined,
			 * it is possible for an agent to register this behavior:
			 * 
			 *     def registerBehavior(attitude : Behavior) : Behavior
			 *
			 * 
			 * This function takes the behavior to be registered, and replies the
			 * same behavior.
			 * When a behavior is registered, it is receiving the events
			 * in the default space of the inner context of the agent, or
			 * received by the agent itself.
			 * 
			 * @filter(.*) 
			 */
			fact "Registering a Behavior"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Behaviors
					import io.sarl.lang.core.Behavior
					behavior B {
					}
					agent A {
						uses Behaviors
						def myaction {
							var b : B
							var c : Behavior
							b = new B(this)
							c = registerBehavior(b)
						}
					}".parsesSuccessfully
			}

			/* Assuming that a behavior was already registered,
			 * it is possible for an agent to unregister it:
			 * 
			 *     def unregisterBehavior(attitude : Behavior) : Behavior
			 *
			 * 
			 * This function takes the behavior to be unregistered, and replies the
			 * same behavior.
			 * When a behavior is unregistering, it is no more receiving the events
			 * in the default space of the inner context of the agent, and
			 * the ones received by the agent itself.
			 * 
			 * @filter(.*) 
			 */
			fact "Unregistering a Behavior"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Behaviors
					import io.sarl.lang.core.Behavior
					behavior B {
					}
					agent A {
						uses Behaviors
						def myaction {
							var b : B
							var c : Behavior
							b = new B(this)
							c = unregisterBehavior(b)
						}
					}".parsesSuccessfully
			}

			/* A behavior is executed through its event handlers.
			 * Consequently, for running a behavior, it is mandatory
			 * to wake it with an event. This particular feature is
			 * supported by:
			 * 
			 *     def wake(evt : Event)
			 *
			 * 
			 * This function emits the given event into the inner context
			 * of the agent (in the default space).
			 * 
			 * <span class="label label-warning">Important</span> It is not
			 * possible to execute a particular behavior explicitly.
			 * All the behaviors that are waiting for a given event will 
			 * be executed by this function.
			 * 
			 * @filter(.*) 
			 */
			fact "Executing a Behavior"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Behaviors
					import io.sarl.lang.core.Event
					event E
					agent A {
						uses Behaviors
						def myaction {
							var e : Event
							e = new E
							wake(e)
						}
					}".parsesSuccessfully
			}

			/* Sometimes, it is useful or mandatory for an agent to listen on the
			 * events in a given space. The following function permits to
			 * retreive the event listener of the agent:
			 * 
			 *     def asEventListener : EventListener
			 * 
			 * 
			 * The listener replied by this function is the one used by the
			 * agent (and its behaviors) for listening events related to
			 * all the contexts (default, external, and inner).
			 * 
			 * @filter(.*) 
			 */
			fact "Creating an Event Listener"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Behaviors
					import io.sarl.lang.core.EventListener
					agent A {
						uses Behaviors
						def myaction {
							var l : EventListener
							l = asEventListener
						}
					}".parsesSuccessfully
			}

  		}

		/* Details on the use of the built-in capacities may be found in the references of
		 * the major behavior-based concepts of SARL:
		 *
		 *  * [Agent](AgentReferenceSpec.html)
		 *  * [Behavior](BehaviorReferenceSpec.html)
		 */
		describe "Use of the Built-in Capacities"{
		}
		
}
