/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.jnario.runner.CreateWith

/**
 * This document describes the builtin capacities in SARL.
 * Before reading this document, it is recommended to read
 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html),
 * the [Capacity Reference](./CapacityReferenceSpec.html),
 * and the [Skill Reference](./SkillReferenceSpec.html).
 * 
 * A *Capacity* is the specification of a collection of actions. This specification 
 * makes no assumptions about its implementation. It could be used to specify 
 * what an agent can do, what a behavior requires for its execution.
 * 
 * A *Skill* is a possible implementation of a capacity fulfilling all the 
 * constraints of this specification.
 * 
 * Every agent in SARL has a set of *built-in capacities* considered essential 
 * to respect the commonly accepted competences of agents.
 * These capacities are considered the main building blocks on top of which other 
 * higher level capacities and skills can be constructed.
 * They are defined on the SARL language but the skill implementing them are provided 
 * by the runtime environment, e.g. the [Janus platform)(http://www.janus-project.io).
 * This runtime environmentis responsible for creating them and injecting them on 
 * the agent before their execution begins.
 * Therefore, when the agent receives the <code>Initialize</code> event they are
 * already available.
 * 
 * The following figure presents the different contexts assocated to an agent.
 * Several builtin capacities permit to access and manage these contexts.
 * The agents are represented by stylized humans, the contexts by the blue boxes,
 * and the spaces by the small colorized boxes in the contexts.   
 * ![Contexts and Spaces.](./Contexts.png)
 */
@CreateWith(SARLSpecCreator)
describe "Builtin Capacity Reference" {

		@Inject extension SARLParser
		@Inject extension IQualifiedNameProvider

		/* The builtin capacity <code>ExternalContextAccess</code> provides access to the 
		 * [context](SpaceReferenceSpec.html) that the agent is a part of, and actions
		 * required to join new [contexts](SpaceReferenceSpec.html), and leave them.
		 * 
		 * The context supported by this builtin capacity is the "external contexts",
		 * illustrated by the top-right context in the figure above.
		 */
		describe "ExternalContextAccess" {
			
			/*  For retreiving the context with a particular ID,
			 * this builtin capacity provides the following function:<pre><code>
			 * def getContext(contextID : UUID) : AgentContext
			 * </code></pre>
			 * 
			 * The agent must have joined (see below) the context before calling this 
			 * action or use its <code>parentContextID</code>.
			 *  
			 * @filter(.*) 
			 */
			fact "Retreiving a Context"{
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
			
			/* The following function enables an agent to retreive
			 * all the contexts in which it is involved:<pre><code>
			 * def getAllContexts : SynchronizedCollection<AgentContext>
			 * </code></pre>
			 * The default context is included in the replied collection.
			 *  
			 * @filter(.*) 
			 */
			fact "Retreiving the Contexts of an Agent"{
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
			 * The following function gives this capability to them:<pre><code>
			 * def join(contextID : UUID, expectedDefaultSpaceID : UUID)
			 * </code></pre>
			 * This action registers the agent in the default space of the context.
			 * 
			 * The agent will be involved in the context with the ID given by <code>contextID</code>.
			 * The parameter <code>expectedDefaultSpaceID</code> is only used to check if
			 * the caller of this function knows the ID of the default space in the context to
			 * be involved in. 
			 * If the given <code>expectedDefaultSpaceID</code> does not match the ID of the
			 * default space in the context <code>contextID</code>, then the access to the context
			 * is forbidden.
			 * 
			 * <span class="label label-warning">Important</span> The context must already 
			 * exists, and the default space inside this context must have the same ID 
			 * as <code>expectedDefaultSpaceID</code>.
			 * 
			 * This action fires two events: <ul>
			 * <li><code>ContextJoined</code> in its inner context default space.</li>
			 * <li><code>MemberJoined</code> in its parent context default space.</li>
			 * </ul>
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

			/* When an agent wants to leave a context, it must invoke:<pre><code>
			 * def leave(contextID : UUID)
			 * </code></pre>
			 * 
			 * This action fires two events: <ul>
			 * <li><code>ContextLeft</code> in its inner context default space.</li>
			 * <li><code>MemberLeft</code> in its parent context default space.</li>
			 * </ul>
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

		/* The builtin capacity <code>InnerContextAccess</code> provides access to 
		 * the inner context of the agent.
		 * This is keystone for holonic agent implementation.
		 * The context supported by this builtin capacity is the "inner context",
		 * illustrated by the bottom context in the figure above.
		 */
		describe "InnerContextAccess" {
			
			/* For retreiving the inner context of an agent,
			 * this builtin capacity provides the following function:<pre><code>
			 * def getInnerContext : AgentContext
			 * </code></pre>
			 *  
			 * @filter(.*) 
			 */
			fact "Retreiving the Inner Context"{
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

			/* For retreiving information on the member agents of the current agent,
			 * several functions are provided by this builtin capacity.
			 * A member agent is an agent which is not the
			 * calling agent, and is a member of at least
			 * one space of the inner context.
			 * 
			 * The first function replies if the calling agent has other agents
			 * as members of its inner context: <pre><code>
			 * def hasMemberAgent : boolean
			 * </code></pre>
			 * 
			 * The second function replies the number of agents that are members
			 * of the inner context of the calling agent: <pre><code>
			 * def getMemberAgentCount : int
			 * </code></pre>
			 *
			 * The third function replies all the member agents in the inner
			 * context: <pre><code>
			 * def getMemberAgents : SynchronizedSet<UUID>
			 * </code></pre>
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
		
		/* The <code>DefaultContextInteractions</code> capacity is actually provided
		 * for convenience. It assumes that the action will be performed on the 
		 * agent's *default context* and its *default space*. These context and space
		 * are illustrated by the top-left context in the figure above. 
		 * 
		 * For instance, the <code>emit</code> action is a shortcut for:<pre><code>
		 * defaultContext.defaultSpace.emit(...)
		 * </code></pre>
		 * Therefore, it is actually created on top of the other builtin capacities.
		 */
		describe "DefaultContextInteractions" {
			
			/* For retreiving the default context of an agent,
			 * this builtin capacity provides the following function:<pre><code>
			 * def getDefaultContext : AgentContext
			 * </code></pre>
			 * 
			 * For retreiving the default space in the default context of an agent,
			 * this builtin capacity provides the following function:<pre><code>
			 * def getDefaultSpace : EventSpace
			 * </code></pre>
			 * 
			 * For obtaining the address of the agent in the default space,
			 * the following function is provided: <pre><code>
			 * def getDefaultAddress : Address
			 * </code></pre>
			 *  
			 * @filter(.*) 
			 */
			fact "Retreiving the Default Context and Space"{
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
  
			/* Most of the time, it is necessary for agent to create a new agent
			 * into the default context. The following function is provided for this
			 * task:
			 * <pre><code>
			 * def spawn(agentType : Class<? extends Agent>, params : Object[]) : UUID
			 * </code></pre>
			 *
			 * This action creates an instance of the given agent type, and launchs the agent
			 * into the default context. The parameters are passed to the spawned agent inside
			 * the <code>Initialize</code> event: the <code>parameters</code> field.
			 * 
			 * This action fires two events: <ul>
			 * <li><code>AgentSpawned</code> in the default space of the default context. The source of the event is this spawning agent.</li>
			 * <li><code>Initialize</code> in spawned agent.</li>
			 * </ul>
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
			 * For emitting an event in the default space of the default context,
			 * the following function is provided: <pre><code>
			 * def emit(e : Event)
			 * </code></pre> 
			 * This function emits a given event with no scope (all registered agent will
			 * receive the event) in the default space of the default context.
			 * 
			 * It is equivalent to: <pre><code>
			 * defaultContext.defaultSpace.emit(e)
			 * </code></pre>
			 *  
			 * @filter(.*) 
			 */
			fact "Emitting an Event in the Default Space"{
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

			/* The previous emitting function assumes that there is no
			 * restriction on the set of the receivers of the event.
			 * It is possible to specify a <code>Scope</code> for
			 * applying a restriction.
			 * <pre><code>
			 * def emit(e : Event, scope : Scope<Address>)
			 * </code></pre> 
			 * 
			 * A scope is a predicates that is evaluated against the
			 * address of the receiver. It is defined as (in Java): <pre><code>
			 * public interface Scope<T> extends Serializable {
			 *     public boolean matches(T element);
			 * }
			 * </code></pre>
			 * 
			 * A default implementation of a scope using addresses is provided:
			 * <code>io.sarl.util.AddressScope</code>.
			 * 
			 * You must also use the utilities functions for creating scopes.
			 * They are defined in the class <code>io.sarl.util.Scopes</code>.
			 * The following example is equivalent to the feature call of
			 * <code>emit</code> without the scoping parameter: <pre><code>
			 * emit(new Event, Scopes::allParticipants)
			 * </code></pre> 
			 * 
			 * 
			 * You are free to create new implementation of <code>Scope</code>
			 * in order to filter the receivers of an event according to your
			 * own critera.
			 *  
			 * @filter(.*) 
			 */
			fact "Emitting an Event to Specific Agents in the Default Space"{
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.util.AddressScope
					import io.sarl.util.Scopes
					event E
					agent A {
						uses DefaultContextInteractions
						def myaction {
							var e : E
							e = new E
							emit(e, AddressScope::getScope)
							emit(e, Scopes::allParticipants)
						}
					}".parsesSuccessfully
			}

		}

		/* The builtin capacity <code>Lifecycle</code> provides actions to 
		 * spawn new agents on different external contexts and 
		 * the innner context, as well as the <code>killMe</code> action to stop 
		 * its own execution.
		 */
		describe "Lifecycle" {
			
			/* Because of the autonomy property of an agent, it can be stopped
			 * only be commiting a suicide. It means that it is impossible to
			 * stop an agent's from another agent: the agent to stop must
			 * be able to accept or reject this query.
			 * 
			 * The <code>Lifecycle</code> capacity provides the following function
			 * for committing a suicide:
			 * <pre><code>
			 * def killMe
			 * </code></pre>
			 *
			 * This action must automatically unregister this agent from the default context
			 * and therefore all its spaces including the default space.
			 * 
			 * <span class="label label-warning">Important</span> If this is a composed agent,
			 * it must not have any members before calling this action. Otherwise a 
			 * RuntimeException will be thrown.
			 * 
			 * This action fires two events: <ul>
			 * <li><code>AgentKilled</code> in the default space of all contexts to which this agent belongs.</li>
			 * <li><code>Destroy</code> inside the agent.</li>
			 * </ul>
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

			/* Most of the time, it is necessary for agent to create a new agent
			 * into a given context. The following function is provided for this
			 * task:
			 * <pre><code>
			 * def spawnInContext(agentType : Class<? extends Agent>, context : AgentContext, params : Object[]) : UUID
			 * </code></pre>
			 *
			 * This action creates an instance of the given agent type, and launchs the agent
			 * into the given context. The parameters are passed to the spawned agent inside
			 * the <code>Initialize</code> event: the <code>parameters</code> field.
			 * 
			 * This action fires two events: <ul>
			 * <li><code>AgentSpawned</code> in the default space of the context. The source of the event is this spawning agent.</li>
			 * <li><code>Initialize</code> in spawned agent.</li>
			 * </ul>
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

		/* The builtin capacity <code>Schedules</code> enables the agent to 
		 * schedule tasks for future or periodic execution.
		 */
		describe "Schedules" {
			
			/* A named task may be created with: <pre><code>
			 * def task(name : String) : AgentTask
			 * </code></pre>
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
			 * provided: <pre><code>
			 * def in(delay : long, procedure : (Agent) => void) : AgentTask
			 * def in(task : AgentTask, delay : long, procedure : (Agent) => void) : AgentTask
			 * </code></pre>
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
			 * provided: <pre><code>
			 * def every(period : long, procedure : (Agent) => void) : AgentTask
			 * def every(period : AgentTask, delay : long, procedure : (Agent) => void) : AgentTask
			 * </code></pre>
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
			 * periodic task. The <code>Schedules</code> capacity
			 * provides two functions for stopping the execution
			 * of an agent task: <pre><code>
			 * def cancel(task : AgentTask) : boolean
			 * def cancel(task : AgentTask, mayInterruptIfRunning : boolean) : boolean
			 * </code></pre>
			 * 
			 * These functions will fail if the task has already completed, has 
			 * already been cancelled, or could not be cancelled for some other reason
			 * (a failure means replying false). 
			 * If successful, and this task has not started when <code>cancel</code> is
			 * called, this task should never run. If the task has already started,
			 * then the <code>mayInterruptIfRunning</code> parameter determines
			 * whether the thread executing this task should be interrupted in
			 * an attempt to stop the task.
			 * 
			 * The first function interrupts ongoing tasks. So, it is equivalent to 
			 * passing <code>true</code> as the value for the parameter 
			 * <tt>mayInterruptIfRunning</tt> to the function
			 * <code>cancel(AgentTask, boolean)</code>.
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

		/* The builtin capacity <code>Behaviors</code> provides the tools to the agents 
		 * for dynamically registered and unregistered sub-behaviors.
		 * 
		 * This capacity is closely related to the <code>InnerContextAccess</code> to 
		 * enable a high-level abstraction on holonic multiagent system development.
		 * 
		 * The definition of a behavior is not detailled in this reference document.
		 * Please read the [Behavior Reference](BehaviorReferenceSpec.html) for details.
		 */
		describe "Behaviors" {
			
			/* Assuming that a behavior was already defined,
			 * it is possible for an agent to register with behavior: <pre><code>
			 * def registerBehavior(attitude : Behavior) : Behavior
			 * </code></pre>
			 * This function takes the behavior to be registered, and replies the
			 * same behavior.
			 * When a behavior is registering, it is receiving the events
			 * in the default space of the inner context of the agent, or
			 * received by the agent.
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
			 * it is possible for an agent to unregister it: <pre><code>
			 * def unregisterBehavior(attitude : Behavior) : Behavior
			 * </code></pre>
			 * This function takes the behavior to be registered, and replies the
			 * same behavior.
			 * When a behavior is unregistering, it is no more receiving the events
			 * in the default space of the inner context of the agent, and
			 * received by the agent.
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
			 * supported by: <pre><code>
			 * def wake(evt : Event)
			 * </code></pre>
			 * 
			 * This function emits the given event into the inner context
			 * of the agent (in its default space).
			 * 
			 * <span class="label label-warning">Important</span> It is not
			 * possible to execute a particular behavior. All the behaviors
			 * waiting for a given event will be executed by this function.
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
			 * retreive the event listener of the agent: <pre><code>
			 * def asEventListener : EventListener
			 * </code></pre>
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

		/* Details on the use of the builtin capacities may be found in the references of
		 * the major behavior-based concepts of SARL:<ul>
		 * <li>[Agent](AgentReferenceSpec.html)</li>
		 * <li>[Behavior](BehaviorReferenceSpec.html)</li>
		 * </ul>
		 */
		describe "Use of the Builtin Capacities"{
		}
		
}
