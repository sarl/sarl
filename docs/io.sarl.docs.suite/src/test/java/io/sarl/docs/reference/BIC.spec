/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors and authors.
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

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

/**
 * @outline
 *
 * <p>This document describes the built-in capacities in SARL.
 * Before reading this document, we recommend that you read
 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
 * the [Capacity Reference](CapacityReferenceSpec.html),
 * and the [Skill Reference](SkillReferenceSpec.html).
 * 
 * <p>A *Capacity* is the specification of a collection of actions. This specification 
 * makes no assumptions about its implementation. It could be used to specify 
 * what an agent can do, what a behavior requires for its execution.
 * 
 * <p>A *Skill* is a possible implementation of a capacity fulfilling all the 
 * constraints of this specification.
 * 
 * <p>In SARL, every agent has a set of *built-in capacities* considered essential 
 * to respect the commonly accepted competencies of agents.
 * These capacities are considered the main building blocks on top of which other 
 * higher level capacities and skills can be constructed.
 * They are defined in the SARL language, but the skills implementing them are provided 
 * by the runtime environment, e.g. the [Janus platform](http://www.janus-project.io).
 * This runtime environment is responsible for creating them and injecting them in 
 * the agent before their execution begins.
 * Therefore, when the agent receives the `Initialize` event they are
 * already available.
 * 
 * <p>The following figure presents the different contexts associated to an agent `A`.
 * Several built-in capacities permit accessing and manage these contexts.
 * The agents are represented by stylized humans, the contexts by the blue boxes,
 * and the spaces by the small color boxes in the contexts.
 * 
 * ![Contexts](./contexts.png)
 */
@CreateWith(SARLSpecCreator)
describe "Built-in Capacity Reference" {

		@Inject extension SARLParser

		/* The built-in capacity `Logging` provides tools for printing
		 * messages in the log associated to the agent.
		 */
		describe "Logging" {
			
			/* For printing an error or a warning message, the two following functions
			 * are provided:
			 * 
			 *     def error(message : Object, exception : Throwable = null)
			 *     def warning(message : Object, exception : Throwable = null)
			 *
			 * 
			 * <p>The `message` parameter is converted to a string for obtaining the
			 * message to output.
			 * The `exception` parameter may be given for printing an exception that
			 * is the cause of the logging action.
			 *  
			 * @filter(.*) 
			 */
			fact "Print an error or a warning message" {
				// Tests URLs from the beginning of the page
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				"CapacityReferenceSpec.html" should beAccessibleFrom this
				"SkillReferenceSpec.html" should beAccessibleFrom this
				"SpaceReferenceSpec.html" should beAccessibleFrom this
				"./contexts.png" should beAccessibleFrom this
				//
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Logging
					agent A {
						uses Logging
						def myaction {
							error(\"mymessage\")
							error(\"mymessage\", new Exception)
							warning(\"mymessage\")
							warning(\"mymessage\", new Exception)
						}
					}".parseSuccessfully
			}

			/* For printing an information message, the two following functions
			 * are provided:
			 * 
			 *     def info(message : Object)
			 *     def println(message : Object)
			 *
			 * 
			 * <p>The `message` parameter is converted to a string for obtaining the
			 * message to output.
			 * There is no difference between these two functions.
			 * The function `println` is provided for backward compatibility, and
			 * will be deprecated in further versions of the capacity.
			 *  
			 * @filter(.*) 
			 */
			fact "Print an information message" {
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Logging
					agent A {
						uses Logging
						def myaction {
							info(\"mymessage\")
							println(\"mymessage\")
						}
					}".parseSuccessfully
			}

			/* For printing a debugging message, the following function
			 * is provided:
			 * 
			 *     def debug(message : Object)
			 *
			 * 
			 * <p>The `message` parameter is converted to a string for obtaining the
			 * message to output.
			 *  
			 * @filter(.*) 
			 */
			fact "Print a debugging message" {
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Logging
					agent A {
						uses Logging
						def myaction {
							debug(\"mymessage\")
						}
					}".parseSuccessfully
			}

			/* The printable messages are associated to a level of logging (error, warning, info, debug).
			 * If a message is given to the logging system, and the current output level is lower
			 * than the message's level, then the message is not output.
			 * 
			 * <p>For retrieving the current logging level, the following function is provided:
			 * 
			 *     def getLogLevel : int
			 *
			 * 
			 * <p>The replied value is 0 when no message is printed, 1 if only error messages
			 * are printed, 2 for error and warning messages, etc.
			 * 
			 * <p>For changing the current logging level, the following function is provided:
			 * 
			 *     def setLogLevel(level : int)
			 *  
			 *  
			 * @filter(.*) 
			 */
			fact "Retrieve and change the logging level" {
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Logging
					agent A {
						uses Logging
						def myaction {
							var l = getLogLevel
							setLogLevel( l + 1 )
						}
					}".parseSuccessfully
			}

			/* The following functions permits testing if a specific logging level is
			 * enabled:
			 * 
			 *     def isErrorLogEnabled : boolean
			 *     def isWarningLogEnabled : boolean
			 *     def isInfoLogEnabled : boolean
			 *     def isDebugLogEnabled : boolean
			 *
			 * @filter(.*) 
			 */
			fact "Testing the logging level" {
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Logging
					agent A {
						uses Logging
						def myaction : boolean {
							   isErrorLogEnabled
							|| isWarningLogEnabled
							|| isInfoLogEnabled
							|| isDebugLogEnabled
						}
					}".parseSuccessfully
			}

			/* By default, the logging message contains the identifier of the agent
			 * associated to the `Logging` capacity.
			 * 
			 * <p>Sometimes, it is helpful to change the printed name of the agent.
			 * The following function gives the opportunity to change this name.
			 * 
			 *     def setLoggingName(name : String)
			 *
			 * @filter(.*) 
			 */
			fact "Change the name of the logger" {
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Logging
					agent A {
						uses Logging
						def myaction {
							setLoggingName(\"toto\")
						}
					}".parseSuccessfully
			}

		}

		/* The built-in capacity `ExternalContextAccess` provides access to the 
		 * [context](SpaceReferenceSpec.html) that the agent is a part of, and actions
		 * required to join new [contexts](SpaceReferenceSpec.html), and leave them.
		 * 
		 * <p>The context supported by this built-in capacity is the "external context,"
		 * illustrated by the top-right context in the figure above.
		 */
		describe "ExternalContextAccess" {
			
			/* For retrieving the context with a particular ID,
			 * this built-in capacity provides the following function:
			 * 
			 *     def getContext(contextID : UUID) : AgentContext
			 *
			 * 
			 * <p>The agent must have joined (see below) the context before calling this 
			 * action. Or, the agent may use its `parentContextID` for accessing the
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
					}".parseSuccessfully
			}
			
			/* The following function enables an agent to retrieve
			 * all the contexts in which it is involved:
			 * 
			 *     def getAllContexts : SynchronizedCollection<AgentContext>
			 *
			 * 
			 * <p>The default context is included in the replied collection.
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
					}".parseSuccessfully
			}
			
			/* Agents must be able to join a new parent context.
			 * The following function gives this capability to them:
			 * 
			 *     def join(contextID : UUID, expectedDefaultSpaceID : UUID)
			 *
			 * 
			 * <p>This action registers the agent in the default space of the context.
			 * 
			 * <p>The agent will be involved in the context with the ID given by `contextID`.
			 * The parameter `expectedDefaultSpaceID` is only used to check if
			 * the caller of this function knows the ID of the default space in the context to
			 * be involved in. 
			 * If the given `expectedDefaultSpaceID` does not match the ID of the
			 * default space in the context `contextID`, then the access to the context
			 * is forbidden.
			 * 
			 * <importantnote> The context must already 
			 * exist, and the default space inside this context must have the same ID 
			 * as `expectedDefaultSpaceID`.</importantnote>
			 * 
			 * <p>This action fires two events:
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
					}".parseSuccessfully
			}

			/* When an agent wants to leave a context, it must invoke:
			 * 
			 *     def leave(contextID : UUID)
			 *
			 * 
			 * <p>This action fires two events:
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
					}".parseSuccessfully
			}
		
			/* The `ExternalContextAccess` provides a collection of utility functions
			 * that test if their parameters are related to the any external context.
			 *
			 * <table>
			 * <thead><tr><th>Function</th><th>Explanation</th></tr></thead>
			 * <tbody>
			 * <tr><td><code>isInSpace(Event, Space)</code></td><td>tests if the given event was emitted in
			 * the given space.</td></tr>
			 * <tr><td><code>isInSpace(Event, SpaceID)</code></td><td>tests if the given event was emitted in
			 * the space with the given identifier.</td></tr>
			 * <tr><td><code>isInSpace(Event, UUID)</code></td><td>tests if the given event was emitted in
			 * the space with the given identifier.</td></tr>
			 * </tbody>
			 * </table>
			 * 
			 * <p>The following example illustrates the use of the `isInSpace` function in the guard
			 * of an behavior unit. In this example, the behavior unit is run only if the event
			 * of type `AnEvent` was emitted in the space `myspace` (declared as attribute in
			 * the container).
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Testing if an element is related to an external context" {
				val model = '''
				on AnEvent [ isInSpace(occurrence, myspace) ] {
					// Do something with the event when it was emitted in the space myspace
				}
				'''.parseSuccessfully(
					"	package io.sarl.docs.reference.bic
						import io.sarl.core.ExternalContextAccess
						import io.sarl.lang.core.Space
						import io.sarl.lang.core.Event
						event AnEvent
						agent MyAgent {
							uses ExternalContextAccess
							var myspace : Space
							def testOtherFunctions(e : Event) : boolean {
			 					return isInSpace(e, myspace.ID)
								    || isInSpace(e, myspace.ID.ID)
							}",
					// TEXT
					"	}"
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.bic"
					it should haveNbImports 3
					it should importClass "io.sarl.core.ExternalContextAccess"
					it should importClass "io.sarl.lang.core.Space"
					it should importClass "io.sarl.lang.core.Event"
					it should haveNbElements 2
				]
				
				model.xtendTypes.get(0) => [
					it should beEvent "AnEvent"
					it should extend _
					it should haveNbElements 0
				]

				model.xtendTypes.get(1) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 4
					
					it.members.get(0) => [
						it should beCapacityUse "io.sarl.core.ExternalContextAccess"
					]

					it.members.get(1) => [
						it should beVariable "myspace"
					]

					it.members.get(2) => [
						it should beAction "testOtherFunctions"
					]

					it.members.get(3) => [
						it should beBehaviorUnit "io.sarl.docs.reference.bic.AnEvent"
						it should beGuardedWith "isInSpace(occurrence, myspace)"
					]
				]
			}

		}

		/* The built-in capacity `InnerContextAccess` provides access to 
		 * the inner context of the agent.
		 * This is a key feature for creating holonic agent implementation.
		 * The context supported by this built-in capacity is the "inner context,"
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
					}".parseSuccessfully
			}

			/* For retrieving information on the member agents of the current agent,
			 * several functions are provided by this built-in capacity.
			 * A member agent is an agent that is not the
			 * calling agent, and is a member of at least
			 * one space of the inner context.
			 * 
			 * <p>The first function replies if the calling agent has other agents
			 * as members of its inner context:
			 * 
			 *     def hasMemberAgent : boolean
			 *
			 * 
			 * <p>The second function replies the number of agents that are members
			 * of the inner context of the calling agent:
			 * 
			 *     def getMemberAgentCount : int
			 *
			 *
			 * <p>The third function replies all the member agents in the inner
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
					}".parseSuccessfully
			}

			/* The `InnerContextAccess` provides a collection of utility functions
			 * that test if their parameters are related to the inner context.
			 *
			 * <table>
			 * <thead><tr><th>Function</th><th>Explanation</th></tr></thead>
			 * <tbody>
			 * <tr><td><code>isInnerDefaultSpace(Space)</code></td><td>tests if the given space is the
			 * default space of the inner context.</td></tr>
			 * <tr><td><code>isInnerDefaultSpace(SpaceID)</code></td><td>tests if the default space of
			 * the inner context has the given identifier.</td></tr>
			 * <tr><td><code>isInnerDefaultSpace(UUID)</code></td><td>tests if the default space of
			 * the inner context has the given identifier.</td></tr>
			 * <tr><td><code>isInInnerDefaultSpace(Event)</code></td><td>tests if the given event was emitted in
			 * the default space of the inner context.</td></tr>
			 * </tbody>
			 * </table>
			 * 
			 * <p>The following example illustrates the use of the `isInInnerDefaultSpace` function in the guard
			 * of an behavior unit. In this example, the behavior unit is run only if the event
			 * of type `AnEvent` was emitted in the default space of the inner context.
			 * 
			 * <note>According to the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
			 * the `event.inInnerDefaultSpace` is equivalent to `isInInnerDefaultSpace(event)`.</note>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Testing if an element is related to the inner context" {
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				val model = '''
				on AnEvent [ occurrence.inInnerDefaultSpace ] {
					// Do something with the event when it was emitted in the inner default space
				}
				'''.parseSuccessfully(
					"	package io.sarl.docs.reference.bic
						import io.sarl.core.InnerContextAccess
						import io.sarl.lang.core.Space
						event AnEvent
						agent MyAgent {
							uses InnerContextAccess
							def testOtherFunctions(s : Space) : boolean {
			 					return isInnerDefaultSpace(s)
								    || isInnerDefaultSpace(s.ID)
								    || isInnerDefaultSpace(s.ID.ID)
							}",
					// TEXT
					"	}"
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.bic"
					it should haveNbImports 2
					it should importClass "io.sarl.core.InnerContextAccess"
					it should importClass "io.sarl.lang.core.Space"
					it should haveNbElements 2
				]
				
				model.xtendTypes.get(0) => [
					it should beEvent "AnEvent"
					it should extend _
					it should haveNbElements 0
				]

				model.xtendTypes.get(1) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 3
					
					it.members.get(0) => [
						it should beCapacityUse "io.sarl.core.InnerContextAccess"
					]

					it.members.get(1) => [
						it should beAction "testOtherFunctions"
					]

					it.members.get(2) => [
						it should beBehaviorUnit "io.sarl.docs.reference.bic.AnEvent"
						it should beGuardedWith "occurrence.inInnerDefaultSpace"
					]
				]
			}

		}
		
		/* The `DefaultContextInteractions` capacity is actually provided
		 * for convenience. It assumes that the action will be performed on the 
		 * agent __default context__ or its __default space__.
		 * These context and space
		 * are illustrated by the top-left context in the figure above. 
		 * 
		 * <p>For instance, the `emit` action is a shortcut for:
		 * 
		 *     defaultContext.defaultSpace.emit(...)
		 *
		 * 
		 * <p>Therefore, it is actually created on top of the other built-in capacities.
		 */
		describe "DefaultContextInteractions" {
			
			/* For retrieving the default context of an agent,
			 * this built-in capacity provides the following function:
			 * 
			 *     def getDefaultContext : AgentContext
			 *
			 * 
			 * <p>For retrieving the default space in the default context of an agent,
			 * this built-in capacity provides the following function:
			 * 
			 *     def getDefaultSpace : EventSpace
			 *
			 * 
			 * <p>For obtaining the address of the agent in the default space,
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
					}".parseSuccessfully
			}
  
			/* Many time, it is useful for agent to create a new agent
			 * into the default context. The following function is provided for this
			 * task:
			 * 
			 *     def spawn(agentType : Class<? extends Agent>, params : Object[]) : UUID
			 *
			 *
			 * <p>This action creates an instance of the given agent type, and launches the agent
			 * into the default context. The parameters are passed to the spawned agent inside
			 * the `Initialize` event: the `parameters` field.
			 * 
			 * <p>This action fires two events:
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
					}".parseSuccessfully
			}

			/* The core mechanism for information exchanges among agents is
			 * [event-based](EventReferenceSpec.html).
			 * For sending an event in the default space of the default context,
			 * the following function is provided:
			 * 
			 *     def emit(e : Event)
			 *
			 *  
			 * <p>This function emits the given event with no scope (i.e., all registered agent will
			 * receive the event) in the default space of the default context.
			 * 
			 * <p>It is equivalent to:
			 * 
			 *     defaultContext.defaultSpace.emit(e)
			 *
			 *  
			 * @filter(.*) 
			 */
			fact "Sending an Event in the Default Space"{
				"EventReferenceSpec.html" should beAccessibleFrom this
				//
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
					}".parseSuccessfully
			}

			/* The previous sending function assumes that there is no
			 * restriction on the set of the receivers of the event.
			 * It is possible to specify a `Scope` for
			 * applying a restriction.
			 *
			 *     def emit(e : Event, scope : Scope<Address>)
			 * 
			 * 
			 * <p>A scope is a predicates that is evaluated against the
			 * addresses of the receivers. It is defined as (in Java):
			 *
			 *     interface Scope<T> extends Serializable {
			 *         def matches(element : T) : boolean
			 *     }
			 *
			 * 
			 * <p>It is recommended using the SARL utility functions for creating scopes.
			 * They are defined in the class `io.sarl.util.Scopes`.
			 * The following example is equivalent to the feature call of
			 * `emit` without the scoping parameter:
			 *
			 *     emit(new Event, Scopes::allParticipants)
			 *
			 * 
			 * <p>A default implementation of a scope using addresses is
			 * implemented in the Java class `io.sarl.util.AddressScope`.
			 * The utility class `Scopes` provides the `addresses` function for
			 * creating an instance of `AddressScope`.
			 *
			 *     emit(new Event, Scopes::addresses(a1, a2))
			 * 
			 * 
			 * <p>The complete list of the functions that are provided by the `Scopes` class is
			 * accessible on the <a href="http://www.sarl.io/docs/api/index.html?io/sarl/util/Scopes.html">Scopes API documentation</a>.
			 * 
			 * <p>You are free to create new implementation of `Scope`
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
					}".parseSuccessfully
				"http://www.sarl.io/docs/api/index.html?io/sarl/util/Scopes.html" should beApiURL ""
			}

			/* The `DefaultContextInteractions` provides a collection of utility functions
			 * that test if their parameters are related to the default context or the
			 * default space.
			 *
			 * <table>
			 * <thead><tr><th>Function</th><th>Explanation</th></tr></thead>
			 * <tbody>
			 * <tr><td><code>isDefaultContext(AgentContext)</code></td><td>tests if the given context is the default
			 * context.</td></tr>
			 * <tr><td><code>isDefaultContext(UUID)</code></td><td>tests if the default context has the given
			 * identifier.</td></tr>
			 * <tr><td><code>isDefaultSpace(Space)</code></td><td>tests if the given space is the default
			 * space of the default context.</td></tr>
			 * <tr><td><code>isDefaultSpace(UUID)</code></td><td>tests if the default space of the
			 * default context has the given identifier.</td></tr>
			 * <tr><td><code>isInDefaultSpace(Event)</code></td><td>tests if the given event was emitted
			 * in the default space of the default context.</td></tr>
			 * </tbody>
			 * </table>
			 * 
			 * <p>The following example illustrates the use of the `isInDefaultSpace` function in the guard
			 * of an behavior unit. In this example, the behavior unit is run only if the event
			 * of type `AnEvent` was emitted in the default space.
			 * 
			 * <note>According to the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
			 * the `event.inDefaultSpace` is equivalent to `isInDefaultSpace(event)`.</note>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Testing if an element is related to the default context" {
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				val model = '''
				on AnEvent [ occurrence.inDefaultSpace ] {
					// Do something with the event when it was emitted in the default space
				}
				'''.parseSuccessfully(
					"	package io.sarl.docs.reference.bic
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
							}",
					// TEXT
					"	}"
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.bic"
					it should haveNbImports 1
					it should importClass "io.sarl.core.DefaultContextInteractions"
					it should haveNbElements 2
				]
				
				model.xtendTypes.get(0) => [
					it should beEvent "AnEvent"
					it should extend _
					it should haveNbElements 0
				]

				model.xtendTypes.get(1) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 3
					
					it.members.get(0) => [
						it should beCapacityUse "io.sarl.core.DefaultContextInteractions"
					]

					it.members.get(1) => [
						it should beAction "testOtherFunctions"
					]

					it.members.get(2) => [
						it should beBehaviorUnit "io.sarl.docs.reference.bic.AnEvent"
						it should beGuardedWith "occurrence.inDefaultSpace"
					]
				]
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
			 * <p>The `Lifecycle` capacity provides the following function
			 * for committing a suicide:
			 *
			 *     def killMe
			 * 
			 *
			 * <p>This action automatically unregisters the calling agent from 
			 * the default context, and therefore all its spaces including 
			 * the default space.
			 * 
			 * <veryimportantnote> If the killed 
			 * agent was a composed agent, it must not have members any more before 
			 * calling this action, otherwise a `RuntimeException` is thrown.
			 * </veryimportantnote>
			 * 
			 * <p>This action fires two events:
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
					}".parseSuccessfully
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
			 * <p>This action creates an instance of the given agent type, and launches the agent
			 * into the given context. The parameters are passed to the spawned agent inside
			 * the `Initialize` event: the `parameters` field.
			 * 
			 * <p>This action fires two events:
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
					}".parseSuccessfully
			}

			/* Some time, it is useful to create an agent with a specific
			 * identifier. The following function permits to spawn an agent
			 * with a given identifier in a specific context:
			 * 
			 *     def spawnInContextWithID(agentType : Class<? extends Agent>,
			 *                              agentId : UUID,
			 *                              context : AgentContext,
			 *                              params : Object[]) : UUID
			 * 
			 *
			 * <p>This action creates an instance of the given agent type, with
			 * the given identifier, and launches the agent
			 * into the given context.
			 * The parameters are passed to the spawned agent inside
			 * the `Initialize` event: the `parameters` field.
			 * 
			 * <p>This action fires two events:
			 *
			 *  * `AgentSpawned` in the default space of the context. The source of the event is the calling agent.
			 *  * `Initialize` in spawned agent.
			 *  
			 * @filter(.*) 
			 */
			fact "Spawning an Agent with a specific identifier"{
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
							aid = spawnInContextWithID(type, aid, c, #[p1, p2])
						}
					}".parseSuccessfully
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
			 * <p>The replied task may be used for future execution, or
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
					}".parseSuccessfully
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
			 * <p>The first function submits the given procedure (a lambda expression as defined in
			 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html)) to
			 * an executor provided by the runtime platform. The execution of the procedure
			 * will be delayed during the given number of milliseconds.
			 * This function replies the agent task for controlling its execution.
			 * 
			 * <p>The second function behaves in a similar way as the first, except that it
			 * accepts an agent task as parameter. This task will attach to the given
			 * procedure. The replied task is the same as the task given as parameter.
			 * 
			 * @filter(.*) 
			 */
			fact "Launching a Delayed Task"{
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				//
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Logging
					import io.sarl.core.Schedules
					import io.sarl.core.AgentTask
					import io.sarl.lang.core.Agent
					agent A {
						uses Schedules, Logging
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
					}".parseSuccessfully
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
			 * <p>The first function submits the given procedure (a lambda expression as defined in
			 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html)) to
			 * an executor provided by the runtime platform. The execution of the procedure
			 * will be launched periodically with a period of the given number of milliseconds.
			 * This function replies the agent task for controlling its execution.
			 * 
			 * <p>The second function behaves in a similar way as the first, except that it
			 * accepts an agent task as parameter. This task will attach to the given
			 * procedure. The replied task is the same as the task given as parameter.
			 * 
			 * <p>If the duration of the task is greater to the given period length, then
			 * multiple task's instances will be run in parallel.
			 * For example, consider the following code:
			 *
			 *     every(500) [ sleep(2000) ]
			 *
			 *
			 * <p>At a given time, four instances of the task are
			 * run in parallel (A, B, C, D for example):
			 *
			 * <table>
			 * <thead>
			 * <tr><th>t=</th><th>0</th><th>500</th><th>1000</th><th>1500</th><th>2000</th><th>2500</th><th>3000</th><th>3500</th><th>4000</th></tr>
			 * </thead>
			 * <tbody>
			 * <tr><td>A</td><td>X</td><td>X</td><td>X</td><td>X</td><td></td><td></td><td></td><td></td><td></td></tr>
			 * <tr><td>B</td><td></td><td>X</td><td>X</td><td>X</td><td>X</td><td></td><td></td><td></td><td></td></tr>
			 * <tr><td>C</td><td></td><td></td><td>X</td><td>X</td><td>X</td><td>X</td><td></td><td></td><td></td></tr>
			 * <tr><td>D</td><td></td><td></td><td></td><td>X</td><td>X</td><td>X</td><td>X</td><td></td><td></td></tr>
			 * <tr><td>E</td><td></td><td></td><td></td><td></td><td>X</td><td>X</td><td>X</td><td>X</td><td></td></tr>
			 * <tr><td>F</td><td></td><td></td><td></td><td></td><td></td><td>X</td><td>X</td><td>X</td><td>X</td></tr>
			 * </tbody>
			 * </table>
			 * 
			 * @filter(.*) 
			 */
			fact "Launching a Periodic Task"{
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				//
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Logging
					import io.sarl.core.Schedules
					import io.sarl.core.AgentTask
					import io.sarl.lang.core.Agent
					agent A {
						uses Schedules, Logging
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
					}".parseSuccessfully
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
			 * <p>These functions will reply `false` if the task has already completed, has 
			 * already been cancelled, or could not be cancelled for some other reason
			 * (a failure means replying false). 
			 * If successful, and this task has not started when `cancel` is
			 * called, this task should never run. If the task has already started,
			 * then the `mayInterruptIfRunning` parameter determines
			 * whether the thread executing this task should be interrupted in
			 * an attempt to stop the task.
			 * 
			 * <p>The first function interrupts ongoing tasks. So, it is equivalent to 
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
					}".parseSuccessfully
			}

		}

		/* The built-in capacity `Behaviors` provides the tools to the agents 
		 * for dynamically registering and unregistering sub-behaviors.
		 * 
		 * <p>This capacity is closely related to the `InnerContextAccess` for 
		 * enabling a high-level abstraction for holonic multi-agent system development.
		 * 
		 * <p>The definition of a behavior is not detailed in this reference document.
		 * Please read the [Behavior Reference](BehaviorReferenceSpec.html) for details.
		 */
		describe "Behaviors" {
			
			/* Assuming that a behavior was already defined,
			 * it is possible for an agent to register this behavior:
			 * 
			 *     def registerBehavior(attitude : Behavior) : Behavior
			 *
			 * 
			 * <p>This function takes the behavior to be registered, and replies the
			 * same behavior.
			 * When a behavior is registered, it is receiving the events
			 * in the default space of the inner context of the agent, or
			 * received by the agent itself.
			 *
			 * <p>An example of call to the registration function is:
			 * 
			 *     var beh = new MyBehavior
			 *     registerBehavior(beh)
			 *
			 * <p>According to the SARL syntax reference, the example could be also written as: 
			 * 
			 *     var beh = new MyBehavior
			 *     beh.registerBehavior
			 * 
			 * @filter(.*) 
			 */
			fact "Registering a Behavior"{
				// Test the URL in the introduction of this section
				"BehaviorReferenceSpec.html" should beAccessibleFrom this
				//
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
					}".parseSuccessfully
			}

			/* Assuming that a behavior was already registered,
			 * it is possible for an agent to unregister it:
			 * 
			 *     def unregisterBehavior(attitude : Behavior) : Behavior
			 *
			 * 
			 * <p>This function takes the behavior to be unregistered, and replies the
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
					}".parseSuccessfully
			}

			/* Assuming that a behavior was already defined,
			 * it is possible for an agent to register this behavior that may received only the events
			 * matching a specific filtering function. For registering such a behavior with its filter,
			 * the following function could be used:
			 * 
			 *     def registerBehavior(attitude : Behavior, filter : (Event) -> boolean) : Behavior
			 *
			 * 
			 * <p>This function takes the behavior to be registered, and replies the
			 * same behavior.
			 * When a behavior is registered, it is receiving the events that are matching the given
			 * filter in the default space of the inner context of the agent, or
			 * received by the agent itself.
			 * The filtering function is invoked for each event that should be given to the behavior.
			 * If the filtering function replies {@code true}, the event is really dispatching into the
			 * behavior. If the function replies {@code false}, the event is discarded to the behavior.
			 *
			 * <p>An example of call to the registration function is:
			 * 
			 *     var beh = new MyBehavior
			 *     registerBehavior(beh, [event | event instanceof MyEvent])
			 *
			 * <p>According to the SARL syntax reference, the example could be also written as: 
			 * 
			 *     var beh = new MyBehavior
			 *     beh.registerBehavior [event | event instanceof MyEvent]
			 * 
			 * @filter(.*) 
			 */
			fact "Registering a Behavior with an event filter"{
				// Test the URL in the introduction of this section
				"BehaviorReferenceSpec.html" should beAccessibleFrom this
				//
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
							c = registerBehavior(b) [true]
						}
					}".parseSuccessfully
			}

			/* A behavior is executed through its event handlers.
			 * Consequently, for running a behavior, it is mandatory
			 * to wake it with an event. This particular feature is
			 * supported by:
			 * 
			 *     def wake(evt : Event)
			 *
			 * 
			 * <p>This function emits the given event into the inner context
			 * of the agent (in the default space).
			 * 
			 * <importantnote> It is not
			 * possible to execute a particular behavior explicitly.
			 * All the behaviors that are waiting for a given event will 
			 * be executed by this function.</importantnote>
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
					}".parseSuccessfully
			}

			/* Sometimes, it is useful or mandatory for an agent to listen on the
			 * events in a given space. The following function permits
			 * retrieving the event listener of the agent:
			 * 
			 *     def asEventListener : EventListener
			 * 
			 * 
			 * <p>The listener replied by this function is the one used by the
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
					}".parseSuccessfully
			}

  		}

		/* The built-in capacity `Time` provides tools for obtaining the current time
		 * from the run-time platform.
		 *
		 * Time definition is application-dependent and platform-dependent. In other words,
		 * the time values replied by this capacity depends on the run-time environment:
		 * it may be the operating system time, or a simulator time.
		 */
		describe "Time" {
			
			/* For obtaining the current time, the `getTime` function is provides by the `Time`
			 * capacity:
			 * 
			 *     def getTime(timeUnit : TimeUnit = null) : double
			 *
			 * 
			 * <p>The timeUnit parameter will enable you to specify the unit of the replied
			 * value (hours, seconds, milliseconds, etc.). If it is not provided,
			 * the values will be expressed in seconds.
			 *  
			 * @filter(.*) 
			 */
			fact "Get the current time" {
				"	package io.sarl.docs.reference.bic
					import io.sarl.core.Time
					import io.sarl.core.Logging
					import java.util.concurrent.TimeUnit
					agent A {
						uses Time
						def myaction {
							var ct = getTime
							var ct2 = getTime(null)
							var ct3 = getTime(TimeUnit::HOURS)
						}
					}".parseSuccessfully
			}

		}

		/* Details on the use of the built-in capacities may be found in the references of
		 * the major behavior-based concepts of SARL:
		 *
		 *  * [Agent](AgentReferenceSpec.html)
		 *  * [Behavior](BehaviorReferenceSpec.html)
		 * 
		 * @filter(.*)
		 */
		fact "Use of the Built-in Capacities"{
			"AgentReferenceSpec.html" should beAccessibleFrom this
			"BehaviorReferenceSpec.html" should beAccessibleFrom this
		}
		
	/* Specification: SARL General-purpose Agent-Oriented Programming Language ("Specification")<br/>
	 * Version: %sarlspecversion%<br/>
	 * Status: %sarlspecreleasestatus%<br/>
	 * Release: %sarlspecreleasedate%
	 * 
	 * 
	 * <p>Copyright &copy; %copyrightdate% %copyrighters%.
	 * 
	 * <p>Licensed under the Apache License, Version 2.0;
	 * you may not use this file except in compliance with the License.
	 * You may obtain a copy of the [License](http://www.apache.org/licenses/LICENSE-2.0).
	 *
	 * @filter(.*) 
	 */
	fact "Legal Notice" {
		// The checks are valid only if the macro replacements were done.
		// The replacements are done by Maven.
		// So, Eclipse Junit tools do not make the replacements.
		System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
		//
		"%sarlversion%" should startWith "%sarlspecversion%"
		("%sarlspecreleasestatus%" == "Stable Release"
			|| "%sarlspecreleasestatus%" == "Draft Release") should be true
		"%sarlspecreleasedate%" should beDate "YYYY-mm-dd"
		"%copyrightdate%" should beNumber "0000";
		("%copyrighters%".empty || "%copyrighters%".startsWith("%")) should be false
	}

}
