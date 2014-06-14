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
package io.sarl.docs.tutorials

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

/**
 * This document describes how to create a simple
 * agent-based application in which agents are
 * exchanging basic messages.
 * Before reading this document, it is recommended to read
 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html).
 * 
 * The elements that are explained in this tutorial are: <ul>
 * <li>Definition of an event;</li>
 * <li>Definition of an agent;</li>
 * <li>Emit an event occurrence in the default space;</li>
 * <li>Receiving event occurrences;</li>
 * <li>Pro-active behavior: waiting for partners.</li>
 * </ul>
 *
 * The principle of the application is the following.
 * The `Ping` agent is sending a `Ping` message to
 * all agents.
 * The `Pong` agent is receiving the `Ping` message,
 * and replies with a `Pong` message to the sender.
 * The `Ping` agent is receiving a `Pong` message and
 * replies to the sender with a new `Ping` message.
 * These messages contains an integer number that
 * indicates the number of the event.
 *  
 * The source code related to this tutorial may be found
 * in the [SARL demos](http://www.github/sarl/sarl-demos).
 */
@CreateWith(SARLSpecCreator)
describe "Agent Communication: the Ping Pong Agents"{

		@Inject extension SARLParser

		/* First, the `Ping` and `Pong` events must be defined.
		 */
		context "Event definition" {

			/* The `Ping` is an event that contains the
			 * index of the event. This index indicates
			 * at which position the event is located in
			 * the sequence of sent `Ping` event.
			 * 
			 * The `index` attribute is a value to make
			 * it unmodifiable after its initialization.
			 * 
			 * For setting the value of the `index` value,
			 * it is mandatory to define a constructor. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */			
			fact "Ping Event" {
				'''
				event Ping {
					val index : int
					new(i : int) {
						this.index = i
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong",
					// TEXT
					""
				)
			}
			
			/* The `Pong` is an event that contains the
			 * index of the `Ping` event for which the
			 * `Pong` event is created.
			 * 
			 * The `index` attribute is a value to make
			 * it unmodifiable after its initialization.
			 * 
			 * For setting the value of the `index` value,
			 * it is mandatory to define a constructor. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */			
			fact "Pong Event" {
				'''
				event Pong {
					val index : int
					new(i : int) {
						this.index = i
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong",
					// TEXT
					""
				)
			}

		}
		
		/* The second step of this tutorial is the definition of the
		 * agent that is waiting for `Ping` events, and replying
		 * `Pong` events.
		 */
		context "Ponging agent" {

			/* The initial defintion of the ponging agent is:
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "First definition" {
				'''
				agent PongAgent {
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong",
					// TEXT
					""
				)
			}

			/* The ponging agent needs to handle the `Ping` events.
			 * For that, a "behavior unit" must be defined in the
			 * agent. According to the 
			 * [General Syntax Reference](../reference/GeneralSyntaxReferenceSpec.html),
			 * the `on` keyword followed by the name of the event 
			 * permits to define a handler of events.
			 * This handler will be invoked by the runtime environment
			 * each time the agent is receiving a `Ping` event.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Handling the Ping event" {
				'''
				agent PongAgent {
					on Ping {
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong
					event Ping {
						val index : int
						new (i : int) {
							this.index = i
						}
					}",
					// TEXT
					""
				)
			}

			/* Now, it is time to define how the ponging agent
			 * is replying with a `Pong` message.
			 * 
			 * First, emitting an event in the default space
			 * must be done with a builtin capacity:
			 * `DefaultContextInteractions`. This capacity
			 * provides a collection of functions that 
			 * enables the agent to interect with the 
			 * default context, and its default space.
			 * 
			 * For using the capacity, it is recommended to
			 * declare it with the `uses` keyword.
			 * This keyword permits to the agent to directly
			 * call the functions of the capacity as if
			 * they are defined as actions in the agent.
			 * 
			 * The `DefaultContextInteractions` capacity
			 * provides the function `emit(Event)` for
			 * emitting an event in the default space of the
			 * default context.
			 * 
			 * The `Pong` event must be built with
			 * a index value as parameter. This parameter
			 * is the index stored in the `Ping` event.
			 * For accessing to the occurrence of the
			 * `Ping` event, you must use the special
			 * keyword `occurrence`.
			 * In the following example, the `Pong`
			 * event is built with the index parameter
			 * stored in the received `Ping` event.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Replying to Ping with a Pong" {
				'''
				agent PongAgent {
					uses DefaultContextInteractions
					on Ping {
						emit( new Pong( occurrence.index ) )
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong
					import io.sarl.core.DefaultContextInteractions
					event Ping {
						val index : int
						new (i : int) {
							this.index = i
						}
					}
					event Pong {
						val index : int
						new (i : int) {
							this.index = i
						}
					}",
					// TEXT
					""
				)
			}

			/* In the previous code, the event is emitted
			 * to all the agents belonging to the default
			 * space, including the ponging agent.
			 * 
			 * For restricting the receiver of the
			 * `Pong` event to the initial sender of the
			 * `Ping` event, you must define a scope for
			 * the `Pong` event.
			 * The `DefaultContextInteractions` capacity
			 * provides the function `emit(Event, Scope<Address>)`
			 * for emitting an event with a specific scope.
			 * 
			 * The SARL SDK contains the class `AddressScope`.
			 * It is an implementation of a `Scope`.
			 * It provides a factory function for creating
			 * a scope on addresses (an address is the identifier
			 * of an agent in the default space).
			 * 
			 * In the following code, the scope permits to
			 * restrict to the initial sender of the `Ping` event. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Restricting the scope of the Pong event" {
				'''
				agent PongAgent {
					uses DefaultContextInteractions
					on Ping {
						emit(
							new Pong( occurrence.index ),
							AddressScope.getScope( occurrence.source )
						)
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.util.AddressScope
					event Ping {
						val index : int
						new (i : int) {
							this.index = i
						}
					}
					event Pong {
						val index : int
						new (i : int) {
							this.index = i
						}
					}",
					// TEXT
					""
				)
			}

		}

		/* The third step of this tutorial is the definition of the
		 * agent that is sending `Ping` events, and waiting for
		 * `Pong` events.
		 */
		context "Pinging Agent" {

			/* The initial defintion of the pinging agent is:
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "First definition" {
				'''
				agent PingAgent {
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong",
					// TEXT
					""
				)
			}

			/* The pinging agent needs to handle the `Pong` events.
			 * For that, a "behavior unit" must be defined in the
			 * agent.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Handling the Pong event" {
				'''
				agent PingAgent {
					on Pong {
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong
					event Pong {
						val index : int
						new (i : int) {
							this.index = i
						}
					}",
					// TEXT
					""
				)
			}

			/* When the pinging agent is receiving a
			 * `Pong` event, it re-sends a `Ping` event
			 * to the sender of the `Pong` event.
			 * This new `Ping` event has an index greater
			 * than the one of the `Pong` event.
			 * 
			 * The reception of the `Ping` event is
			 * restricted to the sender of the
			 * `Pong` event.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Re-sending a Ping on Pong reception" {
				'''
				agent PingAgent {
					uses DefaultContextInteractions
					on Pong {
						emit(
							new Ping( occurrence.index + 1 ),
							AddressScope.getScope( occurrence.source )
						)
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.util.AddressScope
					event Ping {
						val index : int
						new (i : int) {
							this.index = i
						}
					}
					event Pong {
						val index : int
						new (i : int) {
							this.index = i
						}
					}",
					// TEXT
					""
				)
			}

			/* For starting the exchanges among the agents,
			 * it is mandatory to send a first occurence
			 * of the `Ping` event.
			 * 
			 * This emission is done when the pinging agent
			 * is started, i.e. when the agent is
			 * receiving the `Initialize` event.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Sending the first Ping" {
				'''
				agent PingAgent {
					uses DefaultContextInteractions
					on Pong {
						emit(
							new Ping( occurrence.index + 1 ),
							AddressScope.getScope( occurrence.source )
						)
					}
					on Initialize {
						emit( new Ping(0) )
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.core.Initialize
					import io.sarl.util.AddressScope
					event Ping {
						val index : int
						new (i : int) {
							this.index = i
						}
					}
					event Pong {
						val index : int
						new (i : int) {
							this.index = i
						}
					}",
					// TEXT
					""
				)
			}

			/* The previous code has a major problem:
			 * if there is no ponging agent launched
			 * when the pinging agent is emitting
			 * the first `Ping` event, the application
			 * will reach a deadlock, even if
			 * the ponging agent is launched later.
			 * 
			 * For solving this problem, the pinging
			 * agent must wait for sending the initial
			 * `Ping` event until the ponging agent
			 * is belonging to the default space.
			 * 
			 * The concrete implementation is based on
			 * the `Schedules` capacity, which provides
			 * a collection of functions for creating
			 * and launching asynchronous tasks.
			 * 
			 * In following code, a task is created with
			 * the name `waiting_for_partner`.
			 * This task is executed every second with
			 * the `every` function. The code between
			 * the brackets is the instruction block
			 * that will be periodically executed.
			 * 
			 * In this periodically executed code,
			 * the agent is testing if it is the only
			 * one agent belonging to the default space.
			 * If not, the agent is emitting the initial
			 * `Ping` event, and stopping the periodic task.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Delaying the sending of the first Ping" {
				'''
				agent PingAgent {
					uses DefaultContextInteractions, Schedules
					on Pong {
						emit(
							new Ping( occurrence.index + 1 ),
							AddressScope.getScope( occurrence.source )
						)
					}
					on Initialize {
						val task = task("waiting_for_partner")
						task.every(1000) [
							if (defaultSpace.participants.size > 1) {
								emit( new Ping(0) )
								task.cancel
							}
						]
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.core.Initialize
					import io.sarl.core.Schedules
					import io.sarl.util.AddressScope
					event Ping {
						val index : int
						new (i : int) {
							this.index = i
						}
					}
					event Pong {
						val index : int
						new (i : int) {
							this.index = i
						}
					}",
					// TEXT
					""
				)
			}

		}

		/* The fourth step of this tutorial is the definition
		 * of the launching process.
		 * In the rest of this section, we discuss the use
		 * of the [Janus runtime environment](http://www.janusproject.io)
		 * for running the agents.
		 * 
		 * Two methods may be used for launching the agents: <ol>
		 * <li>Execute each agent in a single instance of Janus.<br/>
		 *  On the command line, you must launch Janu with: <pre><code>
		 * 	java -cp app.jar io.janusproject.Boot io.sarl.docs.tutorials.pingpong.PongAgent
		 *  java -cp app.jar io.janusproject.Boot io.sarl.docs.tutorials.pingpong.PingAgent
		 *  </code></pre>
		 *  The file <code>app.jar</code> contains the compiled classes of the tutorial,
		 *  the Janus platform, and the SARL libraries.</li>
		 * <li>Define a specific agent that will launch the two other agents.
		 *     This approach is detailed below.</li>
		 * </ol>
		 */
		context "Launching the agents" {
			
			/* The boot agent uses the `DefaultContextInteractions`
			 * capacity for launching agents in the default context.
			 * This capacity provides the function `spawn(Class<? extends Agent>)`
			 * for launching an agent of the given type.
			 * 
			 * When the boot agent has launched the two expected agents,
			 * it is killing itself. This is done with the `killMe`
			 * function, which is provided by the `Lifecycle` capacity.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*)
			 */
			fact "Defining the Boot agent" {
				'''
				agent BootAgent {
					uses DefaultContextInteractions, Lifecycle
					on Initialize {
						spawn( PongAgent )
						spawn( PingAgent )
						killMe
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.tutorials.pingpong
					import io.sarl.core.DefaultContextInteractions
					import io.sarl.core.Initialize
					import io.sarl.core.Lifecycle
					agent PongAgent { }
					agent PingAgent { }",
					// TEXT
					""
				)
			}
			
		}

}
