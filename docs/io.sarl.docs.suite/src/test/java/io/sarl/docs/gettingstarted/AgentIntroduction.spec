/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.docs.gettingstarted

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import io.sarl.lang.sarl.Action
import io.sarl.lang.sarl.Agent
import org.eclipse.xtext.xbase.XBlockExpression
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*

/**
 * @outline
 * 
 * To create our first agent, right click on the project and follow 
 * **New > File**.
 * Name the file `myproject.sarl`.
 * 
 * The SARL default editor will open.
 */
@CreateWith(SARLSpecCreator)
describe "Agent Definition Introduction" {
	
	@Inject extension SARLParser

	/**
	 * Agents are defined using the `agent` keyword.
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "Basic agent definition" {
		val model = '''
			agent MyAgent {
			}
		'''.parseSuccessfully(
			"package io.sarl.docs.gettingstarted.^agent",
			// TEXT
			""
		)
		
		model => [
			it should havePackage "io.sarl.docs.gettingstarted.agent"
			it should haveNbImports 0
			it should haveNbElements 1
		]
		
		model.elements.get(0) => [
			it should beAgent "MyAgent"
			it should extend _
			it should haveNbElements 0
		]
	} 
	
	/*
	 * SARL elements are organized in packages.
	 * You can define the package using the `package` keyword.
	 * 
	 * The following code will define an agent with a fully qualified 
	 * name of `io.sarl.docs.gettingstarted.agent.MyAgent`.
	 * The character `^` in the package name permits to use a SARL
	 * keyword into a package name.
	 * 
	 * <importantnote>The package keyword defines 
	 * the package for all elements in the same SARL file (see the
	 * [General Syntax Reference](../reference/GeneralSyntaxReferenceSpec.html)
	 * for details).
	 * Therefore FirstAgent and SecondAgent belong to the same package 
	 * (i.e. `io.sarl.docs.gettingstarted.agent`).</importantnote>
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*) 
	 */
	fact "Package definition" {
		"../reference/GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
		//
		val model = '''
			package io.sarl.docs.gettingstarted.^agent
			agent MyAgent {}
			agent SecondAgent {}
		'''.parseSuccessfully

		model => [
			it should havePackage "io.sarl.docs.gettingstarted.agent"
			it should haveNbImports 0
			it should haveNbElements 2
		]
		
		model.elements.get(0) => [
			it should beAgent "MyAgent"
			it should extend _
			it should haveNbElements 0
		]

		model.elements.get(1) => [
			it should beAgent "SecondAgent"
			it should extend _
			it should haveNbElements 0
		]
	}

	/*
	 * Agents need to perceive their environment in order to react to external stimuli.
	 * Perceptions take the form of events
	 * (see [Event](../reference/EventReferenceSpec.html) and
	 * [Agent](../reference/AgentReferenceSpec.html) References for details).
	 */
	context "Agent Perceptions" {
		
		/*
		 * To declare a new event use the `event` keyword.
		 * The following code defines a new event `MyEvent`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Declare an Event"{
			// Test the URLs in the header of the section
			"../reference/EventReferenceSpec.html" should beAccessibleFrom this
			"../reference/AgentReferenceSpec.html" should beAccessibleFrom this
			//
			var model = '''
			event MyEvent
			'''.parseSuccessfully(
				"package io.sarl.docs.gettingstarted.^agent",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.gettingstarted.agent"
				it should haveNbImports 0
				it should haveNbElements 1
			]
			
			model.elements.get(0) => [
				it should beEvent "MyEvent"
				it should extend _
				it should haveNbElements 0
			]
		}
		

		
		/* 
		 * Now, we will want our agent to react to `MyEvent` and 
		 * print a message on the console.
		 * 
		 * To define this event handler, we must use the `on` keyword,
		 * and provide the associated code block.
		 * 
		 * <note>The `println` function is provided by the `Logging` capacity.
		 * It permits printing a message on the log output.</note>
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Define an agent Perceptions"{
			val model = '''
			agent MyAgent {
				uses Logging
				on MyEvent {
					println("Received MyEvent")
				}
			} 
			'''.parseSuccessfully(
				"package io.sarl.docs.gettingstarted.^agent
				import io.sarl.core.Logging
				event MyEvent",
				// TEXT
				""
			)

			model => [
				it should havePackage "io.sarl.docs.gettingstarted.agent"
				it should haveNbImports 1
				it should importClass "io.sarl.core.Logging"
				it should haveNbElements 2
			]
			
			model.elements.get(0) => [
				it should beEvent "MyEvent"
				it should extend _
				it should haveNbElements 0
			]

			model.elements.get(1) => [
				it should beAgent "MyAgent"
				it should extend _
				it should haveNbElements 2
				(it as Agent).features.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				(it as Agent).features.get(1) => [
					it should beBehaviorUnit "io.sarl.docs.gettingstarted.agent.MyEvent"
					it should beGuardedWith _
				]
			]
		}
		
		/*
		 * SARL defines two **lifecycle** events :
		 * 
		 *  * `Initialize`:  Notifies the creation of the agent, and passes the initialization parameters to the agents.
		 *  * `Destroy`: Notifies the destruction of the agent.
		 *
		 *  
		 * This means that when agent has been spawned and it is ready to 
		 * begin its execution, it will receive an `Initialize` event.
		 * You can react to this event just like with any other event defined in SARL.
		 * 
		 * Likewise, when the agent is going to stop its execution 
		 * (we will see how to stop an agent later on), it will 
		 * receive a `Destroy` Event. The purpose of this event is to 
		 * release any system resource properly.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Lifecycle events" {
			val model = '''
				import io.sarl.core.Logging
				import io.sarl.core.Initialize
				import io.sarl.core.Destroy
				
				agent MyAgent {
					uses Logging
					
					on Initialize {
						println("MyAgent spawned")
					}
					
					on Destroy {
						println("MyAgent destroyed")
					}
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.gettingstarted.^agent",
				// TEXT
				""
			)

			model => [
				it should havePackage "io.sarl.docs.gettingstarted.agent"
				it should haveNbImports 3
				it should importClass "io.sarl.core.Logging"
				it should importClass "io.sarl.core.Initialize"
				it should importClass "io.sarl.core.Destroy"
				it should haveNbElements 1
			]
			
			model.elements.get(0) => [
				it should beAgent "MyAgent"
				it should extend _
				it should haveNbElements 3
				(it as Agent).features.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				(it as Agent).features.get(1) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith _
				]
				(it as Agent).features.get(2) => [
					it should beBehaviorUnit "io.sarl.core.Destroy"
					it should beGuardedWith _
				]
			]
		}
		
		/*
		 * Inside a behavior declaration you may need to access the event
		 * instance the agent is reacting to.
		 * 
		 * This instance is called an `occurrence`.
		 * 
		 * In the case of an Initialize events you can access the arguments 
		 * for the agent spawn using `occurrence.parameters`
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Accessing the event's occurrence" {
			val model = '''
				agent MyAgent {
					uses Logging
					
					on Initialize {
						println("MyAgent spawned")
						println("My Parameters are :"
							+ occurrence.parameters.toString)
					}
					
					on Destroy {
						println("MyAgent destroyed")
					}
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.gettingstarted.^agent
				import io.sarl.core.Logging
				import io.sarl.core.Initialize
				import io.sarl.core.Destroy",
				// TEXT
				""
			)

			model => [
				it should havePackage "io.sarl.docs.gettingstarted.agent"
				it should haveNbImports 3
				it should importClass "io.sarl.core.Logging"
				it should importClass "io.sarl.core.Initialize"
				it should importClass "io.sarl.core.Destroy"
				it should haveNbElements 1
			]
			
			model.elements.get(0) => [
				it should beAgent "MyAgent"
				it should extend _
				it should haveNbElements 3
				(it as Agent).features.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				(it as Agent).features.get(1) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith _
				]
				(it as Agent).features.get(2) => [
					it should beBehaviorUnit "io.sarl.core.Destroy"
					it should beGuardedWith _
				]
			]
		}
		
	}
	
	/*
	 * Agents need to send data and stimuli to other agents.
	 * This communication takes the form of event sending
	 * (see [Event](../reference/EventReferenceSpec.html) and
	 * [Agent](../reference/AgentReferenceSpec.html) References for details).
	 */
	context "Agent Communication" {
		
		/* 
		 * Now, we will want our agent to send data to other agents.
		 * The data are embedded into events. The definition of an
		 * event is described above.
		 * 
		 * <note> 
		 * In this document, we limit our explanation to the
		 * sending of the events in the default space of 
		 * the default context of the agent.</note>
		 * 
		 * For sending an event in the default space, the
		 * `DefaultContextInteractions` built-in capacity
		 * should be used.
		 * 
		 * Below, we define an agent that is used this
		 * capacity.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Use the capacity to send an event in the default space"{
			// Test the URLs in the header of the section
			"../reference/EventReferenceSpec.html" should beAccessibleFrom this
			"../reference/AgentReferenceSpec.html" should beAccessibleFrom this
			//
			val model = '''
			agent MyAgent {
				uses DefaultContextInteractions
			} 
			'''.parseSuccessfully(
				"package io.sarl.docs.gettingstarted.^agent
				import io.sarl.core.DefaultContextInteractions",
				// TEXT
				""
			)

			model => [
				it should havePackage "io.sarl.docs.gettingstarted.agent"
				it should haveNbImports 1
				it should importClass "io.sarl.core.DefaultContextInteractions"
				it should haveNbElements 1
			]
			
			model.elements.get(0) => [
				it should beAgent "MyAgent"
				it should extend _
				it should haveNbElements 1
				(it as Agent).features.get(0) should beCapacityUse "io.sarl.core.DefaultContextInteractions"
			]
		}
		
		/* 
		 * The
		 * `DefaultContextInteractions` built-in capacity
		 * provides functions for sending events in the
		 * default space.
		 * 
		 * Below, we define an action in which an
		 * instance of `MyEvent` is created, and then
		 * sent into the default space with the function
		 * `emit(Event)`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Send an event in the default space"{
			val model = '''
			agent MyAgent {
				uses DefaultContextInteractions
				def doSomething {
					var e = new MyEvent
					emit(e)
				}
			} 
			'''.parseSuccessfully(
				"package io.sarl.docs.gettingstarted.^agent
				import io.sarl.core.DefaultContextInteractions
				event MyEvent",
				// TEXT
				""
			)


			model => [
				it should havePackage "io.sarl.docs.gettingstarted.agent"
				it should haveNbImports 1
				it should importClass "io.sarl.core.DefaultContextInteractions"
				it should haveNbElements 2
			]
			
			model.elements.get(0) => [
				it should beEvent "MyEvent"
				it should extend _
				it should haveNbElements 0
			]

			model.elements.get(1) => [
				it should beAgent "MyAgent"
				it should extend _
				it should haveNbElements 2
				(it as Agent).features.get(0) should beCapacityUse "io.sarl.core.DefaultContextInteractions"
				(it as Agent).features.get(1) => [
					it should beAction "doSomething"
					it should reply _
					it should haveNbParameters 0
					it should beVariadic false
					(it as Action).body should be typeof(XBlockExpression)
				]
			]
		}

	}

	/*
	 * In the next section, we will learn how to start a SARL agent on the
	 * command line.
	 * 
	 * [Next>](RunSARLAgentInTheEclipseIDESpec.html)
	 * 
	 * @filter(.*)
	 */
	fact "What's next?" {
		"RunSARLAgentInTheEclipseIDESpec.html" should beAccessibleFrom this
	}

}
