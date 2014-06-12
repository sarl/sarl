/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.docs.gettingstarted

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

/**
 * To create our first agent, right click on the project and follow **New** > **File**.
 * Name the file **demosarl.sarl**.
 * 
 * The SARL default editor will open.
 */
@CreateWith(SARLSpecCreator)
describe "Agent Definition Introduction" {
	
	@Inject extension SARLParser
	
	
	/*
	 * Agents are defined using the `agent` keyword.
	 * 
	 * SARL elements are organized in packages.
	 * You can define the package using the <code>package</code> keyword.
	 * 
	 * The following code will define an agent with a fully qualified 
	 * name of `io.sarl.docs.gettingstarted.agent.MyAgent`.
	 * The character `^` in the package name permits to use a SARL
	 * keyword into a package name.
	 * 
	 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
	 */
	fact "Basic agent definition" {
		val model = '''
			package io.sarl.docs.gettingstarted.^agent
			
			agent MyAgent {
			}
		'''.parsesSuccessfully
		model.mustHavePackage("io.sarl.docs.gettingstarted.agent")
		model.mustNotHaveImport
		model.mustHaveTopElements(1)
		model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(0)
	} 
	
	/*
	 * <span class="label label-info">Important</span> The package keyword defines 
	 * the package for all elements in the same SARL file (see the
	 * [General Syntax Reference](../reference/GeneralSyntaxReferenceSpec.html)
	 * for details). 
	 * 
	 * Therefore FirstAgent and SecondAgent belong to the same package 
	 * (i.e. `io.sarl.docs.gettingstarted.agent`).
	 * 
	 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
	 */
	fact "Package definition" {
		val model = '''
			package io.sarl.docs.gettingstarted.^agent
			agent MyAgent {}
			agent SecondAgent {}
		'''.parsesSuccessfully
		model.mustHavePackage("io.sarl.docs.gettingstarted.agent")
		model.mustNotHaveImport
		model.mustHaveTopElements(2)
		model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(0)
		model.elements.get(1).mustBeAgent("SecondAgent", null).mustHaveFeatures(0)
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
		 * @filter('''|.parsesSuccessfully) 
		 */
		fact "Declare an Event"{
			var model = '''
			event MyEvent
			'''.parsesSuccessfully(
				"package io.sarl.docs.gettingstarted.^agent",
				// TEXT
				""
			)
			model.mustHavePackage("io.sarl.docs.gettingstarted.agent")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			model.elements.get(0).mustBeEvent("MyEvent", null).mustHaveFeatures(0)
		}
		

		
		/* 
		 * Now we will want our agent to react to `MyEvent` and 
		 * print a message on the console.
		 * 
		 * Note that `println` is a shortcut for the Java function
		 * `System.out.println`.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Define an agent Perceptions"{
			val model = '''
			agent MyAgent {
				on MyEvent {
					println("Received MyEvent")
				}
			} 
			'''.parsesSuccessfully(
				"package io.sarl.docs.gettingstarted.^agent
				event MyEvent",
				// TEXT
				""
			)
			model.mustHavePackage("io.sarl.docs.gettingstarted.agent")
			model.mustNotHaveImport
			model.mustHaveTopElements(2)
			model.elements.get(0).mustBeEvent("MyEvent", null).mustHaveFeatures(0)
			var a = model.elements.get(1).mustBeAgent("MyAgent", null).mustHaveFeatures(1)
			a.features.get(0).mustBeBehaviorUnit("io.sarl.docs.gettingstarted.agent.MyEvent", false)
		}
		
		/*
		 * SARL defines two **lifecycle** events :
		 * 
		 *  * `Initialize`:  Notifies the creation of the agent and passes it initialization parameters.
		 *  * `Destroy`: Notifies the Destruction of the agent.
		 * 
		 * This means that when agent has been spawned and its ready to 
		 * begin its execution, it will receive an `Initialize` event.
		 * You can react to this event just like with any other event defined in SARL.
		 * 
		 * Likewise, when the agent is going to stop its execution 
		 * (we will see how to stop an agent later on), it will 
		 * receive a `Destroy` Event. The purpose of this event is to 
		 * release any System resources properly.
		 *
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Lifecycle events" {
			val model = '''
				import io.sarl.core.Initialize
				import io.sarl.core.Destroy
				
				agent MyAgent {
					
					on Initialize {
						println("MyAgent spawned")
					}
					
					on Destroy {
						println("MyAgent destroyed")
					}
				}
			'''.parsesSuccessfully(
				"package io.sarl.docs.gettingstarted.^agent",
				// TEXT
				""
			)
			model.mustHavePackage("io.sarl.docs.gettingstarted.agent")
			model.mustHaveImports(2)
			model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
			model.mustHaveImport(1, "io.sarl.core.Destroy", false, false, false)
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(2)
			a.features.get(0).mustBeBehaviorUnit("io.sarl.core.Initialize", false)
			a.features.get(1).mustBeBehaviorUnit("io.sarl.core.Destroy", false)
		}
		
		/*
		 * Inside a behavior declaration you may need to access the event
		 * instance the agent is reacting to.
		 * 
		 * This instance is called an `occurrence`.
		 * 
		 * In the case of an Initialize events you can access the parameters 
		 * for the agent spawn using `occurrence.parameters`
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Accessing the event's occurrence" {
			val model = '''
				agent MyAgent {
					
					on Initialize {
						println("MyAgent spawned")
						println("My Parameters are :"
							+ occurrence.parameters.toString)
					}
					
					on Destroy {
						println("MyAgent destroyed")
					}
				}
			'''.parsesSuccessfully(
				"package io.sarl.docs.gettingstarted.^agent
				import io.sarl.core.Initialize
				import io.sarl.core.Destroy",
				// TEXT
				""
			)
			model.mustHavePackage("io.sarl.docs.gettingstarted.agent")
			model.mustHaveImports(2)
			model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
			model.mustHaveImport(1, "io.sarl.core.Destroy", false, false, false)
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(2)
			a.features.get(0).mustBeBehaviorUnit("io.sarl.core.Initialize", false)
			a.features.get(1).mustBeBehaviorUnit("io.sarl.core.Destroy", false)
		}
		
	}
	
	
	/*
	 * In the next section, we will learn how to start a SARL agent.
	 * 
	 * [Next](RunSARLAgentSpec.html).
	 */
	describe "What's next?" { }

}
