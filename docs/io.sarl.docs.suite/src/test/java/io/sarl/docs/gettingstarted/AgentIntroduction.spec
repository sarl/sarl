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
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.BehaviorUnit
import io.sarl.lang.sarl.Event
import io.sarl.lang.sarl.SarlScript
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.jnario.runner.CreateWith

import static org.junit.Assert.*

/**
 * Once you have installed SARL SDK in eclipse you can create a new Maven project and 
 * reuse the pom found on the demos.
 * 
 * To create our first agent, right click on the project and follow **New** > **File**.
 * Name the file **demosarl.sarl**.
 * 
 * The SARL default editor will open.
 */
@CreateWith(SARLSpecCreator)
describe "Agent Definition Introduction" {
	
	@Inject extension SARLParser
	@Inject extension IQualifiedNameProvider
	
	
	/*
	 * 
	 * Agents are defined using the `agent` keyword.
	 * 
	 * SARL elements are organized in packages.
	 * You can define the package using the <code>package</code> keyword.
	 * 
	 * The following code will define an agent with a fully qualified name of `myapp.demo.MyAgent`.
	 * 
	 * 
	 * 
	 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
	 */
	fact "Basic agent definition" {
		val model = '''
			package myapp.demo
			
			agent MyAgent {}
		'''.parsesSuccessfully
		assertEquals("myapp.demo.MyAgent",model.elements.filter(Agent).head.fullyQualifiedName.toString)
	} 
	
	/*
	 * <span class="label label-info">Important</span> The package keyword defines the package 
	 * for all elements in the same SARL file. 
	 * 
	 * Therefore FirstAgent and SecondAgent belong to the same package (i.e. `myapp.demo`).
	 * 
	 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
	 */
	fact "Package definition" {
		val model = '''
			package myapp.demo
			agent MyAgent {}
			agent SecondAgent {}
		'''.parsesSuccessfully
		assertEquals("myapp.demo.MyAgent",model.elements.filter(Agent).head.fullyQualifiedName.toString)
		assertEquals("myapp.demo.SecondAgent",model.elements.filter(Agent).last.fullyQualifiedName.toString)
	}

	/*
	 * Agents need to perceive their environment in order to react to external stimuli.
	 * Perceptions take the form of events.
	 * 
	 */
	context "Agent Perceptions"{
		
		
		/*
		 * To declare a new event use the `event` keyword.
		 * The following code defines a new event `MyEvent`
		 * @filter('''|.parsesSuccessfully) 
		 */
		fact "Declare an Event"{
			'''
			event MyEvent {}
			'''.parsesSuccessfully
		}
		

		
		/* 
		 * Now we will want our agent to react to `MyEvent` and print a message on the console
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Define an agent Perceptions"{
			val SarlScript model = '''
			package myapp.demo
			
			event MyEvent
			
			agent MyAgent {
				on MyEvent {
					System.out.println("Received MyEvent")
				}
			} 
			'''.parsesSuccessfully
		}
		
		/*
		 * SARL defines two **lifecycle** events :
		 * 
		 *  * `Initialize`:  Notifies the creation of the agent and passes it initialization parameters.
		 *  * `Destroy`: Notifies the Destruction of the agent.
		 * 
		 * This means that when agent has been spawned and its ready to begin its execution, it will receive an `Initialize` event.
		 * You can react to this event just like with any other event defined in SARL.
		 * 
		 * Likewise, when the agent is going to stop its execution (we will see how to stop an agent later on), it will 
		 * receive a `Destroy` Event. The purpose of this event is to release any System resources properly.
		 *
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Lifecycle events" {
			val model = '''
				package myapp.demo
				
				import io.sarl.core.Initialize
				import io.sarl.core.Destroy
				
				agent MyAgent {
					
					on Initialize {
						System.out.println("MyAgent spawned")
					}
					
					on Destroy {
						System.out.println("MyAgent destroyed")
					}
				}
			'''.parsesSuccessfully
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
				package myapp.demo
				
				import io.sarl.core.Initialize
				import io.sarl.core.Destroy
				
				agent MyAgent {
					
					on Initialize {
						System.out.println("MyAgent spawned")
						System.out.println("My Parameters are :" + occurrence.parameters.toString)
					}
					
					on Destroy {
						System.out.println("MyAgent destroyed")
					}
				}
			'''.parsesSuccessfully
		}
	}
	
	
	/*
	 * In the next section we will learn how to start a SARL agent.
	 * 
	 * 
	 * **[Next](RunningSARLSpec.html)**.
	 * @filter(.*)
	 */
	fact "What's next?"{assertTrue(true)}
	

}
