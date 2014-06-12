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

/* For running an agent, you must launch this agent on the runtime environment.
 * This document explains how to launch an agent on
 * the [Janus platform](http://www.janusproject.io).
 */
@CreateWith(SARLSpecCreator)
describe "Run SARL Agent" {
	
	@Inject extension SARLParser

	/* The Janus platform provides a `Boot` class.
	 * for launching the platform, you must launch this
	 * boot class in the Java virtual machine.
	 * <pre><code>
	 * java -cp app.jar io.janusproject.Boot
	 * </code></pre>
	 * 
	 * <span class="label label-info">Important</span> The `app.jar`
	 * file is a Jar file that is containing the Janus
	 * platform, the SARL libraries, and the application classes.
	 */
	describe "Boot of Janus" { } 

	/* The example given in the previous section causes an error.
	 * Indeed, it is mandatory to specify the fully qualified name
	 * of the agent to launch: <pre><code>
	 * java -cp app.jar io.janusproject.Boot myapp.MonAgent
	 * </code></pre>
	 * 
	 * <span class="label label-info">Important</span> The Janus
	 * platform allows to start only one agent from the command line.
	 * If you want to start a collection of agents, you must select
	 * one of the following approaches: <ul>
	 * <li>launch a separate Janus platform for each agent, or</li>
	 * <li>launch an agent that is spawning the other agents.</li>
	 * </ul>
	 */
	describe "Specify the Agent to Launch" { } 
	
	/* It is possible to give parameters to the launched agent.
	 * Indeed, all the parameters given on the command line
	 * are put in the `parameters` attribute of the `Initialize` event.
	 * This event is fired when the launched agent is started.
	 */
	describe "Command Line Parameters" {

		/* The following example gives the values `FirstParameter` and
		 * `SecondParameter` to the launched agent: <pre><code>
		 * java -cp app.jar io.janusproject.Boot myapp.MonAgent FirstParameter SecondParameter
		 * </code></pre>
		 *
		 * @filter(.*) 
		 */		
		fact "Give Parameters to the Agent" {
			true
		}
		
		/* For retreiving the values passed on the command line,
		 * you must handle the `Initialize` event, as illustrated
		 * by the following example:
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Retreive the Command Line Parameters in the Agent" {
			val model = '''
				agent MyAgent {
					on Initialize {
						println("Command line parameters: "
							+occurrence.parameters)
					}
				}
			'''.parsesSuccessfully(
				"package io.sarl.docs.gettingstarted.runsarlagent
				import io.sarl.core.Initialize",
				// TEXT
				""
			)
			model.mustHavePackage("io.sarl.docs.gettingstarted.runsarlagent")
			model.mustHaveImports(1)
			model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(1)
			a.features.get(0).mustBeBehaviorUnit("io.sarl.core.Initialize", false)
		}

		/* The Janus platform provides a collection of command line options.
		 * For obtaining the list of these options, you should type: <pre><code>
		 * java -cp app.jar io.janusproject.Boot --help
		 * </code></pre>
		 *
		 * @filter(.*) 
		 */		
		fact "Janus Command Line Options" {
			true
		}

	} 

}