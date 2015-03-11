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
import io.sarl.lang.sarl.Agent
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*

/* @outline
 * 
 * For running an agent, you must launch this agent on the runtime 
 * environment.
 * This document explains how to launch an agent on
 * the [Janus platform](http://www.janusproject.io) from the command line.
 */
@CreateWith(SARLSpecCreator)
describe "Run SARL Agent from the Command Line" {
	
	@Inject extension SARLParser

	/* The Janus platform provides a `Boot` class.
	 * For launching the platform, you must execute this
	 * boot class in a Java Virtual Machine.
	 * 
	 * The typical command line is:
	 * 
	 *     java -cp app.jar io.janusproject.Boot
	 * 
	 * 
	 * The option `-cp` specifies the Jar file that contains
	 * the compiled classes. The given `app.jar`
	 * file is a Jar file that is containing the Janus
	 * platform, the SARL libraries, and the application classes.
	 * The last argument is the fully qualified name of
	 * the booting class of Janus: `io.janusproject.Boot`
	 * 
	 * @filter(.*)
	 */
	fact "Boot of Janus" {
		true
	} 

	/* The example given in the previous section causes an error.
	 * Indeed, it is mandatory to specify the fully qualified name
	 * of the agent to launch:
	 * 
	 *     java -cp app.jar io.janusproject.Boot myapp.MyAgent
	 *
	 * 
	 * <veryimportant>The Janus
	 * platform allows to start only one agent from the command line.
	 * If you want to start a collection of agents, you must select
	 * one of the following approaches:
	 * 
	 *  * launch a separate Janus platform for each agent, or
	 *  * launch an agent that is spawning the other agents.
	 * </veryimportant> 
	 *
	 * @filter(.*)
	 */
	fact "Specify the Agent to Launch" {
		true
	} 
	
	/* It is possible to give arguments to the launched agent.
	 * Indeed, all the arguments given on the command line
	 * are put in the `parameters` attribute of the `Initialize` event.
	 * This event is fired when the launched agent is started.
	 */
	describe "Command Line Parameters" {

		/* The following example gives the values `FirstParam` and
		 * `SecondParam` to the launched agent:
		 * 
		 *     java -cp app.jar io.janusproject.Boot myapp.MyAgent FirstParam SecondParam
		 *
		 *
		 * @filter(.*) 
		 */		
		fact "Give Parameters to the Agent" {
			true
		}
		
		/* For retrieving the values passed on the command line,
		 * you must handle the `Initialize` event, as illustrated
		 * by the following example:
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Retrieve the Command Line Parameters in the Agent" {
			val model = '''
				agent MyAgent {
					uses Logging
					on Initialize {
						println("Command line parameters: "
							+occurrence.parameters)
					}
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.gettingstarted.runsarlagent
				import io.sarl.core.Logging
				import io.sarl.core.Initialize",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.gettingstarted.runsarlagent"
				it should haveNbImports 2
				it should importClass "io.sarl.core.Logging"
				it should importClass "io.sarl.core.Initialize"
				it should haveNbElements 1
			]

			model.elements.get(0) => [
				it should beAgent "MyAgent"
				it should extend _
				it should haveNbElements 2
				(it as Agent).features.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				(it as Agent).features.get(1) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith _
				]
			]
		}

		/* The Janus platform provides a collection of command line options.
		 * For obtaining the list of these options, you should type:
		 * 
		 *     java -cp app.jar io.janusproject.Boot --help
		 *
		 *
		 * @filter(.*) 
		 */		
		fact "Janus Command Line Options" {
			true
		}

	}
	 
	/*
	 * In the next section, we will learn how to launch your SARL project from
	 * a Java program.
	 * 
	 * [Next>](RunSARLAgentFromAJavaProgramSpec.html)
	 * 
	 * @filter(.*)
	 */
	fact "What's next?" {
		"./RunSARLAgentFromAJavaProgramSpec.html" should beAccessibleFrom this
	}

}
