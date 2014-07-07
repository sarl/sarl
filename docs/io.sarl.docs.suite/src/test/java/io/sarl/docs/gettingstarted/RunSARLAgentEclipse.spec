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

/* For running an agent, you must launch this agent on the runtime 
 * environment.
 * This document explains how to launch an agent on
 * the [Janus platform](http://www.janusproject.io).
 */
@CreateWith(SARLSpecCreator)
describe "Run SARL Agent in the Eclipse IDE" {
	
	@Inject extension SARLParser

	/* The Janus platform provides a `Boot` class.
	 * For launching the platform, you must execute this
	 * boot class in a Java Virtual Machine.
	 */
	describe "Boot of Janus" { } 

	/* For launching the SARL agents on the Janus runtime environment inside
	 * the Eclipse IDE, you must define a *Run Configuration*.
	 *
	 * Because there is no run configuration dedicated to SARL yet, you
	 * must define a *Java Run Configuration*.
	 */
	describe "Create a Launch Configuration" {

		/* Open the run configuration dialog box by selecting
		 * **Run > Run Configurations**, and create a new Java
		 * Application. You obtain a page similar to:
		 *
		 * <center><img alt="Java Application" src="./EclipseRunConfiguration_0.png" width="60%" /></center>
		 *
		 * Change the *name* of the run configuration, and select the *project*, which
		 * is containing your agent.
		 *
		 * @filter(.*) 
		 */		
		fact "Create a Java application configuration" {
			true
		}

		/* For running your agent with the Janus runtime environment,
		 * you must add the Janus library in the classpath.
		 *
		 * First, **you must download the
		 * <a href="http://maven.janusproject.io/last-janus-release.jar">Janus runtime environment</a>**.
		 *
		 * For adding the downloaded file of Janus, you must **add an external JAR** in
		 * the *Classpath* tab. After adding the Janus JAR file, you obtain a
		 * dialog box similar to:
		 *
		 * <center><img alt="Add Janus" src="./EclipseRunConfiguration_1.png" width="60%" /></center>
		 *
		 * @filter(.*) 
		 */		
		fact "Add the Janus runtime environment" {
			true
		}

		/* You can go back to the *Main* tab, and enter the *Main class*.
		 * The main class **must always be** `io.janusproject.Boot`.
		 *
		 * <center><img alt="Janus Boot Class" src="./EclipseRunConfiguration_2.png" width="60%" /></center>
		 *
		 * @filter(.*) 
		 */		
		fact "Specify the Janus Boot agent" {
			true
		}

		/* The last step is the specification of the agent to launch.
		 * Keep in mind that you can give to the Janus runtime environment
		 * only one start-up agent. The other agents will be spawn by the
		 * specified start-up agent.
		 *
		 * The start-up agent is given in the *Program arguments* field of
		 * the *Arguments* tab. You must enter the fully qualified name
		 * of the agent that must be launched. 
		 *
		 * <center><img alt="Agent to Launch" src="./EclipseRunConfiguration_3.png" width="60%" /></center>
		 *
		 * @filter(.*) 
		 */		
		fact "Specify the agent to execute" {
			true
		}

	}
	
	/* It is possible to give parameters to the launched agent.
	 * Indeed, all the parameters given as program arguments
	 * are put in the `parameters` attribute of the `Initialize` event.
	 * This event is fired when the launched agent is started.
	 */
	describe "Get the program arguments in the agent" {

		/* The following example gives the values `FirstParam` and
		 * `SecondParam` to the launched agent:
		 *
		 * <center><img alt="Program Arguments" src="./EclipseRunConfiguration_4.png" width="60%" /></center>
		 * 
		 * @filter(.*) 
		 */		
		fact "Give parameters to the Agent" {
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

	} 

}
