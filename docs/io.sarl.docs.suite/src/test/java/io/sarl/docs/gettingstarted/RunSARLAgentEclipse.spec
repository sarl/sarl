/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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
import io.sarl.lang.sarl.SarlAgent
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.*

/* @outline
 * 
 * <p>For running an agent, you must launch this agent on the runtime 
 * environment.
 * In this document, we assume that the runtime environment is
 * the [Janus platform](http://www.janusproject.io).
 *
 * <p>Two major methods are available for launching a SARL application inside the SARL Eclipse IDE:
 * 
 * * Use a [SARL launch configuration](#Create_a_Sarl_Launch_Configuration).
 * * Use a [Java launch configuration](#Create_a_Java_Launch_Configuration).
 *
 * <p>The SARL launch configuration is recommended.
 */
@CreateWith(SARLSpecCreator)
describe "Run SARL Agent in the Eclipse IDE" {
	
	@Inject extension SARLParser

	/* For launching the SARL agents on the runtime environment inside
	 * the SARL Eclipse IDE, you must define a *Run Configuration*.
	 *
	 * <veryimportant>If your project is Maven-based, you could not use this
	 * method for launching your application. You must use the
	 * [Java launch configuration](#Create_a_Java_Launch_Configuration).
	 * </veryimportant>
	 */
	describe "Create a SARL Launch Configuration" {
		
		/* Open the run configuration dialog box by selecting
		 * **Run > Run Configurations**, and create a new SARL
		 * application. You obtain a page similar to:
		 *
		 *
		 * ![SARL Application](./EclipseRunConfiguration_0_0.png)
		 *
		 * 
		 * <p>Change the *name* of the run configuration, and select the *project*, which
		 * is containing your agent.
		 *
		 * @filter(.*) 
		 */		
		fact "Create a Java application configuration" {
			"EclipseRunConfiguration_0_0.png" should beAccessibleFrom this
			"#Create_a_Java_Launch_Configuration" should beAccessibleFrom this
			"#Create_a_Sarl_Launch_Configuration" should beAccessibleFrom this
		}

		/* The second step is the specification of the agent to launch.
		 * Keep in mind that you can only give one start-up agent to 
		 * the runtime environment. The other agents will be spawned by the
		 * specified start-up agent.
		 *
		 * <p>The start-up agent is given in the *Agent qualified name* field of
		 * the *Main* tab. You must enter the fully qualified name
		 * of the agent that must be launched. 
		 *
		 * <p>![Agent to Launch](./EclipseRunConfiguration_0_1.png)
		 *
		 * <p>At the bottom of this page, you may change configuration options for the runtime environment.
		 *
		 * @filter(.*) 
		 */		
		fact "Specify the agent to execute" {
			"EclipseRunConfiguration_0_1.png" should beAccessibleFrom this
		}

		/* For running your agent, you must specify a SARL runtime environment.
		 * In this tutorial, we assume that you want to use the [Janus platform](http://www.janusproject.io).
		 *
		 * <p>If you don't want to use the Janus platform, you must download the
		 * runtime environment that you want to use, and install it in the SARL Eclipse environment as follow.
		 * You add a **SARL runtime environment** (or SRE) in
		 * the *Runtime environment* tab. You should click on the **Installed SREs** button for
		 * managing the installed runtime environments (or open the corresponding preference page).
		 * After adding the SRE's JAR file, you obtain a dialog box similar to:
		 *
		 *
		 * <p>![Add Janus](./EclipseRunConfiguration_0_2.png)
		 *
		 * @filter(.*) 
		 */		
		fact "Add the Janus runtime environment" {
			"EclipseRunConfiguration_0_2.png" should beAccessibleFrom this
			// The checks are valid only if the macro replacements were done.
			// The replacements are done by Maven.
			// So, Eclipse Junit tools do not make the replacements.
			System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
			//
			"%janusmavenrepository%" should beURL "!file"
		}

		/* It is possible to give arguments to the launched agent.
		 * Indeed, all the arguments given as program arguments
		 * are put in the `parameters` attribute of the `Initialize` event.
		 * This event is fired when the launched agent is started.
		 * 
		 * <p>The following example gives the values `FirstArgument` and
		 * `SecondArgument` to the launched agent:
		 *
		 *
		 * <p>![Program Arguments](./EclipseRunConfiguration_0_3.png)
		 *
		 * <p>On this page, you could also specify the parameters to give to the SARL runtime environment or
		 * to the Java virtual machine.
		 * 
		 * @filter(.*) 
		 */		
		fact "Give parameters to the Agent" {
			"EclipseRunConfiguration_0_3.png" should beAccessibleFrom this
		}

	}

	/* For launching the SARL agents on the Janus runtime environment inside
	 * the Eclipse IDE, you must define a *Run Configuration*.
	 *
	 * <p>This section is dedicated to the definition of a launcher for Java application (the standard and classical
	 * launching configuration in the Eclipse community).
	 */
	describe "Create a Java Launch Configuration" {

		/* Open the run configuration dialog box by selecting
		 * **Run > Run Configurations**, and create a new Java
		 * Application. You obtain a page similar to:
		 *
		 *
		 * <p>![Java Application](./EclipseRunConfiguration_1_0.png)
		 *
		 * 
		 * <p>Change the *name* of the run configuration, and select the *project*, which
		 * is containing your agent.
		 *
		 * @filter(.*) 
		 */		
		fact "Create a Java application configuration" {
			"EclipseRunConfiguration_1_0.png" should beAccessibleFrom this
		}

		/* For running your agent with the Janus runtime environment,
		 * you must add the Janus library in the class path.
		 *
		 * <p>For adding the Janus library, select the **Classpath** tab, and **User Entries**.
		 * Click on the **Advanced** button. You will be able to select the type of classpath
		 * entry to add. Select "Add Library". 
		 * 
		 * <p>![Add Library in the classpath](./EclipseRunConfiguration_1_1.png)
		 *
		 * Then, you are able to select the library for the Janus runtime environment. 
		 *
		 * <p>![Add Janus Library](./EclipseRunConfiguration_1_2.png)
		 *
		 * @filter(.*) 
		 */		
		fact "Add the Janus runtime environment" {
			"EclipseRunConfiguration_1_1.png" should beAccessibleFrom this
			"EclipseRunConfiguration_1_2.png" should beAccessibleFrom this
			// The checks are valid only if the macro replacements were done.
			// The replacements are done by Maven.
			// So, Eclipse Junit tools do not make the replacements.
			System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
			//
			"%janusmavenrepository%" should beURL "!file"
		}

		/* You can go back to the *Main* tab, and enter the *Main class*.
		 * The main class **must always be** `io.janusproject.Boot`.
		 *
		 *
		 * <p>![Janus Boot Class](./EclipseRunConfiguration_1_3.png)
		 *
		 *
		 * @filter(.*) 
		 */		
		fact "Specify the Janus Boot agent" {
			"EclipseRunConfiguration_1_3.png" should beAccessibleFrom this
		}

		/* The last step is the specification of the agent to launch.
		 * Keep in mind that you can give to the Janus runtime environment
		 * only one start-up agent. The other agents will be spawn by the
		 * specified start-up agent.
		 *
		 * <p>The start-up agent is given in the *Program arguments* field of
		 * the *Arguments* tab. You must enter the fully qualified name
		 * of the agent that must be launched. 
		 *
		 *
		 * <p>![Agent to Launch](./EclipseRunConfiguration_1_4.png)
		 *
		 *
		 * @filter(.*) 
		 */		
		fact "Specify the agent to execute" {
			"EclipseRunConfiguration_1_4.png" should beAccessibleFrom this
		}

		/* It is possible to give arguments to the launched agent.
		 * Indeed, all the arguments given as program arguments
		 * are put in the `parameters` attribute of the `Initialize` event.
		 * This event is fired when the launched agent is started.
		 * 
		 * <p>The following example gives the values `FirstParam` and
		 * `SecondParam` to the launched agent:
		 *
		 *
		 * <p>![Program Arguments](./EclipseRunConfiguration_1_5.png)
		 *
		 * 
		 * @filter(.*) 
		 */		
		fact "Give parameters to the Agent" {
			"EclipseRunConfiguration_1_5.png" should beAccessibleFrom this
		}

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
		
		model.xtendTypes.get(0) => [
			it should beAgent "MyAgent"
			it should extend _
			it should haveNbElements 2
			(it as SarlAgent).members.get(0) => [
				it should beCapacityUse "io.sarl.core.Logging"
			]
			(it as SarlAgent).members.get(1) => [
				it should beBehaviorUnit "io.sarl.core.Initialize"
				it should beGuardedWith _
			]
		]
	}

	/*
	 * In the next section, we will learn how to launch your SARL project from
	 * the command line.
	 * 
	 * <p>[Next>](RunSARLAgentFromTheCommandLineSpec.html)
	 * 
	 * @filter(.*)
	 */
	fact "What's next?" {
		"RunSARLAgentFromTheCommandLineSpec.html" should beAccessibleFrom this
	}

}
