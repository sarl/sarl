/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*

/* @outline
 * 
 * For running an agent, you must launch this agent on the runtime 
 * environment.
 * This document explains how to launch an agent on
 * the [Janus platform](http://www.janusproject.io) from a Java program.
 */
@CreateWith(SARLSpecCreator)
describe "Run SARL Agent from a Java Program" {
	
	@Inject extension SARLParser

	/* The Janus platform provides a `Boot` class.
	 * For launching the platform, you must use this boot class.
	 *
	 * The `Boot` class provides the `startJanus` function, which 
	 * permits to launch Janus programmatically.
	 * 
	 * Let consider you want to launch your agent, defined in the `MyAgent` class.
	 * The following Java code gives you an example of how to launch this agent in Janus.
	 * 
	 *     import io.sarl.lang.core.Agent;
	 *     import io.janusproject.Boot;
	 *     import myprogram.MyAgent;
	 *     public class MyProgram {
	 *         public static void main(String[] args) {
	 *             Class<? extends Agent> agentType = MyAgent.class;
	 *             Boot.startJanus(
	 *                 null,
	 *                 agentType,
	 *                 args);
	 *         }
	 *     }
	 * 
	 * The first parameter of the `startJanus` function is the injection module to use.
	 * When passing `null`, the Janus platform is using the default injection module.
	 * This is the recommended value to pass.
	 * 
	 * The second parameter of the `startJanus` function is the Java type of the agent
	 * to launch.
	 * 
	 * The third parameter of the `startJanus` function is the list of parameters to
	 * pass with the `Initialize` event to the launched agent.
	 * 
	 * <importantnode>The Janus platform enables to launch a single agent at start-up.
	 * If you want to launch more agents, please read the next section.</importantnote>
	 * 
	 * @filter(.*)
	 */
	fact "Boot of Janus" {
		true
	} 

	/* In  the case you want to launch more than one agent programmatically,
	 * you could use the `Kernel` instance provided by Janus.
	 * This instance is replied by the `startJanus` function of the `Boot` class.
	 * 
	 * The `Kernel` type provides the `spawn` function, which permits launching
	 * an agent programmatically.
	 * 
	 * The previous example could be updated for launching two agents of the same type:
	 * 
	 *     import io.sarl.lang.core.Agent;
	 *     import io.janusproject.Boot;
	 *     import io.janusproject.kernel.Kernel;
	 *     import myprogram.MyAgent;
	 *     public class MyProgram {
	 *         public static void main(String[] args) {
	 *             Class<? extends Agent> agentType = MyAgent.class;
	 *             Kernel janusKernel = Boot.startJanus(
	 *                 null,
	 *                 agentType,
	 *                 args);
	 *             janusKernel.spawn(agentType, args);
	 *         }
	 *     }
	 * 
	 * The first parameter of the `spawn` function is the Java type of the agent
	 * to launch.
	 * 
	 * The second parameter of the `spawn` function is the list of parameters to
	 * pass with the `Initialize` event to the launched agent.
	 * 
	 * Note that the first agent is launched by the `startJanus` function, and the 
	 * second agent is launched by the `spawn` function.
	 *
	 * @filter(.*)
	 */
	fact "Launching more agents programmatically in Janus" {
		true
	} 

	/*
	 * Now, you are ready for developing agents with the SARL language.
	 * Please read the rest of the documentation for obtaining more details.
	 * 
	 * [Next>](../SARLDocumentationSuite.html)
	 * 
	 * @filter(.*)
	 */
	fact "What's next?" {
		"../SARLDocumentationSuite.html" should beAccessibleFrom this
	}

}
