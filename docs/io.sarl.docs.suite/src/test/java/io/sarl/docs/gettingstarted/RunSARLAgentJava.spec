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
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

/* @outline
 * 
 * <p>For running an agent, you must launch this agent on the runtime 
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
	 * <p>The `Boot` class provides the `startJanus` function, which 
	 * permits to launch Janus programmatically.
	 * 
	 * <p>Let consider you want to launch your agent, defined in the `MyAgent` class.
	 * The following SARL code gives you an example of how to launch this agent in Janus.
	 * 
	 * <p>The first parameter of the `startJanus` function is the injection module to use.
	 * When passing `null`, the Janus platform is using the default injection module.
	 * This is the recommended value to pass.
	 * 
	 * <p>The second parameter of the `startJanus` function is the Java type of the agent
	 * to launch.
	 * 
	 * <p>The third parameter of the `startJanus` function is the list of parameters to
	 * pass with the `Initialize` event to the launched agent.
	 * 
	 * <importantnode>The Janus platform enables to launch a single agent at start-up.
	 * If you want to launch more agents, please read the next section.</importantnote>
	 * 
	 * @filter(.* = '''|'''|.parse.*) 
	 */
	fact "Boot of Janus" {
		'''
			import io.janusproject.Boot
			import myprogram.MyAgent
			class MyProgram {
			 	static def main(args : String[]) : void {
					Boot::startJanus(
						null,
						typeof(MyAgent),
						args)
				}
			}
		'''.parse
	} 

	/* In  the case you want to launch more than one agent programmatically,
	 * you could use the `Kernel` instance provided by Janus.
	 * This instance is replied by the `startJanus` function of the `Boot` class.
	 * 
	 * <p>The `Kernel` type provides the `spawn` function, which permits launching
	 * an agent programmatically.
	 * 
	 * <p>The previous example could be updated for launching two agents of the same type.
	 * The resulting code is shown below.
	 * 
	 * <p>The first parameter of the `spawn` function is the Java type of the agent
	 * to launch.
	 * 
	 * <p>The second parameter of the `spawn` function is the list of parameters to
	 * pass with the `Initialize` event to the launched agent.
	 * 
	 * <p>Note that the first agent is launched by the `startJanus` function, and the 
	 * second agent is launched by the `spawn` function.
	 * 
	 * @filter(.* = '''|'''|.parse.*) 
	 */
	fact "Launching more agents programmatically with Janus" {
		'''
			import io.janusproject.Boot
			import myprogram.MyAgent
			class MyProgram {
				static def main(args : String[]) : void {
					var janusKernel = Boot::startJanus(
						null,
						typeof(MyAgent),
						args)
					janusKernel.spawn(typeof(MyAgent), args)
				}
			}
		'''.parse
	} 

	/*
	 * Now, you are ready for developing agents with the SARL language.
	 * Please read the rest of the documentation for obtaining more details.
	 * 
	 * <p>[Next>](../SARLDocumentationSuite.html)
	 * 
	 * @filter(.*)
	 */
	fact "What's next?" {
		"../SARLDocumentationSuite.html" should beAccessibleFrom this
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
