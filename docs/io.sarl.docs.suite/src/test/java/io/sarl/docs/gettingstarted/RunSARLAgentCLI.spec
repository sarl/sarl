/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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
 * the [Janus platform](http://www.janusproject.io) from the command line.
 *
 * <p>Three methods could be used for launching an agent with Janus: <ul>
 * <li>[Using the provided janus command-line tool](#Use_the_janus_command-line_tool);</li>
 * <li>[Using the standard java method](#Use_the_standard_java_method);</li>
 * <li>[Using Maven execution plugin](#Use_maven_execution_plugin).</li>
 * <li></li>
 * </ul>
 */
@CreateWith(SARLSpecCreator)
describe "Run SARL Agent from the Command Line" {
	
	@Inject extension SARLParser

	/* The SARL project provides a command-line tool for launching agents on the
	 * Janus runtime environment.
	 */
	describe "Use the janus command-line tool" {

		/*
		 * You could download this command line tool, named "janus" on the [downloading page of SARL](%website%/download.html).
		 * 
		 * @filter(.*)
		 */
		fact "Download the janus command-line tool" {
			// The checks are valid only if the macro replacements were done.
			// The replacements are done by Maven.
			// So, Eclipse Junit tools do not make the replacements.
			System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
			//
			"%website%" should beURL "!file"
		}

		/*
		 * For launching an agent, you must launch the command-line tool with the fully-qualified
		 * name of the agent as parameter (`myapp.MyAgent` in the following example).
		 * 
		 *     janus myapp.MyAgent
		 *
		 * <p>The janus command-line tool provides options that will enable you to tune the launching
		 * configuration:
		 *
		 *     janus --help
		 *  
		 * @filter(.*)
		 */
		fact "Launching the agent" {
			true			
		}

	}
	
	/*
	 */
	describe "Use the standard java method" {

		/* The Janus platform provides a `Boot` class.
		 * For launching the platform, you must execute this
		 * boot class in a Java Virtual Machine.
		 * 
		 * <p>The typical command line is:
		 * 
		 *     java -cp app.jar io.janusproject.Boot
		 * 
		 * 
		 * <p>The option `-cp` specifies the Jar file that contains
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
		 * one of the following approaches:<ul>
		 * <li>launch a separate Janus platform instance for each agent, or</li>
		 * <li>launch an agent that is spawning the other agents.</li>
		 * </ul></veryimportant> 
		 *
		 * @filter(.*)
		 */
		fact "Specify the Agent to Launch" {
			true
		} 

		/* In the previous section, we assume that all the application binary files are
		 * contained into the `app.jar` file.
		 *
		 * <p>You may replace the `app.jar` in the previous command lines by the classpath
		 * that is containing all the jar files required for running your application, including
		 * the Janus jar file(s):
		 * 
		 *      java -cp /path/to/myapplication.jar:/path/to/io.janusproject.kernel-<version>-with-dependencies.jar io.janusproject.Boot myapp.MyAgent
		 *
		 * <p>The `io.janusproject.kernel-<version>-with-dependencies.jar` file may be dowloaded from the [Janus website](http://www.janusproject.io/)
		 * 
		 * <p>You may also create the `app.jar` file with Maven by using the assembly plugin for creating a jar file with all the dependencies inside.
		 *
		 * @filter(.*)
		 */
		fact "What is app.jar?" {
			"http://www.janusproject.io/" should beURL _
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
	 */
	describe "Use Maven ExecutionPlugin" {

		/* Maven provides a plugin for launching an application after automatically building
		 * the application's classpath. This plugin may be used for launching an agent.
		 *
		 * <p>Based on the fact that the Janus platform provides a `Boot` class for launching itself,
		 * you may use the Maven execution plugin for classing this booting class.
		 * 
		 * <p>The typical command line is:
		 * 
		 *     mvn exec:java -Dexec.mainClass="io.janusproject.Boot"
		 * 
		 * 
		 * <p>The option `-Dexec.mainClass` specifies the fully qualified name of
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
		 *     mvn exec:java -Dexec.mainClass="io.janusproject.Boot" -Dexec.args=myapp.MyAgent
		 *
		 * 
		 * <veryimportant>The Janus
		 * platform allows to start only one agent from the command line.
		 * If you want to start a collection of agents, you must select
		 * one of the following approaches:<ul>
		 * <li>launch a separate Janus platform instance for each agent, or</li>
		 * <li>launch an agent that is spawning the other agents.</li>
		 * </ul></veryimportant> 
		 *
		 * @filter(.*)
		 */
		fact "Specify the Agent to Launch" {
			true
		} 

		/* The Janus platform provides a collection of command line options.
		 * For obtaining the list of these options, you should type:
		 * 
		 *      mvn exec:java -Dexec.mainClass="io.janusproject.Boot" -Dexec.args=--help
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
	 * <p>[Next>](RunSARLAgentFromAJavaProgramSpec.html)
	 * 
	 * @filter(.*)
	 */
	fact "What's next?" {
		"./RunSARLAgentFromAJavaProgramSpec.html" should beAccessibleFrom this
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
