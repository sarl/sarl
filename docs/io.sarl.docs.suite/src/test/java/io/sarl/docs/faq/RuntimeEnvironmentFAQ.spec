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
package io.sarl.docs.faq

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

/*
 * @outline
 */
@CreateWith(SARLSpecCreator)
describe "Runtime Environment FAQ" {

		@Inject extension SARLParser

		/*  
		 */
		context "General Questions about Janus" {
			
			/* SRE stands for "SARL Runtime Environment".
			 * The SRE is an implementation of an agent platform, which is able to
			 * run a SARL program.
			 * The official standard SRE supported by the SARL developers is the
			 * [Janus platform](http://www.janusproject.io).
			 * 
			 * @filter(.*) 
			 */
			fact "What is the SRE?" {
				"http://www.janusproject.io" should beURL _
			}

			/* Janus is an open-source multi-agent platform fully implemented 
			 * in Java 1.7. Janus enables developers to quickly create 
			 * web, enterprise and desktop agent-based applications.
			 *  
			 * <p>__Janus is an agent execution platform not an agent-oriented language.__
			 * 
			 * <p>It provides a comprehensive set of features to develop, 
			 * run, display and monitor agent-based applications.
			 *  
			 * <p>Janus-based applications can be distributed across a network. 
			 * Janus could be used as an agent-oriented platform, an 
			 * organizational platform, and/or a holonic platform. It 
			 * also natively manages the concept of recursive agents and 
			 * holons.
			 * 
			 * <p>Official website: [www.janusproject.io](http://www.janusproject.io)
			 * 
			 * @filter(.*) 
			 */
			fact "What is Janus?" {
				"http://www.janusproject.io" should beURL _
			}

			/* If you cannot find an answer to your question in
			 * the FAQ, nor the reference documents, nor
			 * the [existing SARL issues](https://github.com/sarl/sarl/issues),
			 * nor the [existing Janus issues](https://github.com/janus-project/janusproject/issues),
			 * you could ask the SARL developers on 
			 * [this page](https://github.com/janus-project/janusproject/issues/new).
			 * 
			 * @filter(.*) 
			 */
			fact "Where can I ask my question?" {
				"https://github.com/sarl/sarl/issues" should beURL _
				"https://github.com/janus-project/janusproject/issues" should beURL _
				"https://github.com/janus-project/janusproject/issues/new" should beURL _
			}

			/* The release planning of Janus is detailed on the
			 * [milestones' page](https://github.com/janus-project/janusproject/milestones) on
			 * the Github web site.
			 * 
			 * <p>The versions of the latest stable and development releases are displayed on
			 * [this page](http://maven.janusproject.io/VERSION.txt).
			 * 
			 * <p>The release planning of the Janus multi-agent platform is following
			 * the [planning of SARL](https://github.com/sarl/sarl/milestones).
			 * 
			 * 
			 * @filter(.*) 
			 */
			fact "Where can I found information on the release planning of Janus?" {
				"https://github.com/janus-project/janusproject/milestones" should beURL _
				"http://maven.janusproject.io/VERSION.txt" should beURL _
				"https://github.com/sarl/sarl/milestones" should beURL _
			}

		}
		
		/*  
		 */
		context "Installation and Execution" {

			/* The [Janus runtime platform](http://www.janusproject.io)
			 * is a Java application. Every operating system which has 
			 * a Java Virtual Machine with at least with the %compilerlevel%
			 * standard may be used to run Janus. 
			 * 
			 * @filter(.*) 
			 */
			fact "Is my operating system compatible with Janus?" {
				"http://www.janusproject.io" should beURL _
				// The checks are valid only if the macro replacements were done.
				// The replacements are done by Maven.
				// So, Eclipse Junit tools do not make the replacements.
				System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
				//
				"%compilerlevel%" should beMavenVersion false
				"%compilerlevel%" should beJavaRange _
			}

			/* Janus requires the JRE and the JDK %compilerlevel% or higher to run and compile.
			 * Note that if you plan to create Android applications, you may 
			 * configure your JDK to produce 1.6 class files from %compilerlevel% Java code,
			 * depending of the current supported standard on Android platforms.
			 * 
			 * @filter(.*) 
			 */
			fact "What is the version of the Java virtual machine to install?" {
				// The checks are valid only if the macro replacements were done.
				// The replacements are done by Maven.
				// So, Eclipse Junit tools do not make the replacements.
				System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
				//
				"%compilerlevel%" should beMavenVersion false
				"%compilerlevel%" should beJavaRange _
			}

			/* Three methods are available for launching one or more agents in the Janus platform:<ul>
			 * <li>[From the command line](../gettingstarted/RunSARLAgentFromTheCommandLineSpec.html).</li>
			 * <li>[Inside the Eclipse IDE](../gettingstarted/RunSARLAgentInTheEclipseIDESpec.html).</li>
			 * <li>[From a Java program](../gettingstarted/RunSARLAgentFromAJavaProgramSpec.html).</li>
			 * </ul>
			 *
			 * @filter(.*) 
			 */
			fact "How to launch an agent in Janus?" {
				"../gettingstarted/RunSARLAgentFromTheCommandLineSpec.html" should beAccessibleFrom this
				"../gettingstarted/RunSARLAgentInTheEclipseIDESpec.html" should beAccessibleFrom this
				"../gettingstarted/RunSARLAgentFromAJavaProgramSpec.html" should beAccessibleFrom this
			}
			
			/* This error occurs when there is no SARL Runtime Environment (SRE) installed on your
			 * Eclipse environment, OR when the installed SRE is not compatible with the installed
			 * version of the SARL tools, which are embedded in Eclipse.
			 *
			 * <p>For solving this problem, you must download the latest
			 * [Janus platform](http://www.janusproject.io), and install it in your Eclipse
			 * (Menu <code>Window&gt; Preferences&gt; SARL&gt; Installed SREs</code>).
			 * 
			 * <caution>If the latest stable version of Janus is not working, you should
			 * download the latest development version.</caution>
			 * 
			 * @filter(.*) 
			 */
			fact "Error: \"The SRE is not standalone. It does not contain the Java dependencies.\"" {
				"http://www.janusproject.io" should beURL _
				var propertyFile = getBundlePropertyURL("io.sarl.eclipse", "OSGI-INF/l10n/bundle.properties")
				propertyFile should haveProperty "category.name" -> "SARL"
				propertyFile should haveProperty "preference.installedSREs" -> "Installed SREs"
			}

			/* This error occurs when the SARL Runtime Environment (SRE) has a version lower than
			 * the version of the SARL tools, which are embedded in the Eclipse IDE.
			 * This difference of version may cause problems during the execution of your agents, since
			 * the capacities' definitions may not be the same.
			 *
			 * <p>For solving this problem, you must download the version of the SARL Runtime Environment (SRE)
			 * that is implementing the version of the SARL specification that you're using on Eclipse IDE.
			 * For the Janus platform, the versions of the latest stable and development releases are displayed on
			 * [this page](http://maven.janusproject.io/VERSION.txt).
			 * For determining if the Janus platform implements the correct version of the SARL specification,
			 * please read the explanation  on [how Janus version numbers are built](http://www.janusproject.io/#versionnumber).
			 * 
			 * @filter(.*) 
			 */
			fact "Error: \"Incompatible SRE with SARL 1.1.1.1. Version must be lower than 0.0.0.\"" {
				"http://maven.janusproject.io/VERSION.txt" should beURL _
				"http://www.janusproject.io/#versionnumber" should beURL _
			}

			/* When the Janus platform cannot find the class file for the start-up agent, it
			 * displays the error message <code>"Agent class not found"</code>.
			 *
			 * <p>For resolving this problem, you should check if: <ul>
			 * <li>the class with the given name exists in the application's class path.</li>
			 * <li>the class name is given as the first command-line argument to your application.</li>
			 * <li>the class with the given name is a subtype of <code>Agent</code>.</li>
			 * </ul>
			 * 
			 * <p>For showing the arguments given to Janus, you could launch Janus with the command line option:
			 * <code>--cli</code>. This option stops Janus after displaying the command line arguments
			 * (including the <code>--cli</code> option).
			 * 
			 * @filter(.*) 
			 */
			fact "Error: \"Agent class not found.\"" {
				true
			}

			/* When installing Janus as an SRE in the Eclipse interface, the plugin loads the Jar file of the
			 * SRE with the default API.
			 * The Jar archiver uses the default file encoding of the operating system.
			 * On Linux and MacOS 10, it is almost UTF-8. On Windows, it is Latin1. And on MacOS (before 10),
			 * it is Mac-Roman.
			 *
			 * <p>Unfortunately, the Janus Jar file is generated on a Linux operating system (UTF-8).
			 * When the Java virtual machine tries to uncompress and use the content of the Jar, it
			 * complains about an invalid charset format.
			 * 
			 * <p>For solving this issue, you could launch your Eclipse with the command line option
			 * <code>-Dfile.encoding=UTF-8</code>, which is forcing the Eclipse product to consider the
			 * file as encoded with the UTF-8 charset.
			 * 
			 * @filter(.*) 
			 */
			fact "Error: \"Invalid byte 2 of 4-byte UTF-8 sequence.\"" {
				true
			}

		}

		/*  
		 */
		context "Runtime Behavior of Janus" {

			/* __No__.
			 * There is no warranty on the receiving order of the events.
			 * This is a particular implementation choice of the runtime
			 * environment. For example, the
			 * [Janus runtime environment](http://www.janusproject.io) executes
			 * the event handlers in parallel. The real order of execution depends on
			 * how the Java executor is running the handlers on the threads.  
			 * 
			 * @filter(.*) 
			 */
			fact "Are events received in the same order than when they are sent?" {
				"http://www.janusproject.io" should beURL _
			}

			/* __No__.
			 * Janus was designed to discover other kernels automatically.
			 * By default, the different instances of the Janus platform
			 * are connected together without any particular configuration.
			 * The sole constraint is that the kernels must be on the
			 * same local network.  
			 * 
			 * @filter(.*) 
			 */
			fact "Must I configure the Janus kernels to be connected to other Janus kernels?" {
				true
			}

		}
		
		/*  
		 */
		context "Contribute to Janus" {

			/* The sources for Janus are available inside the SARL project repository on
			 * [Github](https://github.com/sarl/sarl/sre/io.janusproject/).
			 * 
			 * @filter(.*) 
			 */
			fact "Where are the sources for SARL?" {
				// The checks are valid only if the macro replacements were done.
				// The replacements are done by Maven.
				// So, Eclipse Junit tools do not make the replacements.
				System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
				//
				"%website%" should beURL "!file"
			}
			
			/* Janus Developers use [Github](https://github.com/sarl/sarl)
			 * to manage bug tracking and project workflow. 
			 * The issues are listed on [Github](https://github.com/sarl/sarl/issues). 
			 * 
			 * @filter(.*) 
			 */
			fact "How can I find the current issues?" {
				"https://github.com/sarl/sarl" should beURL _
				"https://github.com/sarl/sarl/issues" should beURL _
			}

			/* You should submit your issue on 
			 * [this page](https://github.com/sarl/sarl/issues/new).
			 * 
			 * @filter(.*) 
			 */
			fact "How can I report a problem or a bug in Janus components?" {
				"https://github.com/sarl/sarl/issues/new" should beURL _
			}

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
