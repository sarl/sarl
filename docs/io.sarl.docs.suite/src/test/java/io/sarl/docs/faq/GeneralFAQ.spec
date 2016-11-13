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

import static extension io.sarl.docs.utils.SpecificationTools.*;
import static extension org.junit.Assume.assumeFalse

/*
 * @outline
 */
@CreateWith(SARLSpecCreator)
describe "General FAQ on SARL" {

		@Inject extension SARLParser

		/*  
		 */
		context "General Questions about SARL" {
			
			/* SARL is a general-purpose agent-oriented language.
			 * 
			 * <p>SARL aims to provide the fundamental abstractions 
			 * for dealing with concurrency, distribution, interaction, 
			 * decentralization, reactivity, autonomy, and dynamic 
			 * reconfiguration. These high-level features are now 
			 * considered the major requirements for an easy and 
			 * practical implementation of modern complex software 
			 * applications. We are convinced that the agent-oriented 
			 * paradigm holds the keys to effectively meet this challenge.
			 * 
			 * <p>Considering the variety of existing approaches and 
			 * meta-models in the field of agent-oriented engineering 
			 * and more generally multi-agent systems, our approach 
			 * remains as generic as possible and highly extensible 
			 * to easily integrate new concepts and features.
			 * 
			 * <p>__The language is platform- and architecture-independent.__
			 * 
			 * @filter(.*) 
			 */
			fact "What is SARL?" {
				true
			}

			/* __Yes__. 
			 * <p>SARL may be used to build agent based applications. 
			 * Natively, SARL provides features for agent execution and 
			 * direct communication. The agents may be deployed across
			 * multiple networked computers.
			 * 
			 * @filter(.*) 
			 */
			fact "Can I use SARL to make agent-based software?" {
				true
			}

			/* __Yes__. 
			 * SARL may be used for agent based simulations. 
			 * Natively, SARL provides features for agent execution and 
			 * direct communication. An extension is provided for
			 * supporting the simulated environments (time management, 
			 * environment model, etc.).
			 * 
			 * @filter(.*) 
			 */
			fact "Can I use SARL to make agent-based simulation software?" {
				true
			}
			
			/* __Yes__.
			 * Holon is recursively composed of holons. 
			 * In SARL, agents are holons. 
			 * SARL provides a 
			 * complete support for holons.
			 * 
			 * @filter(.*) 
			 */
			fact "Can I use SARL to make holonic software?" {
				true
			}

			/* __Yes__.
			 * An extension to SARL is available that defines
			 * an organizational space based on the
			 * [CRIO meta-model](http://www.aspecs.org/CRIO)
			 * (Capacity-Role-Interaction-Organization).
			 * This meta-model defines a system as a set of organizations 
			 * in which roles are defined and interact together. 
			 * Agents play roles in organization instances (or groups) 
			 * and provides embedded capacity implementations 
			 * required by the played roles.
			 * 
			 * <p>Another organizational model may be in another space.
			 * 
			 * @filter(.*) 
			 */
			fact "Can I use SARL to make organizational software?" {
				"http://www.aspecs.org/CRIO" should beURL _
			}

			/* __Yes and No__.
			 * While SARL is an agent-oriented programming language,
			 * it is possible to use object-oriented concepts
			 * when writing the agent code (skills, etc.).
			 * A part of the grammar of SARL is inherited from the
			 * [Xbase partial programming language](https://wiki.eclipse.org/Xbase),
			 * provided by the [Xtext framework](http://www.eclipse.org/Xtext).
			 * It provides statements and rules that correspond to 
			 * object-oriented languages.
			 * 
			 * @filter(.*) 
			 */
			fact "Is SARL an object-oriented programming language?" {
				"https://wiki.eclipse.org/Xbase" should beURL _
				"http://www.eclipse.org/Xtext" should beURL _
			}

			/* __Yes__.
			 * SARL and Java are 100% interoperable.
			 * There are no exceptional cases and you do not have to think 
			 * in two worlds. You can invoke SARL code from Java and vice 
			 * versa without any surprises or hassles.
			 * 
			 * @filter(.*) 
			 */
			fact "Can I use my Java classes in SARL?" {
				typeof(Integer) should not be null
			}

			/* __No__.
			 * You can create a SARL project with Eclipse without Maven.
			 * Indeed, the SARL Eclipse product supports creation of a SARL project.
			 *
			 * However, the SARL developers recommend Maven because it simplifies the
			 * management of your project's dependencies on the SARL libraries.
			 * 
			 * @filter(.*) 
			 */
			fact "Must I use Maven to create a SARL project?" {
				true
			}

			/* SRE stands for "SARL Runtime Environment."
			 * The SRE is an implementation of an agent platform which is able to
			 * run a SARL program.
			 * The official standard SRE supported by the SARL developers is the
			 * [Janus platform](http://www.janusproject.io).
			 * 
			 * @filter(.*) 
			 */
			fact "What is the SRE?" {
				"http://www.janusproject.io" should beURL _
			}

			/* SARL is a general-purpose agent-oriented language.
			 * Janus is a runtime environment (SRE) for multi-agent applications
			 * that fully supports the concepts of SARL.
			 * 
			 * <p>We can compare the SARL universe with the Java universe:
			 * <center>
			 * <table>
			 * <thead>
			 * <tr><th></th><th>SARL Universe</th><th>Java Universe</th></tr>
			 * </thead>
			 * <tbody>
			 * <tr><td>Language Specification</th><td>SARL Specification</td><td>Java Specification</td></tr>
			 * <tr><td>Standard Development Kit</th><td>SARL SDK</td><td>J(ava)DK</td></tr>
			 * <tr><td>Runtime environment</th><td>Janus</td><td>Hotspot, IcedTea, Dalvik, etc.</td></tr>
			 * </tbody>
			 * </table>
			 * </center>
			 * 
			 * @filter(.*) 
			 */
			fact "What is the difference between SARL and Janus?" {
				true
			}

			/* The release planning of SARL is detailed on the
			 * [milestones page](https://github.com/sarl/sarl/milestones)
			 * on Github.
			 * 
			 * @filter(.*) 
			 */
			fact "Where can I find information on the release planning of SARL?" {
				"https://github.com/sarl/sarl/milestones" should beURL _
			}

			/* If your question is not addressed in
			 * the FAQ, the reference documents, or
			 * the [existing issues](https://github.com/sarl/sarl/issues), you
			 * may ask the SARL developers on 
			 * [this page](https://github.com/sarl/sarl/issues/new).
			 * 
			 * @filter(.*) 
			 */
			fact "Where can I ask my question?" {
				"https://github.com/sarl/sarl/issues" should beURL _
				"https://github.com/sarl/sarl/issues/new" should beURL _
			}

			/* A community driven list of useful SARL libraries, frameworks and software
			 * is maintained on [Github](https://github.com/sarl/awesome-sarl).
			 *
			 * <p>This is not a catalog of all the libraries, just a starting point for
			 * your explorations.
			 *
			 * <p>This list is used by the SARL team to update the official web site
			 * of SARL.
			 * 
			 * @filter(.*) 
			 */
			fact "Where can I find more information and projects related to SARL?" {
				"https://github.com/sarl/awesome-sarl" should beURL _
			}

		}
		
		/*  
		 */
		context "Installation and Execution" {

			/* SARL is based on a part of the Eclipse API.
			 * Every operating system which has a compatible Java 
			 * Virtual Machine with Eclipse may be used to run SARL. 
			 * 
			 * @filter(.*) 
			 */
			fact "Is my operating system compatible with SARL?" {
				true
			}

			/* SARL requires the JRE and the JDK %compilerlevel% or higher to compile and run.
			 * Note that if you plan to create Android applications, you should
			 * configure your JDK to produce 1.6 class files from %compilerlevel% Java code. 
			 * 
			 * @filter(.*) 
			 */
			fact "What version of the Java virtual machine is required?" {
				// The checks are valid only if the macro replacements were done.
				// The replacements are done by Maven.
				// So, Eclipse Junit tools do not make the replacements.
				System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
				//
				"%compilerlevel%" should beMavenVersion false
				"%compilerlevel%" should beJavaRange _
			}

			/* Most of the time the problem is due to an incompatibility between
			 * the configuration of your operating system or Java virtual machine,
			 * and the SARL Eclipse product.
			 *
			 * <p>If a problem occured, find the ".log" file in which Eclipse is writting
			 * the complete error trace. Usually, it is in your home directory or in
			 * the folder of the SARL Eclipse executable file.
			 * 
			 * @filter(.*) 
			 */
			fact "Why does SARL display an error on startup?" {
				true
			}

			/* This is due to a problem in your configuration. Most of the time the log file
			 * (see the previous question) contains the error
			 * "Cannot load 64-bit SWT libraries on 32-bit JVM".
			 *
			 * <p>It means that you're trying to run the 64-bit version of the SARL Eclipse with
			 * a Java virtual machine (JVM) that is 32-bit. You should install a fully 64-bit JVM,
			 * or use the 32-bit version of the SARL Eclipse product.
			 *
			 * <p>If another error occurs, you should go on the SARL forum and report this problem.
			 * 
			 * @filter(.*) 
			 */
			fact "Why does SARL Eclipse fail on Windows 10?" {
				true
			}

			/* This is due to a problem in your configuration. SARL tools need the Eclipse
			 * framework to be run with a Java Development Kit %compilerlevel% or higher.
			 * You are currently running the SARL product with a lower version of the JDK.
			 *
			 * <p>You must run the SARL product with a valid version of the JDK.
			 * Two ways are available for solving this issue:<ol>
			 * <li>install the JDK %compilerlevel%, and configuring your operating system 
			 * to use it by default; or</li>
			 * <li>force the SARL product to use the JDK %compilerlevel% by editing the 
			 * <code>eclipse-sarl.ini</code> file. Add the following parameter on a new 
			 * line: <code>-vm &lt;path&gt;</code>, where <code>&lt;path&gt;</code> 
			 * is the path to the binary file <code>javaw[.exe]</code> or 
			 * <code>java[.exe]</code> of the JDK %compilerlevel%.</li>
			 * </ol>
			 * 
			 * @filter(.*) 
			 */
			fact "Why does the SARL product launch but not contain any features related to SARL?" {
				// The checks are valid only if the macro replacements were done.
				// The replacements are done by Maven.
				// So, Eclipse Junit tools do not make the replacements.
				System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
				//
				"%compilerlevel%" should beMavenVersion false
				"%compilerlevel%" should beJavaRange _
			}

		}

		/*  
		 */
		context "Contribute to SARL" {

			/* The sources for SARL are available on
			 * [Github](https://github.com/sarl/sarl).
			 * Details for getting the source code may be found on the
			 * [download page](%website%/download/). 
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
			
			/* SARL Core Developers use [Github](https://github.com/sarl/sarl)
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
			fact "How can I report a problem or a bug in SARL components?" {
				"https://github.com/sarl/sarl/issues/new" should beURL _
			}

		}
		
}
