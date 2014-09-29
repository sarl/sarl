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
			 * SARL aims at providing the fundamental abstractions 
			 * for dealing with concurrency, distribution, interaction, 
			 * decentralization, reactivity, autonomy and dynamic 
			 * reconfiguration. These high-level features are now 
			 * considered the major requirements for an easy and 
			 * practical implementation of modern complex software 
			 * applications. We are convinced that the agent-oriented 
			 * paradigm holds the keys to effectively meet this challenge.
			 * 
			 * Considering the variety of existing approaches and 
			 * meta-models in the field of agent-oriented engineering 
			 * and more generally multi-agent systems, our approach 
			 * remains as generic as possible and highly extensible 
			 * to easily integrate new concepts and features.
			 * 
			 * __The language is platform- and architecture-independent.__
			 * 
			 * @filter(.*) 
			 */
			fact "What is SARL?" {
				true
			}

			/* __Yes__. 
			 * SARL may be used for agent based applications. 
			 * Natively, SARL provides features for agent execution and 
			 * direct communication. The agents may be deployed
			 * on real computers and over a computer network.
			 * 
			 * @filter(.*) 
			 */
			fact "Could I use SARL to make agent-based software?" {
				true
			}

			/* __Yes__. 
			 * SARL may be used for agent based simulations. 
			 * Natively, SARL provides features for agent execution and 
			 * direct communication. An extension is provided for
			 * supporting the simulated environments (time management, 
			 * environment model, etc.)
			 * 
			 * @filter(.*) 
			 */
			fact "Could I use SARL to make agent-based simulation software?" {
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
			fact "Could I use SARL to make holonic software?" {
				true
			}

			/* __Yes__.
			 * The SARL developers are providing the definition
			 * of an organizational space based on the
			 * [CRIO meta-model](http://www.aspecs.org/CRIO)
			 * (Capacity-Role-Interaction-Organization) for example.
			 * This meta-model defines a system as a set of organizations 
			 * in which roles are defined and interacting together. 
			 * Agents play roles in organization instances (or groups) 
			 * and provides embedded capacity implementations 
			 * required by the played roles.
			 * 
			 * Other organizational may be used, if a dedicated space
			 * is defined for.
			 * 
			 * @filter(.*) 
			 */
			fact "Could I use SARL to make organizational software?" {
				true
			}

			/* __Yes and No__.
			 * SARL is an agent-oriented programming language.
			 * But, it is possible to use object-oriented concepts
			 * for writing the code of the expressions in the agents,
			 * the skills, etc.
			 * A part of the grammar of SARL is inherited from the
			 * [Xbase partial programming language](https://wiki.eclipse.org/Xbase),
			 * provided by the [Xtext framework](http://www.eclipse.org/Xtext).
			 * It provides statements and rules that are close to
			 * object-oriented languages.
			 * 
			 * @filter(.*) 
			 */
			fact "Is SARL an object-oriented programming language?" {
				true
			}

			/* __Yes__.
			 * SARL and Java are 100% interoperable.
			 * There are no exceptional cases and you do not have to think in two worlds.
			 * You can invoke SARL code from Java and vice versa without any surprises or hassles.
			 * 
			 * @filter(.*) 
			 */
			fact "May I use my Java classes in SARL?" {
				true
			}

			/* SARL is a general-purpose agent-oriented language.
			 * Janus is a runtime environment for multi-agent applications
			 * that fully supports the concepts of SARL.
			 * 
			 * We can make a parallel with the Java universe:
			 * <center>
			 * <table class="table-bordered" width="80%">
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
			 * [milestones' page](https://github.com/sarl/sarl/milestones)
			 * on the Github web site.
			 * 
			 * @filter(.*) 
			 */
			fact "Where can I found information on the release planning of SARL?" {
				true
			}

			/* If you cannot find an answer to your question in
			 * the FAQ nor the reference documents nor
			 * the [existing issues](https://github.com/sarl/sarl/issues), you
			 * could ask the SARL developers on 
			 * [this page](https://github.com/sarl/sarl/issues/new).
			 * 
			 * @filter(.*) 
			 */
			fact "Where can I ask my question?" {
				true
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
			fact "Does my operating system is compatible with SARL?" {
				true
			}

			/* SARL requires the JRE and the JDK %compilerlevel% or higher to run and compile.
			 * Note that if you plan to create Android applications, you must 
			 * configure your JDK to produce 1.6 class files from %compilerlevel% Java code. 
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
			}

		}

		/*  
		 */
		context "Contribute to SARL" {

			/* The sources of SARL are available on
			 * [Github](https://github.com/sarl/sarl).
			 * Details for getting the source code may be found on the
			 * [download page](%website%/download/). 
			 * 
			 * @filter(.*) 
			 */
			fact "Where are the sources of SARL?" {
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
			fact "How can I obtain the current issues?" {
				true
			}

			/* You must submit your issue on 
			 * [this page](https://github.com/sarl/sarl/issues/new).
			 * 
			 * @filter(.*) 
			 */
			fact "How can I report a problem or a bug in SARL components?" {
				true
			}

		}
		
}
