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

/*
 * <!-- OUTPUT OUTLINE -->
 */
@CreateWith(SARLSpecCreator)
describe "Runtime Environment FAQ" {

		@Inject extension SARLParser

		/*  
		 */
		context "General Questions about Janus" {
			
			/* Janus is an open-source multi-agent platform fully implemented 
			 * in Java 1.7. Janus enables developers to quickly create 
			 * web, enterprise and desktop multiagent-based applications.
			 *  
			 * It provides a comprehensive set of features to develop, 
			 * run, display and monitor multiagent-based applications.
			 *  
			 * Janus-based applications can be distributed across a network. 
			 * Janus could be used as an agent-oriented platform, an 
			 * organizational platform, and/or an holonic platform. It 
			 * also natively manages the concept of recursive agents and 
			 * holons.
			 * 
			 * __Janus is a platform not a language.__
			 * 
			 * Official website: [www.janusproject.io](http://www.janusproject.io)
			 * 
			 * @filter(.*) 
			 */
			fact "What is Janus?" {
				true
			}

			/* If you cannot find an answer to your question in
			 * the FAQ, nor the reference documents, nor
			 * the [existing SARL issues](https://github.com/sarl/sarl/issues),
			 * nor the [existing Janus issues](https://github.com/janus-project/janusproject/issues),
			 * you could ask to the SARL developers on 
			 * [this page](https://github.com/janus-project/janusproject/issues/new).
			 * 
			 * @filter(.*) 
			 */
			fact "Where can I ask my question?" {
				true
			}

			/* The release planning of Janus is detailed on the
			 * [Milestones' page](https://github.com/janus-project/janusproject/issues/milestones) on
			 * on the Github website.
			 * 
			 * The release planning of the Janus multiagent platform is following
			 * the [planning of SARL](https://github.com/sarl/sarl/issues/milestones).
			 * 
			 * @filter(.*) 
			 */
			fact "Where can I found information on the release planning of Janus?" {
				true
			}

		}
		
		/*  
		 */
		context "Installation and Execution" {

			/* The [Janus runtime platform](http://www.janusproject.io)
			 * is a Java application. Every operating system which has 
			 * a compatible Java Virtual Machine (at least with the 1.7
			 * standard) with Janus may be used to run it. 
			 * 
			 * @filter(.*) 
			 */
			fact "Does my operating system is compatible with Janus?" {
				true
			}

			/* Janus requires the JRE and the JDK 1.7 or higher to run and compile.
			 * Note that if you plan to create Android applications, you must 
			 * configure your JDK to produce 1.6 class files from 1.7 Java code. 
			 * 
			 * @filter(.*) 
			 */
			fact "What is the version of the Java virtual machine to install?" {
				true
			}

		}

		/*  
		 */
		context "Contribute to Janus" {

			/* The sources of Janus are available on
			 * [Github](https://github.com/janus-project/janusproject).
			 * Details for getting the source code may be found on the
			 * [official website of Janus](http://www.janusproject.io). 
			 * 
			 * @filter(.*) 
			 */
			fact "Where are the sources of Janus?" {
				true
			}
			
			/* Janus Core Developers use [Github](https://github.com/janus-project/janusproject)
			 * to manage bug tracking and project workflow. 
			 * The issues are listed on [Github](https://github.com/janus-project/janusproject/issues). 
			 * 
			 * @filter(.*) 
			 */
			fact "How can I obtain the current issues?" {
				true
			}

			/* You must submit your issue on 
			 * [this page](https://github.com/janus-project/janusproject/issues/new).
			 * 
			 * @filter(.*) 
			 */
			fact "How can I report a problem or a bug in Janus components?" {
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
				true
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
		
}
