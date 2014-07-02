/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND
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
package io.sarl.docs.faq

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

/*
 * 
 */
@CreateWith(SARLSpecCreator)
describe "Frequently Asked Questions" {

		@Inject extension SARLParser

		/*  
		 */
		context "General FAQ" {
			
			/* If you cannot find an answer to your question in
			 * the FAQ nor the reference documents nor
			 * the [existing issues](https://github.com/sarl/sarl/issues), you
			 * could ask to the SARL developers on 
			 * [this page](https://github.com/sarl/sarl/issues/new).
			 * 
			 * @filter(.*) 
			 */
			fact "Where can I ask my question?" {
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
			 * of an organisational space based on the
			 * [CRIO metamodel](http://www.aspecs.org/CRIO)
			 * (Capacity-Role-Interaction-Organization) for example.
			 * This metamodel defines a system as a set of organizations 
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
			 * [Xbase partial programming language](https://wiki.eclipse.org/Xbase).
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

			/* SARL requires the JRE and the JDK 1.7 or higher to run and compile.
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
		context "Contribute to SARL" {

			/* The sources of SARL are available on
			 * [Github](http://www.github.org/sarl/sarl).
			 * Details for getting the source code may be found on the
			 * [download page](/download/). 
			 * 
			 * @filter(.*) 
			 */
			fact "Where are the sources of SARL?" {
				true
			}
			
			/* SARL Core Developers use [Github](http://www.github.org/sarl/sarl)
			 * to manage bug tracking and project workflow. 
			 * The issues are listed on [Github](http://www.github.org/sarl/sarl/issues). 
			 * 
			 * @filter(.*) 
			 */
			fact "How can I obtain the current issues?" {
				true
			}

			/* SARL developers use [Github](http://www.github.org/sarl/sarl)
			 * to manage bug tracking and project workflow. To report your 
			 * issue, you may submit a new issue through this platform.
			 * 
			 * @filter(.*) 
			 */
			fact "How can I report a problem or a bug in SARL components?" {
				true
			}

		}

		/*  
		 */
		context "Syntax" {

			/* __No__.
			 * When a decimal point is written in the literal,
			 * the fractional part and the mantissa part must
			 * be specify also, even if these parts are equal
			 * to zero.
			 * 
			 * @filter(.* = '''|'''|.parsesWithError.*) 
			 */
			fact "Can I use the same syntax as in Java for number literals?" {
				'''
					124.0 // Correct syntax
					124.  // Incorrect syntax
					0.123 // Correct syntax
					.123  // Incorrect syntax
				'''.parsesWithError(
						"package io.sarl.docs.faq.syntax
						agent A {
							def action : double {",
						// TEXT
						"} }"
				)
			}
			
			/* It is not allowed to put a SARL keyword, such as
			 * `agent`, in the name of a package.
			 * 
			 * But, if you prefix with the `^` character the string
			 * that corresponds to a keyword, then it is possible
			 * to obtain a package name with one of its components
			 * equals to a SARL keyword:
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Why can I put a string in the package name?" {
				'''
					package io.sarl.docs.faq.syntax.^agent
				'''.parsesSuccessfully
			}

			/* __Yes and No__.
			 * Indeed, the `val` keyword defines a name
			 * that it could be initialized only once time.
			 * It is similar to the `final` modifier of
			 * the Java language.
			 * 
			 * Consider the example below: two values are
			 * defined, `a` and `b`.
			 * The `a` variable is a real constant because it
			 * has a raw type and it is initialized.
			 * The `b` variable is not a real constant
			 * because it is a reference to an object.
			 * The reference is constant, *but* the
			 * referred object is not. Consequently, it is still
			 * possible to call the setters of `b`. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Is the 'val' keyword defining a constant?" {
				'''
					val a : int = 4
					val b : Object = new Object
				'''.parsesSuccessfully(
						"package io.sarl.docs.faq.syntax
						agent A {",
						// TEXT
						"}"
				)
			}

			/* In SARL, the array type may be written
			 * with the classic array syntax, such as
			 * `int[]`, or the object-oriented syntax,
			 * such as `List<Integer>`.
			 * 
			 * SARL considers that the
			 * arrays are lists of something.
			 * Consequently, retreiving the values of the array must
			 * be done with `get(int)`.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Why cannot use the syntax 'a[0]' for arrays?" {
				'''
					var a : Integer[] = #[1, 2, 3]
					var b : List<Integer> = newArrayList(1, 2, 3)
					
					a.get(0) == b.get(0)
				'''.parsesSuccessfully(
						"package io.sarl.docs.faq.syntax
						import java.util.List
						agent A {
							def action : boolean {",
						// TEXT
						"} }"
				)
			}

		}

		/*  
		 */
		context "Runtime" {

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
