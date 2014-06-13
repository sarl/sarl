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
			
			/* **Yes**. 
			 * SARL may be used for agent based applications. 
			 * Natively SARL provides features for agent execution and 
			 * direct communication. The agents may be deployed
			 * on real computers.
			 * 
			 * @filter(.*) 
			 */
			fact "Could I use SARL to make Agent Based software?" {
				true
			}

			/* **Yes**. 
			 * SARL may be used for agent based simulations. 
			 * Natively SARL provides features for agent execution and 
			 * direct communication. An extension is provided for
			 * supporting the simulated environment (time, 
			 * environment model...)
			 * 
			 * @filter(.*) 
			 */
			fact "Could I use SARL to make Agent Based Simulation software?" {
				true
			}
			
			/* **Yes**.
			 * The SARL developers are providing the definition
			 * of an organisational space based on the
			 * [CRIO metamodel](http://www.aspecs.org)
			 * (Capacity-Role-Interaction-Organization).
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
			fact "Could I use SARL to make Organizational software?" {
				true
			}

			/* **Yes**.
			 * SARL natively supports the holon concept. 
			 * Holon is recursively composed of holons. 
			 * In SARL, agents are holons. SARL provides a 
			 * complete support for holons.
			 * 
			 * @filter(.*) 
			 */
			fact "Could I use Janus to make Holonic software?" {
				true
			}

		}
		
		/*  
		 */
		context "Installation and Execution" {

			/* SARL is a collection of features for the Eclipse IDE.
			 * Every operating system which has a compatible Java 
			 * virtual machine with Eclipse may be used to run it. 
			 * 
			 * @filter(.*) 
			 */
			fact "Does my operating system is compatible with SARL?" {
				true
			}

			/* The Janus runtime platform is a Java application.
			 * Every operating system which has a compatible Java 
			 * virtual machine with Janus may be used to run it. 
			 * 
			 * @filter(.*) 
			 */
			fact "Does my operating system is compatible with Janus?" {
				true
			}

			/* SARL requires the JRE and the JDK 1.7 or higher to run and compile.
			 * Note that if you plan to create Android applications, you must 
			 * configure your JDK to produce 1.7 class files. 
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

			/* **No**.
			 * When a decimal point is written in the literal,
			 * the decimal part and the fractional part must
			 * be specify also, evne if these parts are equal
			 * to zero.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Can I use the same syntax as in Java for number literals?" {
				'''
					124.0 // Correct syntax
					124.  // Incorrect syntax
				'''.parsesWithError(
						"package io.sarl.docs.faq.syntax
						agent A {
							def action : double {",
						// TEXT
						"} }"
				)
			}
			
			/* It is not allowed to put a string that is
			 * a reserved keyword of SARL in a package name.
			 * 
			 * However, it is possible to use a keyword
			 * string into a package name if you prefix
			 * it by the `^` character:
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Why can I put a string in the package name?" {
				'''
					package io.sarl.docs.faq.syntax.^agent
				'''.parsesSuccessfully
			}

			/* **Yes and No**.
			 * Indeed, the `val` keyword defines a name
			 * that it could be initialized only once time.
			 * It is somilar to the `final` modifier of
			 * the Java language.
			 * 
			 * Consider the example below: two values are
			 * defined, `a` and `b`.
			 * The `a` variable is a real constant because it
			 * has a raw type and it is initialized.
			 * The `b` variable is not a real constant
			 * because it is a reference to an object.
			 * The reference is constant, *but* the
			 * object is not. Consequently, it is still
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
			 * SARL is an object-oriented language.
			 * Consequently, it considers that the
			 * class array syntax is a list of integer
			 * numbers.
			 * Retreivin gthe values of the array must
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

			/* **No**.
			 * There is no warranty on the reception order of the events.
			 * The runtime environment is executing the event handlers
			 * in parallel. The real order of execution depends on
			 * how the runtime environment is launching the handlers'
			 * execution.  
			 * 
			 * @filter(.*) 
			 */
			fact "Are events received in the same order than when they are sent?" {
				true
			}

		}
		
}
