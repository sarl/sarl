/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors and authors.
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
package io.sarl.docs.reference

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import io.sarl.lang.sarl.SarlAction
import io.sarl.lang.sarl.SarlCapacity
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

/* @outline
 *
 * <p>This document describes how to define Capacities in SARL.
 * Before reading this document, we recommend that you read
 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html).
 * 
 * <p>An *Action* is code (a public method or function) that transforms a part of the 
 * designed system or its environment. This transformation guarantees 
 * resulting properties if the system before the transformation satisfies 
 * a set of constraints. An Action is defined in terms of pre- and post-conditions.
 * 
 * <p>A *Capacity* is the specification of a collection of Actions. This specification 
 * makes no assumptions about the implementation of each Action. It is used to specify 
 * what an Agent can do, what behavior is required for its execution.
 * 
 * <p>A *Skill* is a collections of Actions implementing a Capacity as described in
 * this specification.
 * 
 * <p>An Agent can dynamically evolve by acquiring (learning) new Capacities, and it 
 * can also dynamically change the Skill associated with a given Capacity.
 * Acquiring a new Capacity enables an Agent to get access to new behaviors.
 * This provides Agents with a self-adaptation 
 * mechanism that allows them to dynamically change their architecture according to 
 * their current needs and goals.
 */
@CreateWith(SARLSpecCreator)
describe "Capacity Reference"{

		@Inject extension SARLParser

		describe "Defining a Capacity" {
			
			/* A Capacity is the specification of a collection of Actions. 
			 * Consequently, only Action signatures can be defined inside
			 * a Capacity: no attribute or field is allowed, and no body (code)
			 * for the Action may be present.
			 * 
			 * <p>The definition of a Capacity is done with the `capacity`
			 * keyword. Below, a Capacity that permits logging messages is defined.
			 * This Capacity enables an Agent to log information and debugging messages.
			 * 
			 * <note> Defining a Capacity 
			 * without Actions is a symptom of a design problem.</note>
			 *  
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Capacity Definition"{
				// Test the URLs from the beginning of this page.
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				capacity Logging {
					// Log an information message
					def info(text : String)
					// Log a debugging message
					def debug(text : String)
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.cr",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.cr"
					it should haveNbImports 0
					it should haveNbElements 1
				]
				
				model.xtendTypes.get(0) => [
					it should beCapacity "Logging"
					it should extend _
					it should haveNbElements 2
					(it as SarlCapacity).members.get(0) => [
						it should beActionSignature "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						]
					]
					(it as SarlCapacity).members.get(1) => [
						it should beActionSignature "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						]
					]
				]
			}
		
			/* In some situations, it is useful to specialize the definition
			 * of a Capacity. Capacity specialization is supported by the inheritance
			 * feature of SARL, which has the same semantics as the inheritance
			 * mechanism of the Java object-oriented language.
			 * 
			 * <p>The extended Capacity is specified just after the `extends`
			 * keyword.
			 * 
			 * <veryimportantnote> A Capacity 
			 * type can extend __zero-to-many__ other Capacity types.
			 * This is similar to the implementation of
			 * interfaces in the Java language.</veryimportantnote>
			 * 
			 * <p>In the following code, the `Logging` Capacity (defined
			 * above) is extended to enabling the output of error messages.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Extending a Capacity"{
				val model = '''
				capacity ErrorLogging extends Logging {
					// Log a error message
					def error(text : String)
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.cr
					capacity Logging {
						// Log an information message
						def info(text : String)
						// Log a debugging message
						def debug(text : String)
					}",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.cr"
					it should haveNbImports 0
					it should haveNbElements 2
				]
				
				model.xtendTypes.get(0) => [
					it should beCapacity "Logging"
					it should extend _
					it should haveNbElements 2
					(it as SarlCapacity).members.get(0) => [
						it should beActionSignature "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						]
					]
					(it as SarlCapacity).members.get(1) => [
						it should beActionSignature "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						]
					]
				]
				
				model.xtendTypes.get(1) => [
					it should beCapacity "ErrorLogging"
					it should extend "io.sarl.docs.reference.cr.Logging"
					it should haveNbElements 1
					(it as SarlCapacity).members.get(0) => [
						it should beActionSignature "error"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						]
					]
				]
			}

			/* In some situations, it is useful to define a Capacity by
			 * extending more than one Capacity.
			 * Below, the `Cap3` Capacity is defined as an extension of the Capacities
			 * `Cap1` and `Cap2`.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Extending Multiple Capacities"{
				val model = '''
				capacity Cap1 {
					def action1
				}
				capacity Cap2 {
					def action2
				}
				capacity Cap3 extends Cap1, Cap2 {
					def action3
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.cr",
					// TEXT
					""
				)


				model => [
					it should havePackage "io.sarl.docs.reference.cr"
					it should haveNbImports 0
					it should haveNbElements 3
				]
				
				model.xtendTypes.get(0) => [
					it should beCapacity "Cap1"
					it should extend _
					it should haveNbElements 1
					(it as SarlCapacity).members.get(0) => [
						it should beActionSignature "action1"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]

				model.xtendTypes.get(1) => [
					it should beCapacity "Cap2"
					it should extend _
					it should haveNbElements 1
					(it as SarlCapacity).members.get(0) => [
						it should beActionSignature "action2"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]

				model.xtendTypes.get(2) => [
					it should beCapacity "Cap3"
					it should extend #["io.sarl.docs.reference.cr.Cap1", "io.sarl.docs.reference.cr.Cap2"]
					it should haveNbElements 1
					(it as SarlCapacity).members.get(0) => [
						it should beActionSignature "action3"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]
			}

			/** Modifiers are used to modify declarations of types and type members.
			 * This section introduces the modifiers for Capacity.
			 * The modifiers are usually written before the keyword defining the Capacity.
			 * 
			 * <p>The complete description of the modifier semantics is available in
			 * <a href="./BasicObjectOrientedProgrammingSupportModifiersSpec.html">this section</a>.
			 */
			describe "Modifiers" {
				
				/** A Capacity may be declared with one or more modifiers, 
				 * which affect its runtime behavior: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`public`:  the Capacity is accessible from any other type;</li>
				 *     <li>`package`: the Capacity is accessible only from types 
				 *          in the same package.</li>
				 *     </ul></li>
				 * </ul>
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Capacity Modifiers" {
					'''
						public capacity Example1 {
						}
						package capacity Example2 {
						}
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.cr",
						// TEXT
						""
					)
					// Test URL in the enclosing section text.
					"./BasicObjectOrientedProgrammingSupportModifiersSpec.html" should beAccessibleFrom this
				}
	
				/** The modifiers for the Actions (methods) in a Capacity are: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`public`: the Action is accessible from any type.</li>
				 *     </ul></li>
				 * </ul>
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Method Modifiers" {
					'''
						// Public access function
						public def example1 { }
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.cr
						abstract behavior Behavior1 {",
						// TEXT
						"}"
					)
				}
	
			}

		}
		
		/* Several Capacities are defined and reserved by the SARL Core
		 * Specification.
		 * They compose the minimal set of Capacities that a runtime
		 * environment must support to run a SARL program.
		 *
		 * <veryimportantnote> You must not
		 * define a Capacity with a fully qualified name equals to one
		 * of the reserved Capacities.</veryimportantnote>
		 * 
		 * <p>The built-in Capacities are defined in the 
		 * [Built-in Capacity Reference](BuiltInCapacityReferenceSpec.html).
		 * 
		 * @filter(.*)
		 */
		fact "Built-in Capacities"{
			"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
		}

		/* The use of a Capacity is related to the associated [Skills](SkillReferenceSpec.html).
		 * A Capacity cannot be called by itself since it does not provide
		 * an implementation: this is the role of the Skill.
		 * 
		 * <p>When a function `fct` of the Capacity `C` is called, 
		 * it means that the Agent silently does:
		 * 
		 *  * Find the Skill `S` associated to `C`; and
		 *  * Call `fct` on the object `S`.
		 *  
		 * 
		 * <p>Details on the use of the Capacities may be found in the following:
		 * 
		 *  * [Agent](AgentReferenceSpec.html)
		 *  * [Behavior](BehaviorReferenceSpec.html)
		 * 
		 * @filter(.*)
		 */
		fact "Use of the Capacities"{
			"SkillReferenceSpec.html" should beAccessibleFrom this
			"AgentReferenceSpec.html" should beAccessibleFrom this
			"BehaviorReferenceSpec.html" should beAccessibleFrom this
		}

	/* Specification: SARL General-purpose Agent-Oriented Programming Language ("Specification")<br/>
	 * Version: %sarlspecversion%<br/>
	 * Status: %sarlspecreleasestatus%<br/>
	 * Release: %sarlspecreleasedate%
	 * 
	 * 
	 * <p>Copyright &copy; %copyrightdate% %copyrighters%. All rights reserved.
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
