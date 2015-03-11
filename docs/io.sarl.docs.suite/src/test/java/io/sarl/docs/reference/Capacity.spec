/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Capacity;

/* @outline
 *
 * This document describes how to define capacities in SARL.
 * Before reading this document, it is recommended reading
 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html).
 * 
 * An Action is a specification of a transformation of a part of the 
 * designed system or its environment. This transformation guarantees 
 * resulting properties if the system before the transformation satisfies 
 * a set of constraints. An action is defined in terms of pre- and post-conditions.
 * 
 * A *Capacity* is the specification of a collection of actions. This specification 
 * makes no assumptions about its implementation. It could be used to specify 
 * what an agent can do, what a behavior requires for its execution.
 * 
 * A *Skill* is a possible implementation of a capacity fulfilling all the 
 * constraints of this specification.
 * 
 * An agent can dynamically evolve by learning/acquiring new Capacities, but it 
 * can also dynamically change the Skill associated to a given capacity.
 * Acquiring new capacities also enables an agent to get access to new 
 * behaviors requiring these capacities. This provides agents with a self-adaptation 
 * mechanism that allows them to dynamically change their architecture according to 
 * their current needs and goals.
 */
@CreateWith(SARLSpecCreator)
describe "Capacity Reference"{

		@Inject extension SARLParser

		describe "Defining a Capacity" {
			
			/* A capacity is the specification of a collection of actions. 
			 * Consequently, only action's signatures can be defined inside
			 * a capacity: no attribute nor field is allowed, and no body
			 * for the actions.
			 * 
			 * The definition of a capacity is done with the `capacity`
			 * keyword. Below, a capacity that permits logging messages is defined.
			 * This capacity enables to log information and debugging messages.
			 * 
			 * <note> Defining a capacity 
			 * without action inside is a symptom of a design problem.</note>
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
				
				model.elements.get(0) => [
					it should beCapacity "Logging"
					it should extend _
					it should haveNbElements 2
					(it as Capacity).features.get(0) => [
						it should beActionSignature "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as ActionSignature).params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						]
					]
					(it as Capacity).features.get(1) => [
						it should beActionSignature "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as ActionSignature).params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						]
					]
				]
			}
		
			/* In some use cases, it is useful to specialize the definition
			 * of a capacity. This mechanism is supported by the inheritance
			 * feature of SARL, which has the same semantic as the inheritance
			 * mechanism as the Java object-oriented language.
			 * 
			 * The extended capacity is specified just after the `extends`
			 * keyword.
			 * 
			 * <veryimportantnote> A capacity 
			 * type can extend __zero-to-many__ other capacity types.
			 * This is close to the constraint on the implementation of
			 * interfaces in the Java language.</veryimportantnote>
			 * 
			 * In the following code, the `Logging` capacity (defined
			 * previously) is extended for enabling the output of error messages.
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
				
				model.elements.get(0) => [
					it should beCapacity "Logging"
					it should extend _
					it should haveNbElements 2
					(it as Capacity).features.get(0) => [
						it should beActionSignature "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as ActionSignature).params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						]
					]
					(it as Capacity).features.get(1) => [
						it should beActionSignature "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as ActionSignature).params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						]
					]
				]
				
				model.elements.get(1) => [
					it should beCapacity "ErrorLogging"
					it should extend "io.sarl.docs.reference.cr.Logging"
					it should haveNbElements 1
					(it as Capacity).features.get(0) => [
						it should beActionSignature "error"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as ActionSignature).params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						]
					]
				]
			}

			/* In some use cases, it is useful to define a capacity by
			 * extending more than one capacity.
			 * Below, the `Cap3` capacity is defined as an extension of the capacities
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
				
				model.elements.get(0) => [
					it should beCapacity "Cap1"
					it should extend _
					it should haveNbElements 1
					(it as Capacity).features.get(0) => [
						it should beActionSignature "action1"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]

				model.elements.get(1) => [
					it should beCapacity "Cap2"
					it should extend _
					it should haveNbElements 1
					(it as Capacity).features.get(0) => [
						it should beActionSignature "action2"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]

				model.elements.get(2) => [
					it should beCapacity "Cap3"
					it should extend #["io.sarl.docs.reference.cr.Cap1", "io.sarl.docs.reference.cr.Cap2"]
					it should haveNbElements 1
					(it as Capacity).features.get(0) => [
						it should beActionSignature "action3"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]
			}

		}
		
		/* Several capacities are defined and reserved by the SARL Core
		 * Specification.
		 * They are composing the minimal set of capacities that a runtime
		 * environment must support for running a SARL program.
		 *
		 * <veryimportantnote> You must not
		 * define a capacity with a fully qualified name equals to one
		 * of the reserved capacities.</veryimportantnote>
		 * 
		 * The built-in capacities are defined in the 
		 * [Built-in Capacity Reference](BuiltInCapacityReferenceSpec.html).
		 * 
		 * @filter(.*)
		 */
		fact "Built-in Capacities"{
			"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
		}

		/* The use of the capacity is related to the associated [skills](SkillReferenceSpec.html).
		 * It means that a capacity cannot be called by itself since it is not providing
		 * an implementation: this is the role of the skill.
		 * 
		 * When a function `fct` of the capacity `C` is called, 
		 * it means that the agent silently does:
		 * 
		 *  * Find the skill `S` associated to `C`; and
		 *  * Call `fct` on the object `S`.
		 *  
		 * 
		 * Details on the use of the capacities may be found in the references of
		 * the major behavior-based concepts of SARL:
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
	 * Copyright &copy; %copyrightdate% %copyrighters%. All rights reserved.
	 * 
	 * Licensed under the Apache License, Version 2.0;
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
		("%sarlspecreleasestatus%" == "Final Release"
			|| "%sarlspecreleasestatus%" == "Draft Release") should be true
		"%sarlspecreleasedate%" should beDate "YYYY-mm-dd"
		"%copyrightdate%" should beNumber "0000";
		("%copyrighters%".empty || "%copyrighters%".startsWith("%")) should be false
	}

}
