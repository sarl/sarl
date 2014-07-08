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
package io.sarl.docs.reference

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

/**
 * This document describes how to define capacities in SARL.
 * Before reading this document, it is recommended to read
 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html).
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
 * mechanism that allow them to dynamically change their architecture according to 
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
			 * keyword. Below, a capacity that permits to log messages is defined.
			 * This capacity enables to log information and debugging messages.
			 * 
			 * <span class="label label-info">Note</span> Defining a capacity 
			 * without action inside is a symptom of a design problem.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Capacity Definition"{
				val model = '''
				capacity Logging {
					// Log an information message
					def info(text : String)
					// Log a debugging message
					def debug(text : String)
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.cr",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.cr")
				model.mustNotHaveImport
				model.mustHaveTopElements(1)
				var c = model.elements.get(0).mustBeCapacity("Logging").mustHaveFeatures(2)
				c.features.get(0).mustBeActionSignature("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				c.features.get(1).mustBeActionSignature("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
			}
		
			/* In some use cases, it is useful to specialize the definition
			 * of a capacity. This mechanism is supported by the inheritance
			 * feature of SARL, which has the same semantic as the inheritance
			 * mechanism as the Java object-oriented language.
			 * 
			 * The extended capacity is specified just after the `extends`
			 * keyword.
			 * 
			 * <span class="label label-danger">Important</span> A capacity 
			 * type can extend __zero to many__ other capacity types.
			 * This is close to the constraint on the implementation of
			 * interfaces in the Java language.
			 * 
			 * In the following code, the `Logging` capacity (defined
			 * previously) is extended for enabling the output of error messages.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Extending a Capacity"{
				val model = '''
				capacity ErrorLogging extends Logging {
					// Log a error message
					def error(text : String)
				}
				'''.parsesSuccessfully(
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
				model.mustHavePackage("io.sarl.docs.reference.cr")
				model.mustNotHaveImport
				model.mustHaveTopElements(2)
				var c1 = model.elements.get(0).mustBeCapacity("Logging").mustHaveFeatures(2)
				c1.features.get(0).mustBeActionSignature("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				c1.features.get(1).mustBeActionSignature("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				var c2 = model.elements.get(1).mustBeCapacity("ErrorLogging", "io.sarl.docs.reference.cr.Logging").mustHaveFeatures(1)
				c2.features.get(0).mustBeActionSignature("error", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
			}

			/* In some use cases, it is useful to define a capacity by
			 * extending more than one capacity.
			 * Below, the `Cap3` capacity is defined as an extension of the capacities
			 * `Cap1` and `Cap2`.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.cr",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.cr")
				model.mustNotHaveImport
				model.mustHaveTopElements(3)
				var c1 = model.elements.get(0).mustBeCapacity("Cap1").mustHaveFeatures(1)
				c1.features.get(0).mustBeActionSignature("action1", null, 0, false)
				var c2 = model.elements.get(1).mustBeCapacity("Cap2").mustHaveFeatures(1)
				c2.features.get(0).mustBeActionSignature("action2", null, 0, false)
				var c3 = model.elements.get(2).mustBeCapacity("Cap3", "io.sarl.docs.reference.cr.Cap1", "io.sarl.docs.reference.cr.Cap2").mustHaveFeatures(1)
				c3.features.get(0).mustBeActionSignature("action3", null, 0, false)
			}

		}
		
		/* Several capacities are defined and reserved by the SARL Core
		 * Specification.
		 * They are composing the minimal set of capacities that a runtime
		 * environment must support for running a SARL program.
		 *
		 * <span class="label label-danger">Important</span> You must not
		 * define a capacity with a fully qualified name equals to one
		 * of the reserved capacities.
		 * 
		 * The built-in capacities are defined in the 
		 * [Built-in Capacity Reference](./BuiltinCapacityReferenceSpec.html).
		 */
		describe "Built-in Capacities"{
		}

		/* The use of the capacity is related to the associated [skills](./SkillReferenceSpec.html).
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
		 */
		describe "Use of the Capacities"{
		}

}
