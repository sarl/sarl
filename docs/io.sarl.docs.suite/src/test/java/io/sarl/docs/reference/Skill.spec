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
 * This document describes how to define skills in SARL.
 * Before reading this document, it is recommended to read
 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html),
 * and the [Capacity Reference](./CapacityReferenceSpec.html).
 * 
 * <!-- OUTPUT OUTLINE -->
 * 
 * A *Capacity* is the specification of a collection of actions. This specification 
 * makes no assumptions about its implementation. It could be used to specify 
 * what an agent can do, what a behavior requires for its execution.
 * 
 * A *Skill* is a possible implementation of a capacity fulfilling all the 
 * constraints of this specification.
 */
@CreateWith(SARLSpecCreator)
describe "Skill Reference" {

		@Inject extension SARLParser

		describe "Defining a Skill" {
			
			/* The definition of a skill is done with the `skill`
			 * keyword. A skill must always implement a capacity. This
			 * relationship is specified with the `implements` keyword.
			 * 
			 * Below, a skill is defined for outputting the messages on the standard console
			 * (defined in the [Capacity Reference](CapacityReferenceSpec.html)).
			 * Note that all the actions defined in the implemented capacity must
			 * have a definition (with a body) in the skill.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Basic Definition"{
				val model = '''
				skill ConsoleLogging implements Logging {
					def info(text : String) {
						System.out.println(text)
					}
					def debug(text : String) {
						System.err.println(text)
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.sr
					capacity Logging {
						// Log an information message
						def info(text : String)
						// Log a debugging message
						def debug(text : String)
					}",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.sr")
				model.mustNotHaveImport
				model.mustHaveTopElements(2)
				var c = model.elements.get(0).mustBeCapacity("Logging").mustHaveFeatures(2)
				c.features.get(0).mustBeActionSignature("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				c.features.get(1).mustBeActionSignature("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				var s = model.elements.get(1).mustBeSkill("ConsoleLogging", null, "io.sarl.docs.reference.sr.Logging").mustHaveFeatures(2)
				s.features.get(0).mustBeAction("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				s.features.get(1).mustBeAction("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
			}
		
			/* In several cases, it is useful or mandatory to base the
			 * capacity's implementation on attributes (fields).
			 * 
			 * The following example defines a skill that uses the standard
			 * Java logging system.
			 * For avoiding to create an instance of the Java logger each
			 * time the capacity's actions are invoked, an instance
			 * of the Java logger is created and stored into a field
			 * of the skill.
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Field Definition" {
				val model = '''
				skill StandardJavaLogging implements Logging {
					// A field is defined in the skill
					val logger = Logger.anonymousLogger
					def info(text : String) {
						logger.info(text)
					}
					def debug(text : String) {
						logger.fine(text)
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.sr
					import java.util.logging.Logger
					capacity Logging {
						// Log an information message
						def info(text : String)
						// Log a debugging message
						def debug(text : String)
					}",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.sr")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "java.util.logging.Logger", false, false, false)
				model.mustHaveTopElements(2)
				var c = model.elements.get(0).mustBeCapacity("Logging").mustHaveFeatures(2)
				c.features.get(0).mustBeActionSignature("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				c.features.get(1).mustBeActionSignature("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				var s = model.elements.get(1).mustBeSkill("StandardJavaLogging", null, "io.sarl.docs.reference.sr.Logging").mustHaveFeatures(3)
				s.features.get(0).mustBeAttribute(false, "logger", null, true)
				s.features.get(1).mustBeAction("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				s.features.get(2).mustBeAction("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
			}

			/* As for fields, it is possible to declare additional methods in the skill. 
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Action Definition" {
				val model = '''
				skill MyLogging implements Logging {
					def info(text : String) {
						output(text)
					}
					def debug(text : String) {
						output(text)
					}
					// Define an utility function
					// that is outputting the text
					def output(t : String) {
						System.err.println(t)
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.sr
					import java.util.logging.Logger
					capacity Logging {
						// Log an information message
						def info(text : String)
						// Log a debugging message
						def debug(text : String)
					}",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.sr")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "java.util.logging.Logger", false, false, false)
				model.mustHaveTopElements(2)
				var c = model.elements.get(0).mustBeCapacity("Logging").mustHaveFeatures(2)
				c.features.get(0).mustBeActionSignature("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				c.features.get(1).mustBeActionSignature("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				var s = model.elements.get(1).mustBeSkill("MyLogging", null, "io.sarl.docs.reference.sr.Logging").mustHaveFeatures(3)
				s.features.get(0).mustBeAction("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				s.features.get(1).mustBeAction("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				s.features.get(2).mustBeAction("output", null, 1, false).mustHaveParameter(0, "t", "java.lang.String", false)
			}

			/* By default, it is not needed to specify a
			 * constructor for the skills.
			 * However, the definition of a constructor
			 * is mandatory if a value must be initialized.
			 *  
			 * The constructors defined in the abstract `Skill` class
			 * are:
			 * 
			 *  * The default constructor: `def Skill()`
			 *  * The constructor with owner: `def Skill(owner : Agent)`
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Constructor Definition" {
				val model = '''
				skill StandardJavaLogging implements Logging {
					// A field is defined in the skill
					val logger : Logger
					// The constructor is mandatory
					// for defining the field "logger"
					new (l : Logger) {
						super() // Call the super's constructor
						logger = l
					}
					
					def info(text : String) {
						logger.info(text)
					}
					def debug(text : String) {
						logger.fine(text)
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.sr
					import java.util.logging.Logger
					capacity Logging {
						// Log an information message
						def info(text : String)
						// Log a debugging message
						def debug(text : String)
					}",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.sr")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "java.util.logging.Logger", false, false, false)
				model.mustHaveTopElements(2)
				var c = model.elements.get(0).mustBeCapacity("Logging").mustHaveFeatures(2)
				c.features.get(0).mustBeActionSignature("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				c.features.get(1).mustBeActionSignature("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				var s = model.elements.get(1).mustBeSkill("StandardJavaLogging", null, "io.sarl.docs.reference.sr.Logging").mustHaveFeatures(4)
				s.features.get(0).mustBeAttribute(false, "logger", "java.util.logging.Logger", false)
				s.features.get(1).mustBeConstructor(1, false).mustHaveParameter(0, "l", "java.util.logging.Logger", false)
				s.features.get(2).mustBeAction("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				s.features.get(3).mustBeAction("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
			}

			/* In some use cases, it is useful to define a skill by
			 * implementing more than one capacity.
			 * Below, the `MyLogging` skill is defined as an implementation
			 * of the capacities `Logging` and `LogReader`.
			 * All the methods defined in the implemented interfaces must have
			 * an implementation in the skill.
			 * 
			 * If two implemented capacities has the same action signature,
			 * it must be implemented only once time in the skill.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Multiple Capacity Implementation"{
				val model = '''
				capacity LogReader {
					def open(filename : String) : int
					def info(t : String)
					def close(fid : int)
				}
				skill MyLogging implements Logging, LogReader {
					// Shared implementation for the methods
					// defind in the two capacities.
					def info(text : String) {
						System.out.println(text)
					}
					def debug(text : String) {
						System.out.println(text)
					}
					def open(filename : String) : int {
						return 0
					}
					def close(fid : int) {
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.sr
					capacity Logging {
						// Log an information message
						def info(text : String)
						// Log a debugging message
						def debug(text : String)
					}",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.sr")
				model.mustNotHaveImport
				model.mustHaveTopElements(3)
				var c1 = model.elements.get(0).mustBeCapacity("Logging").mustHaveFeatures(2)
				c1.features.get(0).mustBeActionSignature("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				c1.features.get(1).mustBeActionSignature("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				var c2 = model.elements.get(1).mustBeCapacity("LogReader").mustHaveFeatures(3)
				c2.features.get(0).mustBeActionSignature("open", "int", 1, false).mustHaveParameter(0, "filename", "java.lang.String", false)
				c2.features.get(1).mustBeActionSignature("info", null, 1, false).mustHaveParameter(0, "t", "java.lang.String", false)
				c2.features.get(2).mustBeActionSignature("close", null, 1, false).mustHaveParameter(0, "fid", "int", false)
				var s = model.elements.get(2).mustBeSkill("MyLogging", null, "io.sarl.docs.reference.sr.Logging", "io.sarl.docs.reference.sr.LogReader").mustHaveFeatures(4)
				s.features.get(0).mustBeAction("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				s.features.get(1).mustBeAction("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				s.features.get(2).mustBeAction("open", "int", 1, false).mustHaveParameter(0, "filename", "java.lang.String", false)
				s.features.get(3).mustBeAction("close", null, 1, false).mustHaveParameter(0, "fid", "int", false)
			}

			/* In some use cases, it is useful to specialize the definition
			 * of a skill. This mechanism is supported by the inheritance
			 * feature of SARL, which has the same semantic as the inheritance
			 * mechanism as the Java object-oriented language.
			 * 
			 * The extended skill is specified just after the `extends`
			 * keyword.
			 * 
			 * <span class="label label-danger">Important</span> A skill type
			 * can extend __only one__ other skill type.  This is close
			 * to the constraint on the extension of classes in the Java
			 * language.
			 * 
			 * In the following code, the `StandardJavaLogging` skill (defined
			 * previously) is extended for changing the output of the info.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Extending a Skill"{
				val model = '''
				skill ExtendedLogging extends StandardJavaLogging {
					def info(text : String) {
						super.info("INFO: "+text)
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.sr
					import io.sarl.lang.core.Capacity
					capacity Logging {
						// Log an information message
						def info(text : String)
						// Log a debugging message
						def debug(text : String)
					}
					skill StandardJavaLogging implements Logging {
						def info(text : String) {
						}
						def debug(text : String) {
						}
					}",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.sr")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.lang.core.Capacity", false, false, false)
				model.mustHaveTopElements(3)
				var c = model.elements.get(0).mustBeCapacity("Logging").mustHaveFeatures(2)
				c.features.get(0).mustBeActionSignature("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				c.features.get(1).mustBeActionSignature("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				var s1 = model.elements.get(1).mustBeSkill("StandardJavaLogging", null, "io.sarl.docs.reference.sr.Logging").mustHaveFeatures(2)
				s1.features.get(0).mustBeAction("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				s1.features.get(1).mustBeAction("debug", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
				var s2 = model.elements.get(2).mustBeSkill("ExtendedLogging", "io.sarl.docs.reference.sr.StandardJavaLogging").mustHaveFeatures(1)
				s2.features.get(0).mustBeAction("info", null, 1, false).mustHaveParameter(0, "text", "java.lang.String", false)
			}

		}
		
		/* Several capacities are defined and reserved by the SARL Core
		 * Specification. The corresponding skills are provided
		 * by the runtime environment (such as the [Janus platform](http://www.janusproject.io)).
		 * The built-in skills are described in the 
		 * [Built-in Capacity Reference](./BuiltinCapacityReferenceSpec.html).
		 */
		describe "Built-in Skills"{
		}

		/* Details on the use of the skills may be found in the references of
		 * the major behavior-based concepts of SARL:
		 * 
		 *  * [Agent](AgentReferenceSpec.html)
		 *  * [Behavior](BehaviorReferenceSpec.html)
		 */
		describe "Use of the Skills"{
		}

}
