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
import io.sarl.lang.sarl.ActionSignature
import io.sarl.lang.sarl.Capacity
import io.sarl.lang.sarl.Skill
import io.sarl.lang.sarl.Action
import io.sarl.lang.sarl.Constructor
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

/* @outline
 *
 * This document describes how to define skills in SARL.
 * Before reading this document, it is recommended reading
 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
 * and the [Capacity Reference](CapacityReferenceSpec.html).
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
				// Test the URLs from the beginning of the page
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				"CapacityReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				skill ConsoleLogging implements Logging {
					def info(text : String) {
						System.out.println(text)
					}
					def debug(text : String) {
						System.err.println(text)
					}
				}
				'''.parseSuccessfully(
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
				
				model => [
					it should havePackage "io.sarl.docs.reference.sr"
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
					it should beSkill "ConsoleLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging"]
					it should haveNbElements 2
					(it as Skill).features.get(0) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Skill).features.get(1) => [
						it should beAction "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
				]
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
				'''.parseSuccessfully(
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
				
				model => [
					it should havePackage "io.sarl.docs.reference.sr"
					it should haveNbImports 1
					it should importClass "java.util.logging.Logger"
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
					it should beSkill "StandardJavaLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging"]
					it should haveNbElements 3
					(it as Skill).features.get(0) => [
						it should beValue "logger"
						it should haveType _
						it should haveInitialValue "java.util.logging.Logger.getAnonymousLogger"
					]
					(it as Skill).features.get(1) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Skill).features.get(2) => [
						it should beAction "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
				]
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
				'''.parseSuccessfully(
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
				
				model => [
					it should havePackage "io.sarl.docs.reference.sr"
					it should haveNbImports 1
					it should importClass "java.util.logging.Logger"
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
					it should beSkill "MyLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging"]
					it should haveNbElements 3
					(it as Skill).features.get(0) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Skill).features.get(1) => [
						it should beAction "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Skill).features.get(2) => [
						it should beAction "output"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "t"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
				]
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
				'''.parseSuccessfully(
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

				model => [
					it should havePackage "io.sarl.docs.reference.sr"
					it should haveNbImports 1
					it should importClass "java.util.logging.Logger"
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
					it should beSkill "StandardJavaLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging"]
					it should haveNbElements 4
					(it as Skill).features.get(0) => [
						it should beValue "logger"
						it should haveType "java.util.logging.Logger"
						it should haveInitialValue _
					]
					(it as Skill).features.get(1) => [
						it should beConstructor _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Constructor).params.get(0) => [
							it should beParameter "l"
							it should haveType "java.util.logging.Logger"
							it should haveDefaultValue _
						]
					]
					(it as Skill).features.get(2) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Skill).features.get(3) => [
						it should beAction "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
				]
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
				'''.parseSuccessfully(
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

				model => [
					it should havePackage "io.sarl.docs.reference.sr"
					it should haveNbImports 0
					it should haveNbElements 3
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
					it should beCapacity "LogReader"
					it should extend _
					it should haveNbElements 3
					(it as Capacity).features.get(0) => [
						it should beActionSignature "open"
						it should reply "int"
						it should haveNbParameters 1
						it should beVariadic false
						(it as ActionSignature).params.get(0) => [
							it should beParameter "filename"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Capacity).features.get(1) => [
						it should beActionSignature "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as ActionSignature).params.get(0) => [
							it should beParameter "t"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Capacity).features.get(2) => [
						it should beActionSignature "close"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as ActionSignature).params.get(0) => [
							it should beParameter "fid"
							it should haveType "int"
							it should haveDefaultValue _
						] 
					]
				]

				model.elements.get(2) => [
					it should beSkill "MyLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging", "io.sarl.docs.reference.sr.LogReader"]
					it should haveNbElements 4
					(it as Skill).features.get(0) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Skill).features.get(1) => [
						it should beAction "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Skill).features.get(2) => [
						it should beAction "open"
						it should reply "int"
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "filename"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Skill).features.get(3) => [
						it should beAction "close"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "fid"
							it should haveType "int"
							it should haveDefaultValue _
						] 
					]
				]
			}

			/* In some use cases, it is useful to specialize the definition
			 * of a skill. This mechanism is supported by the inheritance
			 * feature of SARL, which has the same semantic as the inheritance
			 * mechanism as the Java object-oriented language.
			 * 
			 * The extended skill is specified just after the `extends`
			 * keyword.
			 * 
			 * <veryimportantnote> A skill type
			 * can extend __only one__ other skill type.  This is close
			 * to the constraint on the extension of classes in the Java
			 * language.</veryimportantnote>
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
				'''.parseSuccessfully(
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

				model => [
					it should havePackage "io.sarl.docs.reference.sr"
					it should haveNbImports 1
					it should importClass "io.sarl.lang.core.Capacity"
					it should haveNbElements 3
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
					it should beSkill "StandardJavaLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging"]
					it should haveNbElements 2
					(it as Skill).features.get(0) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as Skill).features.get(1) => [
						it should beAction "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
				]

				model.elements.get(2) => [
					it should beSkill "ExtendedLogging"
					it should extend "io.sarl.docs.reference.sr.StandardJavaLogging"
					it should implement _
					it should haveNbElements 1
					(it as Skill).features.get(0) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Action).signature.params.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
				]
			}

		}
		
		/* Several capacities are defined and reserved by the SARL Core
		 * Specification. The corresponding skills are provided
		 * by the runtime environment (such as the [Janus platform](http://www.janusproject.io)).
		 * The built-in skills are described in the 
		 * [Built-in Capacity Reference](BuiltInCapacityReferenceSpec.html).
		 * 
		 * @filter(.*)
		 */
		fact "Built-in Skills"{
			"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
		}

		/* Details on the use of the skills may be found in the references of
		 * the major behavior-based concepts of SARL:
		 * 
		 *  * [Agent](AgentReferenceSpec.html)
		 *  * [Behavior](BehaviorReferenceSpec.html)
		 * 
		 * @filter(.*)
		 */
		fact "Use of the Skills"{
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
