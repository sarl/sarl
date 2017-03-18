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
import io.sarl.lang.sarl.SarlSkill
import org.eclipse.xtend.core.xtend.XtendConstructor
import org.eclipse.xtext.common.types.JvmVisibility
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

/* @outline
 *
 * <p>This document describes how to define Skills in SARL.
 * Before reading this document, we recommend that you read
 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
 * and the [Capacity Reference](CapacityReferenceSpec.html).
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
 */
@CreateWith(SARLSpecCreator)
describe "Skill Reference" {

		@Inject extension SARLParser

		describe "Defining a Skill" {
			
			/* A Skill implements a Capacity and is defined with the `skill`
			 * keyword. This relationship is specified with the `implements` keyword.
			 * 
			 * <p>Below, a Skill is defined to output messages on the standard console
			 * (defined in the [Capacity Reference](CapacityReferenceSpec.html)).
			 * Note that all the Actions defined in the Capacity must
			 * have a definition (with a body containing code) in the Skill.
			 *  
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
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
					it should beSkill "ConsoleLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging"]
					it should haveNbElements 2
					(it as SarlSkill).members.get(0) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlSkill).members.get(1) => [
						it should beAction "debug"
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
		
			/* Often it is useful or necessary to base a Skill (a
			 * Capacity's implementation) on attributes (properties or fields).
			 * 
			 * <p>The following example defines a Skill that uses the standard
			 * Java logging library.
			 * To avoid creating an instance of the Java logger each
			 * time the Capacity's Actions are invoked, an instance
			 * of the Java logger is created and stored in a field
			 * of the Skill.
			 *  
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Field Definition" {
				val model = '''
				skill StandardJavaLogging implements Logging {
					// A field is defined in the Skill
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
					it should beSkill "StandardJavaLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging"]
					it should haveNbElements 3
					(it as SarlSkill).members.get(0) => [
						it should beValue "logger"
						it should haveType _
						it should haveInitialValue "java.util.logging.Logger.getAnonymousLogger"
					]
					(it as SarlSkill).members.get(1) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlSkill).members.get(2) => [
						it should beAction "debug"
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

			/* It is possible to declare methods in the Skill
			 * in addition to those specified by the Capacity. 
			 *  
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
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
					it should beSkill "MyLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging"]
					it should haveNbElements 3
					(it as SarlSkill).members.get(0) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlSkill).members.get(1) => [
						it should beAction "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlSkill).members.get(2) => [
						it should beAction "output"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "t"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
				]
			}

			/* It is not necessary to specify a constructor for Skills unless 
			 * a value will be initialized.
			 *  
			 * <p>Two constructors are defined in the abstract `Skill` class:
			 * 
			 *  * A default constructor: `def Skill()`
			 *  * A constructor with owner: `def Skill(owner : Agent)`
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Constructor Definition" {
				val model = '''
				skill StandardJavaLogging implements Logging {
					// A field is defined in the Skill
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
					it should beSkill "StandardJavaLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging"]
					it should haveNbElements 4
					(it as SarlSkill).members.get(0) => [
						it should beValue "logger"
						it should haveType "java.util.logging.Logger"
						it should haveInitialValue _
					]
					(it as SarlSkill).members.get(1) => [
						it should beConstructor _
						it should haveNbParameters 1
						it should beVariadic false
						(it as XtendConstructor).parameters.get(0) => [
							it should beParameter "l"
							it should haveType "java.util.logging.Logger"
							it should haveDefaultValue _
						]
					]
					(it as SarlSkill).members.get(2) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlSkill).members.get(3) => [
						it should beAction "debug"
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

			/* In some situations it is useful to combine more than one Capacity in a Skill.
			 * Below, the `MyLogging` Skill is defined as an implementation
			 * of the Capacities `Logging` and `LogReader`.
			 * All the Actions defined in a Capacity must have
			 * an implementation in the related Skill.
			 * 
			 * <p>If two implemented Capacities include the same Action signature,
			 * it must be implemented only once in the Skill.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
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
					// defind in the two Capacities.
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
					it should beCapacity "LogReader"
					it should extend _
					it should haveNbElements 3
					(it as SarlCapacity).members.get(0) => [
						it should beActionSignature "open"
						it should reply "int"
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "filename"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlCapacity).members.get(1) => [
						it should beActionSignature "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "t"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlCapacity).members.get(2) => [
						it should beActionSignature "close"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "fid"
							it should haveType "int"
							it should haveDefaultValue _
						] 
					]
				]

				model.xtendTypes.get(2) => [
					it should beSkill "MyLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging", "io.sarl.docs.reference.sr.LogReader"]
					it should haveNbElements 4
					(it as SarlSkill).members.get(0) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlSkill).members.get(1) => [
						it should beAction "debug"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlSkill).members.get(2) => [
						it should beAction "open"
						it should reply "int"
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "filename"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlSkill).members.get(3) => [
						it should beAction "close"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "fid"
							it should haveType "int"
							it should haveDefaultValue _
						] 
					]
				]
			}

			/* In some situations it is useful to specialize the definition
			 * of a Skill. This mechanism is supported by the __inheritance__
			 * feature of SARL, which has the same semantics as the inheritance
			 * mechanism of the Java object-oriented language.
			 * 
			 * <p>The extended Skill is specified just after the `extends`
			 * keyword.
			 * 
			 * <veryimportantnote> A Skill type
			 * can extend __only one__ other Skill type.  This is similar
			 * to the constraint on the extension of classes in the Java
			 * language.</veryimportantnote>
			 * 
			 * <p>In the following code, the `StandardJavaLogging` Skill (defined
			 * above) is extended to override the info output.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
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
					it should beSkill "StandardJavaLogging"
					it should extend _
					it should implement #["io.sarl.docs.reference.sr.Logging"]
					it should haveNbElements 2
					(it as SarlSkill).members.get(0) => [
						it should beAction "info"
						it should reply _
						it should haveNbParameters 1
						it should beVariadic false
						(it as SarlAction).parameters.get(0) => [
							it should beParameter "text"
							it should haveType "java.lang.String"
							it should haveDefaultValue _
						] 
					]
					(it as SarlSkill).members.get(1) => [
						it should beAction "debug"
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

				model.xtendTypes.get(2) => [
					it should beSkill "ExtendedLogging"
					it should extend "io.sarl.docs.reference.sr.StandardJavaLogging"
					it should implement _
					it should haveNbElements 1
					(it as SarlSkill).members.get(0) => [
						it should beAction "info"
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

			/** Modifiers are used to modify declarations of types and type members.
			 * This section introduces the modifiers for the Skill.
			 * The modifiers are usually written before the keyword for defining the Skill.
			 * 
			 * <p>The complete description of the modifiers' semantic is available in
			 * <a href="./BasicObjectOrientedProgrammingSupportModifiersSpec.html">this section</a>.
			 */
			describe "Modifiers" {
				
				/** A Skill may be declared with one or more modifiers, which affect its runtime behavior: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`public`:  the behavior is accessible from any other type (default);</li>
				 *     <li>`package`: the behavior is accessible only from types in the same package.</li>
				 *     </ul></li>
				 * <li>`abstract`: this Skill is abstract and cannot be instantiated; 
				       an extension Skill must implement this behavior.</li>
				 * <li>`final`: an extending Skill may not override this behavior.</li>
				 * </ul>
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Skill Modifiers" {
					'''
						public skill Example1 implements CapacityExample {
						}
						package skill Example2 implements CapacityExample {
						}
						abstract skill Example3 implements CapacityExample {
						}
						final skill Example4 implements CapacityExample {
						}
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.sr
						capacity CapacityExample { }",
						// TEXT
						""
					)
					// Test URL in the enclosing section text.
					"./BasicObjectOrientedProgrammingSupportModifiersSpec.html" should beAccessibleFrom this
				var visib = "capacity C1 {} skill S1 implements C1 {}".parse.xtendTypes.get(1)
				visib should beVisibleWith JvmVisibility::PUBLIC
				}
	
				/** The modifiers for the fields in a Skill are: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`public`:  the field is accessible from everywhere;</li>
				 *     <li>`protected`:  the field is accessible within the same package, and in derived Agents;</li>
				 *     <li>`package`: the field is accessible only within the same package as its Agent;</li>
				 *     <li>`private`: the field is accessible only within its Agent (default).</li>
				 *     </ul></li>
				 * </ul>
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Field Modifiers" {
					'''
						public var example1 : Object;
						protected var example2 : Object;
						package var example3 : Object;
						private var example4 : Object;
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.sr
						capacity C1 { }
						public skill Skill1 implements C1 {",
						// TEXT
						"}"
					)
					// Test default visibility
					var visib = "capacity C1 {} skill S1 implements C1 {var field : int}".parse.xtendTypes.get(1).members.get(0)
					visib should beVisibleWith JvmVisibility::PRIVATE
				}
	
				/** The modifiers for the methods in a Skill are: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`public`:  there are no restrictions on accessing the method (public);</li>
				 *     <li>`protected`:  the method is accessible within the same package, and derived classes;</li>
				 *     <li>`package`: the method is accessible only within the same package as its class;</li>
				 *     <li>`private`: the method is accessible only within its class.</li>
				 *     </ul></li>
				 * <li>`abstract`: the method has no implementation in the class.</li>
				 * <li>`dispatch`: the method provides an implementation for the dispatch method mechanism.</li>
				 * <li>`final`: the method cannot be overridden in derived classes.</li>
				 * </ul>
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Method Modifiers" {
					'''
						// Public access function
						public def example1 { }
						// Protected access function
						protected def example2 { }
						// Package access function
						package def example3 { }
						// Private access function
						private def example4 { }
						// Abstract function
						abstract def example5
						// Not-overridable function
						final def example6 { }
						// Dispatch functions
						dispatch def example7(p : Integer) { }
						dispatch def example7(p : Float) { }
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.sr
						capacity C1 { }
						abstract skill Skill1 implements C1 {",
						// TEXT
						"}"
					)
					// Test default visibility
					var visib = "capacity C1 {} skill S1 implements C1 {def fct {}}".parse.xtendTypes.get(1).members.get(0)
					visib should beVisibleWith JvmVisibility::PUBLIC
				}
	
			}

		}
		
		/* Several Capacities are defined and reserved by the SARL Core
		 * Specification. The corresponding Skills are provided
		 * by the runtime environment (such as the [Janus platform](http://www.janusproject.io)).
		 * The built-in Skills are described in the 
		 * [Built-in Capacity Reference](BuiltInCapacityReferenceSpec.html).
		 * 
		 * @filter(.*)
		 */
		fact "Built-in Skills"{
			"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
		}

		/* Details on the use of Skills may be found in the following:
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
	 * <p>Copyright &copy; %copyrightdate% %copyrighters%.
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
