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
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.Constructor
import io.sarl.lang.sarl.Event
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

/* @outline
 *
 * This document describes how to define events in SARL.
 * Before reading this document, it is recommended reading
 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html).
 * 
 * An event is one of the core concepts in SARL.
 * It is a data structure composed of attributes.
 * Each attribute has a name, a type, and a value.
 * 
 * Events are exchanged among the agents or the behavioral units of agents,
 * inside a given [Space](SpaceReferenceSpec.html).
 * 
 * Each event has:
 * 
 *  * a type, i.e. its qualified name;
 *  * a source, the identifier of the sender of the event; and
 *  * a collection of name-value pairs, i.e. the attributes of the event.
 */
@CreateWith(SARLSpecCreator)
describe "Event Reference"{

		@Inject extension SARLParser

		/* In computer-science literature, there are two major concepts for defining the
		 * data structures that are exchanged by entities:
		 * the event, and the message.
		 * 
		 * It is mandatory to specify the type, the source 
		 * and the potential embedded data for both of them.
		 * The difference is related to the specification of
		 * the receiver. In one hand, the event does not
		 * force the sender to specify the receiver identifier.
		 * On the other hand, the message needs to have at least
		 * on receiver identifier (even if it means "all" the 
		 * possible identifiers).
		 * 
		 * Because the event permits to send data without being
		 * care of the sender, this concept is privileged
		 * by the designers of SARL.
		 * 
		 * Consequently, for sending data to another entity,
		 * you must create an instance of an event, and emit
		 * this object in a [Space](SpaceReferenceSpec.html).
		 * The sending API is detailed in the [Built-in Capacity
		 * Reference](BuiltInCapacityReferenceSpec.html).
		 * 
		 * 
		 * <note> There is 
		 * no message concept in SARL. All the communications are
		 * supported by the concept of `Event`</note>
		 * 
		 * @filter(.*)
		 */
		fact "Event vs. Message"{
			// Test the URLs from the beginning of the page
			"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
			"SpaceReferenceSpec.html" should beAccessibleFrom this
			"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
		}
		
		describe "Defining an Event" {
			
			/* An event can be defined with the `event` keyword.
			 * It must be followed by the name of the event without the
			 * qualified name of its package, which is inferred from the
			 * `package` keyword, if present.
			 * 
			 * When the event is empty, i.e. it does not contain any additional
			 * data than the source of the event, it is specified by an empty
			 * block, or by "nothing", after the event's declaration.
			 * 
			 * The example below contains the definition of the events
			 * `Event1` and `Event2`, which are both empty.
			 * The first event is defined with the "empty block" syntax.
			 * The second event is defined with the "nothing" syntax.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Define an empty event"{
				val model = '''
				event Event1 {  }
				event Event2
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.er",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.er"
					it should haveNbImports 0
					it should haveNbElements 2
				]
				
				model.elements.get(0) => [
					it should beEvent "Event1"
					it should extend _
					it should haveNbElements 0
				]
				
				model.elements.get(1) => [
					it should beEvent "Event2"
					it should extend _
					it should haveNbElements 0
				]
			}
		
			/**
			 * Events can carry information.
			 * This information is described by a set of attributes.
			 * Each attribute is declared according to the "Field Declaration"
			 * of the [General Syntax Reference](GeneralSyntaxReferenceSpec.html).
			 * 
			 * The following code example is defining the event `MyEvent` with
			 * three attributes.
			 * Each declaration of the attributes illustrates one possible syntax for
			 * defining a field:
			 * 
			 *  * declaration with explicit typing: `var number : Integer`
			 *  * declaration with type inference: `var string = "abc"`
			 *  * declaration with free inferred element: `var something`
			 *
			 * 
			 * According to the type inference mechanism used by SARL, the attribute
			 * `something` will have the type `Object`.
			 * 
			 * <note> Because of the use of 
			 * the `var` keyword, the values of the attributes can be modified.</note>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Define an event with attributes"{
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				event MyEvent {
					var number : Integer
					var string = "abc"
					var something
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.er",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.er"
					it should haveNbImports 0
					it should haveNbElements 1
				]
				
				model.elements.get(0) => [
					it should beEvent "MyEvent"
					it should extend _
					it should haveNbElements 3
					(it as Event).features.get(0) => [
						it should beVariable "number"
						it should haveType "java.lang.Integer"
						it should haveInitialValue _
					]
					(it as Event).features.get(1) => [
						it should beVariable "string"
						it should haveType _
						it should haveInitialValue "abc"
					]
					(it as Event).features.get(2) => [
						it should beVariable "something"
						it should haveType _
						it should haveInitialValue _
					]
				]
			}
			
			/**
			 * Events in SARL can carry information that is unmodifiable using the
			 * `val` keyword.
			 * 
			 * <importantnote> The `val`
			 * keyword has the same semantic as the `final` modifier in
			 * the Java language. It means that an element defined with `val`
			 * can be initialized only once. It also means that the element is read-only.
			 * But if the element is a reference to an object, then this object is
			 * not read-only (only the initial reference is).</importantnote>
			 * 
			 * Because the `val` keyword defines a single-initialization
			 * variable, it is mandatory to specify the initialization value.
			 * This value could be specified at the end of the `val`
			 * directive, or by specifying a constructor.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Define an event with value attributes"{
				val model = '''
				event MyEvent {
					val string = "abcd"
					val number : Integer
					
					new(nb : Integer) {
						number = nb
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.er
					import io.sarl.lang.core.Agent",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.er"
					it should haveNbImports 1
					it should importClass "io.sarl.lang.core.Agent"
					it should haveNbElements 1
				]
				
				model.elements.get(0) => [
					it should beEvent "MyEvent"
					it should extend _
					it should haveNbElements 3
					(it as Event).features.get(0) => [
						it should beValue "string"
						it should haveType _
						it should haveInitialValue "abcd"
					]
					(it as Event).features.get(1) => [
						it should beValue "number"
						it should haveType "java.lang.Integer"
						it should haveInitialValue _
					]
					(it as Event).features.get(2) => [
						it should beConstructor _
						it should haveNbParameters 1
						it should beVariadic false
						(it as Constructor).params.get(0) => [
							it should beParameter "nb"
							it should haveType "java.lang.Integer"
							it should haveDefaultValue _
						]
					]
				]
			}
			
			/* In some use cases, it is useful to specialize the definition
			 * of an event. This mechanism is supported by the inheritance
			 * feature of SARL, which has the same semantic as the inheritance
			 * mechanism as the Java object-oriented language.
			 * 
			 * The extended event is specified just after the `extends`
			 * keyword.
			 * 
			 * <veryimportantnote> An event type
			 * can extend __only one__ other event type.  This is close
			 * constraint as the class extension of classes in the Java
			 * language.</veryimportantnote>
			 * 
			 * In the following code, a first event is defined with the name
			 * `Event1` and an attribute named `string`.
			 * A second event `Event2` is defined as the extension
			 * of the first event. It contains a new attribute named
			 * `number`.
			 * It is now possible to create instances of these events.
			 * For `Event1`, only the attribute `string`
			 * is accessible. For `Event2`, the two attributes
			 * are accessible. Indeed, the type `Event2` inherits
			 * the fields of `Event1`.
			 */
			describe "Extending Events"{

				/*
				 * @filter(.* = '''|'''|.parseSuccessfully.*) 
				 */				
				fact "Declaration" {
					val model = '''
					event Event1 {
						var string : String
					}
					event Event2 extends Event1 {
						var number : int
					}
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.er",
						// TEXT
						""
					)

					model => [
						it should havePackage "io.sarl.docs.reference.er"
						it should haveNbImports 0
						it should haveNbElements 2
					]
					
					model.elements.get(0) => [
						it should beEvent "Event1"
						it should extend _
						it should haveNbElements 1
						(it as Event).features.get(0) => [
							it should beVariable "string"
							it should haveType "java.lang.String"
							it should haveInitialValue _
						]
					]
					
					model.elements.get(1) => [
						it should beEvent "Event2"
						it should extend "io.sarl.docs.reference.er.Event1"
						it should haveNbElements 1
						(it as Event).features.get(0) => [
							it should beVariable "number"
							it should haveType "int"
							it should haveInitialValue _
						]
					]
				}

				/*
				 * @filter(.* = '''|'''|.parseSuccessfully.*) 
				 */				
				fact "Use" {
					val model = '''
							// Create an instance of Event1 and set its attribute.
							var e1 = new Event1
							e1.string = "abc"
							// Create an instance of Event2 and set its attributes.
							var e2 = new Event2
							e2.string = "abc"
							e2.number = 345
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.er
						event Event1 {
							var string : String
						}
						event Event2 extends Event1 {
							var number : int
						}
						agent A {
							def example {",
						// TEXT
						"} }"
					)
					
					model => [
						it should havePackage "io.sarl.docs.reference.er"
						it should haveNbImports 0
						it should haveNbElements 3
					]
					
					model.elements.get(0) => [
						it should beEvent "Event1"
						it should extend _
						it should haveNbElements 1
						(it as Event).features.get(0) => [
							it should beVariable "string"
							it should haveType "java.lang.String"
							it should haveInitialValue _
						]
					]
					
					model.elements.get(1) => [
						it should beEvent "Event2"
						it should extend "io.sarl.docs.reference.er.Event1"
						it should haveNbElements 1
						(it as Event).features.get(0) => [
							it should beVariable "number"
							it should haveType "int"
							it should haveInitialValue _
						]
					]

					model.elements.get(2) => [
						it should beAgent "A"
						it should extend _
						it should haveNbElements 1
						(it as Agent).features.get(0) => [
							it should beAction "example"
							it should reply _
							it should haveNbParameters 0
							it should beVariadic false
						]
					]
				}

			}

		}
		
		/* Several events are defined and reserved by the SARL Core
		 * Specification.
		 * They are composing the minimal set of events that a runtime
		 * environment must support for running a SARL program.
		 *
		 * <veryimportantnote> You must not
		 * define an event with a fully qualified name equals to one
		 * of the reserved events.</veryimportantnote>
		 * 
		 * Two types of reserved events exist:
		 * 
		 *  * the events reserved in the SARL Core Specification for the [agent's life cycle](AgentReferenceSpec.html#Behaviors_of_an_Agent); and
		 *  * the events supported by the [Built-in Capacities](BuiltInCapacityReferenceSpec.html).
		 * 
		 * @filter(.*)
		 */
		fact "Reserved Events"{
			"AgentReferenceSpec.html#Behaviors_of_an_Agent" should beAccessibleFrom this
			"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
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
