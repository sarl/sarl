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
package io.sarl.docs.reference

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.jnario.runner.CreateWith

/**
 * This document describes how to define events in SARL.
 * Before reading this document, it is recommended to read
 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html).
 * 
 * An event is one of the core concepts in SARL.
 * It is a data structure composed of a set of attributes.
 * Each attribute has a name, a type, and a value.
 * 
 * Events are exchanged among the agents or the behavioral units of an agent,
 * inside a given [Space](./SpaceReferenceSpec.html).
 * 
 * Each event has:<ul>
 * <li>a type, i.e. its qualified name;</li>
 * <li>a source, the identifier of the emitter of the event;</li>
 * <li>a collection of name-value pairs, i.e. the attributes of the event.</li>
 * </ul>
 */
@CreateWith(SARLSpecCreator)
describe "Event Reference"{

		@Inject extension SARLParser
		@Inject extension IQualifiedNameProvider

		/* In literature, there are two major concepts for defining the
		 * data structures that are exchanged by entities:
		 * the event, and the message.
		 * 
		 * It is mandatory to specify the type, the source 
		 * and the potential embedded data for both of them.
		 * The difference is related to the specification of
		 * the receiver. In one hand, the event does not
		 * force the emitter to specify the receiver identifier.
		 * In the other hand, the message needs to have at least
		 * on receiver identifer.
		 * 
		 * Because the event permits to send data without being
		 * care of the emitter, this concept is privilegied
		 * by the designers of SARL.
		 * 
		 * Consequently, for sending a message to another entity,
		 * you must create the instance of an event, and emit
		 * this object in a [Space](./SpaceReferenceSpec.html), which is restricted to
		 * the desired receivers of the message.
		 * The emission API is detailed in the [Builtin Capacity
		 * Reference](BuiltinCapacityReferenceSpec.html).
		 * 
		 * 
		 * <span class="label label-warning">Important</span> There is 
		 * not message concept in SARL. All the communications are
		 * supported by the concept of <code>Event</code>
		 */
		describe "Event vs. Message"{
		}
		
		describe "Defining an Event" {
			
			/* An event can be defined with the <code>event</code> keyword.
			 * It must be followed by the name of the event without the
			 * qualified name of its package, which is inferred from the
			 * <code>package</code> keyword if present.
			 * 
			 * When the event is empty, i.e. it does not contains any additional
			 * data that the source of the event, it is specified by an empty
			 * block, or by do not write any block, after the event's declaration.
			 * The example below contains the definition of the events
			 * <code>Event1</code> and <code>Event2</code>, whic hare both empty.
			 * The first event is defined with the "empty block" syntax.
			 * The second event is defined with the other syntax.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Define an empty event"{
				val model = '''
				package io.sarl.docs.reference.er
				event Event1 {
				}
				event Event2
				'''.parsesSuccessfully
				model.mustHavePackage("io.sarl.docs.reference.er")
				model.mustNotHaveImport
				model.mustHaveTopElements(2)
				model.elements.get(0).mustBeEvent("Event1", null).mustHaveFeatures(0)
				model.elements.get(1).mustBeEvent("Event2", null).mustHaveFeatures(0)
			}
		
			/**
			 * Events in SARL can carry information.
			 * This information is described by a set of attributes.
			 * Each attribute is declared according to the "Field Declaration"
			 * of the General Syntax Reference.
			 * 
			 * The following code example is defining the event <code>MyEvent</code> with
			 * three attributes.
			 * Each declaration of the attributes illustrates one possible syntax for
			 * defining a field.<ul>
			 * <li>declaration with explicit typing: <code>var number : Integer</code></li>
			 * <li>declaration with type inference: <code>var string = "abc"</code></li>
			 * <li>declaration with free infered element: <code>var something</code></li>
			 * </ul>
			 * According to the type inference mechanism used by SARL, the attribute
			 * <code>something</code> will have the type <code>Object</code>.
			 * 
			 * <span class="label label-warning">Important</span> Because of the use of 
			 * the <code>var</code> keyword, the values of the attributes can be modified.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Define an event with attributes"{
				val model = '''
				package io.sarl.docs.reference.er
				event MyEvent {
					var number : Integer
					var string = "abc"
					var something
				}
				'''.parsesSuccessfully
				model.mustHavePackage("io.sarl.docs.reference.er")
				model.mustNotHaveImport
				model.mustHaveTopElements(1)
				var e = model.elements.get(0).mustBeEvent("MyEvent", null).mustHaveFeatures(3)
				e.features.get(0).mustBeAttribute(true, "number", "java.lang.Integer", false)
				e.features.get(1).mustBeAttribute(true, "string", null, true)
				e.features.get(2).mustBeAttribute(true, "something", null, false)
			}
			
			/**
			 * Events in SARL can carry information that is unmodifiable using the
			 * <code>val</code> keyword.
			 * 
			 * <span class="label label-warning">Important</span> The <code>val</code>
			 * keyword has the same semantic as the <code>final</code> modifier in
			 * the Java language. It means that an element defined with <code>val</code>
			 * can be initialized only once. It also means that the element is read-only.
			 * But if the element is a reference to an object, then this object is
			 * not read-only (only the initial reference is). 
			 * 
			 * Because the <code>val</code> keyword defines a single-initialization
			 * variable, it is mandatory to specify the intialization value.
			 * This value could be specified at the end of the <code>val</code>
			 * directive, or by specifying a constructor.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Define an event with value attributes"{
				val model = '''
				package io.sarl.docs.reference.er
				import io.sarl.lang.core.Agent
				event MyEvent {
					val string = "abcd"
					val number : Integer
					
					new(nb : Integer) {
						number = nb
					}
				}
				'''.parsesSuccessfully
				model.mustHavePackage("io.sarl.docs.reference.er")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.lang.core.Agent", false, false, false)
				model.mustHaveTopElements(1)
				var e = model.elements.get(0).mustBeEvent("MyEvent", null).mustHaveFeatures(3)
				e.features.get(0).mustBeAttribute(false, "string", null, true)
				e.features.get(1).mustBeAttribute(false, "number", "java.lang.Integer", false)
				e.features.get(2).mustBeConstructor(1, false).mustHaveParameter(0, "nb", "java.lang.Integer", false)
			}
			
			/* In some use cases, it is useful to specialize the definition
			 * of an event. This mechanism is supported by the inheritance
			 * feature of SARL, which has the same semantic as the inheritance
			 * mechanism as the Java object-oriented languages.
			 * 
			 * The extended event is specified just after the <code>extends</code>
			 * keyword.
			 * 
			 * <span class="label label-warning">Important</span> An event can
			 * extend only one other event type (same constrain as in the Java
			 * language).
			 * 
			 * In the following code, a first event is defined with the name
			 * <code>Event1</code> and an attribute named <code>string</code>.
			 * A second event <code>Event2</code> is defined as the extension
			 * of the first event. It contains a new attribute named
			 * <code>number</code>.
			 * It is now possible to create instances of these events.
			 * For <code>Event1</code>, only the attribute <code>string</code>
			 * is accessible. For <code>Event2</code>, the two attributes
			 * are accessible, because the type <code>Event2</code> inherits
			 * the fields of <code>Event1</code>.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Extending Events"{
				val model = '''
				package io.sarl.docs.reference.er
				event Event1 {
					var string : String
				}
				event Event2 extends Event1 {
					var number : int
				}
				agent A {
					def example {
						// Create an instance of Event1 and set its attribute.
						var e1 = new Event1
						e1.string = "abc"
						// Create an instance of Event2 and set its attributes.
						var e2 = new Event2
						e2.string = "abc"
						e2.number = 345
					}
				}
				'''.parsesSuccessfully
				model.mustHavePackage("io.sarl.docs.reference.er")
				model.mustNotHaveImport
				model.mustHaveTopElements(3)
				var e1 = model.elements.get(0).mustBeEvent("Event1", null).mustHaveFeatures(1)
				e1.features.get(0).mustBeAttribute(true, "string", "java.lang.String", false)
				var e2 = model.elements.get(1).mustBeEvent("Event2", "io.sarl.docs.reference.er.Event1").mustHaveFeatures(1)
				e2.features.get(0).mustBeAttribute(true, "number", "int", false)
				var a = model.elements.get(2).mustBeAgent("A", null).mustHaveFeatures(1)
				a.features.get(0).mustBeAction("example", null, 0, false)
			}

		}
		
		/* Several events are defined and reserved by the SARL Core
		 * Specification.
		 * They are composing the minimal set of events that a runtime
		 * environment must support for running a SARL program.
		 *
		 * <span class="label label-warning">Important</span> You must not
		 * define an event with a fully qualified name equals to one
		 * of the reserved events.
		 * 
		 * Two types of reserved events exist:<ul>
		 * <li>the events reserved in the SARL Core Specification for the [agent's lifecycle](./AgentReferenceSpec.html#AgentLifeCycle); and </li>
		 * <li>the events supported by the [Builtin Capacities](./BuiltinCapacityReferenceSpec.html).</li>
		 * </ul>
		 */
		describe "Reserved Events"{
		}

}
