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
import io.sarl.lang.sarl.SarlAgent
import io.sarl.lang.sarl.SarlBehavior
import io.sarl.lang.sarl.SarlCapacity
import org.eclipse.xtext.common.types.JvmVisibility
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

/* @outline
 *
 * <p>This document describes the features related to the definition of a behavior in SARL.
 * Before reading this document, we recommend that you read
 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
 * and the [Agent Reference](AgentReferenceSpec.html).
 * 
 * <p>A `Behavior` is the specification of a collection of behavior units.
 * This `Behavior` may be used by an agent for building its global
 * behavior.
 */
@CreateWith(SARLSpecCreator)
describe "Behavior Reference" {
	
		@Inject extension SARLParser
		
		/* A behavior is declared with the `behavior` keyword.
		 * In the behavior's body block, we can declare Mental States 
		 * (in the form of attributes), Actions and Behaviors.
		 */
		describe "Defining a Behavior" {

			/* The following code illustrates the definition of a behavior
			 * named `MyBehavior`, and that is empty.
			 * 
			 * <p>Basically, this behavior does nothing, and does not react
			 * on events. 
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Defining an empty behavior"{
				// Tests the URLs from the beginning of the page
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				"AgentReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.br",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 0
					it should haveNbElements 1
				]
				
				model.xtendTypes.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 0
				]
			}
			
			/* The mental state of an agent is composed by the data
			 * in the knowledge of the agent. A behavior may contain
			 * a part of this mental state.
			 * Most of the time, it is implemented as a
			 * collection of attributes.
			 * 
			 * <p>According to the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
			 * the attributes may be modifiable (when declared with the `var`
			 * keyword), or unmodifiable (when declared with the `val`
			 * keyword).
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Behavior Attributes"{
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
					// Defining a modifiable element of the mental state
					var mentalStateElement1 : String
					// Defining an unmodifiable element of the mental state
					val mentalStateElement2 : boolean = true
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.br",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 0
					it should haveNbElements 1
				]
				
				var b = (model.xtendTypes.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
				]) as SarlBehavior
				
				b.members.get(0) => [
					it should beVariable "mentalStateElement1"
					it should haveType "java.lang.String"
					it should haveInitialValue _
				]

				b.members.get(1) => [
					it should beValue "mentalStateElement2"
					it should haveType "boolean"
					it should haveInitialValue "true"
				]
			}

			/* It is allowed to define actions (methods) in the behavior.
			 * The syntax described in the [General Syntax Reference](GeneralSyntaxReferenceSpec.html)
			 * is used.
			 * 
			 * <p>The example below illustrates the creation of type actions.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Behavior Actions"{
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
					uses Logging
					// Defining an action without parameter nor return type
					def myAction1 {
						println("Hello world")
					}
					// Defining an action with a variadic parameter and no return type
					def myAction2(param : int*) {
						println("params are " + param)
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.br
					import io.sarl.core.Logging",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 1
					it should importClass "io.sarl.core.Logging"
					it should haveNbElements 1
				]
				
				var b = (model.xtendTypes.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 3
				]) as SarlBehavior
				
				b.members.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]

				b.members.get(1) => [
					it should beAction "myAction1"
					it should reply _
					it should haveNbParameters 0
					it should beVariadic false
				]

				b.members.get(2) => [
					it should beAction "myAction2"
					it should reply _
					it should haveNbParameters 1
					it should beVariadic true
					(it as SarlAction).parameters.get(0) => [
						it should beParameter "param"
						it should haveType "int"
						it should haveDefaultValue _
					]
				]
			}
	
			/* In some use cases, it is useful to specialize the definition
			 * of a behavior. This mechanism is supported by the inheritance
			 * feature of SARL, which has the same semantic as the inheritance
			 * mechanism as the Java object-oriented language.
			 * 
			 * <p>The extended behavior is specified just after the `extends`
			 * keyword.
			 * 
			 * <veryimportantnote> A behavior type
			 * can extend __only one__ other behavior type. This is close
			 * to the constraint on the extension of classes in the Java
			 * language.</veryimportantnote>
			 * 
			 * <p>In the following code, a first behavior is defined with the name
			 * `MyBehavior` and an attribute named `attr`.
			 * A second behavior `MySubBehavior` is defined as the extension
			 * of the first behavior. It contains a function named
			 * `action`, which is displaying the inherited attribute.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Extending a Behavior" {
				val model = '''
				behavior MyBehavior {
					protected var attr : String
				}
				behavior MySubBehavior extends MyBehavior {
					uses Logging
					def action {
						println(attr)
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.br
					import io.sarl.core.Logging",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 1
					it should importClass "io.sarl.core.Logging"
					it should haveNbElements 2
				]
				
				model.xtendTypes.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 1
					(it as SarlBehavior).members.get(0) => [
						it should beVariable "attr"
						it should haveType "java.lang.String"
						it should haveInitialValue _
					]
				]
				
				model.xtendTypes.get(1) => [
					it should beBehavior "MySubBehavior"
					it should extend "io.sarl.docs.reference.br.MyBehavior"
					it should haveNbElements 2
					(it as SarlBehavior).members.get(0) => [
						it should beCapacityUse "io.sarl.core.Logging"
					]
					(it as SarlBehavior).members.get(1) => [
						it should beAction "action"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]
			}

			/* A behavior is always owned by an agent.
			 * Consequently, it is mandatory to pass the agent as parameter
			 * of the behavior's constructor.
			 * 
			 * <p>In the following example, a behavior of type `MyBehavior` is
			 * instanced (with the agent as the owner/parameter).
			 * This new behavior is then registered into the agent for enabling 
			 * the reception of the events in the behavior.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Instancing and Use of a Behavior" {
				val model = '''
				agent MyAgent {
					uses Behaviors
					on Initialize {
						// Create the instance of the behavior
						var beh = new MyBehavior(this) // <- the parameter is the agent
						
						// Register the behavior for receiving the events.
						// This function is given by the Behaviors capacity
						registerBehavior(beh)
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.br
					import io.sarl.core.Initialize
					import io.sarl.core.Behaviors
					behavior MyBehavior { }",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 2
					it should importClass "io.sarl.core.Initialize"
					it should importClass "io.sarl.core.Behaviors"
					it should haveNbElements 2
				]
				
				model.xtendTypes.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 0
				]
				
				model.xtendTypes.get(1) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 2
					(it as SarlAgent).members.get(0) should beCapacityUse #["io.sarl.core.Behaviors"]
					(it as SarlAgent).members.get(1) => [
						it should beBehaviorUnit "io.sarl.core.Initialize"
						it should beGuardedWith _
					]
				]
			}

			/** Modifiers are used to modify declarations of types and type members.
			 * This section introduces the modifiers for the behavior.
			 * The modifiers are usually written before the keyword for defining the behavior.
			 * 
			 * <p>The complete description of the modifiers' semantic is available in
			 * <a href="./BasicObjectOrientedProgrammingSupportModifiersSpec.html">this section</a>.
			 */
			describe "Modifiers" {
				
				/** A behavior may be declared with one or more modifiers, which affect its runtime behavior: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`public`:  the behavior is accessible from any other type (default);</li>
				 *     <li>`package`: the behavior is accessible from only the types in the same package.</li>
				 *     </ul></li>
				 * <li>`abstract`: the behavior is abstract and cannot be instanced.</li>
				 * <li>`final`: avoid to be derived.</li>
				 * </ul>
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Behavior Modifiers" {
					'''
						public behavior Example1 {
						}
						package behavior Example2 {
						}
						abstract behavior Example3 {
						}
						final behavior Example4 {
						}
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.br",
						// TEXT
						""
					)
					// Test URL in the enclosing section text.
					"./BasicObjectOrientedProgrammingSupportModifiersSpec.html" should beAccessibleFrom this
					// Test default visibility
					var visib = "behavior B1 {}".parse.xtendTypes.get(0)
					visib should beVisibleWith JvmVisibility::PUBLIC
				}
	
				/** The modifiers for the fields in a behavior are: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`protected`:  the field is accessible within the same package, and derived agents;</li>
				 *     <li>`package`: the field is accessible only within the same package of its agent;</li>
				 *     <li>`private`: the field is accessible only within its agent (default).</li>
				 *     </ul></li>
				 * </ul>
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Field Modifiers" {
					'''
						protected var example1 : Object;
						package var example2 : Object;
						private var example3 : Object;
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.br
						public behavior Behavior1 {",
						// TEXT
						"}"
					)
					// Test default visibility
					var visib = "behavior B1 {var field : int}".parse.xtendTypes.get(0).members.get(0)
					visib should beVisibleWith JvmVisibility::PRIVATE
				}
	
				/** The modifiers for the methods in a behavior are: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`public`:  the method is accessible from everywhere;</li>
				 *     <li>`protected`:  the method is accessible within the same package, and derived classes (default);</li>
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
						public def example0 { }
						// Protected access function
						protected def example1 { }
						// Package access function
						package def example2 { }
						// Private access function
						private def example3 { }
						// Abstract function
						abstract def example4
						// Not-overridable function
						final def example5 { }
						// Dispatch functions
						dispatch def example7(p : Integer) { }
						dispatch def example7(p : Float) { }
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.br
						abstract behavior Behavior1 {",
						// TEXT
						"}"
					)
					// Test default visibility
					var visib = "behavior B1 {def fct {}}".parse.xtendTypes.get(0).members.get(0)
					visib should beVisibleWith JvmVisibility::PROTECTED
				}
	
				/** All the <a href="./BasicObjectOrientedProgrammingSupportSpec.html">modifiers for the
				 * nested types</a> are allowed <strong>except</strong> `public`. 
				 *
				 * @filter(.*)
				 */
				fact "Restriction on the Nested Type Modifiers" {
					'''
						package io.sarl.docs.reference.br
						behavior Behavior1 {
							public class NedtedClass1 { }
						}
					'''.parseWithError
				}
	
			}

		}
		
		/* The behaviors of an agent correspond to the units that are
		 * executed by the agent for exhibiting its general behavior.
		 * 
		 * <p>The `Behavior` statement permits specifying a
		 * subset of the agent's behavior inside a single syntactic entity.
		 * Two types of behaviors are considered:
		 * 
		 *  * reactive: the agent react when it is receiving events, and
		 *  * pro-active: the agent executes by itself one of its behaviors.
		 * 
		 * 
		 * <p>The definition of the reactive behaviors is based on the event handling
		 * mechanism of SARL. Events may be emitted in [spaces](SpaceReferenceSpec.html),
		 * and received by the agents, and their behaviors, belonging to these spaces.
		 * A behavior may indicate that it is interesting for receiving an event by specifying
		 * an event handler using the following syntax:
		 * 
		 *     on <EventName> [<Guard>] {
		 *         <Statements>
		 *     }
		 *
		 * 
		 * <p>`<EventName>` is the name of event to wait for.
		 * `<Guard>` is the optional specification of a predicate
		 * that may be true for executing the `<Statements>`.
		 * The statements are executed only if an event with the given name is
		 * received, *and* if the guard is true.
		 * 
		 * <p>In the guard and the statements, it is possible to use the instance
		 * of the received event: the occurrence. This instance is represented
		 * by the `occurrence` keyword. It is an implicit
		 * variable as the keywords `this` and `it`.
		 */
		describe "Behavior Units of a Behavior" {

			/* When a behavior is ready to be executed by the runtime environment, usually when it
			 * is registered in its owning agent, it receives the `Initialize` event.
			 * This event is defined as:
			 * 
			 *     event Initialize {
			 *         var parameters : Object[]
			 *     }
			 *
			 * 
			 * <p>It contains the list of the parameters given that are never set for behaviors.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Initialization Handler"{
				// Test the URL in the introduction of the section
				"SpaceReferenceSpec.html" should beAccessibleFrom this
				// Test the URL in this section
				"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
					uses Logging
					on Initialize {
						println("I'm initializing my behavior")
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging
					import io.sarl.core.Initialize",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 2
					it should importClass "io.sarl.core.Logging"
					it should importClass "io.sarl.core.Initialize"
					it should haveNbElements 1
				]
				
				var a = (model.xtendTypes.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
				]) as SarlBehavior

				a.members.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				a.members.get(1) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith _
				]
			}

			/* Because `Initialize` is an event, the handler in
			 * the behavior could use a guard. This feature enables
			 * the developer to write different initialization blocks
			 * depending on the guards of the handlers.
			 * 
			 * <p>In the following example, the first event handler is
			 * executed when the `Initialize` event has
			 * no parameter. The second event handler is executed
			 * when the event has at least one parameter.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Guarded Initialization Handler"{
				val model = '''
				behavior MyBehavior {
					uses Logging
					on Initialize [ occurrence.parameters.empty ] {
						println("First initialization")
					}
					on Initialize [ ! occurrence.parameters.empty ] {
						println("First initialization")
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging
					import io.sarl.core.Initialize",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 2
					it should importClass "io.sarl.core.Logging"
					it should importClass "io.sarl.core.Initialize"
					it should haveNbElements 1
				]
				
				var a = (model.xtendTypes.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 3
				]) as SarlBehavior

				a.members.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				a.members.get(1) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith "occurrence.parameters.empty"
				]
				a.members.get(2) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith "! occurrence.parameters.empty"
				]
			}

			/* The counterpart of `Initialize` is the event
			 * `Destroy`.
			 * This event is defined as:
			 * 
			 *     event Destroy
			 *
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Destruction Handler" {
				val model = '''
				behavior MyBehavior {
					uses Logging
					on Destroy {
						println("Destroying the behavior")
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging
					import io.sarl.core.Destroy",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 2
					it should importClass "io.sarl.core.Logging"
					it should importClass "io.sarl.core.Destroy"
					it should haveNbElements 1
				]
				
				var a = (model.xtendTypes.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
				]) as SarlBehavior

				a.members.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				a.members.get(1) => [
					it should beBehaviorUnit "io.sarl.core.Destroy"
					it should beGuardedWith _
				]
			}
			/* As for `Initialize`, the handlers of
			 * the `Destroy` event could be guarded.
			 * 
			 * <p>In the following example, the first event handler is
			 * executed when the `Destroy` is received
			 * and there is resource stored in the corresponding
			 * field. The second event handler is executed
			 * when there is no resource.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Guarded Destruction Handler" {
				val model = '''
				behavior MyBehavior {
					uses Logging
					var resource : Object
					on Destroy [ resource !== null ] {
						println("Destroying the behavior when there is a resource")
					}
					on Destroy [ resource === null ] {
						println("Destroying the behavior when there is no resource")
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging
					import io.sarl.core.Destroy",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 2
					it should importClass "io.sarl.core.Logging"
					it should importClass "io.sarl.core.Destroy"
					it should haveNbElements 1
				]
				
				var a = (model.xtendTypes.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 4
				]) as SarlBehavior

				a.members.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				a.members.get(1) => [
					it should beVariable "resource"
					it should haveType "java.lang.Object"
					it should haveInitialValue _
				]
				a.members.get(2) => [
					it should beBehaviorUnit "io.sarl.core.Destroy"
					it should beGuardedWith "resource !== null"
				]
				a.members.get(3) => [
					it should beBehaviorUnit "io.sarl.core.Destroy"
					it should beGuardedWith "resource === null"
				]
			}

			/* The reactive behavior is specified with a collection
			 * of event handlers. The principle of a reactive behavior
			 * is to execute a part of the behavior when something has happening
			 * in the behavior, the agent or in its environment.
			 * 
			 * <p>In the following example, the behavior is reacting to the reception
			 * of the `SomethingChanged` event.
			 * 
			 * <p>As for all the event handlers, it could be guarded by a predicate.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Reactive Behavior Units"{
				// Test the URLs in the introduction of this section
				"SpaceReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
					uses Logging
					on SomethingChanged {
						println("Reactive behavior")
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.br
					import io.sarl.core.Logging
					event SomethingChanged",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 1
					it should importClass "io.sarl.core.Logging"
					it should haveNbElements 2
				]
				
				model.xtendTypes.get(0) => [
					it should beEvent "SomethingChanged"
					it should extend _
					it should haveNbElements 0
				]
				
				model.xtendTypes.get(1) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
					(it as SarlBehavior).members.get(0) => [
						it should beCapacityUse "io.sarl.core.Logging"
					]
					(it as SarlBehavior).members.get(1) => [
						it should beBehaviorUnit "io.sarl.docs.reference.br.SomethingChanged"
						it should beGuardedWith _
					]
				]
			}

			/* When an event is received and the guard of the corresponding
			 * handler is true, the event handler is said to be triggered.
			 * 
			 * <p>When multiple event handlers are triggered at the same time,
			 * they are all executed in parallel.
			 * In the following example, the two handlers for the
			 * `SomethingChanged` event are executed in parallel.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Parallel Execution of the Reactive Behavior Units"{
				val model = '''
				behavior MyBehavior {
					uses Logging
					on SomethingChanged {
						println("First reactive behavior")
					}
					on SomethingChanged {
						println("Second reactive behavior")
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.br
					import io.sarl.core.Logging
					event SomethingChanged",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 1
					it should importClass "io.sarl.core.Logging"
					it should haveNbElements 2
				]
				
				model.xtendTypes.get(0) => [
					it should beEvent "SomethingChanged"
					it should extend _
					it should haveNbElements 0
				]
				
				model.xtendTypes.get(1) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 3
					(it as SarlBehavior).members.get(0) => [
						it should beCapacityUse "io.sarl.core.Logging"
					]
					(it as SarlBehavior).members.get(1) => [
						it should beBehaviorUnit "io.sarl.docs.reference.br.SomethingChanged"
						it should beGuardedWith _
					]
					(it as SarlBehavior).members.get(2) => [
						it should beBehaviorUnit "io.sarl.docs.reference.br.SomethingChanged"
						it should beGuardedWith _
					]
				]
			}

			/* A proactive behavior is a part of the global behavior of an agent that the 
			 * agent is deciding to execute by itself.
			 * The execution of a reactive behavior is initiated by a part of
			 * the code external to this behavior. In opposite, the initiator
			 * of the execution of a proactive behavior is the agent itself.
			 * 
			 * <p>In SARL, a proactive behavior is a behavior that is scheduled
			 * by the agent or one of its behaviors. The schedule mechanism is provided by the 
			 * [`Schedules` built-in capacity](BuiltInCapacityReferenceSpec.html).
			 * In the following example, the agent execute its proactive behavior
			 * every second.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Pro-active Behavior Units"{
				"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
					uses Schedules, Logging
					on Initialize {
						every(1000) [
							println("Run a pro-active behavior")
						]
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.br
					import io.sarl.core.Logging
					import io.sarl.core.Initialize
					import io.sarl.core.Schedules",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 3
					it should importClass "io.sarl.core.Logging"
					it should importClass "io.sarl.core.Initialize"
					it should importClass "io.sarl.core.Schedules"
					it should haveNbElements 1
				]
				
				model.xtendTypes.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
					(it as SarlBehavior).members.get(0) should beCapacityUse #["io.sarl.core.Schedules", "io.sarl.core.Logging"]
					(it as SarlBehavior).members.get(1) => [
						it should beBehaviorUnit "io.sarl.core.Initialize"
						it should beGuardedWith _
					]
				]
			}

		}

		/* An agent is an autonomous entity having a set of skills to realize the
		 * capacities it exhibits.
		 * An agent has a set of built-in capacities considered essential to respect the
		 * commonly accepted competencies of agents, such autonomy, reactivity, pro-activity
		 * and social capacities. 
		 * 
		 * <p>Consequently, a behavior associated to an agent is able to use the skills of this
		 * agent.
		 */		
		describe "Capacities and Skills" {

			/* The definition of a capacity or a skill is out of the scope
			 * of this reference document. For details, please read
			 * the [Capacity Reference](CapacityReferenceSpec.html), and
			 * the [Skill Reference](SkillReferenceSpec.html).
			 * 
			 * <p>In the rest of this section, it is assumed that the following
			 * capacity and skill are defined:
			 * 
			 *     capacity Cap {
			 *         def action
			 *     }
			 *     skill Ski implements Cap {
			 *         uses Logging
			 *         def action {
			 *             println("Action")
			 *         }
			 *     }
			 * 
			 * 
			 * @filter(.*) 
			 */
			fact "Defining a Capacity and a Skill"{
				"CapacityReferenceSpec.html" should beAccessibleFrom this
				"SkillReferenceSpec.html" should beAccessibleFrom this
			}

			/* When a behavior must use a capacity,
			 * its agent must own an implementation of this capacity: a skill.
			 * 
			 * <p>It is not possible for a behavior to assign a skill to the agent.
			 * 
			 * @filter(.*) 
			 */
			fact "Giving a Skill to the Associated Agent" {
				'''
				package io.sarl.docs.reference.br
				import io.sarl.core.Logging
				import io.sarl.lang.core.Agent
				capacity Cap {
					def action
				}
				skill Ski implements Cap {
					uses Logging
					def action { println(\"Action\") }
				}
				behavior MyBehavior {
					new (owner : Agent) {
						super(owner)
						var theSkill = new Ski
						setSkill( Cap, theSkill )
					}
				}
				'''.parseWithError
			}
	
			/* For invoking a function implemented by a skill,
			 * the two following steps must be done:
			 * 
			 *  * Retrieve the skill instance: the function
			 *    `getSkill(Class<? extends Capacity>)`
			 *    permits retrieving the skill associated to the
			 *    given capacity;
			 *  * Invoke the capacity's action on the retrieved skill.
			 * 
			 * 
			 * <note> This method of
			 * invocation is not recommended by the SARL developers.
			 * You should prefer the use of the extension methods (see below).
			 * </note>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Using a Capacity with the Getters" {
				val model = '''
				behavior MyBehavior {
					on SomeEvent {
						// Retreive the capacity implementation
						var s = getSkill(Cap)
						// Run the action of the skill
						s.action
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.br
					capacity Cap {
						def action
					}
					event SomeEvent",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 0
					it should haveNbElements 3
				]
				
				model.xtendTypes.get(0) => [
					it should beCapacity "Cap"
					it should extend _
					it should haveNbElements 1
					(it as SarlCapacity).members.get(0) => [
						it should beActionSignature "action"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]

				model.xtendTypes.get(1) => [
					it should beEvent "SomeEvent"
					it should extend _
					it should haveNbElements 0
				]

				model.xtendTypes.get(2) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 1
					(it as SarlBehavior).members.get(0) => [
						it should beBehaviorUnit "io.sarl.docs.reference.br.SomeEvent"
						it should beGuardedWith _
					]
				]
			}
	
			/* Invoking a capacity/skill with the getter method is
			 * not user-friendly. Since the
			 * [General Syntax Reference](GeneralSyntaxReferenceSpec.html)
			 * describes the "extension method" mechanism, it is possible
			 * to use it for invoking the capacities.
			 * 
			 * <p>But, instead of using an `import` directive,
			 * the `uses` keyword is provided for importing the
			 * capacities into the agent. In the following example,
			 * the `Cap` capacity is imported.
			 * 
			 * <p>After a capacity was "imported", it is possible to
			 * directly call the functions of the capacity
			 * (according to the extension method syntax).
			 * In the following example, the action
			 * with the name `action` is invoked.
			 * This action is defined in the `Cap`
			 * capacity. 
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Using a Capacity with the Extension Methods" {
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
					uses Cap
					on SomeEvent {
						// Run the action of the skill
						action
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.br
					capacity Cap {
						def action
					}
					event SomeEvent",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 0
					it should haveNbElements 3
				]
				
				model.xtendTypes.get(0) => [
					it should beCapacity "Cap"
					it should extend _
					it should haveNbElements 1
					(it as SarlCapacity).members.get(0) => [
						it should beActionSignature "action"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]

				model.xtendTypes.get(1) => [
					it should beEvent "SomeEvent"
					it should extend _
					it should haveNbElements 0
				]

				model.xtendTypes.get(2) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
					(it as SarlBehavior).members.get(0) should beCapacityUse #["io.sarl.docs.reference.br.Cap"]
					(it as SarlBehavior).members.get(1) => [
						it should beBehaviorUnit "io.sarl.docs.reference.br.SomeEvent"
						it should beGuardedWith _
					]
				]
			}
		
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
