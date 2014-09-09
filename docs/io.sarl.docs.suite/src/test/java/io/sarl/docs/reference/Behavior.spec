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

import static extension io.sarl.docs.utils.SpecificationTools.*
import io.sarl.lang.sarl.Behavior
import io.sarl.lang.sarl.Action
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.Capacity

/* @outline
 *
 * This document describes the features related to the definition of a behavior in SARL.
 * Before reading this document, it is recommended reading
 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
 * and the [Agent Reference](AgentReferenceSpec.html).
 * 
 * A `Behavior` is the specification of a collection of behavior units.
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
			 * Basically, this behavior does nothing, and does not react
			 * on events. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Defining an empty behavior"{
				// Tests the URLs from the beginning of the page
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				"AgentReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.br",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 0
					it should haveNbElements 1
				]
				
				model.elements.get(0) => [
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
			 * According to the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
			 * the attributes may be modifiable (when declared with the `var`
			 * keyword), or unmodifiable (when declared with the `val`
			 * keyword).
			 *
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.br",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 0
					it should haveNbElements 1
				]
				
				var b = (model.elements.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
				]) as Behavior
				
				b.features.get(0) => [
					it should beVariable "mentalStateElement1"
					it should haveType "java.lang.String"
					it should haveInitialValue _
				]

				b.features.get(1) => [
					it should beValue "mentalStateElement2"
					it should haveType "boolean"
					it should haveInitialValue "true"
				]
			}

			/* It is allowed to define actions (methods) in the behavior.
			 * The syntax described in the [General Syntax Reference](GeneralSyntaxReferenceSpec.html)
			 * is used.
			 * 
			 * The example below illustrates the creation of type actions.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Behavior Actions"{
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
					// Defining an action without parameter nor return type
					def myAction1 {
						println("Hello world")
					}
					// Defining an action with a variadic parameter and no return type
					def myAction2(param : int*) {
						println("params are " + param)
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.br",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 0
					it should haveNbElements 1
				]
				
				var b = (model.elements.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
				]) as Behavior
				
				b.features.get(0) => [
					it should beAction "myAction1"
					it should reply _
					it should haveNbParameters 0
					it should beVariadic false
				]

				b.features.get(1) => [
					it should beAction "myAction2"
					it should reply _
					it should haveNbParameters 1
					it should beVariadic true
					(it as Action).signature.params.get(0) => [
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
			 * The extended behavior is specified just after the `extends`
			 * keyword.
			 * 
			 * <veryimportantnote> A behavior type
			 * can extend __only one__ other behavior type. This is close
			 * to the constraint on the extension of classes in the Java
			 * language.</veryimportantnote>
			 * 
			 * In the following code, a first behavior is defined with the name
			 * `MyBehavior` and an attribute named `attr`.
			 * A second behavior `MySubBehavior` is defined as the extension
			 * of the first behavior. It contains a function named
			 * `action`, which is displaying the inherited attribute.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Extending a Behavior" {
				val model = '''
				behavior MyBehavior {
					var attr : String
				}
				behavior MySubBehavior extends MyBehavior {
					def action {
						println(attr)
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.br",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 0
					it should haveNbElements 2
				]
				
				model.elements.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 1
					(it as Behavior).features.get(0) => [
						it should beVariable "attr"
						it should haveType "java.lang.String"
						it should haveInitialValue _
					]
				]
				
				model.elements.get(1) => [
					it should beBehavior "MySubBehavior"
					it should extend "io.sarl.docs.reference.br.MyBehavior"
					it should haveNbElements 1
					(it as Behavior).features.get(0) => [
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
			 * In the following example, a behavior of type `MyBehavior` is
			 * instanced (with the agent as the owner/parameter).
			 * This new behavior is then registered into the agent for enabling 
			 * the reception of the events in the behavior.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
				'''.parsesSuccessfully(
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
				
				model.elements.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 0
				]
				
				model.elements.get(1) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 2
					(it as Agent).features.get(0) should beCapacityUse #["io.sarl.core.Behaviors"]
					(it as Agent).features.get(1) => [
						it should beBehaviorUnit "io.sarl.core.Initialize"
						it should beGuardedWith _
					]
				]
			}

		}
		
		/* The behaviors of an agent correspond to the units that are
		 * executed by the agent for exhibiting its general behavior.
		 * 
		 * The `Behavior` statement permits specifying a
		 * subset of the agent's behavior inside a single syntactic entity.
		 * Two types of behaviors are considered:
		 * 
		 *  * reactive: the agent react when it is receiving events, and
		 *  * pro-active: the agent executes by itself one of its behaviors.
		 * 
		 * 
		 * The definition of the reactive behaviors is based on the event handling
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
		 * `<EventName>` is the name of event to wait for.
		 * `<Guard>` is the optional specification of a predicate
		 * that may be true for executing the `<Statements>`.
		 * The statements are executed only if an event with the given name is
		 * received, *and* if the guard is true.
		 * 
		 * In the guard and the statements, it is possible to use the instance
		 * of the received event: the occurrence. This instance is represented
		 * by the `occurrence` keyword. It is an implicit
		 * variable as the keywords `this` and `it`.
		 */
		describe "Behavior Units of a Behavior" {

			/* The reactive behavior is specified with a collection
			 * of event handlers. The principle of a reactive behavior
			 * is to execute a part of the behavior when something has happening
			 * in the behavior, the agent or in its environment.
			 * 
			 * In the following example, the behavior is reacting to the reception
			 * of the `SomethingChanged` event.
			 * 
			 * As for all the event handlers, it could be guarded by a predicate.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Reactive Behavior Units"{
				// Test the URLs in the introduction of this section
				"SpaceReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
					on SomethingChanged {
						println("Reactive behavior")
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.br
					event SomethingChanged",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 0
					it should haveNbElements 2
				]
				
				model.elements.get(0) => [
					it should beEvent "SomethingChanged"
					it should extend _
					it should haveNbElements 0
				]
				
				model.elements.get(1) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 1
					(it as Behavior).features.get(0) => [
						it should beBehaviorUnit "io.sarl.docs.reference.br.SomethingChanged"
						it should beGuardedWith _
					]
				]
			}

			/* When an event is received and the guard of the corresponding
			 * handler is true, the event handler is said to be triggered.
			 * 
			 * When multiple event handlers are triggered at the same time,
			 * they are all executed in parallel.
			 * In the following example, the two handlers for the
			 * `SomethingChanged` event are executed in parallel.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Parallel Execution of the Reactive Behavior Units"{
				val model = '''
				behavior MyBehavior {
					on SomethingChanged {
						println("First reactive behavior")
					}
					on SomethingChanged {
						println("Second reactive behavior")
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.br
					event SomethingChanged",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 0
					it should haveNbElements 2
				]
				
				model.elements.get(0) => [
					it should beEvent "SomethingChanged"
					it should extend _
					it should haveNbElements 0
				]
				
				model.elements.get(1) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
					(it as Behavior).features.get(0) => [
						it should beBehaviorUnit "io.sarl.docs.reference.br.SomethingChanged"
						it should beGuardedWith _
					]
					(it as Behavior).features.get(1) => [
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
			 * In SARL, a proactive behavior is a behavior that is scheduled
			 * by the agent or one of its behaviors. The schedule mechanism is provided by the 
			 * [`Schedules` built-in capacity](BuiltInCapacityReferenceSpec.html).
			 * In the following example, the agent execute its proactive behavior
			 * every second.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Pro-active Behavior Units"{
				"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				behavior MyBehavior {
					uses Schedules
					on Initialize {
						every(1000) [
							println("Run a pro-active behavior")
						]
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.br
					import io.sarl.core.Initialize
					import io.sarl.core.Schedules",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.br"
					it should haveNbImports 2
					it should importClass "io.sarl.core.Initialize"
					it should importClass "io.sarl.core.Schedules"
					it should haveNbElements 1
				]
				
				model.elements.get(0) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
					(it as Behavior).features.get(0) should beCapacityUse #["io.sarl.core.Schedules"]
					(it as Behavior).features.get(1) => [
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
		 * Consequently, a behavior associated to an agent is able to use the skills of this
		 * agent.
		 */		
		describe "Capacities and Skills" {

			/* The definition of a capacity or a skill is out of the scope
			 * of this reference document. For details, please read
			 * the [Capacity Reference](CapacityReferenceSpec.html), and
			 * the [Skill Reference](SkillReferenceSpec.html).
			 * 
			 * In the rest of this section, it is assumed that the following
			 * capacity and skill are defined:
			 * 
			 *     capacity Cap {
			 *         def action
			 *     }
			 *     skill Ski implements Cap {
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
			 * It is not possible for a behavior to assign a skill to the agent.
			 * 
			 * @filter(.*) 
			 */
			fact "Giving a Skill to the Associated Agent" {
				'''
				package io.sarl.docs.reference.br
				import io.sarl.lang.core.Agent
				capacity Cap {
					def action
				}
				skill Ski implements Cap {
					def action { println(\"Action\") }
				}
				behavior MyBehavior {
					new (owner : Agent) {
						super(owner)
						var theSkill = new Ski
						setSkill( Cap, theSkill )
					}
				}
				'''.parsesWithError
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
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
				'''.parsesSuccessfully(
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
				
				model.elements.get(0) => [
					it should beCapacity "Cap"
					it should extend _
					it should haveNbElements 1
					(it as Capacity).features.get(0) => [
						it should beActionSignature "action"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]

				model.elements.get(1) => [
					it should beEvent "SomeEvent"
					it should extend _
					it should haveNbElements 0
				]

				model.elements.get(2) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 1
					(it as Behavior).features.get(0) => [
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
			 * But, instead of using an `import` directive,
			 * the `uses` keyword is provided for importing the
			 * capacities into the agent. In the following example,
			 * the `Cap` capacity is imported.
			 * 
			 * After a capacity was "imported", it is possible to
			 * directly call the functions of the capacity
			 * (according to the extension method syntax).
			 * In the following example, the action
			 * with the name `action` is invoked.
			 * This action is defined in the `Cap`
			 * capacity. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
				'''.parsesSuccessfully(
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
				
				model.elements.get(0) => [
					it should beCapacity "Cap"
					it should extend _
					it should haveNbElements 1
					(it as Capacity).features.get(0) => [
						it should beActionSignature "action"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]

				model.elements.get(1) => [
					it should beEvent "SomeEvent"
					it should extend _
					it should haveNbElements 0
				]

				model.elements.get(2) => [
					it should beBehavior "MyBehavior"
					it should extend _
					it should haveNbElements 2
					(it as Behavior).features.get(0) should beCapacityUse #["io.sarl.docs.reference.br.Cap"]
					(it as Behavior).features.get(1) => [
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
	 * Copyright &copy; %copyrightdate% %copyrighters%. All rights reserved.
	 * 
	 * Licensed under the Apache License, Version 2.0;
	 * you may not use this file except in compliance with the License.
	 * You may obtain a copy of the [License](http://www.apache.org/licenses/LICENSE-2.0).
	 *
	 * @filter(.*) 
	 */
	fact "Legal Notice" {
		"%sarlversion%" should startWith "%sarlspecversion%"
		("%sarlspecreleasestatus%" == "Final Release"
			|| "%sarlspecreleasestatus%" == "Draft Release") should be true
		"%sarlspecreleasedate%" should beDate "YYYY-mm-dd"
		"%copyrightdate%" should beNumber "0000";
		("%copyrighters%".empty || "%copyrighters%".startsWith("%")) should be false
	}

}
