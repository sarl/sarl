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
 * This document describes the features related to the definition of a behavior in SARL.
 * Before reading this document, it is recommended to read
 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html),
 * and the [Agent Reference](./AgentReferenceSpec.html).
 * 
 * A <code>Behavior</code> is the specification of a collection of behavior units.
 * This <code>Behavior</code> may be used by an agent for building its global
 * behavior.
 */
@CreateWith(SARLSpecCreator)
describe "Behavior Reference" {
	
		@Inject extension SARLParser
		@Inject extension IQualifiedNameProvider
		
		/* A behavior is declared with the <code>behavior</code> keyword.
		 * In the behavior's body block, we can declare Mental States 
		 * (in the form of attributes), Actions and Behaviors.
		 */
		describe "Defining a Behavior" {

			/* The following code illustres the definition of a behavior
			 * named <code>MyBehavior</code>, and that is empty.
			 * 
			 * Basically, this behavior does nothing, and does not react
			 * on events. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Defining an empty behavior"{
				val model = '''
				behavior MyBehavior {
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.br",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.br")
				model.mustNotHaveImport
				model.mustHaveTopElements(1)
				model.elements.get(0).mustBeBehavior("MyBehavior", null).mustHaveFeatures(0)
			}
			
			/* The mental state of an agent is composed by all the data
			 * in the knowledge of the agent. A behavior may contain
			 * a part of this mental state.
			 * Most of the time, it is implemented as a
			 * collection of attributes.
			 * 
			 * According to the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
			 * the attributes may be modifiable (when declared with the <code>var</code>
			 * keyword), or unmodifiable (when declared with the <code>val</code>
			 * keyword).
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Behavior Attributes"{
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
				model.mustHavePackage("io.sarl.docs.reference.br")
				model.mustNotHaveImport
				model.mustHaveTopElements(1)
				var b = model.elements.get(0).mustBeBehavior("MyBehavior", null).mustHaveFeatures(2)
				b.features.get(0).mustBeAttribute(true, "mentalStateElement1", "java.lang.String", false)
				b.features.get(1).mustBeAttribute(false, "mentalStateElement2", "boolean", true)
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
				model.mustHavePackage("io.sarl.docs.reference.br")
				model.mustNotHaveImport
				model.mustHaveTopElements(1)
				var b = model.elements.get(0).mustBeBehavior("MyBehavior", null).mustHaveFeatures(2)
				b.features.get(0).mustBeAction("myAction1", null, 0, false)
				b.features.get(1).mustBeAction("myAction2", null, 1, true).mustHaveParameter(0, "param", "int", false)
			}
	
			/* In some use cases, it is useful to specialize the definition
			 * of a behavior. This mechanism is supported by the inheritance
			 * feature of SARL, which has the same semantic as the inheritance
			 * mechanism as the Java object-oriented language.
			 * 
			 * The extended event is specified just after the <code>extends</code>
			 * keyword.
			 * 
			 * <span class="label label-warning">Important</span> A behavior can
			 * extend only one other behavior type (same constrain as in the Java
			 * language).
			 * 
			 * In the following code, a first behavior is defined with the name
			 * <code>MyBehavior</code> and an attribute named <code>attr</code>.
			 * A second behavior <code>MySubBehavior</code> is defined as the extension
			 * of the first behavior. It contains a function named
			 * <code>action</code>, which is displaying the inherited attribute.
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
				model.mustHavePackage("io.sarl.docs.reference.br")
				model.mustNotHaveImport
				model.mustHaveTopElements(2)
				var b1 = model.elements.get(0).mustBeBehavior("MyBehavior", null).mustHaveFeatures(1)
				b1.features.get(0).mustBeAttribute(true, "attr", "java.lang.String", false)
				var b2 = model.elements.get(1).mustBeBehavior("MySubBehavior", "io.sarl.docs.reference.br.MyBehavior").mustHaveFeatures(1)
				b2.features.get(0).mustBeAction("action", null, 0, false)
			}

			/* A behavior is alway own by an agent.
			 * Consequently, it is mandatory to pass the agent as parameter
			 * of the behavior's constructor.
			 * 
			 * In the following example, a behavior of type <code>MyBehavior</code> is
			 * instanced (with the agent as the owner/parameter).
			 * This new behavior is then register into the agentfor enabling 
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
				model.mustHavePackage("io.sarl.docs.reference.br")
				model.mustHaveImports(2)
				model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
				model.mustHaveImport(1, "io.sarl.core.Behaviors", false, false, false)
				model.mustHaveTopElements(2)
				model.elements.get(0).mustBeBehavior("MyBehavior", null).mustHaveFeatures(0)
				var b = model.elements.get(1).mustBeAgent("MyAgent", null).mustHaveFeatures(2)
				b.features.get(0).mustBeCapacityUses("io.sarl.core.Behaviors")
				b.features.get(1).mustBeBehaviorUnit("io.sarl.core.Initialize", false)
			}

		}
		
		/* The behaviors of an agent correspond to the units that are
		 * executed by the agent for exhibiting its general behavior.
		 * 
		 * The <code>Behavior</code> statement permits to specify a
		 * subset of the agent's behavior inside a single syntactic entity.
		 * Two types of behaviors are considered: <ol>
		 * <li>reactive: the agent react when it is receiving events, and</li>
		 * <li>pro-active: the agent executes by itself one of its behaviors.</li></li>
		 * </ol>
		 * 
		 * The definition of the reactive behaviors is based on the event handling
		 * mechanism of SARL. Events may be emitted in [spaces](./SpaceReferenceSpec.html),
		 * and received by the agents, and their behaviors, belonging to these spaces.
		 * A behavior may indicate that it is interesting for receiving an event by specifying
		 * an event handler using the following syntax: <pre><code>
		 * on &lt;EventName&gt; [&lt;Guard&gt;] {
		 * 		&lt;Statements&gt;
		 * }
		 * </code></pre>
		 * <code>&lt;EventName&gt;</code> is the name of event to wait for.
		 * <code>&lt;Guard&gt;</code> is the optional specification of a predicate
		 * that may be true for executing the <code>&lt;Statements&gt;</code>.
		 * The statements are executed only if an event with the given name is
		 * received, *and* if the guard is true.
		 * 
		 * In the guard and the statements, it is possible to use the instance
		 * of the received event: the occurrence. This instance is represented
		 * by the <code>occurrence</code> keyword. It is an implicit
		 * variable as the keywords <code>this</code> and <code>it</code>.
		 */
		describe "Behavior Units of a Behavior" {

			/* The reactive behavior is specified with a collection
			 * of event handlers. The principle of a reactive behavior
			 * is to execute a part of the behavior when something has appening
			 * in the behavioe, the agent or in its environment.
			 * 
			 * In the following example, the behavior is reacting to the reception
			 * of the <code>SomethingChanged</code> event.
			 * 
			 * As for all the event handlers, it could be guarded by a predicate.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Reactive Behavior Units"{
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
				model.mustHavePackage("io.sarl.docs.reference.br")
				model.mustNotHaveImport
				model.mustHaveTopElements(2)
				model.elements.get(0).mustBeEvent("SomethingChanged", null).mustHaveFeatures(0)
				var b = model.elements.get(1).mustBeBehavior("MyBehavior", null).mustHaveFeatures(1)
				b.features.get(0).mustBeBehaviorUnit("io.sarl.docs.reference.br.SomethingChanged", false)
			}

			/* When an event is received and the guard of the corresponding
			 * handler is true, the event handler is said to be triggered.
			 * 
			 * When multiple event handlers are triggered at the same time,
			 * they are all executed in parallel.
			 * In the following example, the two handlers for the
			 * <code>SomethingChanged</code> event are executed in parallel.
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
				model.mustHavePackage("io.sarl.docs.reference.br")
				model.mustNotHaveImport
				model.mustHaveTopElements(2)
				model.elements.get(0).mustBeEvent("SomethingChanged", null).mustHaveFeatures(0)
				var b = model.elements.get(1).mustBeBehavior("MyBehavior", null).mustHaveFeatures(2)
				b.features.get(0).mustBeBehaviorUnit("io.sarl.docs.reference.br.SomethingChanged", false)
				b.features.get(1).mustBeBehaviorUnit("io.sarl.docs.reference.br.SomethingChanged", false)
			}

			/* A pro-active behavior is a part of the global behavior of an agent that the 
			 * agent is deciding to execute by itself.
			 * The execution of a reactive behavior is initiated by an part of
			 * the code external to this behavior. In opposite, the initiator
			 * of the execution of a pro-active behavior is the agent itself.
			 * 
			 * In SARL, a pro-active behavior is a behavior that is scheduled
			 * by the agent or one of its behaviors. The schedule mechanism is provided by the 
			 * [<code>Schedules</code> builtin capacity](./BuiltinCapacityReferenceSpec.html).
			 * In the following example, the agent execute its pro-active behavior
			 * every second.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Pro-active Behavior Units"{
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
				model.mustHavePackage("io.sarl.docs.reference.br")
				model.mustHaveImports(2)
				model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
				model.mustHaveImport(1, "io.sarl.core.Schedules", false, false, false)
				model.mustHaveTopElements(1)
				var b = model.elements.get(0).mustBeBehavior("MyBehavior", null).mustHaveFeatures(2)
				b.features.get(0).mustBeCapacityUses("io.sarl.core.Schedules")
				b.features.get(1).mustBeBehaviorUnit("io.sarl.core.Initialize", false)
			}

		}

		/* An agent is an autonomous entity having a set of skills to realize the
		 * capacities it exhibits.
		 * An agent has a set of built-in capacities considered essential to respect the
		 * commonly accepted competences of agents, such autonomy, reactivity, proactivity
		 * and social capacities. 
		 * 
		 * Consequently, a behavior associated to an agent is able to use the skills of this
		 * agent.
		 */		
		describe "Capacities and Skills" {

			/* The definition of a capacity or a skill is out of the scope
			 * of this reference document. For details, please read
			 * the [Capacity Reference](./CapacityReferenceSpec.html), and
			 * the [Skill Reference](./SkillReferenceSpec.html).
			 * 
			 * In the rest of this section, it is assumed that the following
			 * capacity and skill are defined: <pre><code>
			 * capacity Cap {
			 *     def action
			 * }
			 * skill Ski implements Cap {
			 *     def action {
			 *         println("Action")
			 *     }
			 * }
			 * </code></pre> 
			 * 
			 * @filter(.*) 
			 */
			fact "Defining a Capacity and a Skill"{
				"no fact to test"
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
			 * the two following steps must be done: <ol>
			 * <li>Retreive the skill instance: the function 
			 * <code>getSkill(Class<? extends Capacity>)</code> 
			 * permits to retreive the skill associated to the given capacity;</li>
			 * <li>Invoke the capacity's action on the retreived skill.</li>
			 * </ol>
			 * 
			 * <span class="label label-warning">Note</span> This method of
			 * invocation is not recommended by the SARL developpers.
			 * You should prefer the use of the extension methods (see below).  
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
				model.mustHavePackage("io.sarl.docs.reference.br")
				model.mustNotHaveImport
				model.mustHaveTopElements(3)
				var c = model.elements.get(0).mustBeCapacity("Cap").mustHaveFeatures(1)
				c.features.get(0).mustBeActionSignature("action", null, 0, false)
				model.elements.get(1).mustBeEvent("SomeEvent", null).mustHaveFeatures(0)
				var b = model.elements.get(2).mustBeBehavior("MyBehavior", null).mustHaveFeatures(1)
				b.features.get(0).mustBeBehaviorUnit("io.sarl.docs.reference.br.SomeEvent", false)
			}
	
			/* Invoking a capacity/skill with the getter method is
			 * not user-friendly. Since the
			 * [General Syntax Reference](./GeneralSyntaxReferenceSpec.html)
			 * describes the "extension method" mechanism, it is possible
			 * to use it for invoking the capacities.
			 * 
			 * But, instead of using an <code>import</code> directive,
			 * the <code>uses</code> keyword is provided for importing the
			 * capacities into the agent. In the following example,
			 * the <code>Cap</code> capacity is imported.
			 * 
			 * After a capacity was "imported", it is possible to
			 * directly call the functions of the capacity
			 * (according to the extension method syntax).
			 * In the following example, the action
			 * with the name <code>action</code> is invoked.
			 * This action is defined in the <code>Cap</code>
			 * capacity. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Using a Capacity with the Extension Methods" {
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
				model.mustHavePackage("io.sarl.docs.reference.br")
				model.mustNotHaveImport
				model.mustHaveTopElements(3)
				var c = model.elements.get(0).mustBeCapacity("Cap").mustHaveFeatures(1)
				c.features.get(0).mustBeActionSignature("action", null, 0, false)
				model.elements.get(1).mustBeEvent("SomeEvent", null).mustHaveFeatures(0)
				var b = model.elements.get(2).mustBeBehavior("MyBehavior", null).mustHaveFeatures(2)
				b.features.get(0).mustBeCapacityUses("io.sarl.docs.reference.br.Cap")
				b.features.get(1).mustBeBehaviorUnit("io.sarl.docs.reference.br.SomeEvent", false)
			}
		
		}
	
}
