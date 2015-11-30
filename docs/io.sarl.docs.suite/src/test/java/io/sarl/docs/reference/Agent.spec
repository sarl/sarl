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
import io.sarl.lang.sarl.SarlSkill
import org.eclipse.xtend.core.xtend.XtendField
import org.eclipse.xtext.xbase.XBooleanLiteral
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse
import io.sarl.lang.sarl.SarlCapacity

/** @outline
 * 
 * <p>This document describes the features related to the definition of an agent in SARL.
 * Before reading this document, it is recommended reading
 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
 * the [Skill Reference](SkillReferenceSpec.html), and the
 * [Built-in Capacity Reference](BuiltInCapacityReferenceSpec.html).
 * 
 * <p>__An agent is an autonomous entity having a set of skills to realize the 
 * capacities it exhibits__.
 */
@CreateWith(SARLSpecCreator)
describe "Agent Reference"{
	
		@Inject extension SARLParser
		
		/* Before detailing the architecture and the definition tools of an agent,
		 * it may be helpful to understand where is "living" an agent in the
		 * multi-agent system.
		 * 
		 * <p>The following figure illustrates the position of an agent (at the center
		 * of the figure) in different contexts. The details are discussed below. 
		 *
		 * <p>![Contexts](./contexts.png)
		 */		
		describe "Where is living an agent?" {
			
			/* When it is spawn, an agent is living inside the system in
			 * a place named "_Context_".
			 * 
			 * <p>__A Context defines the boundary of a sub-system, and 
			 * gathers a collection of spaces__.
			 * A Space is the support of the interaction between agents respecting 
			 * the rules defined in the spaces' specification.
			 * 
			 * <importantnote>In each context,
			 * there is at least one particular space called _Default Space_ to which 
			 * all agents in this context belong.</importantnote>
			 * 
			 * <p>It ensures the existence of a common shared space to all agents
			 * in the same context. Each agent can then create specific public or 
			 * private spaces to achieve its personal goals (the blue space on
			 * the figure above).
			 * 
			 * <importantnote> Since their creation, 
			 * agents are incorporated into a context called the __Default Context__.
			 * It is important to notice that the _Default Context_ is not necessarily
			 * the same for every agent.</importantnote>
			 * 
			 * <p>An agent has an identifier for each space it is involved in.
			 * For the case of event-based interaction spaces, this identifier is
			 * called "address". 
			 *  
			 * @filter(.*) 
			 */
			fact "Default Context" {
				// Test the URLs in the text from the beginning of the page
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				"SkillReferenceSpec.html" should beAccessibleFrom this
				"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
				"./contexts.png" should beAccessibleFrom this
			}
			
			/* During its lifetime, an agent may join and participate in other contexts
			 * that are not the default context. They are called the external contexts of the
			 * agent.
			 * 
			 * <note> There is no restriction
			 * about the number of contexts in which an agent is belonging to, except
			 * that it is always in its default context.</note>
			 * 
			 * <p>For joining or leaving a context, the agent must use the `ExternalContextAccess`
			 * built-in capacity. It is detailed in the
			 * [Built-in Capacity Reference](BuiltInCapacityReferenceSpec.html). 
			 *  
			 * @filter(.*) 
			 */
			fact "External Contexts" {
				"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
			}
			
			/* In 1967, Arthur Koestler coined the term _holon_ as an attempt to
			 * conciliate holistic and reductionist visions of the world.
			 * A holon represents a part-whole construct that can be seen as a 
			 * component of a higher level system or as whole composed of other 
			 * self-similar holons as substructures.
			 * 
			 * <p>Holonic Systems grew from the need to find comprehensive construct 
			 * that could help explain social phenomena. Since then, it came to be 
			 * used in a wide range of domains, including Philosophy,
			 * Manufacturing Systems, and Multi-Agents Systems.
			 * 
			 * <p>Several works have studied this question and they have proposed a number
			 * of models inspired from their experience in different domains.
			 * In many cases, we find the idea of _agents composed of other agents_.
			 * 
			 * <p>More recently, the importance of holonic multi-agent systems has been
			 * recognized by different methodologies such as [ASPECS](http://www.aspecs.org)
			 * or O-MASE.
			 * 
			 * <note> In SARL, we recognize 
			 * that agents can be composed of other agents. Therefore, SARL agents
			 * are in fact holons that can compose each other to define hierarchical 
			 * or recursive multi-agent system, called holarchies.</note>
			 * 
			 * <p>In order to achieve this, SARL agents are self-similar structures that 
			 * compose each other via their contexts. Each agent defines its own context,
			 * called __Inner Context__.
			 * Because this inner context may be joined by other agents, or agents may
			 * be spawn inside this inner context, it is possible to build a holarchy.
			 * 
			 * <veryimportantnote> An agent is always
			 * a participant of the default space of its inner space.</veryimportantnote>
			 * 
			 * <importantnote label="Note"> The unique identifier
			 * (usually a Unique Universal Identifier) of
			 * the inner context is equal to the unique identifier of its owning agent.
			 * </importantnote>
			 *  
			 * @filter(.*) 
			 */
			fact "From Flat to Hierarchical System with the Inner Context" {
				true
			}
			
			/* At the top level of the holarchy, we consider an omnipresent agent. 
			 * It is named the __Universe Agent__ (or _Root Agent_).
			 * The runtime environment will be in charge of spawning the 
			 * first agents in the system as members of the Universe Agent. 
			 * 
			 * <p>The inner context of the Universe Agent is called the
			 * Universe Context, or the Janus Context if you are using the
			 * [Janus runtime environment](http://www.janusproject.io).
			 *  
			 * @filter(.*) 
			 */
			fact "Universe Agent and Universe Context" {
				true
			}
			
		}
		
		/* The architecture of an agent is illustrated by the following figure.
		 * 
		 * <p>![Agent](./agent.png)
		 */
		describe "Open Architecture of an Agent" {
			
			/*
			 * An agent has a set of built-in capacities considered essential to respect the 
			 * commonly accepted competencies of agents, such autonomy, reactivity, pro-activity 
			 * and social capacities. 
			 * 
			 * <p>The full set of Built-in Capacities will be presented in the corresponding
			 * [Reference document](BuiltInCapacityReferenceSpec.html). Among these
			 * built-in capacities, is the `Behaviors` capacity that enables 
			 * agents to incorporate a collection of behaviors that will determine 
			 * its global conduct.
			 *  
			 * @filter(.*) 
			 */
			fact "Built-in Capacities" {
				"./agent.png" should beAccessibleFrom this
				"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
			}

			/*
			 * An agent has also a default behavior directly described within its 
			 * definition.
			 *  
			 * <p>__A Behavior maps a collection of perceptions represented 
			 * by Events to a sequence of Actions.__
			 * The various behaviors of an agent communicate using an event-driven 
			 * approach.
			 * 
			 * <p>__An Event is the specification of some occurrence in a Space that may 
			 * potentially trigger effects by a listener__ (e.g., agent, behavior, etc.) 
			 *  
			 * @filter(.*) 
			 */			
			fact "Agent Behaviors" {
				true
			}
			
		}

		/* An agent is declared with the `agent` keyword.
		 * In the agent's body block, we can declare Mental States 
		 * (in the form of attributes), Actions and Behaviors.
		 */
		describe "Defining an Agent" {

			/* The following code illustrates the definition of an agent
			 * named `MyAgent`, and that is empty.
			 * 
			 * <p>Basically, this agent does nothing, and does not react
			 * on events. 
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Defining an empty agent"{
				val model = '''
				agent MyAgent {
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 0
					it should haveNbElements 1
				]
				
				model.xtendTypes.get(0) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 0
				]
			}
			
			/* The mental state of an agent is composed by the data
			 * in the knowledge of the agent.
			 * Most of the time, the mental state is implemented as a
			 * collection of attributes in the agent.
			 * 
			 * <p>According to the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
			 * the attributes may be modifiable (when declared with the `var`
			 * keyword), or unmodifiable (when declared with the `val`
			 * keyword).
			 *  
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Agent Attributes"{
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				
				val model = '''
				agent MyAgent {
					// Defining a modifiable element of the mental state
					var mentalStateElement1 : String
					// Defining an unmodifiable element of the mental state
					val mentalStateElement2 : boolean = true
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					class MyType { }",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 0
					it should haveNbElements 2
				]
				
				var a = (model.xtendTypes.get(1) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 2
				]) as SarlAgent

				a.members.get(0) => [
					it should beVariable "mentalStateElement1"
					it should haveType "java.lang.String"
					(it as XtendField).initialValue should be null
				]

				a.members.get(1) => [
					it should beValue "mentalStateElement2"
					it should haveType "boolean"
					(it as XtendField).initialValue => [
						it should be typeof(XBooleanLiteral)
						it should beLiteral (true as Object)
					]
				]
			}

			/* It is allowed to define actions (methods) in the agent.
			 * The syntax described is described in the [General Syntax Reference](GeneralSyntaxReferenceSpec.html).
			 * 
			 * <p>The example below illustrates the creation of two actions in the agent.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Agent Actions"{
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				val model = '''
				agent MyAgent {
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
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 1
					it should importClass "io.sarl.core.Logging"
					it should haveNbElements 1
				]
				
				var a = (model.xtendTypes.get(0) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 3
				]) as SarlAgent

				a.members.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]

				a.members.get(1) => [
					it should beAction "myAction1"
					it should reply _;
					it should haveNbParameters 0
					it should beVariadic false
				]

				a.members.get(2) => [
					it should beAction "myAction2"
					it should reply _;
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
			 * of an agent. This mechanism is supported by the inheritance
			 * feature of SARL, which has the same semantic as the inheritance
			 * mechanism as the Java object-oriented language.
			 * 
			 * <p>The extended agent is specified just after the `extends`
			 * keyword.
			 * 
			 * <veryimportantnote> An agent 
			 * type can extend __only one__ other agent type. This is close
			 * to the constraint on the extension of classes in the Java
			 * language.</veryimportantnote>
			 * 
			 * <p>In the following code, a first agent is defined with the name
			 * `MyAgent` and an attribute named `attr`.
			 * A second agent `MySubAgent` is defined as the extension
			 * of the first agent. It contains a function named
			 * `action`, which is displaying the inherited attribute.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Extending an Agent" {
				val model = '''
				agent MyAgent {
					var attr : String
				}
				agent MySubAgent extends MyAgent {
					uses Logging
					def action {
						println(attr)
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 1
					it should importClass "io.sarl.core.Logging"
					it should haveNbElements 2
				]
				
				var a1 = (model.xtendTypes.get(0) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 1
				]) as SarlAgent

				a1.members.get(0) => [
					it should beVariable "attr"
					it should haveType "java.lang.String"
					it should haveInitialValue _
				]

				var a2 = (model.xtendTypes.get(1) => [
					it should beAgent "MySubAgent"
					it should extend "io.sarl.docs.reference.ar.MyAgent"
					it should haveNbElements 2
				]) as SarlAgent

				a2.members.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				a2.members.get(1) => [
					it should beAction "action"
					it should reply _
					it should haveNbParameters 0
					it should beVariadic false
				]
			}

			/** Modifiers are used to modify declarations of types and type members.
			 * This section introduces the modifiers for the agent.
			 * The modifiers are usually written before the keyword for defining the agent.
			 * 
			 * <p>The complete description of the modifiers' semantic is available in
			 * <a href="./BasicObjectOrientedProgrammingSupportModifiersSpec.html">this section</a>.
			 */
			describe "Modifiers" {
				
				/** An agent may be declared with one or more modifiers, which affect its runtime behavior: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`public`:  the agent is accessible from any other type;</li>
				 *     <li>`package`: the agent is accessible from only the types in the same package.</li>
				 *     </ul></li>
				 * <li>`abstract`: the agent is abstract and cannot be instanced.</li>
				 * <li>`final`: avoid to be derived.</li>
			 	 * <li>`strictfp`: avoid the methods of the agent to use intermediate floating number formats.</li>
				 * </ul>
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Agent Modifiers" {
					'''
						public agent Example1 {
						}
						package agent Example2 {
						}
						abstract agent Example3 {
						}
						final agent Example4 {
						}
						strictfp agent Example5 {
						}
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.ar",
						// TEXT
						""
					)
					// Test URL in the enclosing section text.
					"./BasicObjectOrientedProgrammingSupportModifiersSpec.html" should beAccessibleFrom this
				}
	
				/** The modifiers for the fields in an agent are: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`protected`:  the field is accessible within the same package, and derived agents;</li>
				 *     <li>`package`: the field is accessible only within the same package of its agent;</li>
				 *     <li>`private`: the field is accessible only within its agent.</li>
				 *     </ul></li>
				 * <li>`transient`: the field is never serialized.</li>
				 * <li>`volatile`: the field is modified by different threads. It is never cached thread-locally, and synchronized.</li>
				 * </ul>
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Field Modifiers" {
					'''
						protected var example1 : Object;
						package var example2 : Object;
						private var example3 : Object;
						transient var example5 : Object;
						volatile var example6 : Object;
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.ar
						public agent Agent1 {",
						// TEXT
						"}"
					)
				}
	
				/** The modifiers for the methods in an agent are: <ul>
				 * <li>Access modifiers: <ul>
				 *     <li>`protected`:  the method is accessible within the same package, and derived classes;</li>
				 *     <li>`package`: the method is accessible only within the same package as its class;</li>
				 *     <li>`private`: the method is accessible only within its class.</li>
				 *     </ul></li>
				 * <li>`abstract`: the method has no implementation in the class.</li>
				 * <li>`dispatch`: the method provides an implementation for the dispatch method mechanism.</li>
				 * <li>`final`: the method cannot be overridden in derived classes.</li>
				 * <li>`static`: the method is a class method, not an instance method.</li>
				 * <li>`strictfp`: avoid the method to use intermediate floating number formats.</li>
				 * <li>`synchronized`: the method is synchronized on the class instance.</li>
				 * </ul>
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Method Modifiers" {
					'''
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
						// Static function
						static def example6 { }
						// Function with strict floating point management
						strictfp def example7 { }
						// Synchronized function
						synchronized def example8 { }
						// Dispatch functions
						dispatch def example9(p : Integer) { }
						dispatch def example9(p : Float) { }
					'''.parseSuccessfully(
						"package io.sarl.docs.reference.ar
						abstract agent Agent1 {",
						// TEXT
						"}"
					)
				}
	
				/** All the <a href="./BasicObjectOrientedProgrammingSupportSpec.html">modifiers for the
				 * nested types</a> are allowed <strong>except</strong> `public`. 
				 *
				 * @filter(.*)
				 */
				fact "Restriction on the Nested Type Modifiers" {
					'''
						package io.sarl.docs.reference.ar
						agent Agent1 {
							public class NedtedClass1 { }
						}
					'''.parseWithError
				}
	
			}
	
		}
		
		/* The behaviors of an agent correspond to the units that are
		 * executed by the agent for exhibiting its general behavior.
		 * 
		 * <p>The execution of the behaviors are related to the life cycle of
		 * the agents. Every agent is following the steps:
		 * 
		 *  * Initialization: the agent react on the `Initialize` event;
		 *  * Lifetime: the agent execute its behaviors. They may be:
		 *  * * reactive: the agent react when it is receiving events, or
		 *  * * proactive: the agent executes by itself one of its behaviors.
		 *  * Destruction: the agent react on the `Destroy` event.
		 * 
		 * 
		 * <p>The definition of the reactive behaviors is based on the event handling
		 * mechanism of SARL. Events may be emitted in [spaces](SpaceReferenceSpec.html),
		 * and received by the agents belonging to these spaces.
		 * An agent may indicate that it is interesting for receiving an event by specifying
		 * an event handler using the following syntax:
		 * 
		 *     on <EventName> [<Guard>] {
		 * 	       <Statements>
		 *     }
		 *
		 * 
		 * <p>`<EventName>` is the name of the events to wait for.
		 * `<Guard>` is the optional definition of a predicate
		 * that may be true for executing the `<Statements>`.
		 * The statements are executed only if an event with the given name is
		 * received, __and__ if the guard is true.
		 * 
		 * <p>In the guard and the statements, it is possible to use the instance
		 * of the received event: the occurrence. This instance is represented
		 * by the `occurrence` keyword. It is an implicit
		 * variable as the keywords `this` and `it`.
		 */
		describe "Behaviors of an Agent" {
			
			/* When an agent is ready to be executed by the runtime environment,
			 * it receives the `Initialize` event.
			 * This event is defined as:
			 * 
			 *     event Initialize {
			 *         var parameters : Object[]
			 *     }
			 *
			 * 
			 * <p>It contains the list of the parameters given to the spawning
			 * function (as specified in the 
			 * [built-in capacities](BuiltInCapacityReferenceSpec.html)).
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
				agent MyAgent {
					uses Logging
					on Initialize {
						println(
							"My initialization parameters are: "
							+ occurrence.parameters )
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
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 2
				]) as SarlAgent

				a.members.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				a.members.get(1) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith _
				]
			}

			/* Because `Initialize` is an event, the handler in
			 * the agent could use a guard. This feature enables
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
				agent MyAgent {
					uses Logging
					on Initialize [ occurrence.parameters.empty ] {
						println("Initialization without parameters")
					}
					on Initialize [ ! occurrence.parameters.empty ] {
						println("Initialization with parameters: "
							+ occurrence.parameters )
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
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 3
				]) as SarlAgent

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
				agent MyAgent {
					uses Logging
					on Destroy {
						println("Destroying the agent")
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
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 2
				]) as SarlAgent

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
				agent MyAgent {
					uses Logging
					var resource : Object
					on Destroy [ resource !== null ] {
						println("Destroying the agent when there is a resource")
					}
					on Destroy [ resource === null ] {
						println("Destroying the agent when there is no resource")
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
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 4
				]) as SarlAgent

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

			/* The reactive behavior of an agent is specified with a collection
			 * of event handlers. The principle of a reactive behavior
			 * is to execute a part of the global agent behavior when something
			 * has happening in the agent, or in its environment.
			 * 
			 * <p>In the following example, the agent is reacting to the reception
			 * of the `SomethingChanged` event.
			 * 
			 * <p>As for all the event handlers, it could be guarded by a predicate.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Reactive Behaviors"{
				val model = '''
				agent MyAgent {
					uses Logging
					on SomethingChanged {
						println("Reactive behavior")
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging
					event SomethingChanged",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.ar"
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
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 2
					(it as SarlAgent).members.get(0) => [
						it should beCapacityUse "io.sarl.core.Logging"
					]
					(it as SarlAgent).members.get(1) => [
						it should beBehaviorUnit "io.sarl.docs.reference.ar.SomethingChanged"
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
			fact "Parallel Execution of the Reactive Behaviors"{
				val model = '''
				agent MyAgent {
					uses Logging
					on SomethingChanged {
						println("First reactive behavior")
					}
					on SomethingChanged {
						println("Second reactive behavior")
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging
					event SomethingChanged",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.ar"
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
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 3
					(it as SarlAgent).members.get(0) => [
						it should beCapacityUse "io.sarl.core.Logging"
					]
					(it as SarlAgent).members.get(1) => [
						it should beBehaviorUnit "io.sarl.docs.reference.ar.SomethingChanged"
						it should beGuardedWith _
					]
					(it as SarlAgent).members.get(2) => [
						it should beBehaviorUnit "io.sarl.docs.reference.ar.SomethingChanged"
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
			 * by the agent. The scheduling mechanism is provided by the 
			 * [`Schedules` built-in capacity](BuiltInCapacityReferenceSpec.html).
			 * In the following example, the agent execute its proactive behavior
			 * every second.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Pro-active Behaviors"{
				"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
				//
				val model = '''
				agent MyAgent {
					uses Schedules, Logging
					on Initialize {
						every(1000) [
							println("Run a pro-active behavior")
						]
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging
					import io.sarl.core.Initialize
					import io.sarl.core.Schedules",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 3
					it should importClass "io.sarl.core.Logging"
					it should importClass "io.sarl.core.Initialize"
					it should importClass "io.sarl.core.Schedules"
					it should haveNbElements 1
				]
				
				var a = (model.xtendTypes.get(0) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 2
				]) as SarlAgent

				a.members.get(0) should beCapacityUse #["io.sarl.core.Schedules", "io.sarl.core.Logging"]
				a.members.get(1) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith _
				]
			}

		}

		/* An agent is an autonomous entity having a set of skills to realize the
		 * capacities it exhibits.
		 * An agent has a set of built-in capacities considered essential to respect the
		 * commonly accepted competencies of agents, such autonomy, reactivity, pro-activity
		 * and social capacities. 
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
			 * 	       uses Logging
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

			/* When an agent must use a capacity in one of its behaviors,
			 * it must own an implementation of this capacity: a skill.
			 * 
			 * <p>For assigning a skill to an agent, the instance of the skill
			 * must be created. Then, it is associated with the
			 * implemented capacity.
			 * In the following example, the agent is creating the
			 * `Ski` skill. This instance is associated with
			 * the corresponding capacity `Cap` with the
			 * function `setSkill(Class<? extends Capacity>, Skill)`.
			 * 
			 * <p>When the function `setSkill` is returning, the agent
			 * becomes able to use the skill.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Giving a Skill to an Agent" {
				val model = '''
				agent MyAgent {
					on Initialize {
						var theSkill = new Ski
						setSkill( Cap, theSkill )
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging
					import io.sarl.core.Initialize
					capacity Cap {
						def action
					}
					skill Ski implements Cap {
						uses Logging
						def action { println(\"Action\") }
					}",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 2
					it should importClass "io.sarl.core.Logging"
					it should importClass "io.sarl.core.Initialize"
					it should haveNbElements 3
				]
				
				var c = (model.xtendTypes.get(0) => [
					it should beCapacity "Cap"
					it should extend _
					it should haveNbElements 1
				]) as SarlCapacity
				
				c.members.get(0) => [
					it should beActionSignature "action"
					it should reply _
					it should haveNbParameters 0
					it should beVariadic false
				]

				var s = (model.xtendTypes.get(1) => [
					it should beSkill "Ski"
					it should extend _
					it should implement #["io.sarl.docs.reference.ar.Cap"]
					it should haveNbElements 2
				]) as SarlSkill
				
				s.members.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				s.members.get(1) => [
					it should beAction "action"
					it should reply _
					it should haveNbParameters 0
					it should beVariadic false
				]

				var a = (model.xtendTypes.get(2) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 1
				]) as SarlAgent
				
				a.members.get(0) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith _
				]
			}
	
			/* Because the built-in capacities are supported by the runtime environment,
			 * the corresponding skills are given to the agent.
			 * It means that there is no need for an agent to set a skill for
			 * a built-in capacity.
			 * 
			 * <p>However, in rare cases, it is possible to use the function
			 * `setSkill(Class<? extends Capacity>, Skill)` for
			 * changing the implementation of a built-in capacity.
			 * 
			 * @filter(.*) 
			 */
			fact "Giving a Built-in Skill to an Agent" {
				true
			}

			/* After a skill is registered into the agent,
			 * it could be invoked.
			 *
			 * <p>For invoking a function implemented by a skill,
			 * the two following steps must be done:
			 * 
			 *  * Retrieve the skill instance: the function
			 *    `getSkill(Class<? extends Capacity>)`
			 *    permits retrieving the skill associated to the given capacity;
			 *  * Invoke the capacity's action on the retrieved skill.
			 *
			 * 
			 * <note>This method of
			 * invocation is not recommended by the SARL developers.
			 * You should prefer the use of the extension methods (see below).  
			 * </note>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Using a Capacity with the Getters" {
				val model = '''
				agent MyAgent {
					on Initialize {
						var s = new Ski
						setSkill( Cap, s )
					}
					on SomeEvent {
						// Retreive the capacity implementation
						var s = getSkill(Cap)
						// Run the action of the skill
						s.action
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging
					import io.sarl.core.Initialize
					capacity Cap {
						def action
					}
					skill Ski implements Cap {
						uses Logging
						def action { println(\"Action\") }
					}
					event SomeEvent",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 2
					it should importClass "io.sarl.core.Logging"
					it should importClass "io.sarl.core.Initialize"
					it should haveNbElements 4
				]
				
				var c = (model.xtendTypes.get(0) => [
					it should beCapacity "Cap"
					it should extend _
					it should haveNbElements 1
				]) as SarlCapacity
				
				c.members.get(0) => [
					it should beActionSignature "action"
					it should reply _
					it should haveNbParameters 0
					it should beVariadic false
				]
				
				var s = (model.xtendTypes.get(1) => [
					it should beSkill "Ski"
					it should extend _
					it should implement #["io.sarl.docs.reference.ar.Cap"]
					it should haveNbElements 2
				]) as SarlSkill
				
				s.members.get(0) => [
					it should beCapacityUse "io.sarl.core.Logging"
				]
				s.members.get(1) => [
					it should beAction "action"
					it should reply _
					it should haveNbParameters 0
					it should beVariadic false
				]

				model.xtendTypes.get(2) => [
					it should beEvent "SomeEvent"
					it should extend _
					it should haveNbElements 0
				]

				var a = (model.xtendTypes.get(3) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 2
				]) as SarlAgent

				a.members.get(0) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith _
				]

				a.members.get(1) => [
					it should beBehaviorUnit "io.sarl.docs.reference.ar.SomeEvent"
					it should beGuardedWith _
				]
			}
	
			/* The built-in capacities are accessible in the same way
			 * as the other capacities, with the getters.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Using a Built-in Capacity with the Getters" {
				val model = '''
				agent MyAgent {
					on SomeEvent {
						// Retreive the capacity implementation
						var s = getSkill(Lifecycle)
						// Run the action of the skill
						s.killMe
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Lifecycle
					event SomeEvent",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 1
					it should importClass "io.sarl.core.Lifecycle"
					it should haveNbElements 2
				]
				
				model.xtendTypes.get(0) => [
					it should beEvent "SomeEvent"
					it should extend _
					it should haveNbElements 0
				]

				var a = (model.xtendTypes.get(1) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 1
				]) as SarlAgent

				a.members.get(0) => [
					it should beBehaviorUnit "io.sarl.docs.reference.ar.SomeEvent"
					it should beGuardedWith _
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
				agent MyAgent {
					uses Cap
					on Initialize {
						var s = new Ski
						setSkill( Cap, s )
					}
					on SomeEvent {
						// Run the action of the skill
						action
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Logging
					import io.sarl.core.Initialize
					capacity Cap {
						def action
					}
					skill Ski implements Cap {
						uses Logging
						def action { println(\"Action\") }
					}
					event SomeEvent",
					// TEXT
					""
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 2
					it should importClass "io.sarl.core.Logging"
					it should importClass "io.sarl.core.Initialize"
					it should haveNbElements 4
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
					it should beSkill "Ski"
					it should extend _
					it should implement #["io.sarl.docs.reference.ar.Cap"]
					it should haveNbElements 2
					(it as SarlSkill).members.get(0) => [
						it should beCapacityUse "io.sarl.core.Logging"
					]
					(it as SarlSkill).members.get(1) => [
						it should beAction "action"
						it should reply _
						it should haveNbParameters 0
						it should beVariadic false
					]
				]

				model.xtendTypes.get(2) => [
					it should beEvent "SomeEvent"
					it should extend _
					it should haveNbElements 0
				]

				var a = (model.xtendTypes.get(3) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 3
				]) as SarlAgent

				a.members.get(0) should beCapacityUse #["io.sarl.docs.reference.ar.Cap"]

				a.members.get(1) => [
					it should beBehaviorUnit "io.sarl.core.Initialize"
					it should beGuardedWith _
				]

				a.members.get(2) => [
					it should beBehaviorUnit "io.sarl.docs.reference.ar.SomeEvent"
					it should beGuardedWith _
				]
			}
	
			/* The built-in capacities are accessible in the same way
			 * as the other capacities, with the extension methods.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Using a Built-in Capacity with the Extension Methods" {
				val model = '''
				agent MyAgent {
					uses Lifecycle
					on SomeEvent {
						// Run the action of the skill
						killMe
					}
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Lifecycle
					event SomeEvent",
					// TEXT
					""
				)

				model => [
					it should havePackage "io.sarl.docs.reference.ar"
					it should haveNbImports 1
					it should importClass "io.sarl.core.Lifecycle"
					it should haveNbElements 2
				]
				
				model.xtendTypes.get(0) => [
					it should beEvent "SomeEvent"
					it should extend _
					it should haveNbElements 0
				]

				var a = (model.xtendTypes.get(1) => [
					it should beAgent "MyAgent"
					it should extend _
					it should haveNbElements 2
				]) as SarlAgent

				a.members.get(0) should beCapacityUse #["io.sarl.core.Lifecycle"]

				a.members.get(1) => [
					it should beBehaviorUnit "io.sarl.docs.reference.ar.SomeEvent"
					it should beGuardedWith _
				]
		}
		
	}

	/* Specification: SARL General-purpose Agent-Oriented Programming Language ("Specification")<br/>
	 * Version: %sarlspecversion%<br/>
	 * Status: %sarlspecreleasestatus%<br/>
	 * Release: %sarlspecreleasedate%
	 * 
	 * 
	 * <p>Copyright &copy; %copyrightdate% %copyrighters%. All rights reserved.
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
