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
 * This document describes the features related to the definition of an agent in SARL.
 * Before reading this document, it is recommended to read
 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html),
 * the [Skill Reference](./SkillReferenceSpec.html), and the
 * [Built-in Capacity Reference](./BuiltinCapacityReferenceSpec.html).
 * 
 * __An agent is an autonomous entity having a set of skills to realize the 
 * capacities it exhibits__.
 */
@CreateWith(SARLSpecCreator)
describe "Agent Reference"{
	
		@Inject extension SARLParser
		
		/* Before detailing the architecture and the definition tools of an agent,
		 * it may be helpful to understand where is "living" an agent in the
		 * multiagent system.
		 * 
		 * The following figure illustrates the position of an agent (at the center
		 * of the figure) in different context. The details are discussed below. 
		 * <center><img src="./contexts.png"/></center>
		 */		
		describe "Where is living an agent?" {
			
			/* When it is spawn, an agent is living inside the system in
			 * a place named "_Context_".
			 * 
			 * __A Context defines the boundary of a sub-system, and 
			 * gathers a collection of spaces__.
			 * A Space is the support of the interaction between agents respecting 
			 * the rules defined in the spaces' specification.
			 * 
			 * <span class="label label-danger">Important</span> In each context,
			 * there is at least one particular space called _Default Space_ to which 
			 * all agents in this context belong.
			 * 
			 * This ensures the existence of a common shared space to all agents
			 * in the same context. Each agent can then create specific public or 
			 * private spaces to achieve its personal goals (the blue space on
			 * the figure above).
			 * 
			 * <span class="label label-danger">Important</span> Since their creation, 
			 * agents are incorporated into a context called the __Default Context__.
			 * It is important to notice that the _Default Context_ is not necessarily
			 * the same for every agent.
			 * 
			 * An agent has an identifier for each space it is involved in.
			 * For the case of event-based interaction spaces, this identifier is
			 * called "address". 
			 */
			fact "Default Context" {
				true
			}
			
			/* During its lifetime, an agent may join and participate to other contexts
			 * than the default context. they are called the external contexts of the
			 * agent.
			 * 
			 * <span class="label label-info">Note</span> There is no restriction
			 * about the number of contexts in which an agent is belonging to, except
			 * that it is always in its default context.
			 * 
			 * For joining or leaving a context, the agent must use the `ExternalContextAccess`
			 * built-in capacity. It is detailed in the
			 * [Built-in Capacity Reference](./BuiltinCapacityReferenceSpec.html). 
			 */
			fact "External Contexts" {
				true
			}
			
			/* In 1967, Arthur Koestler coined the term _holon_ as an attempt to
			 * conciliate holistic and reductionist visions of the world.
			 * A holon represents a part-whole construct that can be seen as a 
			 * component of a higher level system or as whole composed of other 
			 * self-similar holons as substructures.
			 * 
			 * Holonic Systems grew from the need to find comprehensive construct 
			 * that could help explain social phenomena. Since then, it came to be 
			 * used in a wide range of domains, including Philosophy,
			 * Manufacturing Systems, and Multi-Agents Systems.
			 * 
			 * Several works have studied this question and they have proposed a number
			 * of models inspired from their experience in different domains.
			 * In many cases, we find the idea of _agents composed of other agents_.
			 * 
			 * More recently, the importance of holonic multiagent systems has been
			 * recognize by different methodologies such as [ASPECS](http://www.aspecs.org)
			 * or O-MASE.
			 * 
			 * <span class="label label-info">Note</span> In SARL, we recognize 
			 * that agents can be composed of other agents. Therefore, SARL agents
			 * are in fact holons that can compose each other to define hierarchical 
			 * or recursive multiagent system, called holarchies.
			 * 
			 * In order to achieve this, SARL agents are self-similar structures that 
			 * compose each other via their contexts. Each agent defines its own context,
			 * called __Inner Context__.
			 * Because this inner context may be joined by other agents, or agents may
			 * be spawn inside this inner context, it is possible to build a holarchy.
			 * 
			 * <span class="label label-danger">Important</span> An agent is always
			 * be a participant of the default space of its inner space.
			 * 
			 * <span class="label label-warning">Note</span> The unique identifier
			 * (usually a Unique Universal Identifier) of
			 * the inner context is equal to the unique identifier of its owning agent.
			 */
			fact "From Flat to Hierarchical System with the Inner Context" {
				true
			}
			
			/* At the top level of the holarchy, we consider an omnipresent agent. 
			 * It is named the __Universe Agent__ (or _Root Agent_).
			 * The runtime environment will be in charge of spawning the 
			 * first agents in the system as members of the Universe Agent. 
			 * 
			 * The inner context of the Universe Agent is called the
			 * Universe Context, or the Janus Context if you are using the
			 * [Janus runtime environment](http://www.janusproject.io).
			 */
			fact "Universe Agent and Universe Context" {
				true
			}
			
		}
		
		/* The architecture of an agent is illustrated by the following figure.
		 * 
		 * <center><img src="./agent.png"/></center>
		 */
		describe "Open Architecture of an Agent" {
			
			/*
			 * An agent has a set of built-in capacities considered essential to respect the 
			 * commonly accepted competences of agents, such autonomy, reactivity, pro-activity 
			 * and social capacities. 
			 * 
			 * The full set of Built-in Capacities will be presented in the corresponding
			 * [Reference document](./BuiltinCapacityReferenceSpec.html). Among these
			 * built-in capacities, is the `Behaviors` capacity that enables 
			 * agents to incorporate a collection of behaviors that will determine 
			 * its global conduct.
			 */
			fact "Built-in Capacities" {
				true
			}

			/*
			 * An agent has also a default behavior directly described within its 
			 * definition.
			 *  
			 * __A Behavior maps a collection of perceptions represented 
			 * by Events to a sequence of Actions.__
			 * The various behaviors of an agent communicate using an event-driven 
			 * approach.
			 * 
			 * __An Event is the specification of some occurrence in a Space that may 
			 * potentially trigger effects by a listener__ (e.g., agent, behavior, etc.) 
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
			 * Basically, this agent does nothing, and does not react
			 * on events. 
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Defining an empty agent"{
				val model = '''
				agent MyAgent {
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustNotHaveImport
				model.mustHaveTopElements(1)
				model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(0)
			}
			
			/* The mental state of an agent is composed by the data
			 * in the knowledge of the agent.
			 * Most of the time, the mental state is implemented as a
			 * collection of attributes in the agent.
			 * 
			 * According to the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
			 * the attributes may be modifiable (when declared with the `var`
			 * keyword), or unmodifiable (when declared with the `val`
			 * keyword).
			 *  
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Agent Attributes"{
				val model = '''
				agent MyAgent {
					// Defining a modifiable element of the mental state
					var mentalStateElement1 : String
					// Defining an unmodifiable element of the mental state
					val mentalStateElement2 : boolean = true
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustNotHaveImport
				model.mustHaveTopElements(1)
				var a = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(2)
				a.features.get(0).mustBeAttribute(true, "mentalStateElement1", "java.lang.String", false)
				a.features.get(1).mustBeAttribute(false, "mentalStateElement2", "boolean", true)
			}

			/* It is allowed to define actions (methods) in the agent.
			 * The syntax described is described in the [General Syntax Reference](GeneralSyntaxReferenceSpec.html).
			 * 
			 * The example below illustrates the creation of two actions in the agent.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Agent Actions"{
				val model = '''
				agent MyAgent {
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
					"package io.sarl.docs.reference.ar",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustNotHaveImport
				model.mustHaveTopElements(1)
				var a = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(2)
				a.features.get(0).mustBeAction("myAction1", null, 0, false)
				a.features.get(1).mustBeAction("myAction2", null, 1, true).mustHaveParameter(0, "param", "int", false)
			}
	
			/* In some use cases, it is useful to specialize the definition
			 * of an agent. This mechanism is supported by the inheritance
			 * feature of SARL, which has the same semantic as the inheritance
			 * mechanism as the Java object-oriented language.
			 * 
			 * The extended agent is specified just after the `extends`
			 * keyword.
			 * 
			 * <span class="label label-danger">Important</span> An agent 
			 * type can extend __only one__ other agent type. This is close
			 * to the constraint on the extension of classes in the Java
			 * language.
			 * 
			 * In the following code, a first agent is defined with the name
			 * `MyAgent` and an attribute named `attr`.
			 * A second agent `MySubAgent` is defined as the extension
			 * of the first agent. It contains a function named
			 * `action`, which is displaying the inherited attribute.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Extending an Agent" {
				val model = '''
				agent MyAgent {
					var attr : String
				}
				agent MySubAgent extends MyAgent {
					def action {
						println(attr)
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustNotHaveImport
				model.mustHaveTopElements(2)
				var a1 = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(1)
				a1.features.get(0).mustBeAttribute(true, "attr", "java.lang.String", false)
				var a2 = model.elements.get(1).mustBeAgent("MySubAgent", "io.sarl.docs.reference.ar.MyAgent").mustHaveFeatures(1)
				a2.features.get(0).mustBeAction("action", null, 0, false)
			}

		}
		
		/* The behaviors of an agent correspond to the units that are
		 * executed by the agent for exhibiting its general behavior.
		 * 
		 * The execution of the behaviors are related to the lifecycle of
		 * the agents. Every agent is following the steps:
		 * 
		 *  * Initialization: the agent react on the `Initialize` event;
		 *  * Lifetime: the agent execute its behaviors. They may be:
		 *  * * reactive: the agent react when it is receiving events, or
		 *  * * pro-active: the agent executes by itself one of its behaviors.
		 *  * Destruction: the agent react on the `Destroy` event.
		 * 
		 * 
		 * The definition of the reactive behaviors is based on the event handling
		 * mechanism of SARL. Events may be emitted in [spaces](./SpaceReferenceSpec.html),
		 * and received by the agents belonging to these spaces.
		 * An agent may indicate that it is interesting for receiving an event by specifying
		 * an event handler using the following syntax:
		 * 
		 *     on <EventName> [<Guard>] {
		 * 	       <Statements>
		 *     }
		 *
		 * 
		 * `<EventName>` is the name of the events to wait for.
		 * `<Guard>` is the optional definition of a predicate
		 * that may be true for executing the `<Statements>`.
		 * The statements are executed only if an event with the given name is
		 * received, __and__ if the guard is true.
		 * 
		 * In the guard and the statements, it is possible to use the instance
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
			 * It contains the list of the parameters given to the spawning
			 * function (as specified in the 
			 * [built-in capacities](./BuiltinCapacityReferenceSpec.html)).
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Initialization Handler"{
				val model = '''
				agent MyAgent {
					on Initialize {
						println(
							"My initialization parameters are: "
							+ occurrence.parameters )
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Initialize",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
				model.mustHaveTopElements(1)
				var a = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(1)
				a.features.get(0).mustBeBehaviorUnit("io.sarl.core.Initialize", false)
			}

			/* Because `Initialize` is an event, the handler in
			 * the agent could use a guard. This feature enables
			 * the developer to write different initialization blocks
			 * depending on the guards of the handlers.
			 * 
			 * In the following example, the first event handler is
			 * executed when the `Initialize` event has
			 * no parameter. The second event handler is executed
			 * when the event has at least one parameter.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Guarded Initialization Handler"{
				val model = '''
				agent MyAgent {
					on Initialize [ occurrence.parameters.empty ] {
						println("Initialization without parameters")
					}
					on Initialize [ ! occurrence.parameters.empty ] {
						println("Initialization with parameters: "
							+ occurrence.parameters )
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Initialize",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
				model.mustHaveTopElements(1)
				var a = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(2)
				a.features.get(0).mustBeBehaviorUnit("io.sarl.core.Initialize", true)
				a.features.get(1).mustBeBehaviorUnit("io.sarl.core.Initialize", true)
			}

			/* The counterpart of `Initialize` is the event
			 * `Destroy`.
			 * This event is defined as:
			 * 
			 *     event Destroy
			 *
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Destruction Handler" {
				val model = '''
				agent MyAgent {
					on Destroy {
						println("Destroying the agent")
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Destroy",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.core.Destroy", false, false, false)
				model.mustHaveTopElements(1)
				var a = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(1)
				a.features.get(0).mustBeBehaviorUnit("io.sarl.core.Destroy", false)
			}
			/* As for `Initialize`, the handlers of
			 * the `Destroy` event could be guarded.
			 * 
			 * In the following example, the first event handler is
			 * executed when the `Destroy` is received
			 * and there is resource stored in the corresponding
			 * field. The second event handler is executed
			 * when there is no resource.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Guarded Destruction Handler" {
				val model = '''
				agent MyAgent {
					var resource : Object
					on Destroy [ resource !== null ] {
						println("Destroying the agent when there is a resource")
					}
					on Destroy [ resource === null ] {
						println("Destroying the agent when there is no resource")
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Destroy",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.core.Destroy", false, false, false)
				model.mustHaveTopElements(1)
				var a = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(3)
				a.features.get(0).mustBeAttribute(true, "resource", "java.lang.Object", false)
				a.features.get(1).mustBeBehaviorUnit("io.sarl.core.Destroy", true)
				a.features.get(2).mustBeBehaviorUnit("io.sarl.core.Destroy", true)
			}

			/* The reactive behavior of an agent is specified with a collection
			 * of event handlers. The principle of a reactive behavior
			 * is to execute a part of the agent's global behavior when something
			 * has happening in the agent, or in its environment.
			 * 
			 * In the following example, the agent is reacting to the reception
			 * of the `SomethingChanged` event.
			 * 
			 * As for all the event handlers, it could be guarded by a predicate.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Reactive Behaviors"{
				val model = '''
				agent MyAgent {
					on SomethingChanged {
						println("Reactive behavior")
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					event SomethingChanged",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustNotHaveImport
				model.mustHaveTopElements(2)
				model.elements.get(0).mustBeEvent("SomethingChanged", null).mustHaveFeatures(0)
				var a = model.elements.get(1).mustBeAgent("MyAgent", null).mustHaveFeatures(1)
				a.features.get(0).mustBeBehaviorUnit("io.sarl.docs.reference.ar.SomethingChanged", false)
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
			fact "Parallel Execution of the Reactive Behaviors"{
				val model = '''
				agent MyAgent {
					on SomethingChanged {
						println("First reactive behavior")
					}
					on SomethingChanged {
						println("Second reactive behavior")
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					event SomethingChanged",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustNotHaveImport
				model.mustHaveTopElements(2)
				model.elements.get(0).mustBeEvent("SomethingChanged", null).mustHaveFeatures(0)
				var a = model.elements.get(1).mustBeAgent("MyAgent", null).mustHaveFeatures(2)
				a.features.get(0).mustBeBehaviorUnit("io.sarl.docs.reference.ar.SomethingChanged", false)
				a.features.get(1).mustBeBehaviorUnit("io.sarl.docs.reference.ar.SomethingChanged", false)
			}

			/* A pro-active behavior is a part of the global behavior of an agent that the 
			 * agent is deciding to execute by itself.
			 * The execution of a reactive behavior is initiated by an part of
			 * the code external to this behavior. In opposite, the initiator
			 * of the execution of a pro-active behavior is the agent itself.
			 * 
			 * In SARL, a pro-active behavior is a behavior that is scheduled
			 * by the agent. The scheduling mechanism is provided by the 
			 * [`Schedules` built-in capacity](./BuiltinCapacityReferenceSpec.html).
			 * In the following example, the agent execute its pro-active behavior
			 * every second.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Pro-active Behaviors"{
				val model = '''
				agent MyAgent {
					uses Schedules
					on Initialize {
						every(1000) [
							println("Run a pro-active behavior")
						]
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Initialize
					import io.sarl.core.Schedules",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustHaveImports(2)
				model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
				model.mustHaveImport(1, "io.sarl.core.Schedules", false, false, false)
				model.mustHaveTopElements(1)
				var a = model.elements.get(0).mustBeAgent("MyAgent", null).mustHaveFeatures(2)
				a.features.get(0).mustBeCapacityUses("io.sarl.core.Schedules")
				a.features.get(1).mustBeBehaviorUnit("io.sarl.core.Initialize", false)
			}

		}

		/* An agent is an autonomous entity having a set of skills to realize the
		 * capacities it exhibits.
		 * An agent has a set of built-in capacities considered essential to respect the
		 * commonly accepted competences of agents, such autonomy, reactivity, proactivity
		 * and social capacities. 
		 */		
		describe "Capacities and Skills" {

			/* The definition of a capacity or a skill is out of the scope
			 * of this reference document. For details, please read
			 * the [Capacity Reference](./CapacityReferenceSpec.html), and
			 * the [Skill Reference](./SkillReferenceSpec.html).
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
				"no fact to test"
			}

			/* When an agent must use a capacity in one of its behaviors,
			 * it must own an implementation of this capacity: a skill.
			 * 
			 * For assigning a skill to an agent, the instance of the skill
			 * must be created. Then, it is associated with the
			 * implemented capacity.
			 * In the following example, the agent is creating the
			 * `Ski` skill. This instance is associated to
			 * the corresponding capacity `Cap` with the
			 * function `setSkill(Class<? extends Capacity>, Skill)`.
			 * 
			 * When the function `setSkill` is returning, the agent
			 * becomes able to use the skill.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
			 */
			fact "Giving a Skill to an Agent" {
				val model = '''
				agent MyAgent {
					on Initialize {
						var theSkill = new Ski
						setSkill( Cap, theSkill )
					}
				}
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Initialize
					capacity Cap {
						def action
					}
					skill Ski implements Cap {
						def action { println(\"Action\") }
					}",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
				model.mustHaveTopElements(3)
				var c = model.elements.get(0).mustBeCapacity("Cap").mustHaveFeatures(1)
				c.features.get(0).mustBeActionSignature("action", null, 0, false)
				var s = model.elements.get(1).mustBeSkill("Ski", null, "io.sarl.docs.reference.ar.Cap").mustHaveFeatures(1)
				s.features.get(0).mustBeAction("action", null, 0, false)
				var a = model.elements.get(2).mustBeAgent("MyAgent", null).mustHaveFeatures(1)
				a.features.get(0).mustBeBehaviorUnit("io.sarl.core.Initialize", false)
			}
	
			/* Because the built-in capacities are supported by the runtime environment,
			 * the corresponding skills are given to the agent.
			 * It means that there is no need for an agent to set a skill for
			 * a built-in capacity.
			 * 
			 * However, in rare cases, it is possible to use the function
			 * `setSkill(Class<? extends Capacity>, Skill)` for
			 * changing the implementation of a built-in capacity.
			 * 
			 * @filter(.*) 
			 */
			fact "Giving a Built-in Skill to an Agent" {
				"no fact to test"
			}

			/* After a skill is registered into the agent,
			 * it could be invoked.
			 *
			 * For invoking a function implemented by a skill,
			 * the two following steps must be done:
			 * 
			 *  * Retrieve the skill instance: the function
			 *    `getSkill(Class<? extends Capacity>)`
			 *    permits to retrieve the skill associated to the given capacity;
			 *  * Invoke the capacity's action on the retrieved skill.
			 *
			 * 
			 * <span class="label label-info">Note</span> This method of
			 * invocation is not recommended by the SARL developers.
			 * You should prefer the use of the extension methods (see below).  
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Initialize
					capacity Cap {
						def action
					}
					skill Ski implements Cap {
						def action { println(\"Action\") }
					}
					event SomeEvent",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
				model.mustHaveTopElements(4)
				var c = model.elements.get(0).mustBeCapacity("Cap").mustHaveFeatures(1)
				c.features.get(0).mustBeActionSignature("action", null, 0, false)
				var s = model.elements.get(1).mustBeSkill("Ski", null, "io.sarl.docs.reference.ar.Cap").mustHaveFeatures(1)
				s.features.get(0).mustBeAction("action", null, 0, false)
				model.elements.get(2).mustBeEvent("SomeEvent", null).mustHaveFeatures(0)
				var a = model.elements.get(3).mustBeAgent("MyAgent", null).mustHaveFeatures(2)
				a.features.get(0).mustBeBehaviorUnit("io.sarl.core.Initialize", false)
				a.features.get(1).mustBeBehaviorUnit("io.sarl.docs.reference.ar.SomeEvent", false)
			}
	
			/* The built-in capacities are accessible in the same way
			 * as the other capacities, with the getters.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Lifecycle
					event SomeEvent",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.core.Lifecycle", false, false, false)
				model.mustHaveTopElements(2)
				model.elements.get(0).mustBeEvent("SomeEvent", null).mustHaveFeatures(0)
				var a = model.elements.get(1).mustBeAgent("MyAgent", null).mustHaveFeatures(1)
				a.features.get(0).mustBeBehaviorUnit("io.sarl.docs.reference.ar.SomeEvent", false)
			}

			/* Invoking a capacity/skill with the getter method is
			 * not user-friendly. Since the
			 * [General Syntax Reference](./GeneralSyntaxReferenceSpec.html)
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
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Initialize
					capacity Cap {
						def action
					}
					skill Ski implements Cap {
						def action { println(\"Action\") }
					}
					event SomeEvent",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.core.Initialize", false, false, false)
				model.mustHaveTopElements(4)
				var c = model.elements.get(0).mustBeCapacity("Cap").mustHaveFeatures(1)
				c.features.get(0).mustBeActionSignature("action", null, 0, false)
				var s = model.elements.get(1).mustBeSkill("Ski", null, "io.sarl.docs.reference.ar.Cap").mustHaveFeatures(1)
				s.features.get(0).mustBeAction("action", null, 0, false)
				model.elements.get(2).mustBeEvent("SomeEvent", null).mustHaveFeatures(0)
				var a = model.elements.get(3).mustBeAgent("MyAgent", null).mustHaveFeatures(3)
				a.features.get(0).mustBeCapacityUses("io.sarl.docs.reference.ar.Cap")
				a.features.get(1).mustBeBehaviorUnit("io.sarl.core.Initialize", false)
				a.features.get(2).mustBeBehaviorUnit("io.sarl.docs.reference.ar.SomeEvent", false)
			}
	
			/* The built-in capacities are accessible in the same way
			 * a the other capacities, with the extension methods.
			 * 
			 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
				'''.parsesSuccessfully(
					"package io.sarl.docs.reference.ar
					import io.sarl.core.Lifecycle
					event SomeEvent",
					// TEXT
					""
				)
				model.mustHavePackage("io.sarl.docs.reference.ar")
				model.mustHaveImports(1)
				model.mustHaveImport(0, "io.sarl.core.Lifecycle", false, false, false)
				model.mustHaveTopElements(2)
				model.elements.get(0).mustBeEvent("SomeEvent", null).mustHaveFeatures(0)
				var a = model.elements.get(1).mustBeAgent("MyAgent", null).mustHaveFeatures(2)
				a.features.get(0).mustBeCapacityUses("io.sarl.core.Lifecycle")
				a.features.get(1).mustBeBehaviorUnit("io.sarl.docs.reference.ar.SomeEvent", false)			}

		}

}
