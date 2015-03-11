/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
import io.sarl.lang.core.EventSpace
import io.sarl.lang.core.Space
import io.sarl.util.OpenEventSpace
import io.sarl.util.RestrictedAccessEventSpace
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

/* @outline
 *
 * This document describes the features related to the definition of a space in SARL.
 * Before reading this document, it is recommended reading
 * the [General Syntax Reference](GeneralSyntaxReferenceSpec.html),
 * and the [Agent Reference](AgentReferenceSpec.html).
 * 
 * One of the key elements that characterize and differentiate the main 
 * multi-agent approaches is how interactions between agents are described.
 * Some researchers focus on agent-to-agent interactions and corresponding 
 * protocols. Within organizational approaches, some consider the 
 * organization as a static partition of agents where agents interact in 
 * groups through the roles they play. Others focus on dynamic 
 * organizations and normative aspects. 
 * 
 * Another essential aspect of the interaction is the interaction 
 * Agent-Environment, especially in agent-based simulations.
 * Each of these trends of multi-agent systems has led to numerous 
 * fruitful and innovative contributions.
 * To remain generic, SARL therefore not imposes a single way of 
 * describing the interaction among agents, but rather attempt to 
 * provide means to implement each of these approaches.
 * 
 * It is in this perspective that the concepts of `Space`
 * and `SpaceSpecification` were defined.
 * 
 * __A Space is the support of the interaction between agents respecting 
 * the rules defined in a Space Specification.__
 * 
 * __A Space Specification defines the rules (including action and
 * perception) for interacting within a given set of spaces respecting 
 * this specification.__
 * 
 * SARL natively defines a particular type of `Space` called
 * *Event Space* to provide a support to event-driven interactions.
 * Within an event space, agents communicate using events, the 
 * [built-in capacity `DefaultContextInteractions`](BuiltInCapacityReferenceSpec.html)
 * provides the agent with the means to emit and receive events, respectively 
 * using the `emit` actions and the `on` keyword
 * in behavior definition.
 * A __Default Space__ is precisely an event space.
 * 
 * Within an event space, the notion of `Scope` enables to 
 * precisely control/filter the potential recipients of an event.
 * __A Scope is a predicate used to filter the potentially called 
 * listeners for a given event.__
 * The most basic scope is represented by a collection of addresses.
 */
@CreateWith(SARLSpecCreator)
describe "Space Reference" {
		
		@Inject extension SARLParser

		/* SARL provides a collection of Java interfaces that are representing different
		 * types of spaces.
		 */	
		describe "Types of Spaces" {
			
			/* SARL provides a Java interface that is representing all
			 * the spaces:
			 * 
			 *      public interface Space {
			 * 		    public SpaceID getID();
			 * 		    public SynchronizedSet<UUID> getParticipants();
			 *      }
			 *
			 * 
			 * The `getID` function replies the identifier of the space.
			 * The `getParticipants` function replies the identifiers
			 * of the agents belonging to the space.
			 *  
			 * @filter(.*) 
			 */
			fact "Space" {
				// Test the URLs from the beginning of the page
				"GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
				"AgentReferenceSpec.html" should beAccessibleFrom this
				"BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
				//
				typeof(Space) => [
					it should extend _
					it should haveNbMembers 2
					it should haveMethod "getID : io.sarl.lang.core.SpaceID"
					it should haveMethod "getParticipants : io.sarl.lang.util.SynchronizedSet"
				]
			}
			
			/* Spaces that are based on event propagation mechanism are defined
			 * as:
			 * 
			 *      public interface EventSpace extends Space {
			 * 		    public Address getAddress(UUID id);
			 * 		    public void emit(Event event, Scope<Address> scope);
			 * 		    public void emit(Event event);
			 *      }
			 *
			 * 
			 * The `getAddress` function replies the address in the space
			 * of the agent that has the given identifier.
			 * The `emit` functions permits fire of an event in
			 * the space.
			 *  
			 * @filter(.*) 
			 */
			fact "Event Space" {
				typeof(EventSpace) => [
					it should extend "io.sarl.lang.core.Space"
					it should haveNbMembers(3)
					it should haveMethod "getAddress(java.util.UUID) : io.sarl.lang.core.Address"
					it should haveMethod "emit(io.sarl.lang.core.Event, io.sarl.lang.core.Scope)"
					it should haveMethod "emit(io.sarl.lang.core.Event)"
				]
			}

			/* Event spaces that are allowing the agents to be register 
			 * and unregister are "open event spaces":
			 * 
			 *      public interface OpenEventSpace extends EventSpace {
			 * 		    public Address register(EventListener entity);
			 * 		    public Address unregister(EventListener entity);
			 *      }
			 *
			 * 
			 * The functions `register` and `unregister`
			 * permits an agent to be involved or not.
			 *  
			 * @filter(.*) 
			 */
			fact "Open Event Space" {
				typeof(OpenEventSpace) => [
					it should extend "io.sarl.lang.core.EventSpace"
					it should haveNbMembers(2)
					it should haveMethod "register(io.sarl.lang.core.EventListener) : io.sarl.lang.core.Address"
					it should haveMethod "unregister(io.sarl.lang.core.EventListener) : io.sarl.lang.core.Address"
				]
			}

			/* When an event space needs to control the registration access,
			 * it should be a "restricted access event space":
			 * 
			 *      public interface RestrictedAccessEventSpace extends EventSpace {
			 * 		    public Address register(EventListener entity, Principal requester);
			 * 		    public <P extends EventListener & Principal> Address register(P entity);
			 * 		    public Address unregister(EventListener entity);
			 *      }
			 *
			 * 
			 * The functions given by this type of space permits implementing
			 * a space with restricted access, based on the standard Java API.
			 *  
			 * @filter(.*) 
			 */
			fact "Restricted Access Event Space" {
				typeof(RestrictedAccessEventSpace) => [
					it should extend "io.sarl.lang.core.EventSpace"
					it should haveNbMembers(3)
					it should haveMethod "register(io.sarl.lang.core.EventListener,java.security.Principal) : io.sarl.lang.core.Address"
					it should haveMethod "register(io.sarl.lang.core.EventListener) : io.sarl.lang.core.Address"
					it should haveMethod "unregister(io.sarl.lang.core.EventListener) : io.sarl.lang.core.Address"
				]
			}

		}
	
		/* The definition of a new space must be done with
		 * the [Xtend](https://www.eclipse.org/xtend/) or the Java language.
		 * In the rest of this documentation, we use the Xtend language.
		 * It is an object-oriented language with a syntax close to the
		 * one of SARL.
		 * 
		 * For defining a space, three steps must be followed:
		 * 
		 *  * Definition of the interface of the space;
		 *  * Implementation of the space on a specific runtime environment;
		 *  * Definition of the space specification.
		 * 
		 * In the rest of this section, we use the example of the definition
		 * of a physic space: a space in which objects are located. 
		 */
		describe "Defining a Space" {

			/* The first step for the definition of a new type of space is
			 * the specification of the Java interface that is describing
			 * the functions provided by the space.
			 * 
			 * The new space type must extend one of the predefined types.
			 * In the following example, the new space is related to
			 * the physic environment in which the agents may evolve.
			 * 
			 *      interface PhysicSpace extends Space {
			 * 		    def moveObject(UUID identifier, float x, float y, float z)
			 * 		    def bindBody(EventListener agent)
			 * 		    def unbindBody(EventListener agent)
			 *      }
			 *
			 * This space permits to move an object (including the physical
			 * representation of the agent, its body).
			 * Additionally, the space gives to the agent the ability to be binded
			 * to its body, and to release the control of its body.
			 * The `EventListener` type is the event listening mechanism associated
			 * to the agent. It may be obtained with the `Behaviors` built-in capacity
			 * (see the corresponding
			 * [built-in capacity reference](./BuiltInCapacityReferenceBehaviorsSpec.html)
			 * for details).
			 * 
			 * @filter(.*) 
			 */
			fact "Defining a Space"{
				"./BuiltInCapacityReferenceBehaviorsSpec.html" should beAccessibleFrom this
			}
			
			/* The definition of the space implementation depends upon
			 * the runtime environment.
			 * 
			 * <caution>This section of the space reference document may evolved
			 * in future releases of SARL. Please activate the "deprecated feature
			 * use" warning in your Java compilation configuration for ensuring
			 * that you will be notified about any major changes on this part of
			 * the API.</caution>
			 * 
			 * Below, the implementation
			 * extends one of the abstract classes provided by the
			 * [Janus Platform](http://www.janusproject.io).
			 * 
			 *      class PhysicSpaceImpl extends AbstractSpace implements PhysicSpace {
			 * 		    val Map<UUID,PhysicObject> entities = newTreeMap
			 * 		    new(UUID id) {
			 * 			    super(id)
			 * 		    }
			 * 		    def moveObject(UUID identifier, float x, float y, float z) {
			 * 			    synchronized (this.entities) {
			 *					PhysicObject o = this.entities.get(identifier)
			 * 			    	if (o !== null) {
			 * 				    	o.move(x, y, z)
			 * 			    	}
			 * 				}
			 * 		    }
			 * 			def bindBody(EventListener listener) {
			 * 				synchronized (this.entities) {
			 *					entities.put(listener.ID, new PhysicObject)
			 * 				}
			 * 			}
			 * 			def unbindBody(EventListener listener) {
			 * 				synchronized (this.entities) {
			 *					entities.remove(listener.ID)
			 * 				}
			 * 			}
			 *      }
			 * 
			 * The physic space contains a collection of objects, namely `entities`.
			 * Each object is identified by an UUID.
			 * It is assumed that the `PhysicObject` class provides a method for moving it:
			 * `move(float, float, float)`.
			 * When an agent wants to move an object by calling the `moveObject` method,
			 * the space is retrieving the instance of this object in the `entities`, and
			 * move it. 
			 * 
			 * <important>The previous implementation has a major problem: it does not permit
			 * to distribute the information and the interaction objects over a computer network. The space is
			 * the support of the interaction. Consequently, it should provide the mechanisms for
			 * routing the events to all the agents other the computer network.</important>
			 * 
			 * @filter(.*) 
			 */
			fact "Basic Implementation"{
				true
			}

			/* As described in the previous section, the space implementation
			 * should route the information among the agents over a computer
			 * network.
			 * 
			 * <caution>This section of the space reference document may evolved
			 * in future releases of SARL. Please activate the "deprecated feature
			 * use" warning in your Java compilation configuration for ensuring
			 * that you will be notified about any major changes on this part of
			 * the API.</caution>
			 * 
			 * Below, the implementation
			 * extends one of the abstract classes provided by the
			 * [Janus Platform](http://www.janusproject.io).
			 * 
			 *      class NetworkPhysicSpaceImpl extends AbstractEventSpace implements PhysicSpace {
			 * 			val Map<UUID,PhysicObject> entities;
			 *			public new(SpaceID id, DistributedDataStructureService factory) {
			 *				super(id, factory)
			 * 				this.entities = factory.getMap(id.toString + "-physicObjects")
			 *			}
			 *			def bindBody(EventListener entity) {
			 * 				this.entities.put(entity.ID, new PhysicObject)
			 *				Address a = new Address(ID, entity.ID)
			 *				synchronized (this.participants) {
			 *					return this.participants.registerParticipant(a, entity)
			 *				}
			 *			}
			 *			def unbindBody(EventListener entity) {
			 *				synchronized (this.participants) {
			 *					return this.participants.unregisterParticipant(entity)
			 *				}
			 * 				this.entities.remove(entity.ID)
			 *			}
			 * 		    def moveObject(UUID identifier, float x, float y, float z) {
			 * 			    PhysicObject o = this.entities.remove(identifier)
			 * 			    if (o !== null) {
			 * 				    o.move(x, y, z)
			 * 					this.entities.put(identifier, o)
			 * 			    }
			 * 		    }
			 *      }
			 * 
			 * <important>The collection of the physic objects is a distributed
			 * map over the computer network. It means that each node
			 * of the platform has a direct access to the objects' instances.
			 * If whose to say that is implementation may face some problems,
			 * such as the serialization of the physic objects, and the scalability
			 * of the distributed map.</important>
			 * 
			 * @filter(.*) 
			 */
			fact "Basic Network Support"{
				true
			}

			/* For creating instances of spaces, it is necessary to define
			 * a space specification.
			 * This specification may create the space instance according to rules, or
			 * provide information and rules to the spaces.
			 * 
			 *     class PhysicSpaceSpecification implements SpaceSpecification<PhysicSpace> {
			 *         def PhysicSpace create(SpaceID id, Object... params) {
			 * 	           return new PhysicSpaceImpl(id)
			 * 	       }
			 *     }
			 * 
			 * The example above is the specification related to the first implementation
			 * of the PhysicSpace.
			 *
			 *     class NetworkPhysicSpaceSpecification implements SpaceSpecification<PhysicSpace> {
			 * 	       @Inject
			 * 		   var DistributedDataStructureService factory;
			 * 
			 *         def PhysicSpace create(SpaceID id, Object... params) {
			 * 	           return new NetworkPhysicSpaceImpl(id, factory)
			 * 	       }
			 *     }
			 * 
			 * The example above is the specification that permits to create a physic space
			 * with networking. It retrieves by injection the factory of distributed data structures
			 * provided by the Janus platform.
			 * 
			 * @filter(.*) 
			 */
			fact "Defining a SpaceSpecification"{
				true
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
