/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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
package io.sarl.docs.tutorials

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.*
import io.sarl.core.Logging
import io.sarl.core.DefaultContextInteractions
import io.sarl.eclipse.runtime.SREConstants

/* @outline
 *
 * <p>This document describes how to create a SARL Run-time Environment (SRE) with the
 * <a href="https://github.com/gallandarakhneorg/tinymas">tinyMAS platform</a>.
 *
 * <p>SRE executes or interprets compiled SARL code on an "hardware platform."
 * The figure below illustrates the compilation process of a SARL program in which the
 * run-time environment is involved.
 * 
 * <p>![SARL Generation Process](http://www.sarl.io/images/compilation-process.png)
 * 
 * <p>The Tiny Multiagent Platform (tinyMAS) is a very small software platform, which permits to implement
 * and run agent-based systems. This platform was written by Stéphane GALLAND and Nicolas GAUD for the
 * multiagent courses of the Computer Science Department of the
 * <a href="http://www.utbm.fr">Belfort-Montbéliard University of Technology</a>.
 * 
 * <p>The purpose of this document is to describe the basics steps for making a SRE without
 * changing the source code of neither the platform nor the SARL compiler.
 * The tinyMAS SRE does not support all the elements of the SARL metamodel. Indeed the support for
 * external contexts and inner contexts is not implemented.
 * 
 * <p>Before reading this document, it is recommended reading
 * the [General Syntax Reference](../reference/GeneralSyntaxReferenceSpec.html).
 * 
 * <p><div class="bt-download">
 * <a href="https://github.com/gallandarakhneorg/tinymas"><img alt="Download the Source Code" src="%website%/images/download-icon.png"/></a>
 * </div>
 * The elements that are explained in this tutorial are:
 * 
 *  * the definition of the concepts from the SARL metamodel linked to the tinyMAS concepts;
 *  * the definition of the definition of the boot process;
 *  * the update of the manifest in order to make tinyMAS recognized as a SRE.
 *
 * <p>The source code related to this tutorial may be found
 * in the [tinyMAS Git repository](https://github.com/gallandarakhneorg/tinymas).
 */
@CreateWith(SARLSpecCreator)
describe "Creating a SARL Run-time Environment for the tinyMAS platform"{

	@Inject extension SARLParser

	/* The SARL language and the tinyMAS platform have been defined based on a collection
	 * of concepts that are formally described in their respective metamodels.
	 * This section gives a short overview of the two metamodels, and their possible links.
	 * 
	 * <p>Defining the links between the SARL metamodel and the metamodel of the SRE is the first
	 * mandatory point for transforming the targeted platform to SRE.
	 */
	describe "SARL and tinyMAS metamodels" {

		/* The SARL language is based on a collection of concepts that are described into its metamodel.
		 * 
		 * <p>![Part of the SARL Metamodel](./SARL_metamodel.png)
		 * 
		 * The key elements in the SARL metamodel are:
		 * 
		 *  * `Context`: defines a set of interaction spaces in which agents are involved.
		 *  * `Space`: or interaction space; defines a space in which interactions may occur.
		 *  * `Address`: defines the identifiers of the agents in the interaction spaces.
		 *  * `EventSpace`: defines a specific interaction space in which agents are interacting by firing and receiving events.
		 *  * `Event`: defines an information/event that is exchanged by agents in an event space.
		 *  * `Capacity`: defines a know-how of the agent; actions defines in a capacity could be invoked by the agent.
		 *  * `Skill`: defines a specific implementation of a capacity. When a capacity's action is invoked by an agent, the corresponding implementation in the skill is called.
		 *  * `Behavior`: defines a sub-part of the agent's behavior.
		 *  * Built-in capacities: the capacities that are known by the agents by default. The corresponding skills are provided by the run-time environment.
		 * 
		 * @filter(.*)
		 */
		fact "SARL Metamodel" {
			// Test the URLs in the introduction
			"http://www.sarl.io/images/compilation-process.png" should beURL "!file" 
			"http://www.utbm.fr" should beURL "!file"
			"https://github.com/gallandarakhneorg/tinymas" should beURL "!file"
			"../reference/GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
			// The checks are valid only if the macro replacements were done.
			// The replacements are done by Maven.
			// So, Eclipse Junit tools do not make the replacements.
			System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
			//
			"%website%" should beURL "!file"
			//
			"./SARL_metamodel.png" should beAccessibleFrom this
		}

		/* The tinyMAS platform is based on a collection of concepts that are described into its metamodel.
		 * 
		 * <p>![Part of the tinyMAS Metamodel](./tinyMAS_metamodel.png)
		 * 
		 * The key elements in the tinyMAS metamodel are:
		 * 
		 *  * `AgentIdentifier`: defines the identifiers of the agents.
		 *  * `Agent`: defines the concept of agent. Application agents must be defined in sub-classes.
		 *  * `Message`: defines the base information exchanged by the agents.
		 *  * `MessageTransportService` (and `AgentCommunicationChannel`): define the routing mechanism for the messages.
		 *  * `MailboxManager`: defines a container of received messages for each agent.
		 *  * `WhitePages`: defines a repository of all the agents in the system. It maps the agent identifiers to the agents.
		 *  * `YellowPages`: defines a repository of services that could be provided by the agents. It maps the name of a service to a list of agent identifiers.
		 *  * `Scheduler`: defines the agent execution policy.
		 *  * `Kernel`: represents the entire tinyMAS platform. 
		 *  * `KernelListener`: describes an object that could be notified when a specific event occured in the tinyMAS kernel. 
		 *
		 * @filter(.*)
		 */
		fact "TinyMAS Metamodel" {
			"./tinyMAS_metamodel.png" should beAccessibleFrom this
		}

		/* The elements to define for creating a SRE are described in the following table.
		 * This table provides the mapping of these elements (several SARL concepts) to
		 * the corresponding implementation approach for tinyMAS.
		 *
		 * The first column of the table gives the key point to consider for creating a SRE.
		 * The second column provides the key SARL concepts.
		 * And the third column describes the basics of the implementation for tinyMAS.
		 *
		 * <table><thead>
		 * <tr><th>SARL Element</th><th>SARL Concept</th><th>tinyMAS Implementation</th></tr>
		 * </thead><tbody>
		 * <tr><td>Identifying the agents</td>
		 * 			<td>`Address`</td>
		 * 			<td>tinyMAS provides the concept of `AgentIdentifier`.
		 * 				This identifier contains a reference to the kernel identifier, and
		 * 				the UUID of the agent. The SARL Address is for a specific interaction
		 * 				space. It contains the space identifier, and the UUID of the agent.
		 * 				For making the implementation simple, we assume that there is
		 * 				only the SARL default space available. The other spaces will not
		 * 				be supported by a tinyMAS implementation.</td></tr>
		 * <tr><td>Agent abstraction</td>
		 * 			<td>`Agent`</td>
		 * 			<td>A specific implementation of the tinyMAS `Agent` should be coded.
		 * 				This implementation must have a reference to the SARL `Agent`.
		 * 				Additionally, the tinyMAS `Agent` will be the place to write
		 * 				the support for the agent life-cycle and the built-in capacities.</td></tr>
		 * <tr><td>Exchanged information format for direct interaction</td>
		 * 			<td>`Event`</td>
		 * 			<td>The interaction in the tinyMAS platform is based on the concept of `Message`.
		 * 				tinyMAS provides all the features for routing and delivering the messages.
		 * 				Because the SARL agents are exchanging events by default, the tinyMAS
		 * 				cannot be directly delivered to the SARL agents. Linking the `Message`
		 * 				and `Event` concepts is done by considering that each `Event` occurrence
		 * 				must be enveloped by a `Message`, i.e. the content of a tinyMAS message
		 * 				is a SARL event.</td></tr>
		 * <tr><td>Receiving SARL events</td>
		 * 			<td>Behavior units `on`</td>
		 * 			<td>A specific module in tinyMAS must be written for invoking the
		 * 				SARL behavior units on each SARL event that are in the received
		 * 				tinyMAS messages.</td></tr>
		 * <tr><td>Default context definition</td>
		 * 			<td>`Context`</td>
		 * 			<td>In SARL, every agent exists in a context, named the default context.
		 * 				It is mandatory for tinyMAS to provide an implementation for the
		 * 				default context.</td></tr>
		 * <tr><td>Default space definition</td>
		 * 			<td>`EventSpace`</td>
		 * 			<td>In SARL, every agent could interact through the default interaction
		 * 				space. It is mandatory for tinyMAS to provide an implementation for the
		 * 				default space.</td></tr>
		 * <tr><td>Agent life-cycle</td>
		 * 			<td></td>
		 * 			<td>The agent life-cycle in tinyMAS is based on the calls to the functions
		 * 				`start` for initialization, `live` for running the agent behavior, and
		 * 				`end` for destroying the agent.
		 * 				The agent life-cycle in SARL is different. It is based on the receiving
		 * 				of events. The `Initialize` event represents the initialization of the agent.
		 * 				The `Destroy` event represents the agent destruction. The other events
		 * 				enable to create the agent behavior.
		 * 				Because the tinyMAS agent is the one that will be executed by tinyMAS,
		 * 				it is mandatory to generates the SARL events in the three tinyMAS life-cycle
		 * 				functions, a.k.a. `start`, `live`, `end`.</td></tr>
		 * <tr><td>Managing platform events</td>
		 * 			<td>`AgentSpawned`, `AgentKilled`</td>
		 * 			<td>Several SARL events are assumed to be fired by the SRE. The two events
		 * 				that will be supported by tinyMAS are `AgentSpawned` for agent spawning,
		 * 				and `AgentKilled` for agent destruction.
		 * 				The other SARL platform events are ignored by tinyMAS, e.g. `ContextJoined`,
		 * 				`ContextLeft`, `MemberJoined`, `MemberLeft`.</td></tr>
		 * <tr><td>Agent Spawning</td>
		 * 			<td>`spawn` functions</td>
		 * 			<td>A specific agent spawning function must be written for creating
		 * 				a tinyMAS agent that is embedding a SARL agent definition.
		 * 				This spawning functions will be invoked by the built-in capacities
		 * 				(see below).</td></tr>
		 * <tr><td>Built-in capacity implementation</td>
		 * 			<td><a href="../reference/BuiltinCapacityReferenceSpec.html">BICs</a></td>
		 * 			<td>It is assumed that every SARL agent always contains the skills for a
		 * 			specific set of capacities, named the built-in capacities. These
		 * 			skills are supposed to be provided by the SRE.
		 * 			Consequently, each built-in capacity must have an implementation based
		 * 			on the tinyMAS features.</td></tr>
		 * <tr><td>SRE Booting</td>
		 * 			<td></td>
		 * 			<td>An utility class for booting the tinyMAS SRE must be provided.
		 * 				Usually, this boot class takes the fully qualified name of the
		 * 				agent to launch, and the parameters to give to this agent.
		 * 				The boot class is supposed to set up the tinyMAS platform for
		 * 				being used as a SRE, and to launch SARL agents.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*)
		 */
		fact "Linking the metamodels' concepts" {
			"../reference/BuiltInCapacityReferenceSpec.html" should beAccessibleFrom this
		}

	}
	
	/* `AgentIdentifier` in tinyMAS is based on a UUID and a reference to the identifier of the kernel.
	 * Because `AgentIdentifier` does not provide a function for retrieving the UUID, we
	 * should write an utility class for obtaining it.
	 *
	 * <p>The `AgentIdentifier` string representation contains the UUID, followed by a column character
	 * and the kernel identifier.

	 * <p>This utility class will be used for extracting the UUID of the tinyMAS agent identifier
	 * in order to create SARL address on the fly.
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "Mapping of the identifiers" {
		'''
		final class Identifiers {
		
			static def toUUID(aid : AgentIdentifier) : UUID {
				val aidstr = aid.toString
				val index = aidstr.indexOf(":")
				return UUID::fromString(aidstr.substring(0, index))
			}
		
		}
		'''.parseSuccessfully(
		'''
		package io.sarl.docs.tutorials.tinyMASSRE
		import java.util.UUID
		interface AgentIdentifier {}
		''',
		// TEXT
		'''
		''')
	}

	/* A SARL Context defines the boundary of a sub-system, and gathers a collection of interaction Spaces.
	 * In each context, there is at least one particular space called the default space to which all agents in this
	 * context belong. This ensures the existence of a common shared space to all agents in the same context.
	 * <p>Each agent can then create specific public or private spaces to achieve its personal goals.
	 * Since their creation, agents are incorporated into a context called the default context.
	 *
	 * <p>The concept of context is not explicit in tinyMAS, i.e. all agents are evolving in the same and unique
	 * context.
	 * 
	 * <p>Because SARL agents require to have access to the instance of the default context, a specific
	 * implementation must be provided upon the tinyMAS API.
	 */
	describe "Definition of the default context" {

		/* A SARL context must be a class that is implementing the `AgentContext` type
		 * (provided in the SARL API).
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Definition of the context class" {
			'''
			class TMAgentContext implements io.sarl.lang.core.AgentContext {
				// The code in the rest of this section will appear here
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			abstract
			''',
			// TEXT
			'''
			''')
		}

		/* A SARL context must have a unique identifier.
		 * We assume that only one context, the default context, will exist in
		 * the SARL applications ran with tinyMAS.
		 * 
		 * <p>In order to retrieve easy the UUID of the default (root) context,
		 * we define the identifier as a public constant.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Definition of the context identifier" {
			'''
			public static val TINYMAS_AGENT_CONTEXT_ID = UUID::fromString("cdb0d568-4059-40cf-96c4-d078fee91cb1")

			def getID : UUID {
				TINYMAS_AGENT_CONTEXT_ID
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import io.sarl.lang.core.AgentContext
			abstract class TMAgentContext implements AgentContext {
			''',
			// TEXT
			'''
			}
			''')
		}

		/* A SARL context has a default interaction space.
		 * 
		 * <p>We declare the default space in the context class `TMAgentContext`.
		 * The concrete definition of the default space class named `TMDefaultSpace`
		 * is done later in this document.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Declaration of the default space of the context" {
			'''
			var defaultSpace : TMDefaultSpace

			def getDefaultSpace : EventSpace {
				this.defaultSpace
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import io.sarl.lang.core.AgentContext
			import io.sarl.lang.core.EventSpace
			interface TMDefaultSpace extends EventSpace {}
			abstract class TMAgentContext implements AgentContext {
			''',
			// TEXT
			'''
			}
			''')
		}

		/* A SARL context provides a collection of functions for retrieving the spaces inside the context.
		 *
		 * <p>Please note that we assume that only one context will exist in the tinyMAS application.
		 * This context will be assumed to be the default context.
		 * In the same way, we assume that only one space (the default space) will exist in the default
		 * context.
		 *
		 * <p>Consequently, the function `getSpace()` replies the collection of all the context as
		 * an singleton collection instance that is containing the default space.
		 * This function must reply a auto-synchronized collection. We use the `Collections3` utility
		 * class, provided in the SARL API, for creating the synchronized collection.
		 * The first parameter of the `synchronizedCollection` function is the collection to synchronized,
		 * the second parameter is the object on from which the synchronization token will be obtained.
		 *
		 * <p>The `getSpaces(Class)` function is supposed to reply the existing spaces that were created
		 * by using the given type of space specification.
		 * Because of our assumption on the space singleton in the context, this function replies
		 * only the default space if the given space specification type is `EventSpaceSpecification`.
		 * 
		 * <p>The `getSpace(UUID)` function replies the default space only if the given UUID is the
		 * identifier of the default space. In the other cases, the function replies nothing.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Definition of the getter functions for spaces" {
			'''
			def getSpaces : SynchronizedCollection<? extends Space> {
				Collections3::synchronizedCollection(Collections::singleton(this.defaultSpace), this)
			}

			def getSpaces(spec : Class<? extends SpaceSpecification<S>>)
					: SynchronizedCollection<S>
					with S extends Space {
				if (spec !== null && spec == typeof(EventSpaceSpecification)) {
					return Collections3::synchronizedCollection(Collections::singleton(this.defaultSpace as S), this);
				}
				return Collections3::synchronizedCollection(Collections::emptyList, this)
			}

			def getSpace(spaceUUID : UUID) : S
			 		with S extends Space {
				if (spaceUUID == this.defaultSpace.spaceID.ID) {
					return this.defaultSpace as S
				}
				return null
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import java.util.Collections
			import io.sarl.lang.core.AgentContext
			import io.sarl.lang.core.Space
			import io.sarl.lang.core.SpaceSpecification
			import io.sarl.lang.core.EventSpaceSpecification
			import io.sarl.lang.util.SynchronizedCollection
			import io.sarl.util.Collections3
			abstract class TMAgentContext implements AgentContext {
				var defaultSpace : Space
			''',
			// TEXT
			'''
			}
			''')
		}

		/* The context provides functions for creating spaces.
		 * Because we assume that no additional space could be created upon the tinyMAS
		 * SRE implementation, all these functions generates an "unsupported operation"
		 * exception.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Definition of the creation functions for spaces" {
			'''
			def createSpace(spec : Class<? extends SpaceSpecification<S>>,
					spaceUUID : UUID, creationParams : Object*)
					: S
					with S extends Space {
				throw new UnsupportedOperationException
			}

			def getOrCreateSpaceWithSpec(spec : Class<? extends SpaceSpecification<S>>,
					spaceUUID : UUID,creationParams : Object*)
					: S
					with S extends Space {
				throw new UnsupportedOperationException
			}

			def getOrCreateSpaceWithID(spaceUUID : UUID,
					spec : Class<? extends SpaceSpecification<S>>,
					creationParams : Object*)
					: S
					with S extends Space {
				throw new UnsupportedOperationException
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import io.sarl.lang.core.AgentContext
			import io.sarl.lang.core.Space
			import io.sarl.lang.core.SpaceSpecification
			abstract class TMAgentContext implements AgentContext {
			''',
			// TEXT
			'''
			}
			''')
		}

		/* The constructor of the agent context class must be defined for initializing
		 * the fields of the class.
		 *
		 * <p>The instance of the default space must be provided as parameter in order to
		 * set the `defaultSpace` field.
		 * 
		 * <p>Additionally, the default space instance must be linked to the context
		 * (see the definition of the space class below). In the constructor,
		 * the `setAgentContext` is invoked on the default space. 
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Definition of the constructor" {
			'''
			new (defaultSpace : TMDefaultSpace) {
				this.defaultSpace = defaultSpace
				this.defaultSpace.setAgentContext(this);
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import io.sarl.lang.core.AgentContext
			interface TMDefaultSpace {
				def setAgentContext(context : AgentContext)
			}
			abstract class TMAgentContext implements AgentContext {
				var defaultSpace : TMDefaultSpace
			''',
			// TEXT
			'''
			}
			''')
		}

	}

	/* The default space is the interaction space in which all the agents will be involved.
	 * Because we assume that only one agent context exists in the system, the default space
	 * becomes a singleton (only one default space could exist in a context).
	 */
	describe "Definition of the default space" {
		
		/* A SARL default space must support an event-based interaction mechanism.
		 * Consequently, the default space class must implement the `EventSpace` interface
		 * that is provided by the SARL API.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Definition of the space class" {
			'''
			class TMDefaultSpace implements io.sarl.lang.core.EventSpace {
				// The code in the rest of this section will appear here
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			abstract
			''',
			// TEXT
			'''
			''')
		}

		/* A SARL space must have a unique identifier.
		 * We assume that only one context, the default context, will exist in
		 * the SARL applications ran with tinyMAS. Consequently, the default
		 * space instance becomes a singleton.
		 * 
		 * <p>In order to retrieve easy the UUID of the default (root) context,
		 * we define the identifier as a public constant.
		 * 
		 * <p>We defines the function `getSpaceID`, which is replying the space identifier.
		 *
		 * <p>The initialization of the `spaceID` field is not discussed yet, because
		 * the context identifier is mandatory for building the space identifier.
		 * The space identifier creation is discussed in the following section.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Definition of the context identifier" {
			'''
			public static val TINYMAS_DEFAULT_SPACE_ID = UUID::fromString("1db39309-8be7-4809-ad76-1ede6e792296")

			var spaceID : SpaceID

			def getSpaceID : SpaceID {
				this.spaceID
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import io.sarl.lang.core.EventSpace
			import io.sarl.lang.core.SpaceID
			abstract class TMDefaultSpace implements EventSpace {
			''',
			// TEXT
			'''
			}
			''')
		}

		/* We need to have a reference to the agent context that is containing this space in order
		 * to create the space identifier and to provide a reference to this context from the space.
		 * 
		 * <p>Firstly, the reference to the agent context is declared as a weak reference field, named
		 * `context`.
		 *
		 * <p>The initialization of the `context` field is done by the `setAgentContext` function.
		 * As illustrated in the context definition section, this function is invoked when creating
		 * the agent context in order to be binded to its default space.
		 * 
		 * <p>The space identifier is initialized in the `setAgentContext` because this is the place
		 * where the space's UUID and the context's identifier are known.
		 *
		 * <p>Finally, the getter function for retrieving the agent context is defined too. 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Linking the agent context to the space" {
			'''
			var context : WeakReference<TMAgentContext>

			package def setAgentContext(context : TMAgentContext) {
				this.context = new WeakReference(context)
				this.spaceID = new SpaceID(context.ID, TINYMAS_DEFAULT_SPACE_ID, null)
			}

			def getAgentContext : TMAgentContext {
				this.context.get
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import java.lang.ref.WeakReference
			import io.sarl.lang.core.EventSpace
			import io.sarl.lang.core.SpaceID
			interface TMAgentContext {
				def getID : UUID
			}
			abstract class TMDefaultSpace implements EventSpace {
				var TINYMAS_DEFAULT_SPACE_ID : UUID
				var spaceID : SpaceID
			''',
			// TEXT
			'''
			}
			''')
		}

	}

	/* The next step is the definition of an abstraction for the SARL agent that could be
	 * executed as a tinyMAS agent.
	 * The easiest way to proceed is to create a tinyMAS agent class that has a reference
	 * to the SARL agent. Then, the specific tinyMAS agent implementation will map all
	 * the features from the tinyMAS platform to their equivalent features for the SARL agent.
	 * 
	 */
	describe "First definition of the agent" {

		/* We define a specific tinyMAS agent type, which is named `TMSarlAgent`.
		 * This agent definition contains a reference to the instance of the SARL agent definition,
		 * in the field `sarlAgent`. The getter of the SARL agent is also defined.
		 *
		 * <p>We define the `getID` function in order to easily retrieve the unique identifier of the
		 * agent. This unique identifier is the identifier of the SARL agent.
		 *
		 * <p>A constructor is defined for initializing the `sarlAgent` field.
		 * 
		 * <caution>The constructor will be refined and redefined in the rest of this document.</caution>
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "SARL Agent inside tinyMAS Agent" {
			'''
			class TMSarlAgent extends org.arakhne.tinyMAS.core.Agent {

				val sarlAgent : io.sarl.lang.core.Agent

				protected def getSarlAgent : io.sarl.lang.core.Agent {
					this.sarlAgent
				}

				def getID : UUID {
					this.sarlAgent.ID
				}

				new (sarlAgent : io.sarl.lang.core.Agent) {
					this.sarlAgent = sarlAgent
				}

			}
			'''.parseSuccessfully(
			'''
			package org.arakhne.tinyMAS.core
			import java.util.UUID
			abstract class Agent {
			}
			abstract 
			''',
			// TEXT
			'''
			''')
		}

		/* Event-based interaction is at the heart of the interaction mechanisms that could
		 * be used by the SARL agent. The `TMSarlAgent` class must provide tools for
		 * routing events when the basic interaction mechanism of tinyMAS is based on messages.
		 *
		 * <p>The first feature to implement is the firing/routing of SARL event.
		 * We define the `fireEvent` function that is extracting the SARL agent's behavior units
		 * (the `on` blocks of code), and is calling them with the event given as parameter.
		 * 
		 * <p>Retrieving the SARL behavior unit is a algorithm that is following the SARL specifications.
		 * Fortunately, the SARL API provides an utility class for exploring the Java definition
		 * of an agent (by using the Java reflection mechanism), and providing the list of the
		 * behavior units that are taken a given event as input.
		 * The utility class is named `BehaviorGuardEvaluatorRegistry`. According to the SARL
		 * API, one instance of this registry may be created for each agent.
		 * Consequently, we created a final field named `evaluatorRegistry` that references
		 * the `BehaviorGuardEvaluatorRegistry` instance.
		 *
		 * <p>The first line of `fireEvent` retrieves the list of the behavior units that
		 * are defined in the SARL agent (and any internal behavior) for the given event.
		 *
		 * <p>The second part of the `fireEvent` function goes through the behavior units
		 * for evaluating there guards. The function `evaluateGuard` evaluates the guard
		 * of the behavior unit on the given event, and, if the guard is true,
		 * it fills the given list of handlers with a call to the behavior unit's code. 
		 *
		 * <p>The third part of the `fireEvent` function invokes the behavior units' code
		 * that have a true guard.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Firing SARL events" {
			'''
			val evaluatorRegistry = new BehaviorGuardEvaluatorRegistry
			
			package def fireEvent(^event : Event) {
				var evaluators = this.evaluatorRegistry.getBehaviorGuardEvaluators(^event)

				var handlers : Collection<Runnable> = new ArrayList
				for (evaluator : evaluators) {
					evaluator.evaluateGuard(^event, handlers);
				}
			
				for (handler : handlers) {
					handler.run();
				}
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import java.util.Collection
			import java.util.ArrayList
			import io.sarl.lang.core.Event
			interface BehaviorGuardEvaluator {
				def evaluateGuard(e : Event, handlers : Collection<Runnable>)
			}
			class BehaviorGuardEvaluatorRegistry {
				def getBehaviorGuardEvaluators(e : Event) : Collection<BehaviorGuardEvaluator> { null }
			}
			abstract class TMSarlAgent {
			''',
			// TEXT
			'''
			}
			''')
		}

		/* For enabling the `BehaviorGuardEvaluatorRegistry` instance to retrieve the behavior units
		 * of the SARL agent (and any internal behavior), we must register the SARL agent
		 * as a provider of behavior units to the `BehaviorGuardEvaluatorRegistry` instance.
		 *
		 * <p>The easier way to proceed is to register the SARL agent when the tinyMAS agent is starting;
		 * and to unregister the SARL agent when the tinyMAS agent is stopping.
		 * These two life-cycle functions are supported by the `start` and `stop` functions in the tinyMAS
		 * platform.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Registering the SARL agent as behavior unit provider" {
			'''
			def start {
				super.start
				this.evaluatorRegistry.register(getSarlAgent())
			}

			def stop {
				this.evaluatorRegistry.unregister(getSarlAgent())
				super.stop
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import java.util.Collection
			import io.sarl.lang.core.Event
			interface BehaviorGuardEvaluator {
				def evaluateGuard(e : Event, handlers : Collection<Runnable>)
			}
			abstract class TinyMASAgent {
				def start {}
				def stop {}
			}
			class BehaviorGuardEvaluatorRegistry {
				def register(e : Object) {}
				def unregister(e : Object) {}
			}
			abstract class TMSarlAgent extends TinyMASAgent {
				val evaluatorRegistry = new BehaviorGuardEvaluatorRegistry
				def getSarlAgent : Object { null }
			''',
			// TEXT
			'''
			}
			''')
		}

		/* For enabling the receiving of SARL events, we need to define a specific SARL event listener.
		 * The easier way is to implement the `EventListener` interface from the SARL API (see the code
		 * below).
		 *
		 * <p>The implementation of the `EventListener` interface implies to implement the
		 * `receiveEvent` function. This function is called by the SARL infrastructure each time
		 * an event must be treated by the agent, i.e. when the agent receives the event.
		 *
		 * <p>The function `receiveEvent` calls the already implemented function `fireEvent`.
		 * Nevertheless, the `receiveEvent` function must ensure that the given event has
		 * a source, i.e. the address of the sender of the event.
		 * If the event has no source, the value of the source is forced to the address of the current
		 * SARL agent.
		 * 
		 * <p>In order to create the address of the current agent, we must know the
		 * address of the agent in the default interaction space, and consequently, the default context
		 * in which the agent exists.
		 * We defined the `defaultSpace` field and the corresponds getter function for storing the
		 * default space of the agent.
		 * For initializing this field, we redefine the constructor.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Receiving SARL events" {
			'''
			class TMSarlAgent extends org.arakhne.tinyMAS.core.Agent implements io.sarl.lang.core.EventListener {

				// [...]

				val defaultSpace : WeakReference<TMDefaultSpace>

				protected def getDefaultSpace : TMDefaultSpace {
					this.defaultSpace.get
				}

				def receiveEvent(^event : Event) {
					if (^event.source === null) {
						^event.source = getDefaultSpace.getAddress(getSarlAgent.ID)
					}
					fireEvent(^event)
				}

				new (defaultSpace : TMDefaultSpace, sarlAgent : io.sarl.lang.core.Agent) {
					this.defaultSpace = new WeakReference(defaultSpace)
					this.sarlAgent = sarlAgent
				}

			}
			'''.parseSuccessfully(
			'''
			package org.arakhne.tinyMAS.core
			import java.lang.ref.WeakReference
			import java.util.UUID
			import io.sarl.lang.core.Event
			import io.sarl.lang.core.Address
			interface TMDefaultSpace {
				def getAddress(id : UUID) : Address
			}
			abstract class Agent {
				protected var sarlAgent : Object
				abstract def getSarlAgent : io.sarl.lang.core.Agent
				abstract def fireEvent(e : Event)
			}
			abstract 
			''',
			// TEXT
			'''
			''')
		}

		/* According to the SARL specification, two events must be fired for supporting the
		 * agents' life-cycle. The first event corresponds to the initialization of the agent,
		 * the `Initialize` event. And, the second event corresponds to the destruction of the
		 * agent, the `Destroy` event.
		 *
		 * <p>The tinyMAS implementation of the SRE must fire these two events according
		 * to the tinyMAS agent life-cycle. In this platform, the starting of the agents
		 * is supported by the `start` function. And the destruction of the agents is supported
		 * by the `stop` function.k
		 *
		 * <p>For firing the `Initialize` event, we need to create an instance of this event into
		 * the `start` function. But, from the SARL API documentation, the `Initialize` event
		 * must take the initialization parameters to pass to the agent at its start-up.
		 * For supporting these initialization parameters, we define the `parameters` field that is
		 * initialized in the agent type constructor.
		 * Then, the `start` function is updated for setting the parameters of the `Initialize` event;
		 * and for firing the event with a call to the `receiveEvent` function.
		 *
		 * <p>By symmetry, the `Destroy` event is initialize and fired in the `stop` function of the
		 * agent type.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Initialize and Destroy events" {
			'''
			var spawnerID : UUID
			var parameters : Object[]
			
			new (defaultSpace : TMDefaultSpace, sarlAgent : io.sarl.lang.core.Agent, spawnerID : UUID, parameters : Object[]) {
				this.defaultSpace = new WeakReference(defaultSpace)
				this.sarlAgent = sarlAgent
				this.spawnerID = spawnerID
				this.parameters = parameters
			}

			def start {
				super.start
				this.evaluatorRegistry.register(getSarlAgent())
				
				var initializeEvent = new Initialize(this.spawnerID, this.parameters)
				this.spawnerID = null
				this.parameters = null
				receiveEvent(initializeEvent)
			}

			def stop {
				receiveEvent(new Destroy)
				this.evaluatorRegistry.unregister(getSarlAgent())
				super.stop
			}
				'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import java.lang.ref.WeakReference
			import java.util.Collection
			import io.sarl.lang.core.Event
			import io.sarl.core.Initialize
			import io.sarl.core.Destroy
			interface BehaviorGuardEvaluator {
				def evaluateGuard(e : Event, handlers : Collection<Runnable>)
			}
			abstract class TinyMASAgent {
				def start {}
				def stop {}
			}
			class BehaviorGuardEvaluatorRegistry {
				def register(e : Object) {}
				def unregister(e : Object) {}
			}
			interface TMDefaultSpace {}
			abstract class TMSarlAgent extends TinyMASAgent {
				val defaultSpace : WeakReference<TMDefaultSpace>
				var sarlAgent : Object
				val evaluatorRegistry = new BehaviorGuardEvaluatorRegistry
				def getSarlAgent : Object { null }
				def receiveEvent(e : Event) {}
			''',
			// TEXT
			'''
			}
			''')
		}

	}

	/* Spawning agents is a key feature of the execution platform.
	 * This feature is used for booting the initial agent, and by the agent built-in capacities
	 * for creating new agents.
	 *
	 * <p>In order to provide a reusable spawning function, we define the `Spawner` utility class
	 * that contains the static definition of the spawning functions of SARL agents on the tinyMAS
	 * platform.  
	 */
	describe "Tool for spawning agents" {

		/* The first utility function that we define is `createAgent`.
		 * It enables to create an instance of `TMSarlAgent` from a SARL agent type.
		 *
		 * <p>The parameters of the functions are the ones required for building an instance of a SARL agent:
		 *
		 *  * `defaultSpace`: the instance of the default space in which the SARL agent will interact.
		 *  * `agentType` : the type of SARL agent to create.
		 *  * `spawerID` : the identifier of the agent's spanwer, or <code>null</code> if the platform has spawned the agent.
		 *  * `parentID` : the identifier of the agent's parent, usually the identifier of the default context in the timeMAS SRE.
		 *  * `agentID` : the identifier to give to the created agent, or <code>null</code> if the identifier must be randomly selected.
		 *  * `params` : the initialization parameters to pass to the created SARL agent.
		 * 
		 * <p>The function replies the created tinyMAS agent that is binded to the SARL agent.
		 * 
		 * <p>The code of the SARL agent creation is based on the call to the SARL agent constructor, as
		 * defined in the SARL specification. By default, the SARL agent agents have a
		 * constructor with three parameters:
		 * 
		 *   * first parameter of type `BuiltinCapacitiesProvider`: a provider of built-in capacity. Here we pass <code>null</code> as argument to the constructor in order to ignore the default initialization of the built-in capacities. This initialization will be done manually in one of the following sections.
		 *   * second parameter of type `UUID`: the identifier of the parent context of the created agent.
		 *   * third parameter of type `UUID`: the identifier of the created agent.
		 *
		 * <p>The `createAgent` function should ensure the given agent type could be executed on the current instance of
		 * the tinyMAS platform. For that, we introduce two tests:
		 *
		 *   * check if the given agent type is a sub-type of the `Agent` type provided in the SARL API; and
		 *   * use the `SarlSpecificationChecker` provided in the SARL API for verifying that the given agent type was generated with a SARL specification version that is compatible with the version supported by tinyMAS. We use the default implementation of the checker: `StandardSarlSpecificationChecker`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Creating a tinyMAS agent from a SARL agent" {
			'''
			static val SPECIFICATION_CHECKER : SarlSpecificationChecker = new StandardSarlSpecificationChecker
			
			static def createAgent(
					defaultSpace : TMDefaultSpace, 
					agentType : Class<? extends io.sarl.lang.core.Agent>,
					spawnerID : UUID,
					parentID : UUID,
					agentID : UUID,
					params : Object*)
					: TMSarlAgent {
				if (typeof(io.sarl.lang.core.Agent).isAssignableFrom(agentType)
					&& SPECIFICATION_CHECKER.isValidSarlElement(agentType)) {
					var theAgentID = if (agentID === null) UUID::randomUUID else agentID
					var theAgentType = agentType as Class<? extends io.sarl.lang.core.Agent>
					var cons = theAgentType.getConstructor(typeof(BuiltinCapacitiesProvider), typeof(UUID), typeof(UUID))
					var sarlAgent = cons.newInstance(null, parentID, theAgentID)
					var tmAgent = new TMSarlAgent(defaultSpace, sarlAgent, spawnerID, params)
					return tmAgent
				}
				return null
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import io.sarl.sarlspecification.SarlSpecificationChecker
			import io.sarl.sarlspecification.StandardSarlSpecificationChecker
			interface BuiltinCapacitiesProvider {}
			interface TMDefaultSpace {}
			class TMSarlAgent {
				new (a : TMDefaultSpace, b : io.sarl.lang.core.Agent, c : Object*) {
				}
			}
			class Spawner {
			''',
			// TEXT
			'''
			}
			''')
		}

		/* Now we have a function for creating a tinyMAS-SARL agent, it is necessary to provide
		 * an utility function for launching this tinnyMAS-SARL agent on the tinyMAS kernel.
		 * 
		 * <p>We define the following `spawn` function thats takes as parameter the timasMAS
		 * kernel instance, and the agent to launch.
		 * 
		 * <p>The function create the agent identifier of the agent in the tinyMAS platform, from
		 * the identifier of the kernel, and the SARL agent identifier.
		 *
		 * <p>Finally, the function invokes the agent launching function of the tinyMAS kernel.
		 * This function 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Launching a tinyMAS-SARL agent on the tinyMAS kernel" {
			'''
			static def spawn(
					kernel : Kernel,
					^agent : TMSarlAgent) {
				var tmid = new AgentIdentifier(kernel.kernelId, ^agent.ID.toString)
				kernel.addAgent(tmid, ^agent)
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			class TMSarlAgent {
				public var ID : UUID
			}
			class AgentIdentifier {
				new (a : UUID, b : String) { }
			}
			class Kernel {
				public var kernelId : UUID
				def addAgent(a : AgentIdentifier, b : TMSarlAgent) { }
			}
			class Spawner {
			''',
			// TEXT
			'''
			}
			''')
		}

		/* Now, we could define a general spawning function that invokes the two previously
		 * defined functions.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "General utility function for spawning agents" {
			'''
			static def spawn(
					kernel : Kernel,
					defaultSpace : TMDefaultSpace, 
					agentType : Class<? extends io.sarl.lang.core.Agent>,
					spawnerID : UUID,
					parentID : UUID,
					agentID : UUID,
					params : Object*) : UUID {
				var ^agent = createAgent(defaultSpace, agentType, spawnerID, parentID, agentID, params)
				if (^agent !== null) {
					spawn(kernel, ^agent)
					return ^agent.ID
				}
				return null
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			interface Kernel  {}
			interface TMSarlAgent {
				def getID : UUID
			}
			interface TMDefaultSpace {}
			class Spawner {
				static def createAgent(
					^space : TMDefaultSpace,
					agentType : Class<? extends io.sarl.lang.core.Agent>,
					spanwerID : UUID,
					parentID : UUID,
					agentID : UUID,
					params : Object*) : TMSarlAgent {
					null
				}
				static def spawn(
					kernel : Kernel,
					^agent : TMSarlAgent) {
				}
			''',
			// TEXT
			'''
			}
			''')
		}

	}

	/* One of the key principle in the SARL specification is that each SARL agent is
	 * provided with a collection of built-in capacities that are provided by the run-time
	 * environment.
	 * In this section, we define the built-in capacities that are implemented with the
	 * tinyMAS API.
	 *
	 * <caution>In the following code, we assume that the given implementation is
	 * an inner class of the `TMSArlAgent` class. In this way, we will be able to access
	 * to the features of the agent type: `getId`, `getDefaultSpace`, `killMe`.
	 */
	describe "Definition of the built-in capacities" {

		/* The easiest built-in capacity to implement is the `Logging` capacity.
		 * This capacity enables the SARL agent to log messages on the output console.
		 *
		 * <p>The basic principle for implementing a built-in capacity is to create
		 * a class extending the `Skill` class, and implementing the capacity to
		 * implement, in this case the `Logging` capacity.
		 *
		 * <p>There is not particular issue with the coding of this buil-in capacity.
		 * The code is self-explaining.
		 *  
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Definition of the Logging skill" {
			'''
			class LoggingSkill extends Skill implements Logging {

				def setLoggingName(message : String) {
					getId.stringRepresentation = message
				}

				def isErrorLogEnabled : boolean {
					true
				}

				def isWarningLogEnabled : boolean {
					true
				}

				def isInfoLogEnabled : boolean {
					true
				}

				def isDebugLogEnabled : boolean {
					true
				}

				def getLogLevel : int {
					0
				}

				def setLogLevel(level : int) {
				}


				def error(message : Object, exception : Throwable = null, parameters : Object*) {
					System::out.println("[" + getId.getString + "] ERROR: " + message)
					if (exception !== null) {
						exception.printStackTrace(System::out)
					}
				}

				def warning(message : Object, exception : Throwable = null, parameters : Object*) {
					System::out.println("[" + getId.getString + "] WARNING: " + message)
					if (exception !== null) {
						exception.printStackTrace(System::out)
					}
				}

				def info(message : Object, parameters : Object*) {
					System::out.println("[" + getId.getString + "] INFO: " + message)
				}

				def debug(message : Object, parameters : Object*) {
					System::out.println("[" + getId.getString + "] DEBUG: " + message)
				}

			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import io.sarl.core.Logging
			interface AgentIdentifier {
				def setStringRepresentation(a : String)
				def getString() : String
			}
			abstract class Skill {
				def getId : AgentIdentifier { null }
				def println(message : Object) {	}
			}
			''',
			// TEXT
			'''
			''')
			typeof(Logging) should haveDeprecatedMethod "println(java.lang.Object)"
		}

		/* The `Lifecycle` capacity is one of the must used capacity.
		 * It provides the support of the SARL agent life-cycle.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		describe "Definition of the Lifecycle skill" {
			
			/* Consider the agent execution mechanism in the tinyMAS platform:
			 * inside an infinite loop, each agent is run. This algorithmical
			 * principle may be described by the following algorithm:
			 * 
			 * ```
			 * while (true) {
			 *    for(a : whitePages.allAgents) {
			 *       a.live
			 *    }
			 *    refreshKernelState
			 * } 
			 * ```
			 * The tinyMAS platform is designed for updating the kernel state
			 * after all the agent have been ran.
			 * Consequently, the tinyMAS platform does not support the creation
			 * of agents during the execution of another agent.
			 * If an agent spawns another agent, the real initialization of the
			 * spawned agent must be delayed until the `refreshKernelState` is invoked.
			 *
			 * <p>This particular design of the tinyMAS platform is at the opposite of
			 * the standard spawning principle in SARL: in SARL the agents are spawn
			 * when the spawning function is called.
			 *
			 * <p>For solving this issue, we need to implement a buffer of spawned
			 * agents, that will be filled by the SARL spawning functions, and consumed
			 * by the `refreshKernelState` function.
			 * 
			 * <p>The simplest place where to put this code in the `TMDefaultSpace` type.
			 * Indeed, we could assume that the agent spawning always occurs in the default space.
			 * We update the `TMDefaultSpace` class as described in the following code. 
			 *
			 * <p>We define the `agentToLaunch` field as the spawn agent buffer.
			 *
			 * <p>The `spawn` function creates the agent instance, based on the `Spawner` utility class,
			 * and adds the created agent into the buffer.
			 *
			 * <p>The consumption of the spawn agent buffer is supported by the `consumeAgentToLaunch`
			 * function. This function replies the buffer content, and clears the buffer.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Spawning agents in the default space" {
				'''
				var agentsToLaunch : List<TMSarlAgent> = new ArrayList

				def spawn(
						anAgent : Class<? extends io.sarl.lang.core.Agent>,
						spawnerID : UUID,
						agentID : UUID,
						params : Object*)
						: UUID {
					val ^agent = Spawner::createAgent(
							this,
							anAgent,
							spawnerID,
							TINYMAS_DEFAULT_SPACE_ID,
							agentID,
							params)
					this.agentsToLaunch += ^agent
					return ^agent.ID
				}

				def consumeAgentToLaunch : Iterable<TMSarlAgent> {
					var iterable = this.agentsToLaunch
					this.agentsToLaunch = new ArrayList
					return iterable
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.UUID
				import java.util.List
				import java.util.ArrayList
				interface TMSarlAgent {
					def getID : UUID
				}
				class Spawner {
					static def createAgent(a : TMDefaultSpace,
						b :  Class<? extends io.sarl.lang.core.Agent>,
						z : UUID, c : UUID, d : UUID, e : Object*) : TMSarlAgent { null }
				}
				class TMDefaultSpace {
					static var TINYMAS_DEFAULT_SPACE_ID : UUID
				''',
				// TEXT
				'''
				}
				''')
			}

			/* The functions for spawning the agents delegate the spawning to
			 * the default space (as defined in the previous section).
			 * 
			 * <p>The `killMe` function enables to stop the agent which is invoking
			 * this function. The agent killing is delegated to the tinyMAS agent killing
			 * function, also named `killMe`. The `owner` is a syntactic simplification of
			 * a call to `getOwner`, which replies the agent owning the skill.
			 *
			 * <p>One important point regarding the expected behavior of the SARL `killMe` is
			 * that is must never return from the point of view of its caller.
			 * For simulating this behavior, we could use the exception mechanism by throwing
			 * a specific internal exception that is not a real error but
			 * represents the "no-return code" case.
			 * This specific exception is named `NoReturnCodeException`, and is defined
			 * in the next section.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "First definition of the skill" {
				'''
				class LifecycleSkill extends Skill implements Lifecycle {

					def spawnInContext(
							agentClass : Class<? extends io.sarl.lang.core.Agent>,
							context : AgentContext,
							params : Object*)
							: UUID {
						if (context.ID == defaultSpace.agentContext.ID) {
							return defaultSpace.spawn(agentClass, owner.ID, null, params)
						}
						return null
					}

					def spawnInContextWithID(
							agentClass : Class<? extends io.sarl.lang.core.Agent>,
							agentID : UUID,
							context : AgentContext,
							params : Object*)
							: UUID {
						if (context.ID == defaultSpace.agentContext.ID) {
							return defaultSpace.spawn(agentClass, owner.ID, agentID, params)
						}
						return null
					}

					def killMe {
						(owner as TMSarlAgent).killMe
						throw new NoReturnCodeException
					}

				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.UUID
				import io.sarl.core.Lifecycle
				import io.sarl.lang.core.AgentContext
				interface AgentIdentifier {	}
				interface TMDefaultSpace {
					def getAgentContext : AgentContext
					def spawn(
						anAgent : Class<? extends io.sarl.lang.core.Agent>,
						agentID : UUID,
						params : Object*)
						: UUID
				}
				interface TMSarlAgent {
					def getID : UUID
					def killMe
				}
				class NoReturnCodeException extends RuntimeException { }
				abstract class Skill {
					def getId : AgentIdentifier { null }
					def getDefaultSpace : TMDefaultSpace { null }
					def killMe { }
					def getOwner : TMSarlAgent { null }
				}
				''',
				// TEXT
				'''
				''')
			}

			/* The exception that is simulating a no-return-code is defined as:
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Support of the no-return functions" {
				'''
				class NoReturnCodeException extends RuntimeException {
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				''',
				// TEXT
				'''
				''')
			}

			/* For avoiding the tinyMAS platform to stop because the `NoReturnCodeException`
			 * was not caught, we must redefined the `fireEvent` function in the `TMSarlAgent`
			 * type. Indeed, this function is the one which is running the code of the SARL event
			 * handlers in which the SARL `killMe` function could be invoked.
			 *
			 * <p>Each call to a piece of SARL code is enclosing by a try-catch statement that
			 * is simply ignoring the `NoReturnCodeException` exception.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Catching the no-return-code exception" {
				'''
				package def fireEvent(^event : Event) {
					var evaluators = this.evaluatorRegistry.getBehaviorGuardEvaluators(^event)

					var handlers : Collection<Runnable> = new ArrayList
					for (evaluator : evaluators) {
						try {
							evaluator.evaluateGuard(^event, handlers);
						} catch (e : NoReturnCodeException) {
						}
					}
				
					for (handler : handlers) {
						try {
							handler.run()
						} catch (e : NoReturnCodeException) {
						}
					}
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.Collection
				import java.util.ArrayList
				import io.sarl.lang.core.Event
				class NoReturnCodeException extends RuntimeException {
				}
				interface BehaviorGuardEvaluator {
					def evaluateGuard(e : Event, handlers : Collection<Runnable>)
				}
				class BehaviorGuardEvaluatorRegistry {
					def getBehaviorGuardEvaluators(e : Event) : Collection<BehaviorGuardEvaluator> { null }
				}
				class TMSarlAgent {
					var evaluatorRegistry : BehaviorGuardEvaluatorRegistry
				''',
				// TEXT
				'''
				}
				''')
			}

		}

		/* The `DefaultContextInteractions` capacity enables the agent to have
		 * interaction in the default space, and to spawn agents in the default
		 * context.
		 */
		describe "Definition of the DefaultContextInteractions skill" {

			/* The first and incomplete definition of the `DefaultContextInteractions` capacity
			 * for the tinyMAS platform is provided below.
			 * 
			 * <p>The functions that replies the default context and the default space are defined
			 * for replying the context and space that are referenced in the agent.
			 * The `owner` is a syntactic simplification of a call to `getOwner`, which replies
			 * the agent owning the skill.
			 *
			 * <p>The `getAddress` function replies the address of the owner in the default space.
			 * Its behavior is delegated to the `getAddress(UUID)` of the default space.
			 * 
			 * <p>The `isDefaultContext` functions implemented in order to test if their parameters correspond
			 * to the default context.
			 *
			 * <p>Similarly, the `isDefaultSpace` functions are implemented in order to test if their parameters correspond
			 * to the default space.
			 *
			 * <p>The `isInDefaultSpace` function replies <code>true</code> if the given event is fired
			 * into the default space. The function behavior is delegated to the `isDefaultSpace` function
			 * with the event source identifier as argument.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "First definition of the skill" {
				'''
				class DefaultContextInteractionsSkill extends Skill implements DefaultContextInteractions {

					def getDefaultContext : AgentContext {
						((owner as TMSarlAgent).defaultSpace as TMDefaultSpace).agentContext
					}

					def getDefaultSpace : EventSpace {
						(owner as TMSarlAgent).defaultSpace
					}

					def getDefaultAddress : Address {
						var o = owner as TMSarlAgent
						o.defaultSpace.getAddress(o.sarlAgent.ID)
					}

					def isDefaultContext(context : AgentContext) : boolean {
						context.ID == (defaultSpace as TMDefaultSpace).agentContext.ID
					}

					def isDefaultContext(contextID : UUID) : boolean {
						contextID == (defaultSpace as TMDefaultSpace).agentContext.ID
					}

					def isDefaultSpace(^space : Space) : boolean {
						^space.ID == defaultSpace.spaceID
					}

					def isDefaultSpace(^space : SpaceID) : boolean {
						^space == defaultSpace.spaceID
					}

					def isDefaultSpace(^space : UUID) : boolean {
						^space == defaultSpace.spaceID.ID
					}

					def isInDefaultSpace(^event : Event) : boolean {
						isDefaultSpace(^event.source.spaceId)
					}

				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.UUID
				import io.sarl.lang.core.Address
				import io.sarl.lang.core.AgentContext
				import io.sarl.lang.core.Event
				import io.sarl.lang.core.EventSpace
				import io.sarl.lang.core.Space
				import io.sarl.lang.core.SpaceID
				import io.sarl.core.DefaultContextInteractions
				interface TMDefaultSpace extends EventSpace {
					def getAgentContext : AgentContext
				}
				interface TMSarlAgent {
					def getDefaultSpace : TMDefaultSpace
					def getSarlAgent : io.sarl.lang.core.Agent
				}
				abstract class Skill {
					def getOwner : TMSarlAgent { null }
				}
				abstract 
				''',
				// TEXT
				'''
				''')
			}

			/* Firing events in the default space could be done by calling the `emit`
			 * and `willReceive` functions.
			 *
			 * <p>The `emit` function fires the given event into the default space.
			 * Optionally, a scope could be provided for restricting the set of the
			 * event's receivers. The `emit` function delegates its behavior to
			 * the `emit` function of the default space.
			 * 
			 * <p>One import point in the implementation of the `emit` function is the
			 * check of the event's source. Indeed, sometimes, the given event has no source
			 * address set. Because the default space could not set this address when it is
			 * <code>null</code>, we must check this case and force the value of the event source
			 * address to the address of the current agent.
			 *
			 * <p>The `willReceive` function is a inverted version of the syntactic call to the
			 * firing function. This function enables the developer to send the event
			 * to a specific agent with the following syntax:
			 * ```
			 * receverID.willReceive(^event)
			 * ``` 
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Firing events in the default space" {
				'''
				def emit(^event : Event, scope : Scope<Address> = null) {
					if (^event.source === null) {
						^event.source = defaultSpace.getAddress(owner.ID)
					}
					defaultSpace.emit(^event, scope)
				}

				def willReceive(receiver : UUID, ^event : Event) {
					emit(^event, Scopes::addresses(defaultSpace.getAddress(receiver)))
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.UUID
				import io.sarl.lang.core.Address
				import io.sarl.lang.core.Agent
				import io.sarl.lang.core.Event
				import io.sarl.lang.core.EventSpace
				import io.sarl.lang.core.Scope
				import io.sarl.core.DefaultContextInteractions
				import io.sarl.util.Scopes
				abstract class DefaultContextInteractionsSkill implements DefaultContextInteractions {
					def getDefaultSpace : EventSpace { null }
					def getOwner : Agent
				''',
				// TEXT
				'''
				}
				''')
				typeof(DefaultContextInteractions) should haveDeprecatedMethod "receive(java.util.UUID,io.sarl.lang.core.Event)"
			}

			/* Spawning an agent in the default context could be done by calling the `spawn` function of
			 * the `DefaultContextInteractions` capacity.
			 * This function delegates its behavior to the `spawn` function that is already defined
			 * in the tinyMAS-SARL default space class.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Spawning agents in the default context" {
				'''
				def spawn(agentType : Class<? extends io.sarl.lang.core.Agent>, params : Object*) : UUID {
					(defaultSpace as TMDefaultSpace).spawn(agentType, owner.ID, null, params)
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.UUID
				import io.sarl.lang.core.EventSpace
				import io.sarl.lang.core.Agent
				import io.sarl.core.DefaultContextInteractions
				interface TMDefaultSpace extends EventSpace {
					def spawn(agentType : Class<? extends io.sarl.lang.core.Agent>,
						spawnerID : UUID, agentID : UUID, params : Object*) : UUID
				}
				abstract class DefaultContextInteractionsSkill implements DefaultContextInteractions {
					def getDefaultSpace : EventSpace { null }
					def getOwner : Agent { null }
				''',
				// TEXT
				'''
				}
				''')
			}

		}

		/* The `Behaviors` capacity enables the agent to have
		 * sub-behaviors that could be dynamically added and removed.
		 */
		describe "Definition of the Behaviors skill" {

			/* The first and incomplete definition of the `Behaviors` capacity
			 * for the tinyMAS platform is provided below.
			 * 
			 * <p>The first function implemented is the `asEventListener` function.
			 * It replies the object that could serve as a SARL event listener for
			 * the agent. In the tinyMAS implementation, we have already implemented
			 * the `EventListener` interface in the `TMSarlAgent` class. In other word,
			 * the tinyMAS agent type is already a SARL event listener by itself.
			 * Consequently, the `asEventListener` function replies the owner of the skill,
			 * i.e. the agent. 
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "First definition of the skill" {
				'''
				class BehaviorsSkill extends Skill implements Behaviors {
					
					def asEventListener : EventListener {
						owner as TMSarlAgent
					}

				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import io.sarl.core.Behaviors
				import io.sarl.lang.core.EventListener
				interface TMSarlAgent extends EventListener {
				}
				abstract class Skill {
					def getOwner : io.sarl.lang.core.Agent
				}
				abstract 
				''',
				// TEXT
				'''
				''')
			}

			/* A `Behavior` instance could be registered in order to be run for the current agent.
			 * The `registerBehavior` function enables the SARL developer to add a `Behavior`
			 * instance into the registered sub-behaviors of the agent.
			 *
			 * <p>In order to store the list of the sub-behavior, we must define the
			 * `behaviors` field. The content of this field is increased by the `registerBehavior`
			 * function.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Registering a behavior" {
				'''
				val behaviors : List<Behavior> = new ArrayList

				def registerBehavior(attitude : Behavior) : Behavior {
					if (attitude !== null) {
						this.behaviors += attitude
					}
					return attitude
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.List
				import java.util.ArrayList
				import io.sarl.lang.core.Behavior
				import io.sarl.core.Behaviors
				abstract class BehaviorsSkill implements Behaviors {
				''',
				// TEXT
				'''
				}
				''')
			}

			/* The `unregisterBehavior` function enables the SARL developer to remove a `Behavior`
			 * instance from the registered sub-behaviors of the agent.
			 * The content of the `behaviors` field defined in the previous section is decreased
			 * with the given attitude.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Unregistering a behavior" {
				'''
				def unregisterBehavior(attitude : Behavior) : Behavior {
					if (attitude !== null) {
						this.behaviors -= attitude
					}
					return attitude
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.List
				import java.util.ArrayList
				import io.sarl.lang.core.Behavior
				import io.sarl.core.Behaviors
				abstract class BehaviorsSkill implements Behaviors {
					val behaviors : List<Behavior> = new ArrayList
				''',
				// TEXT
				'''
				}
				''')
			}

			/* The SARL specification indicates that it is possible for an agent to fire an event
			 * inside its internal context. In other words, the event will be received by
			 * the agent, and its internal behaviors, and not by the other agents.
			 *
			 * <p>The `wake` function is provided for supporting this feature.
			 * Its implementation retrieves the SARL event listener of the agent by calling
			 * the `asEventListener` function. And, it invokes the receiving function of
			 * the listener with the event as argument.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Waking the behaviors with an event" {
				'''
				def wake(^event : Event) {
					asEventListener.receiveEvent(^event)
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import io.sarl.lang.core.Event
				import io.sarl.core.Behaviors
				abstract class BehaviorsSkill implements Behaviors {
				''',
				// TEXT
				'''
				}
				''')
			}

			/* There is an key difference between the SARL run-time principle, which is fully
			 * distributed (everything is registered, run, unregistered in parallel),
			 * and the tinyMAS run-time principle, which is fully sequential (everything is
			 * controlled in a big virtual loop).
			 * Consequently, it is mandatory to map the fully-distributed point-of-view
			 * from the SARL specification to the sequential point-of-view from tinyMAS.
			 */
			describe "Updating the tinyMAS agent life-cycle for (un)registering the behaviors" {

				/* In order to map the SARL behavior support to tinyMAS platform,
				 * the `BehaviorsSkill` type must store the registered and unregistered behaviors
				 * separately.
				 * Indeed, the registered behaviors must become SARL event listeners in the agent context.
				 * And, the unregistered behaviors must be SARL event listener anymore.
				 *
				 * <p>In order to separate these two subsets of behaviors, we define the two fields
				 * `registrationWaiters` and `unregistrationWaiters`, which represent the behaviors
				 * that are waiting for event listening registration, and event listener unregistration,
				 * respectively.
				 *
				 * <p>We define the `getRegistrationWaiters` and `getUnregistrationWaiters` functions
				 * for replying and consuming these subsets. These functions will be invoked later,
				 * from the tinyMAS agent living function. 
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Defining buffers of behaviors" {
					'''
					var registrationWaiters : List<Object> = new ArrayList

					var unregistrationWaiters : List<Object> = new ArrayList

					def getRegistrationWaiters : Iterable<Object> {
						val collection = this.unregistrationWaiters
						this.unregistrationWaiters = new ArrayList
						return collection
					}

					def getUnregistrationWaiters : Iterable<Object> {
						val collection = this.registrationWaiters
						this.registrationWaiters = new ArrayList
						return collection
					}
					'''.parseSuccessfully(
					'''
					package io.sarl.docs.tutorials.tinyMASSRE
					import java.util.List
					import java.util.ArrayList
					import io.sarl.lang.core.Behavior
					import io.sarl.core.Behaviors
					abstract class BehaviorsSkill implements Behaviors {
					''',
					// TEXT
					'''
					}
					''')
				}

				/* The `registerBehavior` and `unregisterBehavior` functions that are
				 * already defined in the `BehaviorsSkill` type must be redefined in order
				 * to include the updating of the behavior's buffers.
				 *
				 * <p>In the `registerBehavior` function, the registering attitude (behavior)
				 * is added into the set of behaviors that are waiting for SARL event listening
				 * registration.
				 * 
				 * <p>The registering attitude is also remove from the set of behaviors that are
				 * waiting for SARL event listening unregistration. It avoids to have registration
				 * and unregistration at the same time for the same attitude.
				 *
				 * <p>In the `unregisterBehavior` function, the unregistering attitude (behavior)
				 * is added into the set of behaviors that are waiting for SARL event listening
				 * unregistration. And, it is removed from the set of whose waiting for registration.

				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Updating the buffers of behaviors" {
					'''
					def registerBehavior(attitude : Behavior) : Behavior {
						if (attitude !== null) {
							this.behaviors += attitude
							this.registrationWaiters += attitude
							this.unregistrationWaiters -= attitude
						}
						return attitude
					}

					def unregisterBehavior(attitude : Behavior) : Behavior {
						if (attitude !== null) {
							this.behaviors -= attitude
							this.registrationWaiters -= attitude
							this.unregistrationWaiters += attitude
						}
						return attitude
					}
					'''.parseSuccessfully(
					'''
					package io.sarl.docs.tutorials.tinyMASSRE
					import java.util.List
					import java.util.ArrayList
					import io.sarl.lang.core.Behavior
					import io.sarl.core.Behaviors
					abstract class BehaviorsSkill implements Behaviors {
						var behaviors : List<Behavior>
						var registrationWaiters : List<Object>
						var unregistrationWaiters : List<Object>
					''',
					// TEXT
					'''
					}
					''')
				}

				/* In order to map the (un)registered the SARL behaviors at the correct
				 * instant during the tinyMAS agent life-cycle, we must define the
				 * agent living function (`live`) in the `TMSarlAgent` type.
				 *
				 * <p>We assume that the `behaviorsSkill` field is defined and set
				 * (Section <a href="#Creating_built-in_capacity_instances_in_the_agent">Creating
				 * built-in capacity instances in the agent</a> explains how to proceed).
				 * This field contains a reference to the `BehaviorsSkill` skill owned by the agent.
				 *
				 * <p>We assume that the `evaluatorRegistry` field is defined, and contains
				 * a reference to the `BehaviorGuardEvaluatorRegistry` instance that is used
				 * for managing the agent event bus (see the previous sections for its definition).
				 *
				 * <p>The `live` function is implemented in order to retrieve the behaviors, which
				 * are waiting for SARL event listening (un)registration. For each behavior,
				 * the (un)registration function on the `BehaviorGuardEvaluatorRegistry` instance
				 * is called.
				 *
				 * <p>Putting this code in the `live` function ensures that the agent's behaviors
				 * are (un)registered at the beginning of each step of the agent's life, and not
				 * in parallel to the other agent's tasks. 
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Updating the tinyMAS agent living function" {
					'''
					class TMSarlAgent extends Agent implements EventListener {

						// [...]

						var behaviorsSkill : BehaviorsSkill

						def live {
							for (beh : this.behaviorsSkill.registrationWaiters) {
								this.evaluatorRegistry.register(beh)
							}
							for (beh : this.behaviorsSkill.unregistrationWaiters) {
								this.evaluatorRegistry.unregister(beh)
							}
						}

					}
					'''.parseSuccessfully(
					'''
					package io.sarl.docs.tutorials.tinyMASSRE
					import io.sarl.lang.core.EventListener
					interface BehaviorGuardEvaluatorRegistry {
						def register(beh : Object)
						def unregister(beh : Object)
					}
					abstract class Agent {
						protected var evaluatorRegistry : BehaviorGuardEvaluatorRegistry
					}
					interface BehaviorsSkill {
						def getRegistrationWaiters : Iterable<Object>
						def getUnregistrationWaiters : Iterable<Object>
					}
					abstract 
					''',
					// TEXT
					'''
					''')
					"#Creating_built-in_capacity_instances_in_the_agent" should beAccessibleFrom this
				}

				/* Because a skill could be dynamically removed from an agent, we must
				 * unregister each agent's behavior when the skill is uninstalled.
				 *
				 * <p>In the `BehaviorsSkill` type, we define the `uninstall` function, which
				 * is automatically invoked when the skill is removed from the agent.
				 *
				 * <p>The code of the `uninstall` function goes through all the behaviors, and
				 * invokes the `unregister` function of the agent's event bus with each of them as
				 * argument.
				 *
				 * @filter(.* = '''|'''|.parseSuccessfully.*)
				 */
				fact "Proper un-installation of the skill" {
					'''
					protected def uninstall(stage : UninstallationStage) {
						switch (stage) {
						case PRE_DESTROY_EVENT: {
							}
						case POST_DESTROY_EVENT: {
								for (beh : this.behaviors) {
									(owner as TMSarlAgent).evaluatorRegistry.unregister(beh)
								}
							}
						}
					}
					'''.parseSuccessfully(
					'''
					package io.sarl.docs.tutorials.tinyMASSRE
					import java.util.List
					import io.sarl.lang.core.Behavior
					import io.sarl.lang.core.Skill.UninstallationStage
					import io.sarl.core.Behaviors
					interface Registry {
						def unregister(obj : Object)
					}
					class TMSarlAgent {
						public var evaluatorRegistry : Registry
					}
					abstract class BehaviorsSkill implements Behaviors {
						var behaviors : List<Behavior>
						def getOwner : Object { null }
					''',
					// TEXT
					'''
					}
					''')
				}

			}

		}

		/* The `Schedules` capacity enables the agent to run periodic and not-periodic
		 * tasks.
		 */
		describe "Definition of the Schedules skill" {

			/* The SARL `AgentTask` type represents a task for the SARL agents.
			 * The default implementation of `AgentTask` assumes that the task
			 * will be run asynchronously (usually with a Java thread execution pool).
			 *
			 * <p>Unfortunately, the tinyMAS platform forbids to use a thread execution pool
			 * because of its internal design specification: the tintMAS platform execution
			 * mechanism is synchronous.
			 *
			 * <p>In order to run an `AgentTask` on tinyMAS, we must define a subtype, named `Task`
			 * that is providing the features, which are usually given by the Java thread
			 * execution pool: <ul>
			 * <li>cancellation of the task,</li>
			 * <li>period of execution of the task.<li>
			 * </ul>
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Definition of the synchronous agent task type" {
				'''
				class Task extends AgentTask {

					var isCanceled = false
					
					var period = 0l
					
					def isCanceled : boolean {
						this.isCanceled
					}
					
					def cancel {
						this.isCanceled = true
					}
					
					def getPeriod : long {
						this.period
					}
					
					def setPeriod(period : long) {
						this.period = period
					}

				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import io.sarl.core.AgentTask
				''',
				// TEXT
				'''
				''')
			}

			/* The first and incomplete definition of the `SchedulesSkill` type
			 * is described below. This type is the tinyMAS implementation of
			 * the `Schedules` capacity.
			 *
			 * <p>The `task` function enables to create a SARL task with the given
			 * name. The implementation creates the task instance, and sets the name
			 * of the task with a unique value.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "First definition of the skill" {
				'''
				class SchedulesSkill extends Skill implements Schedules {

					def task(name : String) : AgentTask {
						var theTask = new Task
						theTask.name = name ?: UUID.randomUUID.toString
						return theTask
					}

				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.UUID
				import io.sarl.core.AgentTask
				import io.sarl.core.Schedules
				class Task extends AgentTask {
				}
				abstract class Skill {
				}
				abstract 
				''',
				// TEXT
				'''
				''')
			}

			/* The first function, named `in`, for scheduling an agent task permits to run
			 * the given task after a given delay.
			 *
			 * <p>If the task agent us <code>null</code>, the `in` function creates an agent
			 * task instance with an unique name.
			 * 
			 * <p>The `in` function computes the time at which the task should be run.
			 * This computation is based on a call to the `getSimulationTime` function, which
			 * is providing the current time in the tinyMAS platform. This current time is
			 * increased with the delay given as parameter of the `in` function for obtaining
			 * the task's execution time.
			 *
			 * <p>After associated the task to the procedure given as parameter, the `in` function
			 * schedule the task by calling the `scheduleTask` function.
			 *
			 * <p>The `scheduleTask` function takes two parameters: the time at which the
			 * task must start, and the task to execute. This function updates the collection
			 * of the tasks to execute, which is stored and accessible with the `tasks` field.
			 * The `tasks` field contains a map from the task starting time to the collection
			 * of tasks to be started at this time.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Scheduling an agent task" {
				'''
				def in(task : AgentTask = null, delay : long, procedure : (io.sarl.lang.core.Agent) => void) : AgentTask {
					var theTask = task
					if (theTask === null) {
						theTask = new Task
						theTask.name = UUID.randomUUID.toString
					}
					var time = (owner as TMSarlAgent).getSimulationTime(TimeUnit::MILLISECONDS) as long + delay
					theTask.procedure = procedure
					scheduleTask(time, theTask)
					return theTask
				}

				val tasks : Map<Long, Collection<AgentTask>> = new TreeMap

				private def scheduleTask(at : long, task : AgentTask) {
					var list = this.tasks.get(at)
					if (list === null) {
						list = new ArrayList
						this.tasks.put(at, list)
					}
					list += task
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.UUID
				import java.util.Collection
				import java.util.ArrayList
				import java.util.Map
				import java.util.TreeMap
				import java.util.concurrent.TimeUnit
				import io.sarl.lang.core.Agent
				import io.sarl.core.AgentTask
				import io.sarl.core.Schedules
				class Task extends AgentTask {
				}
				interface TMSarlAgent {
					def getSimulationTime(u : TimeUnit) : double
				}
				abstract class SchedulesSkill implements Schedules {
					def getOwner : Agent { null }
				''',
				// TEXT
				'''
				}
				''')
			}

			/* It is possible to execute a task only one time by calling the `execute` function.
			 * In the tinyMAs implementation, the `execute` function schedule the task for the
			 * next simulation step.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Executed an agent task once" {
				'''
				def execute(task : AgentTask = null, procedure : (io.sarl.lang.core.Agent) => void) : AgentTask {
					return in(
						task,
						(owner as TMSarlAgent).getSimulationStepDuration(TimeUnit::MILLISECONDS) as long,
						procedure)
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.UUID
				import java.util.Collection
				import java.util.ArrayList
				import java.util.Map
				import java.util.TreeMap
				import java.util.concurrent.TimeUnit
				import io.sarl.lang.core.Agent
				import io.sarl.core.AgentTask
				import io.sarl.core.Schedules
				class Task extends AgentTask {
				}
				interface TMSarlAgent {
					def getSimulationStepDuration(u : TimeUnit) : double
				}
				abstract class SchedulesSkill implements Schedules {
					def getOwner : Agent { null }
				''',
				// TEXT
				'''
				}
				''')
			}

			/* It is possible to schedule periodic tasks by calling the `every` function.
			 * The definition of this function is similar to the definition of
			 * the `in` function, except that the `period` field of the `Task` is set
			 * with the period duration, which is given as parameter. 
			 *
			 * <p>The `atFixedDelay` function delegates to the `every` function because the task running
			 * algorithm implies that these two types of execution approach will be the same on tinyMAS.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Scheduling a periodic agent task" {
				'''
				def every(task : AgentTask = null, period : long, procedure : (io.sarl.lang.core.Agent) => void) : AgentTask {
					var theTask = task
					if (theTask === null) {
						theTask = new Task
						theTask.name = UUID.randomUUID.toString
					}
					var time = (owner as TMSarlAgent).getSimulationTime(TimeUnit::MILLISECONDS) as long + period
					theTask.procedure = procedure
					if (theTask instanceof Task) {
						theTask.period = period
					}
					scheduleTask(time, theTask)
					return theTask
				}

				def atFixedDelay(task : AgentTask = null, delay : long, procedure : (io.sarl.lang.core.Agent) => void) : AgentTask {
					return task.every(delay, procedure)
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.UUID
				import java.util.concurrent.TimeUnit
				import io.sarl.lang.core.Agent
				import io.sarl.core.AgentTask
				import io.sarl.core.Schedules
				class Task extends AgentTask {
					def setPeriod(per : long) { }
				}
				interface TMSarlAgent {
					def getSimulationTime(u : TimeUnit) : double
				}
				abstract class SchedulesSkill implements Schedules {
					def getOwner : Agent { null }
					private def scheduleTask(at : long, task : AgentTask) { }
				''',
				// TEXT
				'''
				}
				''')
			}

			/* The `Schedules` capacity provides the feature for canceling a task by calling
			 * the `cancel` function.
			 * This function delegates to the `cancel` function of the `Task` type.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Canceling an agent task" {
				'''
				def cancel(task : AgentTask, mayInterruptIfRunning : boolean = true) : boolean {
					if (task instanceof Task) {
						task.cancel
						return true
					}
					return false
				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import io.sarl.core.AgentTask
				import io.sarl.core.Schedules
				class Task extends AgentTask {
					def cancel { }
				}
				abstract class SchedulesSkill implements Schedules {
				''',
				// TEXT
				'''
				}
				''')
			}

			/* Executing the registered tasks must be supported by a specific function,
			 * named `runTasks`.
			 *
			 * <p>This function determines the current time (`currentTime`) in the tinyMAS platform.
			 * Then, it retrieves the `list` of the tasks to start at the current time.
			 * 
			 * <p<For each task in the `list`, the function tests if the task was cancelled or not.
			 * If the task was not cancelled, the function retrieves the task's guard (the condition
			 * of execution) and evaluates it.
			 * 
			 * <p>If the task's guard is evaluated to <code>true</code>, the function executes
			 * the procedure associated to the task.
			 *
			 * <p>Finally, the `runTasks` function reschedules the task is its periodic execution
			 * period is greater than zero. 
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Running the scheduled tasks" {
				'''
					def runTasks {
						var currentTime = (owner as TMSarlAgent).getSimulationTime(TimeUnit::MILLISECONDS) as long
						var list = this.tasks.remove(currentTime)
						if (list !== null) {
							for (task : list) {
								var canceled = false
								if (task instanceof Task) {
									canceled = task.canceled
								}
								if (!canceled) {
									var guard = task.guard
									if (guard === null || guard.apply((owner as TMSarlAgent).sarlAgent)) {
										var code = task.procedure
										if (code !== null) {
											code.apply((owner as TMSarlAgent).sarlAgent)
										}
										if (task instanceof Task) {
											if (task.period > 0) {
												scheduleTask(currentTime + task.period, task)
											}
										}
									}
								}
							}
						}
					}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.Collection
				import java.util.Map
				import java.util.concurrent.TimeUnit
				import io.sarl.lang.core.Agent
				import io.sarl.core.AgentTask
				import io.sarl.core.Schedules
				class Task extends AgentTask {
					def isCanceled : boolean { true }
					def getPeriod : long { 0 }
				}
				interface TMSarlAgent {
					def getSimulationTime(u : TimeUnit) : double
					def getSarlAgent : Agent
				}
				abstract class SchedulesSkill implements Schedules {
					var tasks : Map<Long, Collection<AgentTask>>
					def getOwner : Agent { null }
					private def scheduleTask(at : long, task : AgentTask) { }
				''',
				// TEXT
				'''
				}
				''')
			}

			/* Now the `runTasks` function is defined in the `SchedulesSkill` class, we could update
			 * the tinyMAS agent life-cycle in order to invoke this function.
			 *
			 * <p>The agent tasks must be run at each step of the tinyMAS agent life. Consequently,
			 * we must redefine the `live` function in the `TMSarlAgent` class for invoking `runTasks`.
			 *
			 * <p>In order to call the `runTasks` function, we must assume that the `schedulesSkill`
			 * field is defined as a reference to the instance of `SchedulesSkill` built-in capacity.
			 * How this field is set will be described in a further section.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Updating the tinyMAS agent living function" {
				'''
				class TMSarlAgent extends Agent implements EventListener {

					// [...]

					var schedulesSkill : SchedulesSkill

					def live {
						for (beh : this.behaviorsSkill.registrationWaiters) {
							this.evaluatorRegistry.register(beh)
						}
						for (beh : this.behaviorsSkill.unregistrationWaiters) {
							this.evaluatorRegistry.unregister(beh)
						}
						this.schedulesSkill.runTasks
					}

				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import io.sarl.lang.core.EventListener
				interface BehaviorGuardEvaluatorRegistry {
					def register(beh : Object)
					def unregister(beh : Object)
				}
				interface BehaviorsSkill {
					def getRegistrationWaiters : Iterable<Object>
					def getUnregistrationWaiters : Iterable<Object>
				}
				abstract class Agent {
					protected var evaluatorRegistry : BehaviorGuardEvaluatorRegistry
					protected var behaviorsSkill : BehaviorsSkill
				}
				interface SchedulesSkill {
					def runTasks
				}
				abstract 
				''',
				// TEXT
				'''
				''')
				"#Creating_built-in_capacity_instances_in_the_agent" should beAccessibleFrom this
			}

		}

		/* The definition of the `Time` built-in capacity is based on the delegation to the
		 * tinyMAS time manager that is accessible from the agent owning the capacity.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Definition of the Time skill" {
			'''
			class TimeSkill extends Skill implements Time {

				def getTime(timeUnit : TimeUnit = null) : double {
					(owner as TMSarlAgent).getSimulationTime(timeUnit ?: TimeUnit::SECONDS)
				}

				def getOSTimeFactor : double {
					(owner as TMSarlAgent).getSimulationStepDuration(TimeUnit::SECONDS)
				}

			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.concurrent.TimeUnit
			import io.sarl.core.Time
			interface TMSarlAgent {
				def getSimulationTime(tm : TimeUnit) : double
				def getSimulationStepDuration(tm : TimeUnit) : double
			}
			abstract class Skill {
				def getOwner : TMSarlAgent { null }
			}
			''',
			// TEXT
			'''
			''')
		}

		/* Because of a design choice, we have decided to no provide the implementation of the 
		 * built-in capacities `ExternalContextAccess` and `InnerContextAccess`.
		 * 
		 * @filter(.*)
		 */
		fact "Definition of the ExternalContextAccess and InnerContextAccess skills" {
			true
		}

		/* Once the built-in capacity are implemented for the tinyMAS platform, we must implement
		 * the built-in capacities' instantiation and release.
		 */
		describe "Creating built-in capacity instances in the agent" {

			/* Because the built-in capacities' instances exist during the whole life of the associated agent,
			 * these instances must be stored in agent's fields.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Storing the built-in capacities in agent fields" {
				'''
				class TMSarlAgent extends Agent implements EventListener {

					// [...]

					var behaviorSkill : BehaviorsSkill
					var loggingSkill : LoggingSkill
					var spaceSkill : DefaultContextInteractionsSkill
					var lifeSkill : LifecycleSkill
					var scheduleSkill : SchedulesSkill
					var timeSkill : TimeSkill

				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import io.sarl.lang.core.EventListener
				interface BehaviorsSkill { }
				interface LoggingSkill { }
				interface DefaultContextInteractionsSkill { }
				interface LifecycleSkill { }
				interface SchedulesSkill { }
				interface TimeSkill { }
				abstract class Agent {
				}
				abstract 
				''',
				// TEXT
				'''
				''')
			}
	
			/* For initializing the agent's built-in capacities, the `start` function of the `TMSarlAgent` type
			 * must be updated for:
			 * <ul>
			 * <li>creating the built-in capacities' instances; and </li>
			 * <li>calling the `setSkill` function on the SARL agent for setting the capacities.</li>
			 * <ul>
			 * 
			 * <p>Calling the `setSkill` function implies the call to the `install` function of the skill.
			 * 
			 * <p>The calls to the `setSkill` function are done by using Java reflection because the
			 * `setSkill` function is a protected function in the SARL agent type. Retrieving the method
			 * by reflection, and enabling the accessibility, allows to ignore the `protected` modifier of
			 * the `setSkill` function.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Initializing the agent built-in capacities" {
				'''
				class TMSarlAgent extends Agent implements EventListener {

					// [...]

					def start {
						super.start
						this.evaluatorRegistry.register(sarlAgent)

						this.behaviorSkill = new BehaviorsSkill
						this.loggingSkill = new LoggingSkill
						this.spaceSkill = new DefaultContextInteractionsSkill
						this.lifeSkill = new LifecycleSkill
						this.scheduleSkill = new SchedulesSkill
						this.timeSkill = new TimeSkill

						var method = typeof(io.sarl.lang.core.Agent).getDeclaredMethod("setSkill", typeof(Class), typeof(Skill))
						method.accessible = true
						method.invoke(sarlAgent, typeof(Behaviors), this.behaviorSkill)
						method.invoke(sarlAgent, typeof(Logging), this.loggingSkill)
						method.invoke(sarlAgent, typeof(DefaultContextInteractions), this.spaceSkill)
						method.invoke(sarlAgent, typeof(Lifecycle), this.lifeSkill)
						method.invoke(sarlAgent, typeof(Schedules), this.scheduleSkill)
						method.invoke(sarlAgent, typeof(Time), this.timeSkill)

						var initializeEvent = new Initialize(this.spawnerID, this.parameters)
						this.spawnerID = null
						this.parameters = null
						receiveEvent(initializeEvent)
					}

				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import java.util.UUID
				import io.sarl.lang.core.EventListener
				import io.sarl.core.Initialize
				import io.sarl.lang.core.Event
				import io.sarl.lang.core.Skill
				interface Behaviors { }
				interface Logging { }
				interface DefaultContextInteractions { }
				interface Lifecycle { }
				interface Schedules { }
				interface Time { }
				class BehaviorsSkill { }
				class LoggingSkill { }
				class DefaultContextInteractionsSkill { }
				class LifecycleSkill { }
				class SchedulesSkill { }
				class TimeSkill { }
				interface Registry {
					def register(obj : Object)
				}
				abstract class Agent {
					protected var behaviorSkill : BehaviorsSkill
					protected var loggingSkill : LoggingSkill
					protected var spaceSkill : DefaultContextInteractionsSkill
					protected var lifeSkill : LifecycleSkill
					protected var scheduleSkill : SchedulesSkill
					protected var timeSkill : TimeSkill
					protected var evaluatorRegistry : Registry
					protected var spawnerID : UUID
					protected var parameters : Object[]

					def getSarlAgent : io.sarl.lang.core.Agent { null }
					def start { }
					def receiveEvent(^event : Event)
				}
				abstract 
				''',
				// TEXT
				'''
				''')
			}

			/* For uninstalling the agent's built-in capacities, the `stop` function of the `TMSarlAgent` type
			 * must be updated for calling the `clearSkill` function on the SARL agent for uninstalling the capacities.
			 * 
			 * <p>Calling the `clearSkill` function implies the call to the `uninstall` function of the skill.
			 * 
			 * <p>The calls to the `clearSkill` function are done by using Java reflection because the
			 * `clearSkill` function is a protected function in the SARL agent type. Retrieving the method
			 * by reflection, and enabling the accessibility, allows to ignore the `protected` modifier of
			 * the `clearSkill` function.
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Uninstalling the agent built-in capacities" {
				'''
				class TMSarlAgent extends Agent implements EventListener {

					// [...]

					def stop {
						receiveEvent(new Destroy)

						var method = typeof(io.sarl.lang.core.Agent).getDeclaredMethod("clearSkill", typeof(Class))
						method.accessible = true
						method.invoke(sarlAgent, typeof(Time))
						method.invoke(sarlAgent, typeof(Schedules))
						method.invoke(sarlAgent, typeof(Lifecycle))
						method.invoke(sarlAgent, typeof(DefaultContextInteractions))
						method.invoke(sarlAgent, typeof(Logging))
						method.invoke(sarlAgent, typeof(Behaviors))

						this.evaluatorRegistry.unregister(sarlAgent)
						super.stop
					}

				}
				'''.parseSuccessfully(
				'''
				package io.sarl.docs.tutorials.tinyMASSRE
				import io.sarl.lang.core.EventListener
				import io.sarl.core.Destroy
				import io.sarl.lang.core.Event
				import io.sarl.lang.core.Skill
				interface Behaviors { }
				interface Logging { }
				interface DefaultContextInteractions { }
				interface Lifecycle { }
				interface Schedules { }
				interface Time { }
				interface Registry {
					def unregister(obj : Object)
				}
				abstract class Agent {
					protected var evaluatorRegistry : Registry

					def getSarlAgent : io.sarl.lang.core.Agent { null }
					def stop { }
					def receiveEvent(^event : Event)
				}
				abstract 
				''',
				// TEXT
				'''
				''')
			}

		}

	}

	/* In the tinyMAs platform, agents are proactive for reading the messages they have received: the agents
	 * call explicitely the functions for reading the messages.
	 * In SARL, the agents are reactive regarding the event handling: the agents are automatically notified
	 * when they received events.
	 *
	 * <p>It is mandatory to redefine the agent living function `live` for mapping the proactive behavior of the
	 * tinyMAS agents to the reactive behavior of the SARL agents.
	 * 
	 * <p>The `live` function loops on the content of the mailbox (`while hasMessage`).
	 * For each message, the function gets its content, and if it is a SARL event, the `live` function
	 * fires the event by calling the `receiveEvent` function.
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "From the tinyMAS proactive message ready to the SARL reactive event handling" {
		'''
		class TMSarlAgent extends Agent implements EventListener {

			// [...]

			def live {
				for (beh : this.behaviorsSkill.registrationWaiters) {
					this.evaluatorRegistry.register(beh)
				}
				for (beh : this.behaviorsSkill.unregistrationWaiters) {
					this.evaluatorRegistry.unregister(beh)
				}

				while (hasMessage) {
					var message = nextMessage
					var content = message.content
					if (content instanceof Event) {
						receiveEvent(content)
					}
				}

				this.schedulesSkill.runTasks
			}

		}
		'''.parseSuccessfully(
		'''
		package io.sarl.docs.tutorials.tinyMASSRE
		import io.sarl.lang.core.EventListener
		import io.sarl.lang.core.Event
		interface BehaviorsSkill {
			def getRegistrationWaiters : Iterable<Object>
			def getUnregistrationWaiters : Iterable<Object>
		}
		interface SchedulesSkill {
			def runTasks
		}
		interface Registry {
			def register(obj : Object)
			def unregister(obj : Object)
		}
		interface Message {
			def getContent : Object
		}
		abstract class Agent {
			protected var behaviorsSkill : BehaviorsSkill
			protected var schedulesSkill : SchedulesSkill
			protected var evaluatorRegistry : Registry

			def hasMessage : boolean { true }
			def getNextMessage : Message

			def receiveEvent(^event : Event) { }
		}
		abstract 
		''',
		// TEXT
		'''
		''')
	}

	/* The next step for making tinyMAS a simple SARL run-time environment (SRE) is to define
	 * the utility class for booting tinyMAS as a SRE. We define the `Boot` class for supporting
	 * the tinyMAS kernel booting.
	 *
	 * <p>As for the <a href="http://www.janusproject.io">standard SRE Janus</a>, we assume that 
	 * the main entry point of the SRE expects the fully qualified name of the agent to launch as
	 * the first command-line argument. the rest of the command-line arguments will be given as
	 * initialization arguments to the launched agent.
	 */
	describe "Booting infrastructure" {

		/* We define the `Boot` class with a main function as the SRE entry point. It extends the `Kernel`
		 * class provided by the tinyMAS platform for representing its kernel.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Definition of the main entry point" {
			'''
			class Boot extends Kernel {

				def static main(args : String[]) {
				}

			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			abstract class Kernel {
			}
			''',
			// TEXT
			'''
			''')
		}

		/* For creating an instance of the tinyMAS platform, three components must be created:
		 * <ul>
		 * <li>`whitePages` for storing the running agents in the tinyMAS kernel;</li>
		 * <li>`yellowPages` as the repository of the services provided by the tinyMAS agents; and</li>
		 * <li>`mts` as the kernel service for routing the messages from a tinyMAS agent to another.</li>
		 * </ul>
		 * 
		 * <p>The three tinyMAS components are defined and used for initializing:
		 * <ul>
		 * <li>the SARL default space, named `defaultSpace`,</li>
		 * <li>the SARL default context, named `context`, and </li>
		 * <li>the tinyMAS kernel instance, named `kernel`.</li>
		 * </li>
		 * 
		 * <p>The reference from the default space to the containing context must be manually set by invoking:
		 * ```
		 * defaultSpace.agentContext = context
		 * ```
		 * 
		 * <p>The `Boot` instance contains the default space as field, named `defaultSpace`.
		 * In the constructor of the `Boot` class, the tinyMAS kernel identifier is given to the default space.
		 * Indeed, the tinyMAS kernel identifier is known only when the tinyMAS kernel instance is created.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Creation of the SARL default context and space instances" {
			'''
			class Boot extends Kernel {

				def static main(args : String[]) {
					var whitePages = new WhitePages
					var yellowPages = new YellowPages
					var mts = new MessageTransportService

					var defaultSpace = new TMDefaultSpace(whitePages, mts)
			
					var kernel = new Boot(defaultSpace, mts, whitePages, yellowPages)
					
					var context = new TMAgentContext(defaultSpace)
					defaultSpace.agentContext = context
				}

				val defaultSpace : TMDefaultSpace

				new (defaultSpace : TMDefaultSpace, mts : MessageTransportService,
						whitePages : WhitePages, yellowPages : YellowPages) {
					super(mts, whitePages, yellowPages)
					this.defaultSpace = defaultSpace
					this.defaultSpace.kernelID = kernelId
				}

			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import java.lang.reflect.Array
			class WhitePages {
			}
			class YellowPages {
			}
			class MessageTransportService {
			}
			class TMDefaultSpace {
				protected static var TINYMAS_DEFAULT_SPACE_ID : UUID
				new(wp : WhitePages, mts : MessageTransportService) { }
				def setAgentContext(context : TMAgentContext) { }
				def setKernelID(u : UUID) { }
			}
			class TMAgentContext {
				new(defaultSpace : TMDefaultSpace) { }
			}
			abstract class Kernel {
				new (mts : MessageTransportService, whitePages : WhitePages, yellowPages : YellowPages) { }
				def getKernelId : UUID { null }
				def run { }
			}
			''',
			// TEXT
			'''
			''')
		}

		/* The main entry point must parse the command-line arguments for extracting
		 * the fully qualified name of the agent to launch (named `agentType`), and the
		 * agent initialization parameters, named `params`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Management of the command-line parameters" {
			'''
			class Boot extends Kernel {

				def static main(args : String[]) {
					var whitePages = new WhitePages
					var yellowPages = new YellowPages
					var mts = new MessageTransportService

					var defaultSpace = new TMDefaultSpace(whitePages, mts)
			
					var kernel = new Boot(defaultSpace, mts, whitePages, yellowPages)
					
					var context = new TMAgentContext(defaultSpace)
					defaultSpace.agentContext = context
					
					val agentName = args.get(0)
					var agentType = Class::forName(agentName)
					var params = Array.newInstance(typeof(Object), args.size - 1) as Object[]
					for (var i = 0; i < params.length; i++) {
						params.set(i, args.get(i + 1))
					}
				}

				val defaultSpace : TMDefaultSpace

				new (defaultSpace : TMDefaultSpace, mts : MessageTransportService,
						whitePages : WhitePages, yellowPages : YellowPages) {
					super(mts, whitePages, yellowPages)
					this.defaultSpace = defaultSpace
					this.defaultSpace.kernelID = kernelId
				}

			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import java.lang.reflect.Array
			class WhitePages {
			}
			class YellowPages {
			}
			class MessageTransportService {
			}
			class TMDefaultSpace {
				protected static var TINYMAS_DEFAULT_SPACE_ID : UUID
				new(wp : WhitePages, mts : MessageTransportService) { }
				def setAgentContext(context : TMAgentContext) { }
				def setKernelID(u : UUID) { }
			}
			class TMAgentContext {
				new(defaultSpace : TMDefaultSpace) { }
			}
			abstract class Kernel {
				new (mts : MessageTransportService, whitePages : WhitePages, yellowPages : YellowPages) { }
				def getKernelId : UUID { null }
				def run { }
			}
			''',
			// TEXT
			'''
			''')
		}

		/* The launching process starts an agent by invoked the function `spawn` of the utility class
		 * `Spawner`.
		 * 
		 * <p>The `spawn` function adds the agent into the registry of the agents, but it does not run the
		 * tinyMAS platform. For running the tinyMAS platform, the `run` function of the tinyMAS `Kernel`
		 * must be invoked. 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Launching" {
			'''
			class Boot extends Kernel {

				def static main(args : String[]) {
					var whitePages = new WhitePages
					var yellowPages = new YellowPages
					var mts = new MessageTransportService

					var defaultSpace = new TMDefaultSpace(whitePages, mts)
			
					var kernel = new Boot(defaultSpace, mts, whitePages, yellowPages)
					
					var context = new TMAgentContext(defaultSpace)
					defaultSpace.agentContext = context
					
					val agentName = args.get(0)
					var agentType = Class::forName(agentName)
					var params = Array.newInstance(typeof(Object), args.size - 1) as Object[]
					for (var i = 0; i < params.length; i++) {
						params.set(i, args.get(i + 1))
					}

					if (typeof(io.sarl.lang.core.Agent).isAssignableFrom(agentType)) {
						Spawner.spawn(
							kernel,
							defaultSpace,
							agentType as Class<? extends io.sarl.lang.core.Agent>,
							null,
							TMDefaultSpace.TINYMAS_DEFAULT_SPACE_ID,
							null,
							params)
					}

					kernel.run
				}

				val defaultSpace : TMDefaultSpace

				new (defaultSpace : TMDefaultSpace, mts : MessageTransportService,
						whitePages : WhitePages, yellowPages : YellowPages) {
					super(mts, whitePages, yellowPages)
					this.defaultSpace = defaultSpace
					this.defaultSpace.kernelID = kernelId
				}

			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import java.lang.reflect.Array
			class WhitePages {
			}
			class YellowPages {
			}
			class MessageTransportService {
			}
			class Spawner {
				static def spawn(kernel : Kernel, defaultSpace : TMDefaultSpace,
						agentType : Class<? extends io.sarl.lang.core.Agent>,
						spawnerId : UUID, parentId : UUID, id : UUID, params : Object*) : UUID { null }
			}
			class TMDefaultSpace {
				protected static var TINYMAS_DEFAULT_SPACE_ID : UUID
				new(wp : WhitePages, mts : MessageTransportService) { }
				def setAgentContext(context : TMAgentContext) { }
				def setKernelID(u : UUID) { }
			}
			class TMAgentContext {
				new(defaultSpace : TMDefaultSpace) { }
			}
			abstract class Kernel {
				new (mts : MessageTransportService, whitePages : WhitePages, yellowPages : YellowPages) { }
				def getKernelId : UUID { null }
				def run { }
			}
			''',
			// TEXT
			'''
			''')
		}

	}

	/* According to the SARL specification, a SARL run-time environment should fire specific platform-level events:
	 * <ul>
	 * <li>`AgentSpawned` should be fired in the default space when an agent is created;</li>
	 * <li>`AgentKilled` should be fired in the default space when an agent is killed;</li>
	 * <li>`ContextJoined` should be fired in the agent when it has joint a context;</li>
	 * <li>`ContextLeft` should be fired in the agent when it has left a context;</li>
	 * <li>`MemberJoined` should be fired in the default space when an agent has joint a context;</li>
	 * <li>`MemberLeft` should be fired in the default space when an agent has left a context.</li>
	 * </ul>
	 *
	 * <p>According to the design choice of supporting only one context and one interaction space, only
	 * the `AgentSpawned` and `AgentKilled` must be really supported by the tinyMAS platform.
	 *
	 * <p>According to the tinyMAS specification, the only one way to determine when an agent is created or killed is
	 * to register a specific event listener on the tinyMAS kernel.
	 * Since this specific listener must exist during the entire life of the kernel, we register the listener
	 * at the creation of the `Boot` class.
	 * 
	 * <p>Even for `AgentSpawned` and `AgentKilled`, the event firing follows the same steps:
	 * <ul>
	 * <li>The source of the event is the tinyMAS platform, represented by the address with the default context identifier.</li>
	 * <li>The created/destroyed agent's type is the type of the SARL agent or <code>null</code>.</li>
	 * <li>The event is built from the computed information and the agent identifier given as parameter.</li>
	 * <li>The event is fired in the default space.</li>
	 * </ul>
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "General Events" {
			'''
			class Boot extends Kernel {

				static class TinyMASKernelListener extends KernelAdapter {

					val containingBoot : Boot

					new(containingBoot : Boot) {
						this.containingBoot = containingBoot
					}

					def kernelAgentAdded(kernel : Kernel, ^agent : Agent, id : AgentIdentifier) {
						var source = new Address(
								containingBoot.defaultSpace.spaceID,
								TMDefaultSpace::TINYMAS_DEFAULT_SPACE_ID)
						var agentType : String
						if (^agent instanceof TMSarlAgent) {
							agentType = ^agent.sarlAgent.class.name
						} else {
							agentType = null
						}
						var spawnEvent = new AgentSpawned(source,
								Identifiers::toUUID(id),
								agentType)
						containingBoot.defaultSpace.emit(spawnEvent)
					}

					def kernelAgentRemoved(kernel : Kernel, ^agent : Agent, id : AgentIdentifier) {
						var source = new Address(
								containingBoot.defaultSpace.spaceID,
								TMDefaultSpace::TINYMAS_DEFAULT_SPACE_ID)
						var agentType : String
						if (^agent instanceof TMSarlAgent) {
							agentType = ^agent.sarlAgent.class.name
						} else {
							agentType = null
						}
						var spawnEvent = new AgentKilled(source,
								Identifiers::toUUID(id),
								agentType)
						containingBoot.defaultSpace.emit(spawnEvent)
					}

				}

				new (defaultSpace : TMDefaultSpace, mts : MessageTransportService,
						whitePages : WhitePages, yellowPages : YellowPages) {
					super(mts, whitePages, yellowPages)
					this.defaultSpace = defaultSpace
					this.defaultSpace.kernelID = kernelId

					addKernelListener(new TinyMASKernelListener(this))
				}

			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			import java.util.UUID
			import java.lang.reflect.Array
			import io.sarl.lang.core.Address
			import io.sarl.lang.core.Event
			import io.sarl.lang.core.SpaceID
			import io.sarl.core.AgentSpawned
			import io.sarl.core.AgentKilled
			class WhitePages {
			}
			class YellowPages {
			}
			class MessageTransportService {
			}
			class TMDefaultSpace {
				public static var TINYMAS_DEFAULT_SPACE_ID : UUID
				def setKernelID(u : UUID) { }
				def getSpaceID : SpaceID { null }
				def emit(^event : Event) { }
			}
			class TMAgentContext {
			}
			class TMSarlAgent {
				def getSarlAgent : io.sarl.lang.core.Agent { null }
			}
			class KernelAdapter {
			}
			class Agent {
			}
			class AgentIdentifier {
			}
			class Identifiers {
				static def toUUID(id : AgentIdentifier) : UUID { null }
			}
			abstract class Kernel {
				protected var defaultSpace : TMDefaultSpace

				new (mts : MessageTransportService, whitePages : WhitePages, yellowPages : YellowPages) { }
				def getKernelId : UUID { null }
				def addKernelListener(listener : KernelAdapter) { }
			}
			''',
			// TEXT
			'''
			''')
	}

	/* The agent spawning function defined in the `TMDefaultSpace` type assumes the created agents are
	 * buffered until their real addition into the tinyMAS platform.
	 * Until this point of the document, we have not implemented the addition of the buffered agents
	 * into the tinyMAS kernel.
	 * 
	 * <p>According to the tinyMAS specification, the agents should be added at the end of each step
	 * of the tinyMAS kernel. It is not possible to override a function for added a code at the end
	 * of the tinyMAS kernel loop. Fortunately, each tinyMAs event listener is notified when
	 * the tinyMAs kernel has finished its internal execution loop: the `kernelRefreshAllowed` function
	 * of the `KernelListener` (or its `KernelAdapter` implementation) is called.
	 * 
	 * <p>We update the `TinyMASKernelListener` inner class, which is defined in the previous section by
	 * adding the definition of the `kernelRefreshAllowed` function.
	 * 
	 * <p>The `kernelRefreshAllowed` function retrieves the buffered agents from the default space, by
	 * invoking the `consumeAgentToLaunch` function. Each agent replied by the `consumeAgentToLaunch`
	 * function is spawned in the tinyMAS platform by calling the `Spawner` utility class. 
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*)
	 */
	fact "Finalizing the agent spawning" {
			'''
			def kernelRefreshAllowed(kernel : Kernel) {
				for (^agent : containingBoot.defaultSpace.consumeAgentToLaunch) {
					Spawner.spawn(kernel, ^agent)
				}
			}
			'''.parseSuccessfully(
			'''
			package io.sarl.docs.tutorials.tinyMASSRE
			class Spawner {
				static def spawn(kernel : Kernel, ^agent : TMSarlAgent) { }
			}
			class Kernel {
			}
			class TMSarlAgent {
			}
			class TMDefaultSpace {
				def consumeAgentToLaunch : Iterable<TMSarlAgent> { null }
			}
			class Boot {
				def getDefaultSpace : TMDefaultSpace { null }
			}
			abstract class TinyMASKernelListener {
				var containingBoot : Boot
			''',
			// TEXT
			'''
			}
			''')
	}

	/* Now, the code for making a SARL run-time environment (SRE) with the tinyMAS platform is ready.
	 * For enabling the tinyMAS platform to be recognized as a SRE by the Eclipse SARL development
	 * environment, the manifest file of the Jar archive of tinyMAs must contains mandatory information
	 * that are explained in this section.
	 */
	describe "Update of the Jar Manifest for making the archive as a SRE" {

		/* The configuration elements for defining a SRE are:
		 *
		 * <table>
		 * <thead>
		 * <tr><th>Name</th><th>Explanation</th></tr>
		 * </thead>
		 * <tbody>
		 * <tr><td>Main-Class</td>
		 * 		<td>The fully qualified name of the main class of the program, usually the `Boot` class that is
		 * 			previously defined.</td></tr>
		 * <tr><td>SARL-Spec-Version</td>
		 * 		<td>The version number of the SARL specification from which this SRE was created.</td></tr>
		 * <tr><td>SRE-Name</td>
		 * 		<td>The name of the SRE.</td></tr>
		 * <tr><td>VM-Arguments</td>
		 * 		<td>The arguments to pass to the Java virtual machine that will run the SRE.</td></tr>
		 * <tr><td>Program-Arguments</td>
		 * 		<td>The arguments to pass to the application, i.e. the launched agent.</td></tr>
		 * <tr><td>Standalone-SRE</td>
		 * 		<td>Indicates if the SRE's archive contains all the classes that are needed for running
		 * 			the SRE (if the value of Standalone-SRE is <code>true</code>), or if the SRE's
		 * 			archive contains only the SRE classes and not the dependency classes (if the value
		 * 			of Standalone-SRE is <code>false</code>).</td></tr>
		 * <tr><td>CLI-Show-Logo</td>
		 * 		<td>The command-line option to pass to the SRE for displaying the SRE logo at startup.</td></tr>
		 * <tr><td>CLI-Hide-Logo</td>
		 * 		<td>The command-line option to pass to the SRE for hiding the SRE logo at startup.</td></tr>
		 * <tr><td>CLI-Show-Info</td>
		 * 		<td>The command-line option to pass to the SRE for enabling information logging.</td></tr>
		 * <tr><td>CLI-Hide-Info</td>
		 * 		<td>The command-line option to pass to the SRE for disabling information logging.</td></tr>
		 * <tr><td>CLI-Default-Context-ID</td>
		 * 		<td>The command-line option to pass to the SRE for using the default context identifier.</td></tr>
		 * <tr><td>CLI-Random-Context-ID</td>
		 * 		<td>The command-line option to pass to the SRE for computing a random value for the default context identifier.</td></tr>
		 * <tr><td>CLI-BootAgent-Context-ID</td>
		 * 		<td>The command-line option to pass to the SRE for computing a value from the boot agent classname for the default context identifier.</td></tr>
		 * <tr><td>CLI-Offline</td>
		 * 		<td>The command-line option to pass to the SRE for disabling the network support (network is assumed to be on by default).</td></tr>
		 * <tr><td>CLI-No-More-Option</td>
		 * 		<td>The command-line option to pass to the SRE for indicating that the rest of the command-line arguments could not be command-line optoins (usually <code>--</code>).</td></tr>
		 * <tr><td>CLI-Embedded</td>
		 * 		<td>The command-line option to pass to the SRE for indicating to the SRE it is running inside the process of the Eclipse SARL development environment.</td></tr>
		 * </tbody>
		 * </table>
		 * 
		 * @filter(.*)
		 */
		fact "Definition of the configuration elements" {
			true
		}

		/* The previously defined configuration elements must appear inside the manifest file of the Jar archive of the
		 * tinyMAS platform.
		 * 
		 * <p>The <code>Main-Class</code> entry must appear in the main section of the manifest. For example,
		 * the `Boot` class that is defined in the previous sections of this document is the main class of the tinyMAS SRE.
		 * It must be specified in the manifest file as:
		 * 
		 * ```
		 * Main-Class: org.arakhne.tinymas.sarl.Boot
		 * ```
		 *
		 * <p>The other configuration elements from the previous section must be specified in the manifest file in
		 * a specific section, named <code>SARL-Runtime-Environment</code>.
		 *
		 * <p>The following manifest context gives an example of the tinyMAS SRE declaration:
		 * ```
		 * Main-Class: org.arakhne.tinymas.sarl.Boot
		 * 
		 * SARL-Runtime-Environment:
		 * SRE-Name: TinyMAS
		 * SARL-Spec-Version: %sarlspecversion%
		 * Standalone-SRE : true
		 * VM-Arguments: -ea
		 * Program-Arguments:
		 * CLI-Show-Logo:
		 * CLI-Hide-Logo:
		 * CLI-Show-Info:
		 * CLI-Hide-Info:
		 * CLI-Default-Context-ID:
		 * CLI-Random-Context-ID:
		 * CLI-BootAgent-Context-ID:
		 * CLI-Offline:
		 * CLI-Embedded:
		 * CLI-No-More-Option:
		 * ```
		 *
		 * @filter(.*)
		 */
		fact "How to update the Jar Manifest by hand" {
			// The checks are valid only if the macro replacements were done.
			// The replacements are done by Maven.
			// So, Eclipse Junit tools do not make the replacements.
			System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse

			SREConstants::MANIFEST_CLI_BOOT_AGENT_CONTEXT_ID should be "CLI-BootAgent-Context-ID"
			SREConstants::MANIFEST_CLI_DEFAULT_CONTEXT_ID should be "CLI-Default-Context-ID"
			SREConstants::MANIFEST_CLI_EMBEDDED should be "CLI-Embedded"
			SREConstants::MANIFEST_CLI_HIDE_INFO should be "CLI-Hide-Info"
			SREConstants::MANIFEST_CLI_HIDE_LOGO should be "CLI-Hide-Logo"
			SREConstants::MANIFEST_CLI_NO_MORE_OPTION should be "CLI-No-More-Option"
			SREConstants::MANIFEST_CLI_RANDOM_CONTEXT_ID should be "CLI-Random-Context-ID"
			SREConstants::MANIFEST_CLI_SHOW_INFO should be "CLI-Show-Info"
			SREConstants::MANIFEST_CLI_SHOW_LOGO should be "CLI-Show-Logo"
			SREConstants::MANIFEST_CLI_SRE_OFFLINE should be "CLI-Offline"
			SREConstants::MANIFEST_MAIN_CLASS should be "Main-Class"
			SREConstants::MANIFEST_PROGRAM_ARGUMENTS should be "Program-Arguments"
			SREConstants::MANIFEST_SARL_SPEC_VERSION should be "SARL-Spec-Version"
			SREConstants::MANIFEST_SRE_NAME should be "SRE-Name"
			SREConstants::MANIFEST_STANDALONE_SRE should be "Standalone-SRE"
			SREConstants::MANIFEST_VM_ARGUMENTS should be "VM-Arguments"
		}

		/* The SARL project provides a <a href="http://maven.apache.org">Maven</a> plugin that enables the
		 * SRE developer to update the manifest file automatically when the SRE project is built/compile (with
		 * Maven).
		 * 
		 * <p>The Maven plugin is:
		 * <ul>
		 * <li>GroupID: <code>io.sarl.maven</code></li>
		 * <li>ArtifactID: <code>io.sarl.maven.sre</code></li>
		 * </ul>
		 * 
		 * <p>The mojo action defined by the SRE Maven plugin is `updatemanifest`. This mojo action
		 * updates the existing manifest file with the SRE information.
		 *
		 * <p>The following XML code gives an example of Maven configuration that enables to use the SRE Maven plugin.
		 * 
		 * <pre><code>
		 * &lt;build&gt;
		 * 	&lt;plugins&gt;
		 *		&lt;plugin&gt;
		 *			&lt;groupId&gt;io.sarl.maven&lt;/groupId&gt;
		 *			&lt;artifactId&gt;io.sarl.maven.sre&lt;/artifactId&gt;
		 *			&lt;version&gt;${sarl.version}&lt;/version&gt;
		 *			&lt;configuration&gt;
		 *				&lt;sreName&gt;TinyMAS&lt;/sreName&gt;
		 *				&lt;commandLineOptions&gt;
		 *					&lt;hideInfo&gt;&lt;/hideInfo&gt;
		 *					&lt;hideLogo&gt;&lt;/hideLogo&gt;
		 *					&lt;showInfo&gt;&lt;/showInfo&gt;
		 *					&lt;showLogo&gt;&lt;/showLogo&gt;
		 *					&lt;defaultContextId&gt;&lt;/defaultContextId&gt;
		 *					&lt;randomContextId&gt;&lt;/randomContextId&gt;
		 *					&lt;bootAgentContextId&gt;&lt;/bootAgentContextId&gt;
		 *					&lt;offline&gt;&lt;/offline&gt;
		 *					&lt;embedded&gt;&lt;/embedded&gt;
		 *					&lt;noMoreOption&gt;&lt;/noMoreOption&gt;
		 *					&lt;standaloneSRE&gt;true&lt;/standaloneSRE&gt;
		 *				&lt;/commandLineOptions&gt;
		 *				&lt;mainClass&gt;${cliRunnerMainClass}&lt;/mainClass&gt;
		 *			&lt;/configuration&gt;
		 *	
		 *			&lt;executions&gt;
		 *				&lt;execution&gt;
		 *					&lt;id&gt;update-manifest-standard&lt;/id&gt;
		 *					&lt;goals&gt;
		 *						&lt;goal&gt;updatemanifest&lt;/goal&gt;
		 *					&lt;/goals&gt;
		 *				&lt;/execution&gt;
		 *			&lt;/executions&gt;
		 *		&lt;/plugin&gt;
		 * 	&lt;/plugins&gt;
		 * &lt;/build&gt;
		 * </code></pre>
		 * 
		 * @filter(.*)
		 */
		fact "Maven Plugin for Updating the Manifest" {
			"http://maven.apache.org" should beURL "!file"
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
