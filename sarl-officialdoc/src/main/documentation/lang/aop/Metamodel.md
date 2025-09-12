# Metamodel of SARL in Brief

[:Outline:]

Guided by specific design principles, SARL aims to enable developers to extend the language to support various models and application domains. SARL's architecture agnostic design allows developers to implement MAS without being tied to specific architectural constraints. It emphasizes open for extension, ensuring that new functionalities can be seamlessly integrated into the framework. SARL also prioritizes easy interoperability with existing systems via Java support; allowing the language to leverage high-standard tools and robust developer support to facilitate adoption. Its approachable syntax further reduces the barrier to entry, making SARL accessible to a wide range of developers interested in harnessing the power of agent-based systems for various applications.
SARL's design deliberately allows the MAS designer to capture three dimensions of agent systems:

* **Individual:** focuses on the architecture of each agent within the system, encompassing their unique capabilities, behaviors, and decision-making processes. This dimension explores how individual agents are structured and how their internal mechanisms contribute to the overall functioning of the system.
* **Social:** refers to interactions, relationships, or phenomena that involve individuals or groups within a society. It focuses on the interactions between agents, norms, and institutions.
* **Collective:** emphasizes the aggregation or combination of individuals or things into a unified whole. This perspective zooms out to examine emergent behaviors and outcomes resulting from the interactions of all agents combined, highlighting how individual actions aggregate to shape system-level behavior and performance. Researchers have proposed different names for this aggregate agents, including *Collective agents*; *Agentified Groups*; and more.

## Major Features of SARL

SARL offers features to enable developers to capture the dimensions of a multi-agent system mentioned above.

### Dynamic agent reconfiguration and extensible architecture - The individual dimension

Adaptability is a fundamental characteristic of agents, yet many agent programming platforms traditionally lack the capability for dynamic agent reconfigurations. This limitation hinders developers' ability to efficiently adjust behaviors and skills in response to changing environments or task requirements. However, the SARL programming paradigm stands out by facilitating adaptability through a native API that supports dynamically registering (and unregistering) behaviors and skills. This feature allows agents to dynamically evolve their capabilities, enabling them to effectively cope with evolving tasks and contexts, thereby enhancing their overall flexibility and utility in complex scenarios. SARL supports this feature by the concepts of *Agent and Behavior*; and *Capacity and Skill*.

[:Success:]
	import io.sarl.api.core.Lifecycle
	import io.sarl.api.core.Logging
	import io.sarl.api.core.Initialize
	[:On]
	agent [:agentnamehw](HelloWorld) {

	    uses [:loggingcapname](Logging), [:lifecyclecapname](Lifecycle)

	    [:oninit](on [:initevent]{Initialize}) {
		[:infofct](info)("Hello World!")
		[:killmefct](killMe)
	    }
	}
[:End:]

Previous listing illustrates the fundamental structure of a SARL agent, including capacity usage, event handling, log, and lifecycle management.
It defines an agent named [:agentnamehw:] that,  after initialization,  outputs a simple message in the application log.

Capacities allow agents to integrate new capabilities. This agent uses two built-in capacities: [[:loggingcapname:]](./Logging.md) and [[:lifecyclecapname:]](./Lifecycle.md). The [[:loggingcapname:]](./Logging.md) capacity allows sending messages to a log or console by providing actions such as [:infofct:]. The [[:lifecyclecapname:]](./Lifecycle.md) capacity manages the agent's lifecycle features, such as the agent creation and termination, i.e., [:killmefct:] action on the last line of the provided code.

The [[:oninit:] block](./Agent.md#initialization-handler) is executed when the agent must be initialized at its first startup.
In SARL, agents perceive events that trigger its internal reactions. The [:initevent:] event is received by the agent when it starts to run.

            
### Domain-oriented social environment - The social dimension

Social capabilities have always been fundamental for multi-agent systems. Although most agent platforms support inter-agent communication; SARL, with an *open for extension* approach, goes beyond conventional platforms by supporting an array of communication channels ([Space](./Space.md#types-of-spaces) concept), each capable of unique rules and characteristics ([Space Specification](./Space.md#defining-a-space)). SARL simplifies agent interactions with a [default event-driven communication mechanism](./Space.md#event-space), but the space concept's flexibility allows to seamlessly integrate alternative communication abstractions such as protocols or organizational structures, and even diverse technologies like MQTT or Bluetooth. This adaptability ensures that SARL remains versatile and accommodates a wide spectrum of agent interactions and scenarios.

[:Success:]
	import io.sarl.api.core.DefaultContextInteractions
	import io.sarl.api.core.Logging
	[:On]
	event [:helloevent](Hello)

	event [:welcomeevent](Welcome)

	agent [:greetingsagent](Greetings) {

	    uses Logging, [:dcicapname](DefaultContextInteractions)

	    on Hello {
		info("Received a 'Hello' event from agent " + occurrence.source)
		[:emitfct](emit)(new Welcome) [[:eventtarget]([:itkeyword]{it.ID}) == [:eventsource]([:occurrencekeyword]{occurrence}.source.ID)]
	    }
	}
[:End:]

Previous listing illustrates how SARL agents can respond to specific events and engage in communication with other agents by emitting events. It begins by [defining two types of events](./Event.md): [:helloevent:] and [:welcomeevent:]. These events serve as signals (or messages) that can be sent and received between agents. For example, the first event is emitted by an agent when it presents itself to the other agents. The second event is the response of an agent to the one that initially emitted [:helloevent:].

The agent [:greetingsagent:] utilizes its [[:dcicapname:]](./DefaultContextInteractions.md) capacity to have access to the default interaction mechanisms, simplifying the process of handling and responding to events in a default interaction space. Upon receiving the [:helloevent:] event, the agent logs a message and emits a [:welcomeevent:] event using the [[:emitfct:] function](./DefaultContextInteractions.md), which is provided by the capacity [[:dcicapname:]](./DefaultContextInteractions.md). The [:welcomeevent:] event is transmitted back to the source agent that originally sent the [:helloevent:] event.
This is accomplished by matching the target agent identifier [:eventtarget:] with the source agent identifier [:eventsource:], using the lambda function (brackets) notation that can be assimilated to a condition in this context. The keywords [:itkeyword:] and [:occurrencekeyword:] represent the contextual object (address of the receiver here) and the current event instance, respectively.


### Collective agents - The collective dimension

SARL differentiates itself with its native support for collective agents, named *Holonic Agents* within the framework. In SARL, every agent is inherently a holon, characterized by a self-similar structure with the ability to compose with other agents through their contexts. This unique feature enables SARL agents to organically form hierarchical structures, facilitating complex interactions and cooperative behaviors within multi-agent systems. By embracing holonic principles, SARL empowers developers to create collective agents capable of seamlessly interacting with its composing (sub)agents.

[:Success:]
	import io.sarl.api.core.Logging
	[:On]
	event [:performtaskevent](PerformTask)

	agent [:workeragentname](Worker) {

	    uses Logging

	    [:onperformtask](on PerformTask) {
		info("Performing task as requested")
	    }
	}
[:End:]

Previous listing illustrates the definition of a subholon named [:workeragentname:] that responds to a specific event to perform a task. Within the subholon, the [:onperformtask:] block defines the behavior that should occur when the agent receives a [:performtaskevent:] event and, therefore, performs the expected task.

[:Success:]
	import io.sarl.api.core.Logging
	import io.sarl.api.core.Lifecycle
	import io.sarl.api.core.Initialize
	import io.sarl.api.core.InnerContextAccess
	import io.sarl.api.core.ExternalContextAccess
	import java.util.UUID
	agent Worker {}
	event PerformTask
	[:On]
	event [:requesttaskevent](RequestTask)

	agent Manager {

	    uses Lifecycle, InnerContextAccess, ExternalContextAccess
	    
	    var [:subagentidvar](subagentId) = UUID::randomUUID
	    
	    on Initialize {
		typeof(Worker).[:sicwidfct](spawnInContextWithID)(subagentId, innerContext)
	    }

	    on RequestTask {
		innerDefaultSpace.emit(new PerformTask) [it.ID == subagentId]
	    }
	}
[:End:]

Previous listing illustrates how a manager agent can spawn and interact with a subholon, showcasing the use of inner context management and targeted event emission for task delegation. It begins by defining an event type called [:requesttaskevent:]. This event serves as a signal that can be sent to the superholon to trigger specific behaviors. The capacity [[:lifecyclecapname:]](./Lifecycle.md) provides functions for creating subholons, here [:sicwidfct:]. A variable [:subagentidvar:] is initialized with a unique identifier to uniquely refer to the subholon that the supe-holon will create. During its initialization, the superholon [spawns an agent](./Lifecycle.md) of type [:workeragentname:] within its [inner context](./InnerContextAccess.md), assigning it the unique identifier stored in [:subagentidvar:]. When receiving a [:requesttaskevent:] event, the super-holon emits a [:performtaskevent:] event to the subholon identified by [:subagentidvar:].


## Metamodel of SARL

The three dimensions presented in the previous section and their associated features are supported by the underlying metamodel of SARL. This metamodel provides fundamental abstractions for implementing the three aforementioned dimensions and features. It draws inspiration from the CRIO model: [Capacity-Role-Interaction-Organization](http://www.aspecs.org).

![Major concepts of SARL metamodel.](./metamodel.png)

The main concepts of SARL are explained below and depicted in the previous figure.

*An [**agent**](./Agent.md) is an autonomous entity endowed with a set of skills to realize the capacities it exhibits.*
Natively, an agent has a set of [built-in capacities](./BIC.md) (BICs) considered essential to meet commonly accepted agent competencies, such as autonomy, reactivity, proactivity, and social abilities. Among these [BIC](./BIC.md), [Behaviors](./Behaviors.md)' capacity enables agents to incorporate a collection of behaviors that determine their global conducts. An agent also has a default behavior specified directly within its definition.

*A [**Behavior**](./Behavior.md) maps a collection of perceptions, represented by [Events](./Event.md), to a sequence of [Actions](../expr/FuncDecls.md).*
Each [action](../expr/FuncDecls.md) is a subroutine that may use [fields](../expr/VarDecls.md) (variables) that are declared within the [behavior](./Behavior.md) to realize its tasks. The various behaviors of an agent communicate using an [event-driven approach](./Space.md#event-space) by default.
*An [**event**](./Event.md) is the specification of some occurrence in a space that may potentially trigger effects by a listener*, such as an [agent](./Agent.md) or [behavior](./Behavior.md).

*An [**action**](../expr/FuncDecls.md) specifies a transformation of a part of the designed system or its environment*, guaranteeing resulting properties if the system before the transformation satisfies a set of constraints.

*A [**capacity**](./Capacity.md) is the specification of a collection of [actions](../expr/FuncDecls.md)* makes no assumptions about their implementation. It can specify what an agent can do and what a behavior requires for its execution.

*A [**skill**](./Skill.md) is a possible implementation of a [capacity](./Capacity.md) fulfilling all the constraints of this specification.*
An agent can dynamically evolve by learning or acquiring new capacities, but it can also dynamically change the skill associated with a given capacity.
Acquiring new capacities enables an agent to access new behaviors, providing a self-adaptation mechanism that allows agents to dynamically change their architecture according to their current needs and goals.

*A [**context**](./Agent.md) defines the perimeter or boundary of a sub-system, and gathers a collection of [spaces](./Space.md). A [**space**](./Space.md) represents the support for interaction between agents.*
In each context, there is at least one particular space called *Default Space* to which all agents in this context belong, ensuring a common space for all agents within the same context. Each agent can then create specific public or private spaces to achieve its personal goals. 
Since their creation, agents are incorporated into a context called the *Default Context*.
The notion of Context makes complete sense when agents are considered composed or holonic.



[:Include:](../../includes/legal.inc)
