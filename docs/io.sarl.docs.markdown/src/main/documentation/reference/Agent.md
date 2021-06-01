# Agent Reference

[:Outline:]

This document describes the features related to the definition of an agent in SARL.
Before reading this document, we recommend that you read
the [General Syntax Reference](./GeneralSyntax.md),
the [Skill Reference](./Skill.md), and the
[Built-in Capacity Reference](./BIC.md).

__An agent is an autonomous entity having a set of skills to realize the capacities it exhibits__.


## Where is living an agent?

Before detailing the architecture and the definition tools of an agent, it may be helpful to understand
where is "living" an agent in the multi-agent system.

The following figure illustrates the position of an agent (at the center of the figure) in different contexts.
The details are discussed below. 

![Contexts](./contexts.png)


### Default Context

When it is spawn, an agent is living inside the system in a place named "_Context_".

__A Context defines the boundary of a sub-system, and gathers a collection of spaces__.
A Space is the support of the interaction between agents respecting the rules defined in the spaces' specification.

> **_Important Note:_** In each context, there is at least one particular space called _Default Space_ to which
> all agents in this context belong.

It ensures the existence of a common shared space to all agents in the same context. Each agent can then create
specific public or private spaces to achieve its personal goals (the blue space on the figure above).

> **_Important Note:_** Since their creation, agents are incorporated into a context called the __Default Context__.
> It is important to notice that the _Default Context_ is not necessarily the same for every agent.

An agent has an identifier for each space it is involved in. For the case of event-based interaction
spaces, this identifier is  called "address". 


### External Contexts

During its lifetime, an agent may join and participate in other contexts that are not the default context.
They are called the external contexts of the agent.

> **_Note:_** There is no restriction about the number of contexts in which an agent is belonging to, except
> that it is always in its default context.

For joining or leaving a context, the agent must use the `ExternalContextAccess` built-in capacity. It is detailed in the
[Built-in Capacity Reference](./BIC.md).


### From Flat to Hierarchical System with the Inner Context

In 1967, Arthur Koestler coined the term _holon_ as an attempt to
conciliate holistic and reductionist visions of the world.
A holon represents a part-whole construct that can be seen as a
component of a higher level system or as whole composed of other
self-similar holons as substructures.

Holonic Systems grew from the need to find comprehensive construct
that could help explain social phenomena. Since then, it came to be
used in a wide range of domains, including Philosophy,
Manufacturing Systems, and Multi-Agents Systems.

Several works have studied this question and they have proposed a number
of models inspired from their experience in different domains.
In many cases, we find the idea of _agents composed of other agents_.

More recently, the importance of holonic multi-agent systems has been
recognized by different methodologies such as [ASPECS](http://www.aspecs.org) or O-MASE.

> **_Note:_** In SARL, we recognize that agents can be composed of other agents. Therefore, SARL agents
> are in fact holons that can compose each other to define hierarchical
> or recursive multi-agent system, called holarchies.

In order to achieve this, SARL agents are self-similar structures that 
compose each other via their contexts. Each agent defines its own context, called __Inner Context__.
Because this inner context may be joined by other agents, or agents may
be spawn inside this inner context, it is possible to build a holarchy.

> **_Very Important Note:_** An agent is always a participant of the default space of its inner space.

> **_Important Note:_** The unique identifier (usually a Unique Universal Identifier) of
> the inner context is equal to the unique identifier of its owning agent.


### Universe Agent and Universe Context

At the top level of the holarchy, we consider an omnipresent agent. It is named the __Universe Agent__ (or _Root Agent_).
The runtime environment will be in charge of spawning the first agents in the system as members of the Universe Agent.

The inner context of the Universe Agent is called the Universe Context, or the Janus Context if you are using the
[Janus runtime environment](http://www.janusproject.io).


## Open Architecture of an Agent

The architecture of an agent is illustrated by the following figure.

![Agent](./agent.png)


### Built-in Capacities

An agent has a set of built-in capacities considered essential to respect the
commonly accepted competencies of agents, such autonomy, reactivity, pro-activity
and social capacities. 

The full set of Built-in Capacities will be presented in the corresponding
[Reference document](./BIC.md). Among these
built-in capacities, is the `Behaviors` capacity that enables 
agents to incorporate a collection of behaviors that will determine 
its global conduct.


### Agent Behaviors

An agent has also a default behavior directly described within its definition.

__A Behavior maps a collection of perceptions represented by Events to a sequence of Actions.__
The various behaviors of an agent communicate using an event-driven approach.

__An Event is the specification of some occurrence in a Space that may
potentially trigger effects by a listener__ (e.g., agent, behavior, etc.) 


## Defining an Agent

An agent is declared with the [:agentkw:] keyword. In the agent's body block, we can declare Mental States
(in the form of attributes), Actions and Behaviors.


### Defining an empty agent

The following code illustrates the definition of an agent named [:agenttype1:], and that is empty.
Basically, this agent does nothing, and does not react on events. 

[:Success:]
	package io.sarl.docs.reference.ar
	[:On]
	[:agentkw](agent) [:agenttype1](MyAgent) {
	}
[:End:]


### Agent Attributes

The mental state of an agent is composed by the data in the knowledge of the agent.
Most of the time, the mental state is implemented as a collection of attributes in the agent.

According to the [General Syntax Reference](./GeneralSyntax.md),
the attributes may be modifiable (when declared with the [:varmodifier:] keyword), or unmodifiable (when
declared with the [:valmodifier:] keyword).

[:Success:]
	package io.sarl.docs.reference.ar
	[:On]
	agent MyAgent {
		// Defining a modifiable element of the mental state
		[:varmodifier](var) mentalStateElement1 : String
		// Defining an unmodifiable element of the mental state
		[:valmodifier](val) mentalStateElement2 : boolean = true
	}
[:End:]


### Agent Actions

It is allowed to define actions (methods) in the agent.
The syntax described is described in the [General Syntax Reference](./GeneralSyntax.md).

The example below illustrates the creation of two actions in the agent.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	[:On]
	agent MyAgent {
		uses Logging
		// Defining an action without parameter nor return type
		def myAction1 {
			info("Hello world")
		}
		// Defining an action with a variadic parameter and no return type
		def myAction2(param : int*) {
			info("params are " + param)
		}
	}
[:End:]


### Extending an Agent

In some use cases, it is useful to specialize the definition
of an agent. This mechanism is supported by the inheritance
feature of SARL, which has the same semantic as the inheritance
mechanism as the Java object-oriented language.

The extended agent is specified just after the [:extendskw:] keyword.

> **_Very Important Note:_** An agent type can extend __only one__ other agent type. This is close
> to the constraint on the extension of classes in the Java language.

In the following code, a first agent is defined with the name
[:agenttype1:] and an attribute named [:attrfield:].
A second agent [:agenttype2:] is defined as the extension of the first agent. It contains a function named
[:actiondef:], which is displaying the inherited attribute.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	[:On]
	agent [:agenttype1!] {
		protected var [:attrfield](attr) : String
	}
	agent [:agenttype2](MySubAgent) [:extendskw](extends) [:agenttype1!] {
		uses Logging
		def [:actiondef](action) {
			info(attr)
		}
	}
[:End:]


### Modifiers

Modifiers are used to modify declarations of types and type members.
This section introduces the modifiers for the agent.
The modifiers are usually written before the keyword for defining the agent.

The complete description of the modifiers' semantic is available in
[this section](./OOP.md#definition-of-all-the-supported-modifiers).


#### Agent Modifiers

An agent may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]: the agent is accessible from any other type (default);
	* [:packagemodifier:]: the agent is accessible from only the types in the same package.
* [:abstractmodifier:]: the agent is abstract and cannot be instanced.
* [:finalmodifier:]: avoid to be derived.

Examples:

[:Success:]
	package io.sarl.docs.reference.ar
	[:On]
	[:publicmodifier](public) agent Example1 {
	}
	[:packagemodifier](package) agent Example2 {
	}
	[:abstractmodifier](abstract) agent Example3 {
	}
	[:finalmodifier](final) agent Example4 {
	}
[:End:]


#### Field Modifiers

The modifiers for the fields in an agent are:

* Access modifiers:
	* [:protectedmodifier:]: the field is accessible within the same package, and derived agents;
	* [:packagemodifier:]: the field is accessible only within the same package of its agent;
	* [:privatemodifier:]: the field is accessible only within its agent (default).
* [:staticmodifier:]: the field is a class field, not an instance field.

Examples:

[:Success:]
	package io.sarl.docs.reference.ar
	agent MyAgent {
	[:On]
		[:protectedmodifier](protected) var example1 : Object
		package var example2 : Object
		[:privatemodifier](private) var example3 : Object
		[:staticmodifier](static) var example4 : Object
	[:Off]
	}
[:End:]


#### Method Modifiers

The modifiers for the methods in an agent are:

* Access modifiers:
	* [:protectedmodifier:]: the method is accessible within the same package, and derived classes (default);
	* [:packagemodifier:]: the method is accessible only within the same package as its class;
	* [:privatemodifier:]: the method is accessible only within its class.
* [:abstractmodifier:]: the method has no implementation in the class.
* [:dispatchmodifier:]: the method provides an implementation for the dispatch method mechanism.
* [:finalmodifier:]: the method cannot be overridden in derived classes.
* [:staticmodifier:]: the method is a class method, not an instance method.
* [:synchronizedmodifier:]: the method is synchronized on the class instance.

Examples:

[:Success:]
	package io.sarl.docs.reference.ar
	abstract agent MyAgent {
	[:On]
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
		[:staticmodifier](static) def example6 { }
		// Synchronized function
		[:synchronizedmodifier](synchronized) def example7 { }
		// Dispatch functions
		[:dispatchmodifier](dispatch) def example8(p : Integer) { }
		dispatch def example8(p : Float) { }
	[:Off]
	}
[:End:]


#### Restriction on the Nested Type Modifiers

All the [modifiers for the nested types](./OOP.md) are allowed **except**
[:publicmodifier:]. 

[:Failure:]
	package io.sarl.docs.reference.ar
	agent Agent1 {
		public static class NedtedClass1 {
		}
	}
[:End:]


## Behaviors of an Agent

The behaviors of an agent correspond to the units that are executed by the agent for exhibiting its general behavior.

The execution of the behaviors are related to the life cycle of the agents. Every agent is following the steps:

* Initialization: the agent react on the [:initializeevent:] event;
* Lifetime: the agent execute its behaviors. They may be:
	* reactive: the agent react when it is receiving events, or
	* proactive: the agent executes by itself one of its behaviors.
* Destruction: the agent react on the [:destroyevent:] event.

The definition of the reactive behaviors is based on the event handling
mechanism of SARL. Events may be emitted in [spaces](./Space.md),
and received by the agents belonging to these spaces.
An agent may indicate that it is interesting for receiving an event by specifying
an event handler using the following syntax:

[:Success:]
	package io.sarl.docs.reference.ar
	event EventName
	agent Agent1 {
		@Pure
		static def Guard : boolean { true }
		def Statements : void { }
	[:On]
		on [:eventnameref](EventName) [[:guardref](Guard)] {
			[:statementref](Statements)
		}
	[:Off]
	}
[:End:]


[:eventnameref:] is the name of the events to wait for. [:guardref:] is the optional definition of a predicate
that may be true for executing the [:statementref:]. The statements are executed only if an event with the given name is
received, __and__ if the guard is true.

In the guard and the statements, it is possible to use the instance of the received event: the occurrence.
This instance is represented by the [:occurrencekw:] keyword. It is an implicit
variable as the keywords `this` and `it`.


### Initialization Handler

#### General Description

When an agent is ready to be executed by the runtime environment, it receives the [:initializeevent:] event.
This event is defined as:

[:ShowType:](io.sarl.core.[:initializeevent]{Initialize})


It contains the list of the parameters given to the spawning function (as specified in the 
[built-in capacities](./BIC.md)).

[:Success:]
	package io.sarl.docs.reference.ar
	import java.util.UUID
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	agent TestInitializeContent {
		on Initialize {
			var spawnerID : UUID = occurrence.spawner
			var parameters : Object[] = occurrence.parameters
		}
	}
	[:On]
	agent MyAgent {
		uses Logging
		on Initialize {
			info("My spawner is " + occurrence.spawner)
			info(
				"My initialization parameters are: "
				+ [:occurrencekw](occurrence).parameters )
		}
	}
[:End:]


#### Guarded Initialization Handler

Because [:initializeevent:] is an event, the handler in the agent could use a guard. This feature enables
the developer to write different initialization blocks depending on the guards of the handlers.

In the following example, the first event handler is executed when the [:initializeevent:] event has
no parameter. The second event handler is executed when the event has at least one parameter.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	[:On]
	agent MyAgent {
		uses Logging
		on Initialize [ occurrence.parameters.empty ] {
			info("Initialization without parameters")
		}
		on Initialize [ ! occurrence.parameters.empty ] {
			info("Initialization with parameters: "
				+ occurrence.parameters )
		}
	}
[:End:]


#### Execution of the Initialization Handler

The `on Initialize` event handler in agents is a bit special, as it is the code run when an agent is born.
As such, its execution is more "synchronous" than other on-behavior rules. In particular:

1. Any event emitted within an `on Initialize`, will not be processed until that
   `on Initialize` code finishes. So, your agent initialization should not depend
   (and wait) on any fired event being processed, as they won't!
2. When spawning an agent in `on Initialize`, the spawn instructions will return only
   after the agent has been created. However, creation of the agent (i.e., of the
   corresponding object) does not include initialization of the agent via its 
   `on Initialize` handler. Said so, the Java thread manager may process those
   initialization processes of the new agent before continuing with the execution
   of the spawning agent (and this seems to be the case in many Linux boxes
   where the executor service of Java tends to have the same behavior during
   all the runs). If you change computer, it may be different. In the following
   example, the thread executor service of Java seems to give the priority to
   the `on Initialize` of [:agenttwoname:] instead of continuing the run of the
   spawn function.

[:Success:]
    package io.sarl.docs.faq.general
    import io.sarl.core.Initialize
    import io.sarl.core.Logging
    import io.sarl.core.Lifecycle
    [:On]
    agent Agent1 {
        uses Logging, Lifecycle
        var agent_name = "agent1"
        on Initialize {
            info(agent_name + " spawned")
            info(agent_name + " spawning Agent2")
            spawn(Agent2)
            info(agent_name + " end")
        }
    }

    agent [:agenttwoname](Agent2) {
        uses Logging
        var agent_name = "agent2"
        on Initialize {
            info(agent_name + " spawned")
            info(agent_name + " sleeping")
            Thread::sleep(5000)
            info(agent_name + " woke up")
            info(agent_name + " end")
        }
        on Initialize {
            info(agent_name + " init2")
            info(agent_name + " init2 end")
        }
    }
[:End:]


The output has been:

```text
Launching the agent: Agent1
agent1 spawned
agent1 spawning Agent2
agent2 spawned
agent2 init2
agent2 sleeping
agent2 init2 end
agent2 woke up
agent2 end
agent1 end
```

Here it appears as the `on Initialize` behaviors have been run all before
the execution resumes after the `spawn()` statement, but this is just one way
and one should not rely on that behavior being guaranteed: once the spawned
agent is created, the `spawn()` commands returns.


#### Multiple Initialization Handlers

It is allowed to declare multiple initialization handlers into a single agent type, as illustrated by:

[:Success:]
    package io.sarl.docs.faq.general
    import io.sarl.core.Initialize
    import io.sarl.core.Logging
    [:On]
    agent Agent3 {
    	uses Logging
        on Initialize {
            info("1")
        }
        on Initialize {
            info("2")
        }
        on Initialize {
            info("3")
        }
    }
[:End:]


According to the SARL operational semantic, the three event handlers for `Initialize` are run in parallel.
The initialization event handlers are not constructors (as defined in object-oriented programming paradigm),
they are reacting to the receiving of an `Initialize` occurrence.
Consequently, we could say that there is a single `Initialize` occurrence during the whole life of an agent;
But, it may have multiple handlers to react to the receiving of this event.


#### Initialization Handler within the Inheritance Hierarchy

The example in the previous section could be extended in order to illustrate how the initialization handlers
are run when the type of the agent (here [:agent4name:]) is declared within a inheritance hierarchy.

[:Success:]
    package io.sarl.docs.faq.general
    import io.sarl.core.Initialize
    import io.sarl.core.Logging
    agent Agent3 {
    }
    [:On]
    agent [:agent4name](Agent4) extends [:agent3name](Agent3) {
    	uses Logging
        on Initialize {
            info("4")
        }
        on Initialize {
            info("5")
        }
    }
[:End:]

According to the SARL operational semantic, all the initialization handlers are run in parallel.
In the previous example, five event handlers will be run: three are defined into [:agent3name:], and
two are defined into [:agent4name:]. This mechanism is generalized to all the events within an agent.


### Destruction Handler

#### General Description

The counterpart of [:initializeevent:] is the event [:destroyevent:]. This event is defined as:

[:ShowType:](io.sarl.core.Destroy)


Example:

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Destroy
	[:On]
	agent MyAgent {
		uses Logging
		on [:destroyevent](Destroy) {
			info("Destroying the agent")
		}
	}
[:End:]


#### Guarded Destruction Handler

As for [:initializeevent:], the handlers of the [:destroyevent:] event could be guarded.

In the following example, the first event handler is executed when the [:destroyevent:]
is receivedand there is resource stored in the corresponding field. The second event handler
is executed when there is no resource.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Destroy
	[:On]
	agent MyAgent {
		uses Logging
		var resource : Object
		on Destroy [ resource !== null ] {
			info("Destroying the agent when there is a resource")
		}
		on Destroy [ resource === null ] {
			info("Destroying the agent when there is no resource")
		}
	}
[:End:]


### Reactive Behaviors

The reactive behavior of an agent is specified with a collection
of event handlers. The principle of a reactive behavior is to execute a part of the global
agent behavior when something has happening in the agent, or in its environment.

In the following example, the agent is reacting to the reception of the [:eventtype1:] event.

As for all the event handlers, it could be guarded by a predicate.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	event SomethingChanged
	[:On]
	agent MyAgent {
		uses Logging
		on [:eventtype1](SomethingChanged) {
			info("Reactive behavior")
		}
	}
[:End:]


### Parallel Execution of the Reactive Behaviors

When an event is received and the guard of the corresponding handler is true, the event
handler is said to be triggered.

When multiple event handlers are triggered at the same time, they are all executed in parallel.
In the following example, the two handlers for the [:eventtype1:] event are executed in parallel.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	event SomethingChanged
	[:On]
	agent MyAgent {
		uses Logging
		on [:eventtype1!] {
			info("First reactive behavior")
		}
		on [:eventtype1!] {
			info("Second reactive behavior")
		}
	}
[:End:]


### Pro-active Behaviors

A proactive behavior is a part of the global behavior of an agent that the
agent is deciding to execute by itself.
The execution of a reactive behavior is initiated by a part of the code external to this
behavior. In opposite, the initiator of the execution of a proactive behavior is the
agent itself.

In SARL, a proactive behavior is a behavior that is scheduled by the agent. The scheduling
mechanism is provided by the [[:schedulescap:] built-in capacity](./BIC.md).
In the following example, the agent execute its proactive behavior every second.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	import io.sarl.core.Schedules
	[:On]
	agent MyAgent {
		uses [:schedulescap](Schedules), Logging
		on Initialize {
			every(1000) [
				info("Run a pro-active behavior")
			]
		}
	}
[:End:]


## Capacities and Skills

An agent is an autonomous entity having a set of skills to realize the capacities it exhibits.
An agent has a set of built-in capacities considered essential to respect the commonly
accepted competencies of agents, such autonomy, reactivity, pro-activity and social capacities. 


### Defining a Capacity and a Skill

The definition of a capacity or a skill is out of the scope of this reference document. For details, please read
the [Capacity Reference](./Capacity.md), and the [Skill Reference](./Skill.md).

In the rest of this section, it is assumed that the following capacity and skill are defined:

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	[:On]
	capacity Cap {
		def action
	}
	
	skill Ski implements Cap {
		uses Logging
		def action {
			info("Action")
		}
	}
[:End:]


### Giving a Skill to an Agent

When an agent must use a capacity in one of its behaviors, it must own an implementation of this capacity: a skill.

For assigning a skill to an agent, the instance of the skill must be created. Then, it is associated with the
implemented capacity.
In the following example, the agent is creating the [:skiskill:] skill. This instance is associated with
the corresponding capacity [:capcap:] with the function `[:setskillfctfull]{[:setskillfct](setSkill)(Skill, Class<? extends Capacity>*)}`.

When the function [:setskillfct:] is returning, the agent becomes able to use the skill.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	capacity Cap {
		def action
	}
	skill [:skiskill](Ski) implements [:capcap](Cap) {
		uses Logging
		def action {
			info("Action")
		}
	}
	[:On]
	agent MyAgent {
		on Initialize {
			var theSkill = new Ski
			[:setskillfct!](theSkill)
		}
	}
[:End:]



If some cases, you may want to set the skill if one was not set up before. The specific behavior
is supported by `[:setskillifabsfctfull]{[:setskillifabsfct](setSkillIfAbsent)(Skill, Class<? extends Capacity>*)}`.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	capacity Cap {
		def action
	}
	skill [:skiskill](Ski) implements [:capcap](Cap) {
		uses Logging
		def action {
			info("Action")
		}
	}
	[:On]
	agent MyAgent {
		on Initialize {
			var theSkill = new Ski
			[:setskillifabsfct!](theSkill)
		}
	}
[:End:]


### Giving a Built-in Skill to an Agent

Because the built-in capacities are supported by the runtime environment, the corresponding
skills are given to the agent. It means that there is no need for an agent to set a skill for
a built-in capacity.

However, in rare cases, it is possible to use the function [:setskillfctfull:] for
changing the implementation of a built-in capacity.


### Using a Capacity with the Getters

After a skill is registered into the agent, it could be invoked.

For invoking a function implemented by a skill, the two following steps must be done:

* Retrieve the skill instance: the function `[:getskillfct!](Class<? extends Capacity>)` permits retrieving the skill associated to the given capacity;
* Invoke the capacity's action on the retrieved skill.

> **_Note:_** This method of invocation is not recommended by the SARL developers. You should
> prefer the use of the extension methods (see below).

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	capacity Cap {
		def action
	}
	skill Ski implements Cap {
		uses Logging
		def action {
			info("Action")
		}
	}
	event SomeEvent
	[:On]
	agent MyAgent {
		on Initialize {
			var s = new Ski
			setSkill( s, Cap )
		}
		on SomeEvent {
			// Retreive the capacity implementation
			var s = [:getskillfct](getSkill)(Cap)
			// Run the action of the skill
			s.action
		}
	}
[:End:]


### Using a Built-in Capacity with the Getters

The built-in capacities are accessible in the same way as the other capacities, with the getters.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Lifecycle
	event SomeEvent
	[:On]
	agent MyAgent {
		on SomeEvent {
			// Retreive the capacity implementation
			var s = getSkill(Lifecycle)
			// Run the action of the skill
			s.killMe
		}
	}
[:End:]


### Using a Capacity with the Extension Methods

Invoking a capacity/skill with the getter method is not user-friendly. Since the
[General Syntax Reference](./GeneralSyntax.md) describes the "extension method"
mechanism, it is possible to use it for invoking the capacities.

But, instead of using an [:importkw:] directive, the [:useskw:] keyword is provided for importing the
capacities into the agent. In the following example, the [:capcap:] capacity is imported.

After a capacity was "imported", it is possible to directly call the functions of the capacity
(according to the extension method syntax). In the following example, the action
with the name [:action1:] is invoked. This action is defined in the [:capcap:] capacity. 

[:Success:]
	package io.sarl.docs.reference.ar
	[:importkw](import) io.sarl.core.Logging
	import io.sarl.core.Initialize
	capacity [:capcap!] {
		def action
	}
	skill [:skiskill!] implements [:capcap!] {
		uses Logging
		def action {
			info("Action")
		}
	}
	event SomeEvent
	[:On]
	agent MyAgent {
		[:useskw](uses) [:capcap!]
		on Initialize {
			var s = new [:skiskill!]
			setSkill(s)
		}
		on SomeEvent {
			// Run the action of the skill
			[:action1](action)
		}
	}
[:End:]


### Using a Built-in Capacity with the Extension Methods

The built-in capacities are accessible in the same way as the other capacities, with the extension methods.

Example:

[:Success:]
	package io.sarl.docs.reference.ar
	[:importkw](import) io.sarl.core.Logging
	import io.sarl.core.Lifecycle
	event SomeEvent
	[:On]
	agent MyAgent {
		uses Lifecycle
		on SomeEvent {
			// Run the action of the skill
			killMe
		}
	}
[:End:]


[:Include:](../legal.inc)
