# Behavior Reference

[:Outline:]

This document describes the features related to the definition of a behavior in SARL.
Before reading this document, we recommend that you read
the [General Syntax Reference](./GeneralSyntax.md),
and the [Agent Reference](./Agent.md).

A Behavior is the specification of a collection of behavior units (or event handlers).
This Behavior may be used by an agent for building its global behavior.


## Defining a Behavior

A behavior is declared with the [:behaviorkw:] keyword. In the behavior's body block, we can declare Mental States
(in the form of attributes), Actions and Behaviors.


### Defining an empty behavior

The following code illustrates the definition of a behavior named [:behaviortype1], and that is empty.

Basically, this behavior does nothing, and does not react on events.

[:Success:]
	package io.sarl.docs.reference.br
	[:On]
	[:behaviorkw](behavior) [:behaviortype1](MyBehavior) {
	}
[:End:]


### Behavior Attributes

The mental state of an agent is composed by the data in the knowledge of the agent. A behavior may contain
a part of this mental state. Most of the time, it is implemented as a collection of attributes.

According to the [General Syntax Reference](./GeneralSyntax.md), the attributes may be
modifiable (when declared with the [:varmodifier:] keyword), or unmodifiable (when declared with the
[:valmodifier:] keyword).

[:Success:]
	package io.sarl.docs.reference.br
	[:On]
	behavior [:behaviortype1!] {
		// Defining a modifiable element of the mental state
		[:varmodifier](var) mentalStateElement1 : String
		// Defining an unmodifiable element of the mental state
		[:valmodifier](val) mentalStateElement2 : boolean = true
	}
[:End:]


### Behavior Actions

It is allowed to define actions (methods) in the behavior. The syntax described in the
[General Syntax Reference](./GeneralSyntax.md) is used.

The example below illustrates the creation of type actions.

[:Success:]
	package io.sarl.docs.reference.br
	import io.sarl.core.Logging
	[:On]
	behavior [:behaviortype1!] {
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


### Extending a Behavior

In some use cases, it is useful to specialize the definition of a behavior. This mechanism is supported
by the inheritance feature of SARL, which has the same semantic as the inheritance
mechanism as the Java object-oriented language.

The extended behavior is specified just after the [:extendskw:] keyword.

> **_Very Important Note:_** A behavior type can extend __only one__ other behavior type. This is close
> to the constraint on the extension of classes in the Java language.

In the following code, a first behavior is defined with the name [:behaviortype1:] and an attribute named [:attrfield:].
A second behavior [:behaviortype2!] is defined as the extension of the first behavior. It contains a function named
[:actiondef:], which is displaying the inherited attribute.

[:Success:]
	package io.sarl.docs.reference.br
	import io.sarl.core.Logging
	[:On]
	behavior [:behaviortype1!] {
		protected var [:attrfield](attr) : String
	}
	behavior [:behaviortype2](MySubBehavior) [:extendskw](extends) [:behaviortype1!] {
		uses Logging
		def [:actiondef](action) {
			info(attr)
		}
	}
[:End:]


### Instancing and Use of a Behavior

A behavior is always owned by an agent. Consequently, it is mandatory to pass the agent as parameter
of the behavior's constructor.

In the following example, a behavior of type [:behaviortype1:] is instanced (with the agent as the
owner/parameter). This new behavior is then registered into the agent for enabling the reception of
the events in the behavior.

[:Success:]
	package io.sarl.docs.reference.br
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	import io.sarl.core.Behaviors
	behavior [:behaviortype1!] { }
	[:On]
	agent MyAgent {
		uses Behaviors
		on Initialize {
			// Create the instance of the behavior
			var beh = new [:behaviortype1!](this) // <- the parameter is the agent
			
			// Register the behavior for receiving the events.
			// This function is given by the Behaviors capacity
			registerBehavior(beh)
		}
	}
[:End:]


### Modifiers

Modifiers are used to modify declarations of types and type members.
This section introduces the modifiers for the behavior. The modifiers are usually
written before the keyword for defining the behavior.

The complete description of the modifiers' semantic is available in
[this section](./OOP.md#definition-of-all-the-supported-modifiers).


#### Behavior Modifiers

A behavior may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]: the behavior is accessible from any other type (default);
	* [:packagemodifier]: the behavior is accessible from only the types in the same package.
* [:abstractmodifier:]: the behavior is abstract and cannot be instanced.
* [:finalmodifier:]: avoid to be derived.

Examples:

[:Success:]
	package io.sarl.docs.reference.br
	[:On]
	[:publicmodifier](public) behavior Example1 {
	}
	[:packagemodifier](package) behavior Example2 {
	}
	[:abstractmodifier](abstract) behavior Example3 {
	}
	[:finalmodifier](final) behavior Example4 {
	}
[:End:]


#### Field Modifiers

The modifiers for the fields in a behavior are:

* Access modifiers:
	* [:publicmodifier:]: the field is accessible from everywhere;
	* [:protectedmodifier:]: the field is accessible within the same package, and derived agents;
	* [:packagemodifier:]: the field is accessible only within the same package of its agent;
	* [:privatemodifier:]: the field is accessible only within its agent (default).
* [:staticmodifier:]: the field is a class field, not an instance field.

Examples:

[:Success:]
	package io.sarl.docs.reference.br
	behavior MyBehavior {
	[:On]
		[:publicmodifier](public) var example0 : Object
		[:protectedmodifier](protected) var example1 : Object
		package var example2 : Object
		[:privatemodifier](private) var example3 : Object
		[:staticmodifier](static) var example4 : Object
	[:Off]
	}
[:End:]


#### Method Modifiers

The modifiers for the methods in a behavior are:

* Access modifiers:
	* [:publicmodifier:]: the method is accessible from everywhere;
	* [:protectedmodifier:]: the method is accessible within the same package, and derived classes (default);
	* [:packagemodifier:] the method is accessible only within the same package as its class;
	* [:privatemodifier:]: the method is accessible only within its class.
* [:abstractmodifier:]: the method has no implementation in the class.
* [:dispatchmodifier:]: the method provides an implementation for the dispatch method mechanism.
* [:finalmodifier:]: the method cannot be overridden in derived classes.
* [:staticmodifier:]: the method is a class method, not an instance method.

Examples:

[:Success:]
	package io.sarl.docs.reference.br
	abstract behavior MyBehavior {
	[:On]
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
		[:dispatchmodifier](dispatch) def example7(p : Integer) { }
		dispatch def example7(p : Float) { }
		// Static / Class function
		static def example8 { }
	[:Off]
	}
[:End:]


## Behavior Units of a Behavior

The behaviors of an agent correspond to the units that are executed by the agent for exhibiting
its general behavior.

The Behavior statement permits specifying a subset of the agent's behavior inside a single syntactic entity.
Two types of behaviors are considered:

* reactive: the agent react when it is receiving events, and
* pro-active: the agent executes by itself one of its behaviors.


The definition of the reactive behaviors is based on the event handling
mechanism of SARL. Events may be emitted in [spaces](./Space.md),
and received by the agents, and their behaviors, belonging to these spaces.
A behavior may indicate that it is interesting for receiving an event by specifying
an event handler using the following syntax:

[:Success:]
	package io.sarl.docs.reference.ar
	event EventName
	behavior Behavior1 {
		@Pure
		def Guard : boolean { true }
		def Statements : void { }
	[:On]
		on [:eventtype1](EventName) [ [:guard](Guard) ] {
			[:statements](Statements)
		}
	[:Off]
	}
[:End:]


[:eventtype1:] is the name of event to wait for [:guard:] is the optional specification of a predicate
that may be true for executing the [:statements:].
The statements are executed only if an event with the given name is received, *and* if the guard is true.

In the guard and the statements, it is possible to use the instance of the received event:
the occurrence. This instance is represented by the [:occurrencekw:] keyword. It is an implicit
variable as the keywords `this` and `it`.


### Initialization Handler


#### General Description

When a behavior is ready to be executed by the runtime environment, usually when it
is registered in its owning agent, it receives the [:initializeevent:] event.
This event is defined as:

[:ShowType:](io.sarl.core.[:initializeevent]{Initialize})

It contains the list of the parameters given that are never set for behaviors.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	[:On]
	behavior MyBehavior {
		uses Logging
		on [:initializeevent!] {
			info("I'm initializing my behavior")
		}
	}
[:End:]


#### Guarded Initialization Handler

Because [:initializeevent:] is an event, the handler in the behavior could use a guard. This feature enables
the developer to write different initialization blocks depending on the guards of the handlers.

In the following example, the first event handler is executed when the [:initializeevent:] event has
no parameter. The second event handler is executed when the event has at least one parameter.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	[:On]
	behavior MyBehavior {
		uses Logging
		on [:initializeevent!] [ [:occurrencekw](occurrence).parameters.empty ] {
			info("First initialization")
		}
		on [:initializeevent!] [ ! occurrence.parameters.empty ] {
			info("First initialization")
		}
	}
[:End:]


#### Execution of the Initialization Handler

The `on Initialize` event handler in behaviors is a bit special, as it is the code run when a behavior is attached to its agent.
As such, its execution is more "synchronous" than other on-behavior rules. In particular:

1. Any event emitted within an `on Initialize`, will not be processed until that
   `on Initialize` code finishes. So, your behavior initialization should not depend
   (and wait) on any fired event being processed, as they won't!
2. When spawning an agent in `on Initialize`, the spawn instructions will return only
   after the agent has been created. However, creation of the agent (i.e., of the
   corresponding object) does not include initialization of the agent via its 
   `on Initialize` handler. Said so, the Java thread manager may process those
   initialization processes of the new agent before continuing with the execution
   of the spawning agent (and this seems to be the case in many Linux boxes
   where the executor service of Java tends to have the same behavior during
   all the runs). If you change computer, it may be different. 


#### Multiple Initialization Handlers

It is allowed to declare multiple initialization handlers into a single behavior type, as illustrated by:

[:Success:]
    package io.sarl.docs.faq.general
    import io.sarl.core.Initialize
    import io.sarl.core.Logging
    [:On]
    behavior Beh1 {
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


#### Initialization Handler within the Inheritance Hierarchy

The example in the previous section could be extended in order to illustrate how the initialization handlers
are run when the type of the behavior (here [:beh2name:]) is declared within a inheritance hierarchy.

[:Success:]
    package io.sarl.docs.faq.general
    import io.sarl.core.Initialize
    import io.sarl.core.Logging
    behavior Beh1 {
    }
    [:On]
    behavior [:beh2name](Beh2) extends [:beh1name](Beh1) {
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
In the previous example, five event handlers will be run: three are defined into [:beh1name:], and
two are defined into [:beh2name:]. This mechanism is generalized to all the events within a behavior.


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
	behavior MyBehavior {
		uses Logging
		on [:destroyevent](Destroy) {
			info("Destroying the behavior")
		}
	}
[:End:]


#### Guarded Destruction Handler

As for [:initializeevent:], the handlers of the [:destroyevent:] event could be guarded.

In the following example, the first event handler is executed when the [:destroyevent:] is received
and there is resource stored in the corresponding field. The second event handler is executed
when there is no resource.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Destroy
	[:On]
	behavior MyBehavior {
		uses Logging
		var resource : Object
		on Destroy [ resource !== null ] {
			info("Destroying the behavior when there is a resource")
		}
		on Destroy [ resource === null ] {
			info("Destroying the behavior when there is no resource")
		}
	}
[:End:]


### Reactive Behavior Units

The reactive behavior is specified with a collection of event handlers. The principle of a reactive behavior
is to execute a part of the behavior when something has happening in the behavior, the agent or in its environment.

In the following example, the behavior is reacting to the reception of the [:eventtype2:] event.

As for all the event handlers, it could be guarded by a predicate.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	event SomethingChanged
	[:On]
	behavior MyBehavior {
		uses Logging
		on [:eventtype2](SomethingChanged) {
			info("Reactive behavior")
		}
	}
[:End:]


### Parallel Execution of the Reactive Behavior Units

When an event is received and the guard of the corresponding handler is true, the event handler is said to be triggered.

When multiple event handlers are triggered at the same time, they are all executed in parallel.
In the following example, the two handlers for the [:eventtype2:] event are executed in parallel.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	event [:eventtype2!]
	[:On]
	behavior MyBehavior {
		uses Logging
		on [:eventtype2!] {
			info("First reactive behavior")
		}
		on [:eventtype2!] {
			info("Second reactive behavior")
		}
	}
[:End:]


### Pro-active Behavior Units

A proactive behavior is a part of the global behavior of an agent that the
agent is deciding to execute by itself.
The execution of a reactive behavior is initiated by a part of
the code external to this behavior. In opposite, the initiator
of the execution of a proactive behavior is the agent itself.

In SARL, a proactive behavior is a behavior that is scheduled
by the agent or one of its behaviors. The schedule mechanism is provided by the
[[:schedulescap:] built-in capacity](./BIC.md).
In the following example, the agent execute its proactive behavior every second.

[:Success:]
	package io.sarl.docs.reference.ar
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	import io.sarl.core.Schedules
	[:On]
	behavior MyBehavior {
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
An agent has a set of built-in capacities considered essential to respect the commonly accepted
competencies of agents, such autonomy, reactivity, pro-activity and social capacities. 

Consequently, a behavior associated to an agent is able to use the skills of this agent.


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


### Giving a Skill to the Associated Agent

When a behavior must use a capacity, its agent must own an implementation of this capacity: a skill.
It is possible for a behavior to assign a skill to its agent.

[:Success:]
	package io.sarl.docs.reference.br
	import io.sarl.core.Logging
	[:importkw](import) io.sarl.lang.core.Agent
	capacity Cap {
		def action
	}
	skill Ski implements Cap {
		uses Logging
		def action { info("Action") }
	}
	[:On]
	behavior MyBehavior {
		new (owner : Agent) {
			super(owner)
			var theSkill = new Ski
			setSkill( theSkill, Cap )
		}
	}
[:End:]


If some cases, you may want to set the skill if one was not set up before. The specific behavior
is supported by `[:setskillifabsfctfull]{[:setskillifabsfct](setSkillIfAbsent)(Skill, Class<? extends Capacity>*)}`.

[:Success:]
	package io.sarl.docs.reference.br
	import io.sarl.core.Logging
	import io.sarl.lang.core.Agent
	capacity Cap {
		def action
	}
	skill Ski implements Cap {
		uses Logging
		def action { info("Action") }
	}
	[:On]
	behavior MyBehavior {
		new (owner : Agent) {
			super(owner)
			var theSkill = new Ski
			setSkillIfAbsent( theSkill, Cap )
		}
	}
[:End:]


### Using a Capacity with the Getters

For invoking a function implemented by a skill, the two following steps must be done:

* Retrieve the skill instance: the function `[:getskillfct](getSkill)(Class<? extends Capacity>)` permits retrieving the skill associated to the given capacity;
* Invoke the capacity's action on the retrieved skill.

> **_Note:_** This method of invocation is not recommended by the SARL developers.
> You should prefer the use of the extension methods (see below).

[:Success:]
	package io.sarl.docs.reference.ar
	capacity Cap {
		def action
	}
	event SomeEvent
	[:On]
	behavior MyBehavior {
		on SomeEvent {
			// Retreive the capacity implementation
			var s = [:getskillfct!](Cap)
			// Run the action of the skill
			s.action
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
with the name [:actiondef:] is invoked. This action is defined in the [:capcap:] capacity. 

[:Success:]
	package io.sarl.docs.reference.ar
	capacity Cap {
		def action
	}
	event SomeEvent
	[:On]
	behavior MyBehavior {
		[:useskw](uses) [:capcap](Cap)
		on SomeEvent {
			// Run the action of the skill
			[:actiondef](action)
		}
	}
[:End:]



[:Include:](../legal.inc)
