# Capacity Reference

[:Outline:]

This document describes how to define Capacities in SARL. Before reading this document, we recommend that you read
the [General Syntax Reference](./GeneralSyntax.md).

An *Action* is code (a public method or function) that transforms a part of the designed system or its environment.
This transformation guarantees resulting properties if the system before the transformation satisfies
a set of constraints. An Action is defined in terms of pre- and post-conditions.

A *Capacity* is the specification of a collection of Actions. This specification makes no assumptions about the
implementation of each Action. It is used to specify what an Agent can do, what behavior is required for its execution.

A *Skill* is a collections of Actions implementing a Capacity as described in this specification.

An Agent can dynamically evolve by acquiring (learning) new Capacities, and it can also dynamically change the Skill
associated with a given Capacity. Acquiring a new Capacity enables an Agent to get access to new behaviors.
This provides Agents with a self-adaptation mechanism that allows them to dynamically change their architecture
according to their current needs and goals.

## Defining a Capacity


### Capacity Definition

A Capacity is the specification of a collection of Actions. Consequently, only Action signatures can be defined inside
a Capacity: no attribute or field is allowed, and no body (code) for the Action may be present.

The definition of a Capacity is done with the [:capacitykw:] keyword. Below, a Capacity that permits logging messages
is defined. This Capacity enables an Agent to log information and debugging messages.

> **_Note:_** Defining a Capacity without Actions is a symptom of a design problem.

[:Success:]
	package io.sarl.docs.reference.cr
	[:On]
	[:capacitykw](capacity) [:loggingcap](Logging) {
		// Log an information message
		def info(text : String)
		// Log a debugging message
		def debug(text : String)
	}
[:End:]


### Extending a Capacity

In some situations, it is useful to specialize the definition of a Capacity. Capacity specialization is supported
by the inheritance feature of SARL, which has the same semantics as the inheritance mechanism of the Java
object-oriented language.

The extended Capacity is specified just after the [:extendskw:] keyword.

> **_Very Important Note:_** A Capacity type can extend __zero-to-many__ other Capacity types.
> This is similar to the implementation of interfaces in the Java language.

In the following code, the [:loggingcap:] Capacity (defined above) is extended to enabling the output of error messages.

[:Success:]
	package io.sarl.docs.reference.cr
	capacity Logging {
		def info(text : String)
		def debug(text : String)
	}
	[:On]
	capacity ErrorLogging [:extendskw](extends) Logging {
		// Log a error message
		def error(text : String)
	}
[:End:]


### Extending Multiple Capacities

In some situations, it is useful to define a Capacity by extending more than one Capacity.
Below, the [:cap3cap:] Capacity is defined as an extension of the Capacities [:cap1cap:] and [:cap2cap:].

[:Success:]
	package io.sarl.docs.reference.cr
	[:On]
	capacity [:cap1cap](Cap1) {
		def action1
	}
	capacity [:cap2cap](Cap2) {
		def action2
	}
	capacity [:cap3cap](Cap3) extends Cap1, Cap2 {
		def action3
	}
[:End:]


### Modifiers

Modifiers are used to modify declarations of types and type members. This section introduces the modifiers for Capacity.
The modifiers are usually written before the keyword defining the Capacity.

The complete description of the modifier semantics is available in
[this section](./OOP.md#definition-of-all-the-supported-modifiers).


#### Capacity Modifiers

A Capacity may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]: the Capacity is accessible from any other type (default);
	* [:packagemodifier:]: the Capacity is accessible only from types in the same package.

Examples:

[:Success:]
	package io.sarl.docs.reference.cr
	[:On]
	[:publicmodifier](public) capacity Example1 {
	}
	[:packagemodifier](package) capacity Example2 {
	}
[:End:]


#### Method Modifiers

The modifiers for the Actions (methods) in a Capacity are:

* Access modifiers:
	* [:publicmodifier:]: the Action is accessible from any type.

Example:

[:Success:]
	package io.sarl.docs.reference.cr
	capacity Capacity1 {
	[:On]
		// Public access function
		public def example1
	[:Off]
	}
[:End:]


## Built-in Capacities

Several Capacities are defined and reserved by the SARL Core Specification.
They compose the minimal set of Capacities that a runtime environment must support to run a SARL program.

> **_Very Important Note:_** You must not define a Capacity with a fully qualified name equals to one
> of the reserved Capacities.

The built-in Capacities are defined in the [Built-in Capacity Reference](./BIC.md).


## Use of the Capacities

The use of a Capacity is related to the associated [Skills](./Skill.md).
A Capacity cannot be called by itself since it does not provide an implementation: this is the role of the Skill.

When a function `fct` of the Capacity `C` is called, it means that the Agent silently does:

* Find the Skill `S` associated to `C`; and
* Call `fct` on the object `S`.

Details on the use of the Capacities may be found in the following:

* [Agent](./Agent.md)
* [Behavior](./Behavior.md)

## Associating a default skill to a capacity

As described within the previous section, when a capacity's function is invoked, the function's implementation is retreived from
the [skill](./Skill.md) that was associated to the capacity.

The standard way for associating a capacity and a skill is to call the `setSkill` function within an agent or a behavior, as
detailed in the [agent's documentation](./Agent.md).

Nevertheless, let the case in which we would like to bind a capacity to a default skill.
In other words, we would like to specify that a skill should be associated by default to a capacity if the `setSkill` function
is not invoked.

The way to realize this it is to the [:defaultskillannon:].
This annotation may be attached to a capacity's declaration in order to specify the skill that should be binded by default to
the capacity.  

In the following example, the [:mycapacitydecl:] capacity and the [:myskilldecl:] skill are declared.
The [:defaultskillannon] annotation specifies that an instance of [:myskilldecl:] should be binded to the
[:mycapacitydecl:] capacity if the developer does not change the binding programmatically.

Examples:

[:Success:]
	package io.sarl.docs.reference.cr
	import io.sarl.lang.core.DefaultSkill
	[:On]
	[:defaultskillannon](@DefaultSkill)(typeof(MySkill))
	capacity [:mycapacitydecl](MyCapacity) {
		def myfunction
	}
	skill [:myskilldecl](MySkill) implements MyCapacity {
		def myfunction {
		}
	}
[:End:]


[:Include:](../legal.inc)
