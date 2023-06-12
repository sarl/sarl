# Skill Reference

[:Outline:]

This document describes how to define Skills in SARL.
Before reading this document, we recommend that you read
the [General Syntax Reference](./GeneralSyntax.md),
and the [Capacity Reference](./Capacity.md).

An *Action* is code (a public method or function) that transforms a part of the 
designed system or its environment. This transformation guarantees 
resulting properties if the system before the transformation satisfies 
a set of constraints. An Action is defined in terms of pre- and post-conditions.

A *Capacity* is the specification of a collection of Actions. This specification 
makes no assumptions about the implementation of each Action. It is used to specify 
what an Agent can do, what behavior is required for its execution.

A *Skill* is a collection of Actions implementing a Capacity as described in
this specification.


## Defining a Skill


### Basic Definition

A Skill implements a Capacity and is defined with the [:skillkw:]
keyword. This relationship is specified with the `implements` keyword.

Below, a Skill is defined to output messages on the standard console
(defined in the [Capacity Reference](./Capacity.md)).
Note that all the Actions defined in the Capacity must
have a definition (with a body containing code) in the Skill.

[:Success:]
	package io.sarl.docs.reference.sr
	capacity Logging {
		// Log an information message
		def info(text : String)
		// Log a debugging message
		def debug(text : String)
	}
	[:On]
	[:skillkw](skill) ConsoleLogging implements Logging {
		def info(text : String) {
			System.out.println(text)
		}
		def debug(text : String) {
			System.err.println(text)
		}
	}
[:End:]


### Field Definition

Often it is useful or necessary to base a Skill (a
Capacity's implementation) on attributes (properties or fields).

The following example defines a Skill that uses the standard Java logging library.
To avoid creating an instance of the Java logger each time the Capacity's Actions are invoked, an instance
of the Java logger is created and stored in a field of the Skill.

[:Success:]
	package io.sarl.docs.reference.sr
	import java.util.logging.Logger
	capacity Logging {
		// Log an information message
		def info(text : String)
		// Log a debugging message
		def debug(text : String)
	}
	[:On]
	skill StandardJavaLogging implements Logging {
		// A field is defined in the Skill
		val logger = Logger.anonymousLogger
		def info(text : String) {
			logger.info(text)
		}
		def debug(text : String) {
			logger.fine(text)
		}
	}
[:End:]


### Action Definition

It is possible to declare methods in the Skill in addition to those specified by the Capacity. 

[:Success:]
	package io.sarl.docs.reference.sr
	import java.util.logging.Logger
	capacity Logging {
		// Log an information message
		def info(text : String)
		// Log a debugging message
		def debug(text : String)
	}
	[:On]
	skill MyLogging implements Logging {
		def info(text : String) {
			output(text)
		}
		def debug(text : String) {
			output(text)
		}
		// Define an utility function
		// that is outputting the text
		def output(t : String) {
			System.err.println(t)
		}
	}
[:End:]


### Initialization of a skill

Several elements of the skill can be used only after the skill is attached to its owning agent.
For example, the value returned by the function [:getownerfct:] is not `null` only when the skill is
attached to an agent, i.e. its owner. 

In order to enable the developer to write a code that is run when the skill is attached, the function
[:installfct:] could be defined and implemented. The code below provides an example in which the value
returned by [:getownerfct:] is checked.

[:Success:]
	package io.sarl.docs.reference.sr
	import java.util.logging.Logger
	capacity Logging {
		def info(text : String)
	}
	skill StandardJavaLogging implements Logging {
		def info(text : String) {}
	[:On]
		def [:installfct](install) {
			// Initialization of the skill
			assert [:getownerfct](getOwner) !== null
		}
	[:Off]
	}
[:End:]


### Uninitialization of a skill

In a similar way as [:installfct:], it is possible to execute a code when the skill is detached from it owning agent.
For this purpose, the [:uninstallfct:] function should be defined, as illustrated below: 

[:Success:]
	package io.sarl.docs.reference.sr
	import java.util.logging.Logger
	capacity Logging {
		def info(text : String)
	}
	skill StandardJavaLogging implements Logging {
		def info(text : String) {}
	[:On]
		def [:uninstallfct](uninstall) {
			// Do uninstallation statements
		}
	[:Off]
	}
[:End:]


### Constructor Definition

It is not necessary to specify a constructor for Skills unless a value will be initialized.

Two constructors are defined in the abstract `Skill` class: 

[:ShowType:](io.sarl.lang.core.[:skilltype]{Skill})

Example of constructor definition:
[:Success:]
	package io.sarl.docs.reference.sr
	import java.util.logging.Logger
	capacity Logging {
		// Log an information message
		def info(text : String)
		// Log a debugging message
		def debug(text : String)
	}
	skill StandardJavaLogging implements Logging {
		// A field is defined in the Skill
		val logger : Logger
	[:On]
		// The constructor is mandatory
		// for defining the field "logger"
		new (l : Logger) {
			super() // Call the super's constructor
			logger = l
		}
	[:Off]
		def info(text : String) {
			logger.info(text)
		}
		def debug(text : String) {
			logger.fine(text)
		}
	}
[:End:]


If no constructor is defined in the skill type and a super-type is declared, implicit constructors will be assumed.
Implicit constructors has the same prototypes as the constructors of the super type.
Details on implicit constructors are given in the reference documentation related to the
[synthetic functions](./general/SyntheticFunctions.md).


### Multiple Capacity Implementation

In some situations it is useful to combine more than one capacity in a skill.
Below, the [:skilltype1:] skill is defined as an implementation of the capacities
[:capacitytype1:] and [:capacitytype2:].
All the Actions defined in a Capacity must have an implementation in the related Skill.

If two implemented Capacities include the same Action signature, it must be implemented only once in the Skill.

[:Success:]
	package io.sarl.docs.reference.sr
	import java.util.logging.Logger
	capacity [:capacitytype1](Logging) {
		// Log an information message
		def info(text : String)
		// Log a debugging message
		def debug(text : String)
	}
	[:On]
	capacity [:capacitytype2](LogReader) {
		def open(filename : String) : int
		def info(t : String)
		def close(fid : int)
	}

	skill [:skilltype1](MyLogging) implements Logging, LogReader {
		// Shared implementation for the methods
		// defind in the two Capacities.
		def info(text : String) {
			System.out.println(text)
		}
		def debug(text : String) {
			System.out.println(text)
		}
		def open(filename : String) : int {
			return 0
		}
		def close(fid : int) {
		}
	}
[:End:]


### Extending a Skill

In some situations it is useful to specialize the definition of a Skill. This mechanism is supported by the __inheritance__
feature of SARL, which has the same semantics as the inheritance mechanism of the Java object-oriented language.

The extended Skill is specified just after the [:extendskw:] keyword.

> **_Very Important Note:_** A Skill type can extend __only one__ other Skill type.  This is similar
> to the constraint on the extension of classes in the Java language.

In the following code, the `StandardJavaLogging` Skill (defined above) is extended to override the info output.

[:Success:]
	package io.sarl.docs.reference.sr
	import io.sarl.lang.core.Capacity
	capacity Logging {
		// Log an information message
		def info(text : String)
		// Log a debugging message
		def debug(text : String)
	}
	skill StandardJavaLogging implements Logging {
		def info(text : String) {
		}
		def debug(text : String) {
		}
	}
	[:On]
	skill ExtendedLogging [:extendskw](extends) StandardJavaLogging {
		def info(text : String) {
			super.info("INFO: "+text)
		}
	}
[:End:]


### Modifiers

Modifiers are used to modify declarations of types and type members.
This section introduces the modifiers for the Skill.
The modifiers are usually written before the keyword for defining the Skill.

The complete description of the modifiers' semantic is available in
[this section](./OOP.md#definition-of-all-the-supported-modifiers).

#### Skill Modifiers

A Skill may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]: the behavior is accessible from any other type (default);
	* [:packagemodifier:]: the behavior is accessible only from types in the same package.
* [:abstractmodifier:]: this Skill is abstract and cannot be instantiated; an extension Skill must implement this behavior.
* [:finalmodifier:]: an extending Skill may not override this behavior.

Examples:

[:Success:]
	package io.sarl.docs.reference.sr
	import io.sarl.lang.core.Capacity
	capacity CapacityExample {
	}
	[:On]
	[:publicmodifier](public) skill Example1 implements CapacityExample {
	}
	[:packagemodifier](package) skill Example2 implements CapacityExample {
	}
	[:abstractmodifier](abstract) skill Example3 implements CapacityExample {
	}
	[:finalmodifier](final) skill Example4 implements CapacityExample {
	}
[:End:]


#### Field Modifiers

The modifiers for the fields in a Skill are:

* Access modifiers:
	* [:publicmodifier:]: the field is accessible from everywhere;
	* [:protectedmodifier:]: the field is accessible within the same package, and in derived Agents;
	* [:packagemodifier:]: the field is accessible only within the same package as its Agent;
	* [:privatemodifier:]: the field is accessible only within its Agent (default).
* [:staticmodifier:]: the field is a class field, not an instance field.

Examples:

[:Success:]
	package io.sarl.docs.reference.sr
	import io.sarl.lang.core.Capacity
	capacity CapacityExample {
	}
	public skill Example1 implements CapacityExample {
	[:On]
		public var example1 : Object
		[:protectedmodifier](protected) var example2 : Object
		package var example3 : Object
		[:privatemodifier](private) var example4 : Object
		[:staticmodifier](static) var example5 : Object
	[:Off]
	}
[:End:]


#### Method Modifiers

The modifiers for the methods in a Skill are:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the method (public);
	* [:protectedmodifier:]: the method is accessible within the same package, and derived classes;
	* [:packagemodifier:]: the method is accessible only within the same package as its class;
	* [:privatemodifier:]: the method is accessible only within its class.
* [:abstractmodifier:]: the method has no implementation in the class.
* [:dispatchmodifier:]: the method provides an implementation for the dispatch method mechanism.
* [:finalmodifier:]: the method cannot be overridden in derived classes.
* [:staticmodifier:]: the method is a class method, not an instance method.

Examples:

[:Success:]
	package io.sarl.docs.reference.sr
	import io.sarl.lang.core.Capacity
	capacity CapacityExample {
	}
	abstract skill Example1 implements CapacityExample {
	[:On]
		// Public access function
		public def example1 { }
		// Protected access function
		protected def example2 { }
		// Package access function
		package def example3 { }
		// Private access function
		private def example4 { }
		// Abstract function
		abstract def example5
		// Not-overridable function
		final def example6 { }
		// Dispatch functions
		[:dispatchmodifier](dispatch) def example7(p : Integer) { }
		dispatch def example7(p : Float) { }
		// Static / Class function
		static def example8 { }
	[:Off]
	}
[:End:]


## Built-in Skills

Several Capacities are defined and reserved by the SARL Core Specification. The corresponding Skills are provided
by the runtime environment (such as the [Janus platform](http://www.janusproject.io)).
The built-in Skills are described in the [Built-in Capacity Reference](./BIC.md).


## Use of the Skills

Details on the use of Skills may be found in the following:

* [Agent](./Agent.md)
* [Behavior](./Behavior.md)




[:Include:](../legal.inc)
