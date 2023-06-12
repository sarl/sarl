# Event Reference

[:Outline:]

This document describes how to define events in SARL. Before reading this document, we recommend that you read
the [General Syntax Reference](./GeneralSyntax.md).

An event is one of the core concepts in SARL. It is a data structure composed of attributes where
each attribute has a name, a type, and a value.

Events are exchanged among the agents or the behavioral units of agents,
inside a given [Space](./Space.md).

Each event has:

* a type, i.e. its qualified name;
* a source, the identifier of the sender of the event; and
* a collection of name-value pairs, i.e. the attributes of the event.


## Event vs. Message

In computer-science literature, there are two main approaches 
for communicating between entities: (1) an event and (2) a message.

An event and a message are similar in that they each have a name 
or type), a source, and optional data (arguments or parameters).
The difference is in whether there is a receiver:
an event does not specify a receiver,
while a message needs to have at least
one receiver (even if it is a group such as "all"  
possible receivers).

Because the event approach is more general, it is preferred 
by the designers of SARL.

So, to send data to another entity in SARL,
you create an instance of an event and emit
the event in a [Space](./Space.md).
The sending API is detailed in the [Built-in Capacity
Reference](./BIC.md).

> **_Note:_** There is no message concept in SARL. All communication is done by using an [:eventtype:].

[:Fact:]{typeof(io.sarl.lang.core.[:eventtype](Event))}


## Defining an Event


### Define an empty event

An event is defined with the [:eventkw:] keyword followed by the name of the event (without the qualified name of its package,
which is inferred from the [:packagekw:] keyword, if present).

When an event does not contain any additional data (so is "empty"), nothing further is required (though an empty
block is allowed).

The example below contains the definition of [:eventtype1:] and [:eventtype2:], which are both empty.
The first event is defined with the "empty block" syntax.
The second event is defined with the "nothing" syntax.

[:Success:]
	[:packagekw](package) io.sarl.docs.reference.er
	[:On]
	[:eventkw](event) [:eventtype1](Event1) {  }
	event [:eventtype2](Event2)
[:End:]


### Define an event with attributes

An Event may can carry additional information beyond its name (or type).
This information is described by a set of attributes (or typed 
key/value pairs). Each attribute is declared according to the 
"Field Declaration" of the [General Syntax Reference](./GeneralSyntax.md).

The following code example defines the event [:eventtype3:] with three attributes.
Each declaration of the attributes illustrates one possible syntax for defining a field:

* declaration with explicit typing: [:code1:]
* declaration with type inference: [:code2:]
* declaration with free inferred element: [:code3:]


According to the type inference mechanism used by SARL, the attribute [:code4:] will have the type [:code5:].

> **_Note:_** Because of the use of the [:varkw:] keyword, the values of these attributes can be modified.

[:Success:]
	package io.sarl.docs.reference.er
	[:On]
	event [:eventtype3](MyEvent) {
		[:code1]([:varkw]{var} number : Integer)
		[:code2](var string = "abc")
		[:code3](var [:code4]{something}) : [:code5](Object)
	}
[:End:]


### Define an event with value attributes

Events in SARL will carry data that is unmodifiable 
when an attribute is defined using the [:valmodifier:] keyword.

> **_Important Note:_** The [:valmodifier:] keyword has the same semantics as the `final` modifier in
> the Java language. It means that an element defined with [:valmodifier:] can be initialized only once. It
> also means that the element is read-only.
> But if the element is a reference to an object, then the referenced object
> is not read-only (only the initial reference is).

Because the [:valmodifier:] keyword defines a single-initialization
variable, there should be a way to specify the initial value.
The initial value can be specified at the end of the [:valmodifier:]
directive or by specifying a constructor.

[:Success:]
	package io.sarl.docs.reference.er
	import io.sarl.lang.core.Agent
	[:On]
	event MyEvent {
		[:valmodifier](val) string = "abcd"
		val number : Integer
		
		new(nb : Integer) {
			number = nb
		}
	}
[:End:]


If no constructor is defined in the event type and a super-type is declared, implicit constructors will be assumed.
Implicit constructors has the same prototypes as the constructors of the super type.
Details on implicit constructors are given in the reference documentation related to the
[synthetic functions](./general/SyntheticFunctions.md).


### Extending Events

In some use cases, it is useful to specialize the definition
of an event. This mechanism is supported by the inheritance
feature of SARL, which has the same semantic as the inheritance
mechanism as the Java object-oriented language.

The extended event is specified just after the [:extendsfw:] keyword.

> **_Very Important Note:_** An event type can extend __only one__ other event type.  This is similar to the
> model for class extensions in the Java language.

#### Declaration

In the following code, the first event is defined with the name [:eventtype1:] and an attribute named [:stringfield:].
A second event [:eventtype2:], is defined as an extension of the first event. It contains a new attribute named
[:numberfield:].
It is now possible to create instances of these events. For [:eventtype1:], only the attribute [:stringfield:]
is available. For [:eventtype2:], two attributes are available ([:eventtype2:] inherits one field from
[:eventtype2:] and defines one field).

[:Success:]
	package io.sarl.docs.reference.er
	import io.sarl.lang.core.Agent
	[:On]
	event Event1 {
		var [:stringfield](string) : String
	}
	event Event2 [:extendsfw](extends) Event1 {
		var [:numberfield](number) : int
	}
[:End:]


#### Use

The following code illustrates the use of event instances.

[:Success:]
	package io.sarl.docs.reference.er
	event Event1 {
		var string : String
	}
	event Event2 extends Event1 {
		var number : int
	}
	class X {
		def myfct {
	[:On]
			// Create an instance of Event1 and set its attribute.
			var e1 = new Event1
			e1.string = "abc"
			// Create an instance of Event2 and set its attributes.
			var e2 = new Event2
			e2.string = "abc"
			e2.number = 345
	[:Off]
		}
	}
[:End:]


### Modifiers

Modifiers are used to modify declarations of types and type members.
This section introduces the modifiers for the event.
The modifiers are usually written before the keyword for defining the event.

The complete description of the modifiers' semantic is available in
[this section](./OOP.md#definition-of-all-the-supported-modifiers).


#### Event Modifiers

An event may be declared with one or more modifiers, which affect its behavior:

* Access modifiers:
	* [:publicmodifier:]: the behavior is accessible from any other type (default);
	* [:packagemodifier:]: the behavior is accessible from only the types in the same package.
* [:finalmodifier:]: cannot be extended.

Examples:

[:Success:]
	package io.sarl.docs.reference.er
	[:On]
	[:publicmodifier](public) event Example1 {
	}
	[:packagemodifier](package) event Example2 {
	}
	[:finalmodifier](final) event Example3 {
	}
[:End:]


#### Field Modifiers

The modifiers for the fields in an event are:

* Access modifiers:
	* [:publicmodifier:]: the field is accessible from everywhere.

Example:

[:Success:]
	package io.sarl.docs.reference.er
	event MyEvent {
	[:On]
		public var example1 : Object
	[:Off]
	}
[:End:]


## Reserved Events

Several events are defined and reserved by the SARL Core Specification.
They describe the minimal set of events that a runtime environment must support to run a SARL program.

> **_Very Important Note:_** You must not define an event with a fully qualified name equals to one of the reserved
> events.

Two types of reserved events exist:

* the events reserved in the SARL Core Specification for the [agent's life cycle](./Agent.md#behaviors-of-an-agent); and
* the events supported by the [Built-in Capacities](./BIC.md).



[:Include:](../legal.inc)
