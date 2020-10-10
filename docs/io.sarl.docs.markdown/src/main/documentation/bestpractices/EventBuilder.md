# Event Creation with a Builder

[:Outline:]

This document describes the basics of the creation of events with a builder.
In some cases, each event must have a unique identifier.
But because static fields are not allowed in the event definitions,
it is impossible to store the next available event ID in a static field.
The best way for creating events with unique identifiers is to apply the
[builder design pattern](https://en.wikipedia.org/wiki/Software_design_pattern).  

The elements that are explained in this document are:

* the definition of an event;
* the definition of an event builder;
* the use of the event builder.


## Definition of the event

The purpose of this document is to create an event, which has a unique identifier.
This identifier is an integer number that should be incremented each time an
event instance is created.

The definition of the event should define the event with a read-only identifier ([:valkw:]).
Because the identifier is a value, it must be initialize in the constructor of the event.
Consequently, a constructor is defined with the identifier value as parameter.

[:Success:]
	package io.sarl.docs.bestpractices.eventbuilder
	[:On]event MyEvent {
		[:valkw](val) id : long
		new (id : long) {
			this.id = id
		}
	}
[:End:]


## Definition of the event builder

For creating the event instances, we apply the
[builder design pattern](https://en.wikipedia.org/wiki/Software_design_pattern)

A builder is a class that is able to create an instance of the event when it is invoked.
The next available unique identifier for the events is stored into a field of the builder ([:id:]).

[:Success:]
	package io.sarl.docs.bestpractices.eventbuilder
	event MyEvent {
		val [:id](id) : long
		new(id : long) {
			this.id = id
		}
	}
	[:On]class MyEventBuilder {
		private var id : long = 0
		def [:newinst](newInstance) : MyEvent {
			val eventId = id
			id++
			return new MyEvent(eventId)
		}
	}
[:End:]


The function [:newinst:] is defined for creating an instance of the event. This function
gets the next available global identifier to give it to the event instance, and it increments
the next available global identifier for the next call to [:newinst:].
Finally, the [:newinst:] function create the event instance with the appropriate identifier. 


## Use of the event builder

For using the event builder, you have simply to create an instance of the [:builderimpl:]
class, and use it as follow (two events are created in the example):

[:Success:]
	package io.sarl.docs.bestpractices.eventbuilder
	event MyEvent
	class MyEventBuilder {
		private var id : long = 0
		def newInstance : MyEvent {
			null
		}
	}
	class [:builderimpl](MyEventBuilderUser) {
		def use {
			[:On]val builder = new MyEventBuilder
			var event1 = builder.newInstance
			var event2 = builder.newInstance[:Off]
		}
	}
[:End:]



[:Include:](../legal.inc)

