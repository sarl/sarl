# Event Handler Overriding

[:Outline:]

This document describes the basics of the overriding of the event handlers.
The key feature that is considered in this document is the overriding of the behavior associated to an event handler.
Let an event occurrence of [:myevent:]. In the definition of the abstract agent named [:myabstractagent:], the message
"hello world" is printed out when an occurrence of [:myevent:] is received.
Let consider that you want to avoid this message printing in a sub-type of [:myabstractagent:], but do something other.

In the SARL language, it is impossible to override the event handlers, i.e. the `on` statements.
So, __how to override the behavior of an inherited `on` statement?__

In this document, the operational semantic of the event handlers is reminded. Then, the best practice for overriding
the event handler behavior is explained.


## Operational semantic of the event handlers

The operational semantics of the event handler is the following:
when an event is received by an agent (or a behavior), all the event handlers matching the
event type and with a valid guard on the event occurrence are run in parallel.

The event handlers from the current agent type, and from all the super agent types are considered.
Let the example below. When the agent [:myagent:] receives an occurrence of [:myevent:], the two
event handlers are runs. It means that the CODE1 is run and the message "hello world" is printed out.
These two event handlers are run in parallel.

[:Success:]
	package io.sarl.docs.bestpractices.eventbuilder
	capacity Logging {
		def info(s : String)
	}
	[:On]event [:myevent](MyEvent)

	agent [:myabstractagent](MySuperAgent) {
	  uses Logging
	  on MyEvent {
		info("hello world")
	  }
	}

	agent [:myagent](MyAgent) extends MySuperAgent {
	  on MyEvent {
		// CODE1
	  }
	}
[:End:]


## Defining the super behavior in a function

For overriding the behavior associated to an event handler, it is mandatory to use the function overriding mechanism
that is provided by the SARL language.

A function must be defined in the super type for each event handler that could be overridden.
In the following example, the function is named [:myfct:]. The code of this function is the code
of the event handler from the previous section. The event handler invokes the defined function.

[:Success:]
	package io.sarl.docs.bestpractices.eventbuilder
	capacity Logging {
		def info(s : String)
	}
	[:On]event MyEvent

	agent MySuperAgent {
	  uses Logging
	  on MyEvent {
		myEventBehavior
	  }
	  def [:myfct](myEventBehavior) {
		info("hello world")
	  }
	}
[:End:]


## Overriding the super function

For overriding the behavior, the function [:myfct:] could be overridden in the sub-type.
With the example below, the message "hello world" is no more printed out because of the function overriding.

[:Success:]
	package io.sarl.docs.bestpractices.eventbuilder
	event MyEvent
	agent MySuperAgent {
	  def myEventBehavior {
	  }
	}
	[:On]agent MyAgent extends MySuperAgent {
	  def myEventBehavior {
		// CODE1
	  }
	}
[:End:]


[:Include:](../legal.inc)
