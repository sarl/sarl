# Override the Agent Event Bus with the Behaviors Capacity

[:Outline:]

By default, the SARL agents are exchanging data and information by using [events](../reference/Event.md).
These events are received by the agent event handlers that are declared with the `on` keyword.
In order to invoke these handlers, the SARL Run-time Environment (SRE) implements an event bus that is in
charge of routing the events for an agent.

This tutorial page explains how to override the event routing mechanism by defining a new [skill](../reference/Skill.md)
for the [standard built-in capacities](../reference/bic/Behaviors.md) in charge of the event routing in the agent.

## Role of the Behavior Built-in Capacity

The [:behaviorscapacity:] built-in capacity provides the tools to the agents for dynamically
registering and unregistering sub-behaviors, and it provides the *event listener that could
be used by external entities (such as the SRE's communication service)
to give the events to the agents.

The key function that must be overridden is related to this last feature. The name of the
function is [:aseventlistenerfct:]. This function replies an internal object that implements
the [:eventlistenertype:] interface. This object is invoked each time an event should delivered
to the agent. 


## Definition of a Behaviors capacity

This section describes step-by-step on to create a new [:behaviorscapacity:] implementation,
that is of course an agent skill.

### Declaration and Link to the Default Skill

Since the default built-in skill that is implementing the [:behaviorscapacity:] capacity is provided
by the SRE, it may be based on internal features that is neither accessible nor visible.
That's why it is preferable to keep the reference to the original [:behaviorscapacity:] implementation
in order to let us invoking these low-level features.

> **_Note:_** In this tutorial, the new [:behaviorscapacity:] implementation will filter the incoming
> events according to a given type. In order words, only the events of the given type will pass through.

The SARL code of the new [:behaviorscapacity:] implementation is written into the skill with the name
[:mybehaviorname:]:

[:Success:]
	package io.sarl.docs.tutorials.overrideeventbuscapacity
	import io.sarl.core.[:behaviorscapacity](Behaviors)
	import io.sarl.lang.core.Event
	abstract
	[:On]
	skill [:mybehaviorname](FilteringEventDispatchingBehavior) implements Behaviors {

		val acceptedType : Class<? extends Event>

		val behaviorDelegate : Behaviors

		new (acceptedType : Class<? extends Event>, behaviorDelegate : Behaviors) {
			this.acceptedType = acceptedType
			this.behaviorDelegate = behaviorDelegate
		}

	}
[:End:]

In the previous code, the [:mybehaviorname:] skill is an implementation of the built-in [:behaviorscapacity:] capacity.
It take as argument of its constructor the type of event that is accepted, and the reference to the original
[:behaviorscapacity:] implementation that is provided by the SRE.
There two arguments are stored into local attributes of the skill.

### Declaration of the event listener

In the implementation of the [:behaviorscapacity:] capacity, the function [:aseventlistenerfct:] is defined for
returning the event listener associated to the agent.
Thus, it is necessary to define this event listener.
We decided to declare it as an *inner class* of [:mybehaviorname:].

[:Success:]
	package io.sarl.docs.tutorials.overrideeventbuscapacity
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.Event
	import io.sarl.lang.core.EventListener
	import java.util.UUID
	abstract skill FilteringEventDispatchingBehavior implements Behaviors {
		var acceptedType : Class<? extends Event>
		var behaviorDelegate : Behaviors
	[:On]
private static class [:filteringeventlistener](FilteringEventListener) implements [:eventlistenertype](EventListener) {
	val parent : FilteringEventDispatchingBehavior

	new (parent : FilteringEventDispatchingBehavior) {
		this.parent = parent
	}
	
	override [:receiveeventfct](receiveEvent)(occ : Event) {
		if (this.parent.acceptedType.isInstance(occ)) {
			this.parent.behaviorDelegate.asEventListener.receiveEvent(occ)
		}
	}

	@Pure
	override getID : UUID {
		this.parent.ID
	}
}
	[:Off]
	}
[:End:]

This internal implementation of [:eventlistenertype:] has a reference to its containing skill, as argument of
the constructor. This reference is defined in order to have access to the filtering type and to the
original implementation of the [:behaviorscapacity:] capacity.

The type [:filteringeventlistener:] must implement the function [:receiveeventfct:] that is invoked
each time an event must be delivered to the agent (either internal or external event).
The code of this function checks if the type of the event is compatible with the filtering type.
And, if it is compatible, it delivers the event to the agent by using the original [:behaviorscapacity:] capacity.


### Upgrading the skill implementation with the new event listener

Now, it is necessary to reply an instance of the event listener (that is defined in the previous section) in
the [:mybehaviorname:] skill.

[:Success:]
	package io.sarl.docs.tutorials.overrideeventbuscapacity
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.Event
	import io.sarl.lang.core.EventListener
	import java.util.UUID
	abstract skill FilteringEventDispatchingBehavior implements Behaviors {
		private static class FilteringEventListener implements EventListener {
			new (parent : FilteringEventDispatchingBehavior) {}
			override receiveEvent(occ : Event) {}
			override getID : UUID {}
		}
	[:On]
@Pure
override [:aseventlistenerfct](asEventListener) : EventListener {
	new FilteringEventListener(this)
}
	[:Off]
	}
[:End:]

### Implementation of the over capacity functions

Several functions must be implemented into the [:filteringeventlistener:] skill in order
to have a complete implementation of the [:behaviorscapacity:] capacity.
All the functions invoke their equivelent functions into the original built-in
capacity.

[:Success:]
	package io.sarl.docs.tutorials.overrideeventbuscapacity
	import io.sarl.core.Behaviors
	import io.sarl.lang.core.Address
	import io.sarl.lang.core.Behavior
	import io.sarl.lang.core.Event
	import io.sarl.lang.core.EventListener
	import io.sarl.lang.core.Scope
	import io.sarl.lang.util.ConcurrentCollection
	import java.util.UUID
	skill FilteringEventDispatchingBehavior implements Behaviors {
		var acceptedType : Class<? extends Event>
		var behaviorDelegate : Behaviors
		private static class FilteringEventListener implements EventListener {
			new (parent : FilteringEventDispatchingBehavior) {}
			override receiveEvent(occ : Event) {}
			override getID : UUID {}
		}
		@Pure
		override [:aseventlistenerfct](asEventListener) : EventListener {
			new FilteringEventListener(this)
		}
	[:On]
override hasRegisteredBehavior : boolean {
	this.behaviorDelegate.hasRegisteredBehavior
}

override getRegisteredBehaviors : ConcurrentCollection<Behavior> {
	this.behaviorDelegate.getRegisteredBehaviors
}

override registerBehavior(attitude : Behavior, filter : (Event) => boolean, initializationParameters : Object*) : Behavior {
	this.behaviorDelegate.registerBehavior(attitude, filter, initializationParameters)
}

override unregisterBehavior(attitude : Behavior) : Behavior {
	this.behaviorDelegate.unregisterBehavior(attitude)
}

override wake(^event : Event, scope : Scope<Address>) {
	this.behaviorDelegate.wake(^event, scope)
}

override wake(beh : Behavior, ^event : Event) {
	this.behaviorDelegate.wake(beh, ^event)
}

override wake(behs : Iterable<Behavior>, ^event : Event) {
	this.behaviorDelegate.wake(behs, ^event)
}
	[:Off]
	}
[:End:]


## Using the overriding skill in the agent

The last step to implement in this tutorial is the registration of the new [:mybehaviorname:] in place of the
built-in [:behaviorscapacity:] capacity.
This action is usually done during the initialization stage of the agent.
In the following code, the [:myeventtype:] is assumed to be defined as a SARL event that is the only
one type of event accepted by the agents of type [:filteringeventagent:].

[:Success:]
	package io.sarl.docs.tutorials.overrideeventbuscapacity
	import io.sarl.lang.core.Event
	import io.sarl.lang.core.Behavior
	import io.sarl.lang.core.Scope
	import io.sarl.lang.core.Address
	import io.sarl.core.Behaviors
	import io.sarl.core.Initialize
	import io.sarl.lang.core.EventListener
	import java.util.UUID
	import io.sarl.lang.util.ConcurrentCollection
	skill FilteringEventDispatchingBehavior implements Behaviors {
		new (acceptedType : Class<? extends Event>, behaviorDelegate : Behaviors) {
		}
		@Pure
		override asEventListener : EventListener {
		}
		override hasRegisteredBehavior : boolean {
			false
		}
		override getRegisteredBehaviors : ConcurrentCollection<Behavior> {
		}
		override registerBehavior(attitude : Behavior, filter : (Event) => boolean, initializationParameters : Object*) : Behavior {
		}
		override unregisterBehavior(attitude : Behavior) : Behavior {
		}
		override wake(^event : Event, scope : Scope<Address>) {
		}
		override wake(beh : Behavior, ^event : Event) {
		}
		override wake(behs : Iterable<Behavior>, ^event : Event) {
		}
	}
	event [:myeventtype](MyEvent)
	[:On]
	agent [:filteringeventagent](FilteringEventAgent) {

		on Initialize {
			val [:originalskillvar](originalSkill) = getSkill(typeof(Behaviors))
			val newSkill = new FilteringEventDispatchingBehavior(typeof(MyEvent), originalSkill)
			setSkill(newSkill)
		}

	}
[:End:]

The initialization process does the following steps:

1. Retrieve the original implementation of the [:behaviorscapacity:] capacity that is provided by the SRE, into the local variable [:originalskillvar:].
2. Create the instance of the new [:mybehaviorname:] skill by passing the event type under interest, and the reference to the original skill.
3. Register the new instance of [:mybehaviorname:] skill, that will cause the replacement of the [:behaviorscapacity:] skill provided by the SRE.


[:Include:](../legal.inc)
