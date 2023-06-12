# Built-in Capacity Reference

[:Outline:]

This document describes the built-in capacities in SARL.
Before reading this document, we recommend that you read
the [General Syntax Reference](./GeneralSyntax.md), the [Capacity Reference](./Capacity.md),
and the [Skill Reference](./Skill.md).

A *Capacity* is the specification of a collection of actions. This specification makes no assumptions about
its implementation. It could be used to specify what an agent can do, what a behavior requires for its execution.

A *Skill* is a possible implementation of a capacity fulfilling all the 
 * constraints of this specification.

In SARL, every agent has a set of *built-in capacities* considered essential 
to respect the commonly accepted competencies of agents.
These capacities are considered the main building blocks on top of which other 
higher level capacities and skills can be constructed.
They are defined in the SARL language, but the skills implementing them are provided 
by the runtime environment, e.g. the [Janus platform](http://www.janusproject.io).
This runtime environment is responsible for creating them and injecting them in 
the agent before their execution begins.
Therefore, when the agent receives the `Initialize` event they are
already available.

The following figure presents the different contexts associated to an agent `A`.
Several built-in capacities permit accessing and manage these contexts.
The agents are represented by stylized humans, the contexts by the blue boxes,
and the spaces by the small color boxes in the contexts.

![Contexts](./contexts.png)


## Details on the Built-In Capacities

Each built-in capacity is detailed inside a dedicated section:

* [Behaviors](./bic/Behaviors.md)
* [DefaultContextInteractions](./bic/DefaultContextInteractions.md)
* [ExternalContextAccess](./bic/ExternalContextAccess.md)
* [InnerContextAccess](./bic/InnerContextAccess.md)
* [Lifecycle](./bic/Lifecycle.md)
* [Logging](./bic/Logging.md)
* [Schedules](./bic/Schedules.md)
* [Time](./bic/Time.md)


## Use of the Built-in Capacities

Details on the use of the built-in capacities may be found in the references of the major
behavior-based concepts of SARL:

* [Agent](./Agent.md)
* [Behavior](./Behavior.md)



[:Include:](../legal.inc)
