# Space Reference

[:Outline:]

This document describes the features related to the definition of a space in SARL.
Before reading this document, we recommend that you read the [General Syntax Reference](./GeneralSyntax.md),
and the [Agent Reference](./Agent.md).

One of the key elements that characterize and differentiate the main 
multi-agent approaches is how interactions between agents are described.
Some researchers focus on agent-to-agent interactions and corresponding 
protocols. Within organizational approaches, some consider the 
organization as a static partition of agents where agents interact in 
groups through the roles they play. Others focus on dynamic 
organizations and normative aspects. 

Another essential aspect of the interaction is the interaction 
Agent-Environment, especially in agent-based simulations.
Each of these trends of multi-agent systems has led to numerous 
fruitful and innovative contributions.
To remain generic, SARL therefore not imposes a single way of 
describing the interaction among agents, but rather attempt to 
provide means to implement each of these approaches.

It is in this perspective that the concepts of [:spacedef:]
and [:spacespecdef:] were defined.

__A Space is the support of the interaction between agents respecting the rules defined in a Space Specification.__

__A Space Specification defines the rules (including action and
perception) for interacting within a given set of spaces respecting 
this specification.__

SARL natively defines a particular type of [:spacedef:] called
*Event Space* to provide a support to event-driven interactions.
Within an event space, agents communicate using events, the 
[built-in capacity `DefaultContextInteractions`](./BIC.md)
provides the agent with the means to emit and receive events, respectively 
using the [:emitfct:] actions and the `on` keyword
in behavior definition.
A __Default Space__ is precisely an event space.

Within an event space, the notion of [:scopedef:] enables to 
precisely control/filter the potential recipients of an event.
__A [:scopedef!] is a predicate used to filter the potentially called 
listeners for a given event.__
The most basic scope is represented by a collection of addresses.


## Types of Spaces

SARL provides a collection of interfaces that are representing different types of spaces.


### Space

SARL provides an interface that is representing all the spaces:

		[:ShowType:]{io.sarl.lang.core.[:spacedef](Space)}
		[:Fact:]{typeof(io.sarl.lang.core.Space).shouldHaveMethod("[:getspaceidfct](getSpaceID) : io.sarl.lang.core.SpaceID")}
		[:Fact:]{typeof(io.sarl.lang.core.Space).shouldHaveMethod("[:getparticipantsfct](getParticipants) : io.sarl.lang.util.SynchronizedSet")}

The [:getspaceidfct:] function replies the identifier of the space.
The [:getparticipantsfct:] function replies the identifiers
of the agents belonging to the space.


### Event Space

Spaces that are based on event propagation mechanism are defined as:

		[:ShowType:]{io.sarl.lang.core.[:eventspacedef](EventSpace)}
		[:Fact:]{typeof(io.sarl.lang.core.EventSpace).shouldHaveMethod("[:getadrfct](getAddress)(java.util.UUID) : io.sarl.lang.core.Address")}
		[:Fact:]{typeof(io.sarl.lang.core.EventSpace).shouldHaveMethod("[:emitfct](emit)(java.util.UUID,io.sarl.lang.core.Event, io.sarl.lang.core.[:scopedef](Scope))")}
		[:Fact:]{typeof(io.sarl.lang.core.EventSpace).shouldHaveMethod("emit(java.util.UUID,io.sarl.lang.core.Event)")}


The [:getadrfct:] function replies the address in the space of the agent that has the given identifier.
The [:emitfct:] functions permits fire of an event in the space.


### Open Event Space

Event spaces that are allowing the agents to be register and unregister are "open event spaces":

		[:ShowType:](io.sarl.core.OpenEventSpace)
		[:Fact:]{typeof(io.sarl.core.OpenEventSpace).shouldHaveMethod("[:registerfct](register)(io.sarl.lang.core.[:eventlistener](EventListener)) : io.sarl.lang.core.Address")}
		[:Fact:]{typeof(io.sarl.core.OpenEventSpace).shouldHaveMethod("[:unregisterfct](unregister)(io.sarl.lang.core.EventListener) : io.sarl.lang.core.Address")}

The functions [:registerfct:] and [:unregisterfct:] permit an agent to be involved or not.
The function [:registerfct:] fires the event `ParticipantJoined`.
And, the function [:unregisterfct:] fires the event `ParticipantLeft`.

[:Fact:]{typeof(io.sarl.core.ParticipantJoined)}
[:Fact:]{typeof(io.sarl.core.ParticipantLeft)}


### Restricted Access Event Space

When an event space needs to control the registration access, it should be a "restricted access event space":

		[:ShowType:](io.sarl.util.RestrictedAccessEventSpace)

The functions given by this type of space permits implementing a space with restricted access, based on the standard Java API.


##	Defining a Space

The definition of a new space must be done with object-oriented language's features.

For defining a space, three steps must be followed:

* Definition of the interface of the space;
* Implementation of the space on a specific runtime environment;
* Definition of the space specification.

In the rest of this section, we use the example of the definition of a physic space: a space in which objects have a 
spatial position. 


### Defining the PhysicSpace

The first step for the definition of a new type of space is the specification of the interface that is describing
the functions provided by the space.

The new space type must extend one of the predefined types, below [:spacetypename:]. In the following example, the new space
is related to the physic environment in which the agents may evolve.

		[:Success:]
			import io.sarl.lang.core.Space
			import java.util.UUID
			import io.sarl.lang.core.EventListener
			[:On]
			interface PhysicSpace extends [:spacetypename](Space) {
				def moveObject(identifier : UUID, x : float, y : float, z : float)
				def bindBody(^agent : EventListener)
				def unbindBody(^agent : EventListener)
			}
		[:End:]

This space permits to move an object, i.e. the physical representation of the agent 
(named body). Additionally, the space gives to the agent the ability to be binded to its body, and
to release the control of its body.
The [:eventlistener:] type is the event listening mechanism associated to the agent. It may be obtained with
the `Behaviors` built-in capacity (see the corresponding
[built-in capacity reference](./bic/Behaviors.md) for details).

		[:Fact:]{typeof(io.sarl.core.Behaviors)}


### Basic Implementation

The definition of the space implementation depends upon the runtime environment.

<caution>This section of the space reference document may evolved in future releases of SARL. Please activate
the "deprecated feature use" warning in your compilation configuration for ensuring
that you will be notified about any major changes on this part of the API.</caution>

Below, the implementation extends one of the abstract classes provided by the [Janus Platform](http://www.janusproject.io).

		[:Success:]
			import io.sarl.lang.core.Space
			import io.sarl.lang.core.SpaceID
			import java.util.UUID
			import io.sarl.lang.core.EventListener
			import io.janusproject.kernel.^space.AbstractEventSpace
			interface PhysicSpace extends Space {
			}
			class PhysicObject {
				def move(x : float, y : float, z : float) { }
			}
			[:On]
			class PhysicSpaceImpl extends AbstractEventSpace implements PhysicSpace {
				val [:entityfield](entities) = <[:uuid](UUID), PhysicObject>newHashMap

				def [:moveobjectfct](moveObject)(identifier : UUID, x : float, y : float, z : float) {
					synchronized (this.entities) {
						var o = this.entities.get(identifier)
						if (o !== null) {
							o.move(x, y, z)
						}
					}
				}

				def bindBody(listener : EventListener) {
					synchronized (this.entities) {
						entities.put(listener.ID, new [:physicobject](PhysicObject))
					}
				}

				def unbindBody(listener : EventListener) {
					synchronized (this.entities) {
						entities.remove(listener.ID)
					}
				}
			}
		[:End:]


The physic space contains a collection of objects, namely [:entityfield:].
Each object is identified by an [:uuid:]. It is assumed that the [:physicobject:] class provides a method for moving it:
`move(float, float, float)`.
When an agent wants to move an object by calling the [:moveobjectfct:] method,
the space is retrieving the instance of this object in the [:entityfield:], and
move it. 

<important>The previous implementation has a major problem: it does not permit
to distribute the information and the interaction objects over a computer network. The space is
the support of the interaction. Consequently, it should provide the mechanisms for
routing the events to all the agents other the computer network.</important>


### Basic Network Support

As described in the previous section, the space implementation should route the information among the agents over a computer
network.

<caution>This section of the space reference document may evolved in future releases of SARL. Please activate the
"deprecated feature use" warning in your compilation configuration for ensuring that you will be notified
about any major changes on this part of the API.</caution>

Below, the implementation extends one of the abstract classes provided by the [Janus Platform](http://www.janusproject.io).

		[:Success:]
			import io.sarl.lang.core.Space
			import io.sarl.lang.core.SpaceID
			import io.sarl.lang.core.Address
			import java.util.UUID
			import java.util.Map
			import io.sarl.lang.core.EventListener
			import io.janusproject.kernel.^space.AbstractEventSpace
			import io.janusproject.services.distributeddata.DistributedDataStructureService
            import com.google.inject.Provider
            import java.util.concurrent.locks.ReadWriteLock
			interface PhysicSpace extends Space {
			}
			class PhysicObject {
				def move(x : float, y : float, z : float) { }
			}
			[:On]
			class NetworkPhysicSpaceImpl extends AbstractEventSpace implements PhysicSpace {
				val entities : Map<UUID,PhysicObject>
				
				public new(id : SpaceID, factory : DistributedDataStructureService,
				        lockProvider : Provider<ReadWriteLock>) {
					super(id, factory, lockProvider)
					this.entities = factory.getMap(id.toString + "-physicObjects")
				}
				
				def bindBody(entity : EventListener) {
					this.entities.put(entity.ID, new PhysicObject)
					var a = new Address(spaceID, entity.ID)
					synchronized (this.participantInternalDataStructure) {
						return this.participantInternalDataStructure.registerParticipant(a, entity)
					}
				}
				
				def unbindBody(entity : EventListener) {
					this.entities.remove(entity.ID)
					synchronized (this.participantInternalDataStructure) {
						return this.participantInternalDataStructure.unregisterParticipant(entity)
					}
				}
				
				def moveObject(identifier : UUID, x : float, y : float, z : float) {
					var o = this.entities.remove(identifier)
					if (o !== null) {
						o.move(x, y, z)
						this.entities.put(identifier, o)
					}
				}
			}
		[:End:]

<important>The collection of the physic objects is a distributed map over the computer network. It means that each node
of the platform has a direct access to the objects' instances. If whose to say that is implementation may face some problems,
such as the serialization of the physic objects, and the scalability of the distributed map.</important>


### Defining a SpaceSpecification

For creating instances of spaces, it is necessary to define a space specification.
This specification may create the space instance according to rules, or provide information and rules to the spaces.

		[:Success:]
			import io.sarl.lang.core.Space
			import io.sarl.lang.core.SpaceSpecification
			import io.sarl.lang.core.SpaceID
			import io.sarl.lang.util.SynchronizedSet
			import java.util.UUID
			import io.sarl.lang.core.EventListener
			interface PhysicSpace extends Space {
			}
			class PhysicSpaceImpl implements PhysicSpace {
				new (id : SpaceID) { }
				def moveObject(identifier : UUID, x : float, y : float, z : float) { }
				def bindBody(listener : EventListener) { }
				def unbindBody(listener : EventListener) { }
				def getID : SpaceID { null }
				def getSpaceID : SpaceID { null }
				def getParticipants : SynchronizedSet<UUID> { null }
			}
			[:On]
			class PhysicSpaceSpecification implements [:spacespecdef](SpaceSpecification)<PhysicSpace> {
				def create(id : SpaceID, params : Object*) : [:physicspacedef](PhysicSpace) {
					return new PhysicSpaceImpl(id)
				}
			}
		[:End:]

The example above is the specification related to the first implementation of the [:physicspacedef:].

		[:Success:]
			import io.sarl.lang.core.Space
			import io.sarl.lang.core.SpaceSpecification
			import io.sarl.lang.core.SpaceID
			import java.util.UUID
			import io.sarl.lang.util.SynchronizedSet
			import io.sarl.lang.core.EventListener
			import javax.inject.Inject
			import io.janusproject.services.distributeddata.DistributedDataStructureService
			interface PhysicSpace extends Space {
			}
			class NetworkPhysicSpaceImpl implements PhysicSpace {
				new (id : SpaceID, serv : DistributedDataStructureService) { }
				def moveObject(identifier : UUID, x : float, y : float, z : float) { }
				def bindBody(listener : EventListener) { }
				def unbindBody(listener : EventListener) { }
				def getID : SpaceID { null }
				def getSpaceID : SpaceID { null }
				def getParticipants : SynchronizedSet<UUID> { null }
			}
			[:On]
			class NetworkPhysicSpaceSpecification implements SpaceSpecification<PhysicSpace> {
				@Inject
				var factory : DistributedDataStructureService

				def create(id : SpaceID, params : Object*) : PhysicSpace {
					return new NetworkPhysicSpaceImpl(id, factory)
				}
			}
		[:End:]

The example above is the specification that permits to create a physic space with networking. It retrieves
by injection the factory of distributed data structures provided by the Janus platform.


### Access to the Default Space Instance within a space specification

If the space instance needs to be linked to the default space of the context, it is 
necessary to retrieve the instance of the default space within the space specification.
Then, this specification is in charge of passing the default space instance to the 
space instance.

By contract, the default space instance could be []injected into the space specification 
instance](https://en.wikipedia.org/wiki/Dependency_injection). The following constraints 
apply:

1. The injected feature is an object field of the space specification, or a formal parameter;
2. The injected feature must be of type `OpenEventSpace`;
3. The injected feature must be marked with one of the two following methods:
   a. annotated with `[:namedannotation!]("defaultSpace")`, or
   b. annotated with `@[:defaultspaceannotation!]`.

The following example illustrates the first method of marking of an object field:

        [:Success:]
            import java.util.UUID
            import io.sarl.lang.util.SynchronizedSet
            import io.sarl.lang.core.SpaceID
            import io.sarl.lang.core.SpaceSpecification
            import io.sarl.lang.core.Space
            import io.sarl.core.OpenEventSpace
            import javax.inject.Inject
            import com.google.inject.name.Named
            interface MySpace extends Space {
            }
            class MySpaceImpl implements MySpace {
                new (obj : OpenEventSpace) {}
                def getID : SpaceID { null }
                def getSpaceID : SpaceID { null }
                def getParticipants : SynchronizedSet<UUID> { null }
            }
            [:On]
            class MySpaceSpecification implements SpaceSpecification<MySpace> {

                @Inject
                [:namedannotation](@Named)("defaultSpace")
                var defaultSpace : OpenEventSpace

                def create(id : SpaceID, params : Object*) : MySpace {
                    return new MySpaceImpl(this.defaultSpace)
                }
            }
        [:End:]



The following example illustrates the second method of marking of an object field:

        [:Success:]
            import java.util.UUID
            import io.sarl.lang.util.SynchronizedSet
            import io.sarl.lang.core.SpaceID
            import io.sarl.lang.core.SpaceSpecification
            import io.sarl.lang.core.Space
            import io.sarl.core.OpenEventSpace
            import javax.inject.Inject
            interface MySpace extends Space {
            }
            class MySpaceImpl implements MySpace {
                new (obj : OpenEventSpace) {}
                def getID : SpaceID { null }
                def getSpaceID : SpaceID { null }
                def getParticipants : SynchronizedSet<UUID> { null }
            }
            [:On]
            class MySpaceSpecification implements SpaceSpecification<MySpace> {

                @Inject
                @io.sarl.util.[:defaultspaceannotation](DefaultSpace)
                var defaultSpace : OpenEventSpace

                def create(id : SpaceID, params : Object*) : MySpace {
                    return new MySpaceImpl(this.defaultSpace)
                }
            }
        [:End:]



[:Include:](../legal.inc)
