# Creating Space with Operation User Accessibility 

[:Outline:]

This document describes the basics of the writing of a space that is able to have the identity of
the caller of a space's function.

The concept is inspired by the Agents & Artifacts metamodel implemented by the [CArtAgO project](http://cartago.sourceforge.net/).
In this approach, artifacts have operations, properties an can trigger events. In addition, artifact implementations can
identify their operation callers in order to prevent agents to manipulate the artifact using someone else's identity.

The idea using a space is very similar: agents "register" in the space in order to receive its events, but cannot emit any,
and can invoke space operations like an artifact.
The question was regarding the identity parameter, since CArtAgO allows to determine the caller implicitly.

In this document, a space is defined with caller identity as parameter. An a capacity/skill is defined for invoking
the operation on the space.


## Capacity Definition

The first step is the [definition of a capacity](../reference/Capacity.md) that enables an agent to access to
the operations of the space.

In the following example, the [:mycapacityname:] capacity is defined with the [:fctname:] function. 

[:Success:]
	[:On]
	capacity [:mycapacityname](MyCapacity) {
	
		def [:fctname](functionAccessibleToTheAgent)(parameter : Object)
	
	}
	[:Off]
[:End:]


## Space Definition

Currently, the definition of a space into SARL with specific keywords is not yet supported.
You must define a space with the definition of a class that extends the [:spacetype:] type.
In the following example, the [:myspacename:] space is declared:

[:Success:]
	import io.sarl.lang.core.Space
	import io.sarl.lang.core.SpaceID
	import java.util.concurrent.ConcurrentSkipListSet
	import java.util.UUID
	abstract [:On]
	class [:myspacename](MySpace) implements [:spacetype](Space) {
	
		val id : SpaceID

		new (id : SpaceID) {
			this.id = id
		}

	    def getSpaceID : SpaceID {
	    	this.id
	    }

	    def getParticipants : ConcurrentSkipListSet<UUID> {
	    	null
	    }

	}
	[:Off]
[:End:]


The [:fctname:] function, initially defined into the [:mycapacityname:] capacity must be defined into the
[:myspacename:] space. But for enabling the space to have the identity of the function caller, the identity
must be given as formal parameter, in addition to the already defined parameters.
Back to the example, the code becomes:

[:Success:]
	import io.sarl.lang.core.Space
	import io.sarl.lang.core.SpaceID
	import java.util.concurrent.ConcurrentSkipListSet
	import java.util.UUID
	abstract [:On]
	class [:myspacename](MySpace) implements [:spacetype](Space) {
	
		def [:fctname!](callerIdentity : UUID, parameter : Object) {
			// Do something
		}

		val id : SpaceID

		new (id : SpaceID) {
			this.id = id
		}

	    def getSpaceID : SpaceID {
	    	this.id
	    }

	    def getParticipants : ConcurrentSkipListSet<UUID> {
	    	null
	    }

	}
	[:Off]
[:End:]


In order to enable the creation of an instance of [:myspacename!], a space specification must be defined:

[:Success:]
	import io.sarl.lang.core.Space
	import io.sarl.lang.core.SpaceID
	import io.sarl.lang.core.SpaceSpecification
	import java.util.concurrent.ConcurrentSkipListSet
	import java.util.UUID
	class [:myspacename!] implements [:spacetype!] {
		def [:fctname!](callerIdentity : UUID, parameter : Object) {
		}
		new (id : SpaceID) {}
	    def getSpaceID : SpaceID {}
	    def getID : SpaceID {}
	    def isPseudoEmpty(id : UUID) { false }
		def getNumberOfStrongParticipants : int { 0 }
		def getNumberOfWeakParticipants : int { 0 }
		def forEachStrongParticipant(cb : (UUID)=>void) {}
		def forEachWeakParticipant(cb : (UUID)=>void) {}
	}
	[:On]
	class [:myspacespecname]([:myspacename!]Specification) implements SpaceSpecification<[:myspacename!]> {
		def create(id : SpaceID, params : Object*) : [:myspacename!] {
			new [:myspacename!](id)
		}
	}
	[:Off]
[:End:]



## Skill Definition

In order to do a bridge between the defined capacity and the space, a skill must be defined.
The main role of this skill is to determine the identity of the operation's caller for giving
it to the space.
Back to the example, the code for the skill is:

[:Success:]
	import io.sarl.lang.core.Space
	import io.sarl.lang.core.SpaceID
	import io.sarl.lang.core.SpaceSpecification
	import java.util.concurrent.ConcurrentSkipListSet
	import java.util.UUID
	capacity [:mycapacityname!] {
		def [:fctname!](parameter : Object)
	}
	class [:myspacename!] implements [:spacetype!] {
		def [:fctname!](callerIdentity : UUID, parameter : Object) {}
		new (id : SpaceID) {}
	    def getSpaceID : SpaceID {}
	    def getID : SpaceID {}
	    def isPseudoEmpty(id : UUID) { false }
		def getNumberOfStrongParticipants : int { 0 }
		def getNumberOfWeakParticipants : int { 0 }
		def forEachStrongParticipant(cb : (UUID)=>void) {}
		def forEachWeakParticipant(cb : (UUID)=>void) {}
	}
	class [:myspacespecname]([:myspacename!]Specification) implements SpaceSpecification<[:myspacename!]> {
		def create(id : SpaceID, params : Object*) : [:myspacename!] {
			new [:myspacename!](id)
		}
	}
	[:On]
	skill [:myskillname](MySkill) implements [:mycapacityname!] {

		var [:spacename](^space) : [:myspacename!]

		def [:fctname!](parameter : Object) {
			this.[:spacename!].[:fctname!](this.owner.ID, parameter)
		}
	}
	[:Off]
[:End:]


In the previous code, the reference to the space is put into a skill's field, named [:spacename:].
This field is not initialized in the previous example. You could initialize it into the installation function of
the skill : `def install() : void`

The definition of the [:fctname!] name is based on the delegation design pattern : the skill calls the similar space's function.
The difference is that the skill's call include the identifier of the function's caller, i.e. the agent's identifier.


## Accessing to the function caller instance

Sometimes, it is useful to obtain the instance of the object, which has called the skill's function.
The caller may be the agent itself or one of its behaviors.
The definition of the abstract type [:skilltype:] includes the [:getcallerfct:] function:

[:Fact:]{typeof(io.sarl.lang.core.[:skilltype](Skill)).shouldHaveMethod(
	"[:getcallerfct](getCaller) : io.sarl.lang.core.AgentTrait")
}

Into the skill's functions, you could use this function for obtaining the skill function's caller.
This function replies the behavior instance, which has called the function, or `ǹull` if the caller
is the agent.

Back to the previous example, let's change the type of caller identity from `UUID` to `Object` in order
to give the instance of the caller to the space. The code of the skill becomes:

[:Success:]
	import io.sarl.lang.core.Space
	import io.sarl.lang.core.SpaceID
	import io.sarl.lang.core.SpaceSpecification
	import java.util.concurrent.ConcurrentSkipListSet
	import java.util.UUID
	capacity [:mycapacityname!] {
		def [:fctname!](parameter : Object)
	}
	class [:myspacename!] implements [:spacetype!] {
		def [:fctname!](callerIdentity : Object, parameter : Object) {}
		new (id : SpaceID) {}
	    def getSpaceID : SpaceID {}
	    def getID : SpaceID {}
	    def isPseudoEmpty(id : UUID) { true }
		def getNumberOfStrongParticipants : int { 0 }
		def getNumberOfWeakParticipants : int { 0 }
		def forEachStrongParticipant(cb : (UUID)=>void) {}
		def forEachWeakParticipant(cb : (UUID)=>void) {}
	}
	[:On]
	skill [:myskillname](MySkill) implements [:mycapacityname!] {
	
		var [:spacename](^space) : [:myspacename!]
	
		def [:fctname!](parameter : Object) {
			var theCaller = getCaller ?: this.owner
			this.[:spacename!].[:fctname!](theCaller, parameter)
		}

	}
	[:Off]
[:End:]


[:Include:](../legal.inc)
