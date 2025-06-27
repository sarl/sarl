# Interface

[:Outline:]

An interface is a description of the actions that an object can do.
For example when you flip a light switch, the light goes on, you don't
care how, just that it does. In object-oriented programming, an
interface is a description of all functions that an object must have
in order to be an "X".

The purpose of interfaces is to allow the program to enforce these
properties, and to know that an object of type T (whatever the interface
is) must have functions called X,Y,Z, etc.


## Define an Interface

In the following example, the [:lighttype:] interface is defined with the two methods [:turnonfct:] and [:turnofffct:].

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	interface [:lighttype](Light) {
		def [:turnonfct](turnOn)
		def [:turnofffct](turnOff)
	}
[:End:]


## Interface Inheritance

It is possible to specialize the definition of an interface. In the following example, the [:varlight:]
interface that is refining the previous [:lighttype:] interface and add specific functions.

[:Success:]
	package io.sarl.docs.reference.oop
	interface Light {
		def turnOn
		def turnOff
	}
	[:On]
	interface [:varlight](VariableIntensityLight) [:extendskw](extends) Light {
		def setLightIntensity(intensity : float)
		def getLightIntensity : float
	}
[:End:]


## Define a Generic Interface

A generic interface declaration looks like a non-generic interface declaration, except that the interface name
is followed by a type parameter section.

The type parameter section of a generic interface can have one or more type parameters separated
by commas. These interfaces are known as parameterized interfaces or parameterized types
because they accept one or more parameters.

There may be times when you'll want to restrict the kinds of types that are allowed to be passed
to a type parameter. For example, a method that operates on numbers might only want to
accept instances of Number or its subclasses. This is what bounded type parameters are for.
To declare a bounded type parameter, list the type parameter's name, followed by:

* the [:extendskw:] keyword, followed by its upper bound; or
* the [:superkw:] keyword, followed by its lower bound.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	interface AnInterface<T> {
		def add(t : T)
	
		def get : T
	}
	
	interface Vector<T extends Number> {
		def norm : Vector<? [:superkw](super) Number>
	}
[:End:]


## Interface Implementation

A class is able to implement an interface. The [:implementskw:] keyword is used for defining
the implementation relationship between a class and an interface.
The class must provide an implementation of all the functions defined in the interface.
The only one exception is when the class is abstract. In this case, the derived classes must implement the
functions of the interface.

[:Success:]
	package io.sarl.docs.reference.oop
	interface Light {
	  def turnOn
	  def turnOff
	}
	[:On]
	class TheLight [:implementskw](implements) Light {
		var isSwitchedOn = false
		def turnOn {
			this.isSwitchedOn = true
		}
		def turnOff {
			this.isSwitchedOn = false
		}
	}
[:End:]


## Default Implementation of Interface Functions

As in Java 8, interface types could provide default function implementation if the implementation class is
not providing one. This feature is known as the default method mechanism.

In the following example, two default implementations of the [:turnonfct:] and [:turnofffct:] functions are provided.
The implementing class needs to provide a function code only for the [:switchfct:] function:
 
[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	interface Light {
		def turnOn {
			switchLight(true)
		}
		def turnOff {
			switchLight(false)
		}
		def [:switchfct](switchLight)(state : boolean)
	}
	class TheLight implements Light {
		var isSwitchedOn = false
		def switchLight(state : boolean) {
			this.isSwitchedOn = state
		}
	}
[:End:]


## Modifiers

Modifiers are used to modify declarations of types and type members. This section introduces the modifiers for the interface.
The modifiers are usually written before the keyword for defining the interface.

The complete description of the modifiers' semantic is available on [this page](./Modifiers.md).


### Top Interface Modifiers

A top interface may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]: the class is accessible from any other type (default);
	* [:packagemodifier:]: the class is accessible from only the types in the same package.
* [:abstractmodifier:]: the interface is abstract (not needed since all the interfaces are abstract).
* [:strictfpmodifier:]: avoid the methods of the implementing classes to use intermediate floating number formats.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	public interface TopInterface1 {
	}
	package interface TopInterface2 {
	}
	abstract interface TopInterface3 {
	}
[:End:]


### Nested Interface Modifiers

A nested interface may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the interface (default);
	* [:protectedmodifier:]: the interface is accessible within the same package, and derived classes;
	* [:packagemodifier:]: the interface is accessible only within the same package as its class;
	* [:privatemodifier:]: the interface is accessible only within its class.
* [:abstractmodifier:]: the interface is abstract (not needed since all the interfaces are abstract).
* [:staticmodifier:]: the inner interface do not have access to the non-static members of the enclosing type.
* [:strictfpmodifier:]: avoid the methods of the interface to use intermediate floating number formats.

> **_Terminology:_** Nested interfaces are divided into two categories: static and non-static.
> Nested interfaces that are declared static are called **static nested interfaces**.
> Non-static nested interfaces are called **inner interfaces**.

> **_Note:_** The modifiers may differ from the previously described, depending on the enclosing type, e.g. agent.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class EnclosingClass {
		[:publicmodifier](public) interface NestedInterface1 {
		}
		[:protectedmodifier](protected) interface NestedInterface2 {
		}
		[:packagemodifier](package) interface NestedInterface3 {
		}
		[:privatemodifier](private) interface NestedInterface4 {
		}
		[:abstractmodifier](abstract) interface NestedInterface5 {
		}
		[:staticmodifier](static) interface NestedInterface6 {
		}
		[:strictfpmodifier](strictfp) interface NestedInterface7 {
		}
	}
[:End:]


### Field Modifiers

The modifiers for the fields in an interface are:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the field (default);
* [:staticmodifier:]: the field is a class field, not an instance field (default).

> **_Caution:_** Only fields defined with `val` can be put in an interface.

[:Success:]
	package io.sarl.docs.reference.oop
	public interface MyInterface1 {
	[:On]
		public val a : Object;
		static val e : Object;
	[:Off]
	}
[:End:]


### Method Modifiers

The modifiers for the methods in an interface are:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the method (default);
* [:abstractmodifier:]: the method is abstract (not needed since all the interface methods are abstract).

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	public interface MyInterface1 {
	[:On]
		public def fct1
		abstract def fct5
	[:Off]
	}
[:End:]

[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
