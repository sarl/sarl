# Basic Object-Oriented Programming Support

[:Outline:]

SARL enables to declare objects with an object-oriented programming approach for convenience to develop complex programs, i.e.
the SARL developer must not learn another language such as Java for programming the objects that are used within the
SARL program.
Because of this convenience, the support of object-oriented programming may be less complete than in main stream object programming
languages.
 
This document describes the basic support of object-oriented programming provided by SARL.
Before reading this document, we recommend that you read
the [General Syntax Reference](./GeneralSyntax.md).

The support of the object-oriented programming (OOP) statements in SARL is less complete
than a real object-oriented language such as Java.
The basics of OOP are enabled in the SARL scripts. If you need more complex or more
complete support of the OOP, you should use a dedicated language, such as Java,
[Xtend](https://eclipse.org/xtend/), or [Scala](http://www.scala-lang.org/).

> **_Note:_** The SARL Eclipse product includes the tools for programming with the Java and Xtend languages.


## Comparison between SARL and Other Languages

A comparison between SARL and other programming languages could be found [here](./LanguageComparison.md).


## Class

Objects are structures that contain both data and procedures. Classes are definitions for the data format and
available procedures for a given type or class of objects. They may also contain data and
procedures (known as class methods) themselves.


### Define a Class

For defining a class, you could use the [:classkw:] keyword. The following example defines the class [:classname:].

The members of the class, i.e. the fields, methods and constructors must be written between the braces that are
following the class declaration.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	[:classkw](class) [:classname](MyClass) {  }
[:End:]


### Class Inheritance

Languages that support classes almost always support inheritance. This allows classes to be arranged in a
hierarchy that represents "is-a-type-of" relationships.

For example, class [:employeetype:] might inherit from class [:persontype:].
All the data and methods available to the parent class also appear in the child class with the same names.
For example, class [:persontype:] might define variables [:firstnamefield:] and [:lastnamefield:] with
method [:getfullnamefct:]. These will also be available in class Employee, which might add the variables
[:positionfield:] and [:salaryfield:].

The definition of the inheritance relationship between two classes is done with the [:extendskw:] keyword.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class [:persontype](Person) {
		
		var [:firstnamefield](firstName) : String
		var [:lastnamefield](lastName) : String
		
		def [:getfullnamefct](getFullName) : String {
			this.firstName + " " + this.lastName
		}

	}
	
	class [:employeetype](Employee) [:extendskw](extends) Person {
		
		var [:positionfield](position) : String
		
		var [:salaryfield](salary) : float
	
	}
[:End:]


### Define a Generic Class

A generic class declaration looks like a non-generic class declaration, except that the class name
is followed by a type parameter section.

The type parameter section of a generic class can have one or more type parameters separated
by commas. These classes are known as parameterized classes or parameterized types
because they accept one or more parameters.

There may be times when you'll want to restrict the kinds of types that are allowed to be passed
to a type parameter. For example, a method that operates on numbers might only want to
accept instances of Number or its subclasses. This is what bounded type parameters are for.
To declare a bounded type parameter, list the type parameter's name, followed by:

* the [:extendskw:] keyword, followed by its upper bound; or
* the `super` keyword, followed by its lower bound.

Example:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class AType<T> {
	
		var t : T
	
		def add(t : T) {
			this.t = t
		}
	
		def get : T {
			return this.t
		}
	
	}
	
	class Vector<T extends Number> {
	
		var x : T
		var y : T
	
		def norm : Vector<? extends Number> {
			var v = new Vector
			var n = Math::sqrt(this.x.floatValue**2 + this.y.floatValue**2)
			v.x = this.x.floatValue / n
			v.y = this.y.floatValue / n
			return v
		}
	}
[:End:]


### Constructor Definition

An SARL class can define any number of constructors. Unlike Java, you do not have to repeat the name of the
class over and over again, but use the keyword [:newkw:] to declare a constructor.

Constructors can also delegate to other constructors using ```[:thiskw!](args...)``` in their first line.

If the super class does not define a no-argument constructor, you have to explicitly call
one using ```[:superkw!](args...)``` as the first expression in the body of the constructor.

[:Success:]
	package io.sarl.docs.reference.oop
	class AnotherClass {
	  new (s : String) { }
	}
	[:On]
	class MyClass extends AnotherClass {
	  [:newkw](new) (s : String) {
	    [:superkw](super)(s)
	  }
	
	  new() {
	    [:thiskw](this)("default")
	  }
	}
[:End:]


If no constructor is defined in the type and a super-type is declared, implicit constructors will be assumed.
Implicit constructors has the same prototypes as the constructors of the super type.
Details on implicit constructors are given in the reference documentation related to the
[synthetic functions](./general/SyntheticFunctions.md).

### Static Constructor Definition

A static constructor is used to initialize any static data, or to perform a particular action that needs to
be performed once only. It is called automatically before the first instance is created or any static members are referenced.
In the Java programming language, the static constructor is known as *static initialization block*.

In the following example, the static field [:astaticfield:] is defined with the `final` modifier.
It means the field must be intialized into the field declaration or into a static constructor.
The static constructor is defined with the [:newkw:] prefixed by the [:staticmodifier:].
The code block of the static constructor contains an assignment that is initializing the [:astaticfield:] field.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class MyClass {
	  static val [:astaticfield](staticField) : int
	  
	  static [:newkw!] {
	    staticField = 2
	  }
	}
[:End:]


Static constructors have the following properties:

* A static constructor does not take access modifiers or have parameters.
* A static constructor is called automatically to initialize the class before the first instance is created.
* A static constructor cannot be called directly.
* The user has no control on when the static constructor is executed in the program.
* A static constructor cannot throw an exception, except an *unchecked exception* (instance of `RuntimeException` or `Error`).



### Field Definition

A field could be declared into a class following the [variable declaration syntax](./general/VarDecls.md).

A field may be declared with the [:staticmodifier:] modifier. In this case, it becomes a static field, or class variable.
A static field is a variable defined in a class of which a single copy exists, regardless of how many instances of the class exist.
A static field is not an instance variable. It is a special type of class attribute (or class property, field, or data member).


### Method Definition

A method could be declared into a class following the [function declaration syntax](./general/FuncDecls.md).
The overriding of an inhertited method is explained in section ["Method Overriding"](#method-overriding).

A method may be declared with the [:staticmodifier:] modifier. In this case, it becomes a static method, or class method.
A static method is a function that is not associated to a specific instance of the class.



### Modifiers

Modifiers are used to modify declarations of types and type members. This section introduces the modifiers for the class.
The modifiers are usually written before the keyword for defining the class.

The complete description of the modifiers' semantic is available in [this section](#definition-of-all-the-supported-modifiers).


#### Top Class Modifiers

A top class may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]:  the class is accessible from any other type (default);
	* [:packagemodifier:]: the class is accessible from only the types in the same package.
* [:abstractmodifier:]: the class is abstract and cannot be instanced.
* [:finalmodifier:]: avoid to be derived.
* [:strictfp:]: avoid the methods of the class to use intermediate floating number formats.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	[:publicmodifier](public) class TopClass1 {
	}
	[:packagemodifier](package) class TopClass2 {
	}
	[:abstractmodifier](abstract) class TopClass3 {
	}
	[:finalmodifier](final) class TopClass4 {
	}
	[:strictfpmodifier](strictfp) class TopClass5 {
	}
[:End:]


#### Nested Class Modifiers

A nested class may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the class (default);
	* [:protectedmodifier:]: the class is accessible within the same package, and derived classes;
	* [:packagemodifier:]: the class is accessible only within the same package as its class;
	* [:privatemodifier:]: the class is accessible only within its class.
* [:abstractmodifier:]: the class is abstract and cannot be instanced.
* [:finalmodifier:]: avoid to be derived.
* [:staticmodifier:]: the inner class do not have access to the non-static members of the enclosing type.
* [:strictfpmodifier:]: avoid the methods of the class to use intermediate floating number formats.

> **_Terminology:_** Nested classes are divided into two categories: static and non-static.
> Nested classes that are declared static are called **static nested classes**.
> Non-static nested classes are called **inner classes**.

> **_Note:_** The modifiers may differ from the previously described, depending on the enclosing type, e.g. agent.

> **_Caution:_** Until now, all the nested classes must be declared as static. This restriction may be removed in later versions.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class EnclosingClass {
		public static class NestedClass1 {
		}
		[:protectedmodifier](protected) static class NestedClass2 {
		}
		package static class NestedClass3 {
		}
		[:privatemodifier](private) static class NestedClass4 {
		}
		abstract static class NestedClass5 {
		}
		final static class NestedClass6 {
		}
		strictfp static class NestedClass7 {
		}
	}
[:End:]


#### Field Modifiers

The modifiers for the fields in a class are:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the field;
	* [:protectedmodifier:]: the field is accessible within the same package, and derived classes;
	* [:packagemodifier:]: the field is accessible only within the same package as its class;
	* [:privatemodifier:]: the field is accessible only within its class (default).
* [:staticmodifier:]: the field is a class field, not an instance field.
* [:transientmodifier:]: the field is never serialized.
* [:volatilemodifier:]: the field is modified by different threads. It is never cached thread-locally, and synchronized.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	public abstract class MyClass1 {
	[:On]
		public var a : Object
		protected var b : Object
		package var c : Object
		private var d : Object
		static var e : Object
		[:transientmodifier](transient) var h : Object
		[:volatilemodifier](volatile) var g : Object
	[:Off]
	}
[:End:]


#### Method Modifiers

The modifiers for the methods in a class are:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the method (default);
	* [:protectedmodifier:]:  the method is accessible within the same package, and derived classes;
	* [:packagemodifier:]: the method is accessible only within the same package as its class;
	* [:privatemodifier:]: the method is accessible only within its class.
* [:abstractmodifier:]: the method has no implementation in the class.
* [:dispatchmodifier:]: the method provides an implementation for the dispatch method mechanism.
* [:finalmodifier:]: the method cannot be overridden in derived classes.
* [:nativemodifier:]: the implementation of the method is inside a native library (DLL, SO, DYNLIB).
* [:staticmodifier:]: the method is a class method, not an instance method.
* [:strictfpmodifier:]: avoid the method to use intermediate floating number formats.
* [:synchronizedmodifier:]: the method is synchronized on the class instance.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	public abstract class MyClass1 {
	[:On]
		public def fct1 { }
		protected def fct2 { }
		package def fct3 { }
		private def fct4 { }
		abstract def fct5
		final def fct6 { }
		[:nativemodifier](native) def fct7
		static def fct8 { }
		strictfp def fct9 { }
		[:synchronizedmodifier](synchronized) def fct10 { }
		// Dispatch functions
		[:dispatchmodifier](dispatch) def fct11(p : Integer) { }
		dispatch def fct11(p : Float) { }
	[:Off]
	}
[:End:]


#### Constructor Modifiers

The modifiers for the constructors of a class are:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the constructor (default);
	* [:protectedmodifier:]: the constructor is accessible within the same package, and derived classes;
	* [:packagemodifier:]: the constructor is accessible only within the same package as its class;
	* [:privatemodifier:]: the constructor is accessible only within its class.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	public abstract class MyClass1 {
	[:On]
		public new(p : int) { }
		protected new(p : float) { }
		package new(p : char) { }
		private new(p : boolean) { }
	[:Off]
	}
[:End:]


### Method Overriding

Method overriding is a language feature that allows a subclass or child class
to provide a specific implementation of a method that is already provided by
one of its super classes or parent classes.

The implementation in the subclass overrides (replaces) the implementation in
the superclass by providing a method that has same name, same parameters or
signature, and same return type as the method in the parent class.

The version of a method that is executed will be determined by the object that is
used to invoke it. If an object of a parent class is used to invoke the method,
then the version in the parent class will be executed, but if an object of the
subclass is used to invoke the method, then the version in the child class
will be executed.

The following code defines the class [:personextype:] as a subclass of [:persontype:],
and in which the title (mister, madam, miss) is added.
Then the full name of the person becomes the sequence of the title, first name and last name.
Since the first name and last name are already sequenced in the function
[:getfullnamefct:] of the superclass, we should override this function for changing
its behavior. The [:overridemodifier:] keyword is specified for clearly marking this
implementation of [:getfullnamefct:] as an override of the parent's implementation.

> **_Note:_** The return type of the [:getfullnamefct:] method (called with the name [:fullnamefct:],
> according to the [property access syntax](./general/MemberAccess.md#property-syntax))
> is not specified in the overriding prototype since it could be inferred by the SARL compiler.

For preventing a function to be overridden, you should add the [:finalmodifier:] modifier in the signature of
the method (as in Java).

[:Success:]
	package io.sarl.docs.reference.oop
	class [:persontype](Person) {
		var firstName : String
		var lastName : String
	
		def [:getfullnamefct](getFullName) : String {
			this.firstName + " " + this.lastName
		}
	}
	[:On]
	class [:personextype](PersonEx) extends Person {
		
		var title : String
		
		[:overridemodifier](override) getFullName {
			return title + " " + super.[:fullnamefct](fullName)
		} 
	}
[:End:]


### Accessing Local Variables of the Enclosing Scope

Local classes (or nested classes, i.e. classes defined inside an other type) have
the [:staticmodifier:] modifier. It means that a nested class cannot have access to the
fields and methods of the instance of the enclosing type. Only accesses to static members are allowed.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class EnclosingClass {
		static var field1 : int
		static def fct1 {
		}

		static class NestedClass {
			def fct2 : int {
				// Explicit type specification for the field access
				EnclosingClass::field1
			}
	
			def fct3 {
				// Implicit type specification for the function's call
				fct1
			}
		}
	}
[:End:]


## Interface

An interface is a description of the actions that an object can do.
For example when you flip a light switch, the light goes on, you don't
care how, just that it does. In object-oriented programming, an
interface is a description of all functions that an object must have
in order to be an "X".

The purpose of interfaces is to allow the program to enforce these
properties, and to know that an object of type T (whatever the interface
is) must have functions called X,Y,Z, etc.


### Define an Interface

In the following example, the [:lighttype:] interface is defined with the two methods [:turnonfct:] and [:turnofffct:].

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	interface [:lighttype](Light) {
		def [:turnonfct](turnOn)
		def [:turnofffct](turnOff)
	}
[:End:]


### Interface Inheritance

It is possible to specialize the definition of an interface. In the following example, the [:varlight:]
interface that is refining the previous [:lighttype:] interface and add specific functions.

[:Success:]
	package io.sarl.docs.reference.oop
	interface Light {
		def turnOn
		def turnOff
	}
	[:On]
	interface [:varlight](VariableIntensityLight) extends Light {
		def setLightIntensity(intensity : float)
		def getLightIntensity : float
	}
[:End:]


### Define a Generic Interface

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
		def norm : Vector<? extends Number>
	}
[:End:]


### Interface Implementation

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


### Default Implementation of Interface Functions

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


### Modifiers

Modifiers are used to modify declarations of types and type members. This section introduces the modifiers for the interface.
The modifiers are usually written before the keyword for defining the interface.

The complete description of the modifiers' semantic is available in [this section](#definition-of-all-the-supported-modifiers).


#### Top Interface Modifiers

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
	strictfp interface TopInterface4 {
	}
[:End:]


#### Nested Interface Modifiers

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
		public interface NestedInterface1 {
		}
		protected interface NestedInterface2 {
		}
		package interface NestedInterface3 {
		}
		private interface NestedInterface4 {
		}
		abstract interface NestedInterface5 {
		}
		static interface NestedInterface6 {
		}
		strictfp interface NestedInterface7 {
		}
	}
[:End:]


#### Field Modifiers

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


#### Method Modifiers

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


## Enumeration

An enumeration specifies a list of constant values assigned to a type.

The SARL enumeration is not object-oriented unlike the enumeration in the Java programming language. It means
that you cannot define methods nor attributes in the enumeration.


### Define an Enumeration

For defining an enumeration, you could use the [:enumkw:] keyword.
The following example defines the enumeration [:myenumtype:] with two constants: 

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	[:enumkw](enum) [:myenumtype](MyEnum) {
		CONSTANT_1,
		CONSTANT_2
	}
[:End:]


### Modifiers

Modifiers are used to modify declarations of types and type members. This section introduces the modifiers for the enumeration.
The modifiers are usually written before the keyword for defining the enumeration.

The complete description of the modifiers' semantic is available in [this section](#definition-of-all-the-supported-modifiers).


#### Top Enumeration Modifiers

A top enumeration may be declared with one or more modifiers, which affect its runtime behavior:
* Access modifiers:
	* [:publicmodifier:]:  the class is accessible from any other type (default);
	* [:packagemodifier:]: the class is accessible from only the types in the same package.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	public enum TopEnumeration1 {
		CST1, CST2
	}
	package enum TopEnumeration2 {
		CST3, CST4
	}
[:End:]


#### Nested Enumeration Modifiers

A nested interface may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the enumeration (default);
	* [:protectedmodifier:]: the enumeration is accessible within the same package, and derived classes;
	* [:packagemodifier:]: the enumeration is accessible only within the same package as its class;
	* [:privatemodifier:]: the enumeration is accessible only within its class.
* [:staticmodifier:]: the inner enumeration do not have access to the non-static members of the enclosing type.

> **_Terminology:_**  Nested enumerations are divided into two categories: static and non-static.
> Nested enumerations that are declared static are called **static nested enumerations**.
> Non-static nested enumerations are called **inner enumerations**.

> **_Note:_** The modifiers may differ from the previously described, depending on the enclosing type, e.g. agent.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class EnclosingClass {
		public enum NestedClass1 {
			CST1, CST2
		}
		protected enum NestedClass2 {
			CST3, CST4
		}
		package enum NestedClass3 {
			CST5, CST6
		}
		private enum NestedClass4 {
			CST7, CST8
		}
		static enum NestedClass5 {
			CST9, CST10
		}
	}
[:End:]


## Annotation Type

An annotation is a form of syntactic metadata that can be added to SARL source code.
Annotations can be reflective in that they can be embedded in binary files generated by the
SARL compiler, and may be retained by the Virtual Machine to be made retrievable at run-time.


### Define an Annotation

For defining an annotation, you could use the [:annotationkw:] keyword.

The following example defines the annotation [:annotationtype:].
This annotation defines three parameters:

* [:valuefield:], an array of strings of characters, without default value;
* [:istrickyfield:], a boolean value, with the default [:falsekw:];
* [:lotterynumberfield:], an array of integer numbers, with a default value.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	[:annotationkw](annotation) [:annotationtype](MyAnnotation) {
	  val [:valuefield](value) : String[]
	  val [:istrickyfield](isTricky) : boolean = [:falsekw](false)
	  val [:lotterynumberfield](lotteryNumbers) : int[] = #[ 42, 137 ]
	}
[:End:]


### Modifiers

Modifiers are used to modify declarations of types and type members.
This section introduces the modifiers for the annotation types.
The modifiers are usually written before the keyword for defining the annotation type.

The complete description of the modifiers' semantic is available in [this section](#definition-of-all-the-supported-modifiers).

#### Top Annotation Type Modifiers

A top annotation type may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]: the annotation type is accessible from any other type (default);
	* [:packagemodifier:]: the annotation type is accessible from only the types in the same package.
* [:abstractmodifier:]: the annotation type is abstract and cannot be instanced.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	public annotation TopAnnotationType1 {
	}
	package annotation TopAnnotationType2 {
	}
	abstract annotation TopAnnotationType3 {
	}
[:End:]


#### Nested Annotation Type Modifiers

A nested annotation type may be declared with one or more modifiers, which affect its runtime behavior:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the annotation type (default);
	* [:protectedmodifier:]: the annotation type is accessible within the same package, and derived classes;
	* [:packagemodifier:]: the annotation type is accessible only within the same package as its class;
	* [:privatemodifier:]: the annotation type is accessible only within its class.
* [:abstractmodifier:]: the annotation type is abstract and cannot be instanced.
* [:staticmodifier:]: the inner annotation type do not have access to the non-static members of the enclosing type.

> **_Terminology:_** Nested annotation types are divided into two categories: static and non-static.
> Nested annotation types that are declared static are called **static nested annotation types**.
> Non-static nested annotation types are called**inner annotation types**.

> **_Note:_** The modifiers may differ from the previously described, depending on the enclosing type, e.g. agent.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class EnclosingClass {
		public annotation NestedAnnotationType1 {
		}
		protected annotation NestedAnnotationType2 {
		}
		package annotation NestedAnnotationType3 {
		}
		private annotation NestedAnnotationType4 {
		}
		abstract annotation NestedAnnotationType5 {
		}
		static annotation NestedAnnotationType6 {
		}
	}
[:End:]


#### Value Modifiers

The modifiers for the values in an annotation type are:

* Access modifiers:
	* [:publicmodifier:]: there are no restrictions on accessing the value;
* [:staticmodifier:]: the value is a class value, not an instance value.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	annotation MyAnnotationType1 {
	[:On]
		public val val1 : int
		static val val2 : int
	[:Off]
	}
[:End:]


## Anonymous Class

Anonymous classes enable you to make your code more concise. They enable you to declare and instantiate a
class at the same time. They are like local classes except that they do not have a name. Use them if you
need to use a local class only once.


### Declaring Anonymous Classes

While local classes are class declarations, anonymous classes are expressions, which means that you define
the class in another expression.

The following example, [:anontype:], uses anonymous classes in the
initialization statements of the local variables [:frenchfield:] and [:spanishfield:]:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	interface HelloWorld {
	    def greetSomeone(someone : String)
	}

	class [:anontype](HelloWorldAnonymousClasses) {
	    def sayHello {
	        var [:frenchfield](frenchGreeting) = new HelloWorld {
	            var name = "tout le monde"
	            def greetSomeone(someone : String) {
	                name = someone
	                println("Salut " + name)
	            }
	        }
	
	        var [:spanishfield](spanishGreeting) = new HelloWorld {
	            var name = "mundo"
	            def greetSomeone(someone : String) {
	                name = someone
	                println("Hola, " + name)
	            }
	        }

	        frenchGreeting.greetSomeone("Marc")
	        spanishGreeting.greetSomeone("Marco")
	    }
	}
[:End:]


### Syntax of Anonymous Classes

As mentioned previously, an anonymous class is an expression.
The syntax of an anonymous class expression is like the invocation of a constructor,
except that there is a class definition contained in a block of code.

Consider the instantiation of the frenchGreeting object ion the code below.

The anonymous class expression consists of the following:

* The [:newkw:] operator
* The name of an interface to implement or a class to extend. In this example, the anonymous class is implementing the interface [:anontype2:].
* Parentheses that contain the arguments to a constructor, just like a normal class instance creation expression. When you implement an interface, there is no constructor, so you do not need to put parameters, as in this example.
* A body, which is a class declaration body. More specifically, in the body, method declarations are allowed but statements are not.

Because an anonymous class definition is an expression, it must be part of a statement.
In this example, the anonymous class expression is part of the statement that instantiates
the [:frenchfield2:] object.

[:Success:]
	package io.sarl.docs.reference.oop
    interface [:anontype2](HelloWorld) {
        def greetSomeone(someone : String)
    }
	class HelloWorldAnonymousClasses {
	    def sayHello {
	[:On]
			var [:frenchfield2](frenchGreeting) = new HelloWorld {
			    var name = "tout le monde"
			    def greetSomeone(someone : String) {
			        name = someone
			        println("Salut " + name)
			    }
			}
	[:Off]
		}
	}
[:End:]


### Accessing Local Variables of the Enclosing Scope, and Declaring and Accessing Members of the Anonymous Class

Anonymous classes can capture variables; they have the same access to
local variables of the enclosing scope:

* An anonymous class has access to the members of its enclosing class.
* An anonymous class cannot access local variables in its enclosing scope that are not declared as final or effectively final.
* A declaration of a type (such as a variable) in an anonymous class shadows any other declarations in the enclosing scope that have the same name.

Anonymous classes have restrictions with respect to their members:

* You cannot declare static initializers or member interfaces in an anonymous class.
* An anonymous class can have static members provided that they are constant variables.

> **_Note:_** You can declare the following in anonymous classes: fields, extra methods
> (even if they do not implement any methods of the supertype), instance initializers, 
> local classes. However, you cannot declare constructors in an anonymous class.

[:Success:]
	package io.sarl.docs.reference.oop
    interface HelloWorld {
        def greetSomeone(someone : String)
    }
	[:On]
	class HelloWorldAnonymousClasses {
		var name : String
		def sayHello {
			var frenchGreeting = new HelloWorld {
				def greetSomeone(someone : String) {
					name = someone
					println("Salut " + name)
				}
			}
		}
	}
[:End:]



## Definition of all the supported modifiers

In this section, the semantic of the different modifiers is explained.


### abstract Modifier

An abstract type is a type that is declared `abstract` (it may or may not include abstract methods).
Abstract types cannot be instantiated, but they can be derived.

An abstract method is a method that is declared without an implementation 
(without braces, and followed by a semicolon), like this:

[:Success:]
	package io.sarl.docs.reference.oop
	abstract class MyType {
	[:On]
		abstract def moveTo(deltaX : double, deltaY : double)
	[:Off]
	}
[:End:]


If a type includes abstract methods, then the type itself must be declared abstract, as in:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	abstract class GraphicObject {
		// declare fields
		// declare nonabstract methods
		abstract def draw
	}
[:End:]


When an abstract type is derived, the subtype usually provides implementations for all of the
abstract methods in its parent type. However, if it does not, then the subtype must also be
declared abstract.


### Access Modifiers: public, protected, package, private


Access level modifiers determine whether other types can use a particular field or invoke
a particular method. There are two levels of access control:

* At the top level in a SARL script.
* At the member level - inside another type.

A type may be declared with the modifier [:publicmodifier:], in which case that type is visible to
all types everywhere. If a type has the modifier [:packagemodifier:], it is visible only within its own package
(packages are named groups of related types).

At the member level, you can also use the [:publicmodifier:] modifier or [:packagemodifier:] modifier just as
with top-level types, and with the same meaning.
For members, there are two additional access modifiers: [:privatemodifier:] and [:protectedmodifier:].
The [:privatemodifier:] modifier specifies that the member can only be accessed in its own type.
The [:protectedmodifier:] modifier specifies that the member can only be accessed within its own package
as with `package`) and, in addition, by a derived type of its type in another package.

The following table shows the access to members permitted by each modifier.


| Modifier              | From the type | From the package | From subtypes | Other |
| --------------------- | ------------- | ---------------- | ------------- | ----- |
| [:publicmodifier:]    | yes           | yes              | yes           | yes   |
| [:protectedmodifier:] | yes           | yes              | yes           | no    |
| [:packagemodifier:]   | yes           | yes              | no            | no    |
| [:privatemodifier:]   | yes           | no               | no            | no    |



The first column indicates whether the type itself has access to the member defined by the
access level. As you can see, a type always has access to its own members.
The second column indicates whether types in the same package as the type (regardless of their parentage)
have access to the member.
The third column indicates whether subtypes of the type declared outside this package have access to the member.
The fourth column indicates whether all types have access to the member.

Examples:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class C1 {
	   [:publicmodifier](public) def pubfct {}
	   [:protectedmodifier](protected) def protfct {}
	   [:packagemodifier](package) def packfct {}
	   [:privatemodifier](private) def privfct {}
	
	   def test0 {
	   	pubfct;
	   	protfct;
	   	packfct;
	    privfct;
	   }
	}
	class C2 extends C1 {
	   def test1 {
	   	pubfct;
	   	protfct;
	   	packfct;
	   }
	}
	class C3 {
	   def test3(obj : C1) {
	   	obj.pubfct;
	   	obj.protfct;
	   	obj.packfct;
	   }
	}
[:End:]


### dispatch Modifier

Generally, method resolution and binding is done statically at compile time.
Method calls are bound based on the static types of arguments.

Sometimes this is not what you want. Especially in the context of extension methods
you would like to have polymorphic behavior.

The [:dispatchmodifier:] modifier permits defining a [dispatch method](./general/FuncDecls.md#dispatch-function).
For a set of visible dispatch methods in the current type hierarchy with the same name and
the same number of arguments, the compiler infers a synthetic dispatcher method.
This dispatcher uses the common super type of all declared arguments.

[:Success:]
	package io.sarl.docs.reference.oop
	class MyClass {
		def println(o : Object) {
		}
	[:On]
		[:dispatchmodifier](dispatch) def getType(x : Number) { 
		  "it's a number" 
		}
		 
		[:dispatchmodifier!] def getType(x : Integer) { 
		  "it's an int" 
		}

		[:dispatchmodifier!] def getType(x : String) { 
		  "it's a string" 
		}

		def clientCode {
			getType(4.5).println
			getType(4).println
			getType("a string").println
		}
	[:Off]
	}
[:End:]


### extension Modifier

This modifier enables to mark a field, a formal parameter, or a local variable as
an [extension provider](./general/Extension.md).

Extension methods allow adding new methods to existing  types without modifying them. This is really 
helpful as they can greatly improve the readability. They use a simple syntactic trick: the first parameter of a method
can either be passed in after opening the parentheses or before the method call. For example, given a method:

[:Success:]
	package io.sarl.docs.reference.oop
	class MyClass {
	[:On]
		def removeVowels(s : String) { 
			s.replaceAll("[aeiouAEIOU]", "") 
		}
	[:Off]
	}
[:End:]


We can call this method either like in Java:

[:Success:]
	package io.sarl.docs.reference.oop
	class MyClass {
		def removeVowels(s : String) { }
		def tmp {
	[:On]
			removeVowels("Hello") 
	[:Off]
		}
	}
[:End:]


or as an extension method of String:

[:Success:]
	package io.sarl.docs.reference.oop
	class MyClass {
		def removeVowels(s : String) { }
		def tmp {
	[:On]
			"Hello".removeVowels 
	[:Off]
		}
	}
[:End:]


By adding the [:extensionmodifier:] keyword to a field, a local variable or a parameter declaration, its
instance methods become extension methods.

In the following example, three functions are defined for illustrating the three types of
extension providers.

[:Success:]
	package io.sarl.docs.reference.oop
	import java.util.ArrayList
	[:On]
	class Examples {
		//
		// Example of an extension provider on a class field.
		//
		[:extensionmodifier](extension) var list : ArrayList<String> = newArrayList
		
		def extensionFieldExample(value : String) : boolean {
			value.contains // calls this.list.contains(value)
		}
		
		//
		// Example of an extension provider on a method parameter.
		//
		def extensionParameterExample(value : String, extension o : ArrayList<String>) : boolean {
			value.contains // calls o.contains(value)
		}
		
		//
		// Example of an extension provider on a local variable.
		//
		def extensionLocalVariableExample(value : String) : boolean {
			extension var o : ArrayList<String> = newArrayList
			value.contains // calls o.contains(value)
		}
	}
[:End:]


### final Modifier

The [:finalmodifier:] keyword is used in several different contexts to define an entity which may only be
assigned once.

Once a final variable has been assigned, it always contains the same value. If a final variable
holds a reference to an object, then the state of the object may be changed by operations on the
object, but the variable will always refer to the same object.

> **_Caution:_** The SARL compiler complains if you write the [:finalmodifier:] modifer in conjonction with the [:varmodifier:]
> or [:valmodifier:] modifiers. Indeed, the [:valmodifier:] modifier defines a final variable; and the
> [:varmodifier:] modifier defines a no-final variable.

This applies also to arrays, because arrays are objects; if a final variable holds a reference to
an array, then the components of the array may be changed by operations on the array, but the variable
will always refer to the same array.

A final method cannot be overridden or hidden by subclasses. This is used to prevent unexpected
behavior from a subtype altering a method that may be crucial to the function or consistency of
the type.

A final type cannot be derived. Doing this can confer security and efficiency benefits, so many
of the Java standard library classes are final, such as `java.lang.System` and
`java.lang.String`. All methods in a final type are implicitly final.

[:Success:]
	package io.sarl.docs.reference.oop
	class C {
		[:varmodifier](var) xxx = 1
	}
	[:On]
	class A {
		//
		// Final field
		//
		[:valmodifier](val) field = 4
		
		//
		// Final method
		//
		[:finalmodifier](final) def cannotBeOverriden {
		}
	}
	final class B {
		// This class cannot be derived
	}
[:End:]


### native Modifier

The [:nativemodifier:] keyword is applied to a method to indicate that the method is implemented in
native code, i.e. outside SARL and Java, using the Java Native Interface.

> **_Note:_** This modifier is provided for enabling agents to access to low-level resources that
> are not supported by the Java API. You should not use this modifier if you can use
> a higher-level API in place of.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class A {
		[:nativemodifier](native) def fct
	}
[:End:]


### static Modifier

The [:staticmodifier:] keyword is used for creating fields and methods that belong to the type, rather than to an
instance of the type.

Sometimes, you want to have variables that are common to all instances. This is accomplished with
the [:staticmodifier:] modifier. Fields that have the static modifier in their declaration are called static
fields (or class variables in object-oriented programming). They are associated with the type,
rather than with any instance. Every instance of the type shares a static field, which is in one
fixed location in memory. Any instance can change the value of a static variable, but static variables
can also be manipulated without creating an instance of the type.

> **_Note:_** Constants are usually defined as final static fields.

Static methods, which have the [:staticmodifier:] modifier in their declarations, should be invoked with the
type name, without the need for creating an instance of the type. 

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class A {
		//
		// Static field
		//
		[:staticmodifier](static) var field : int
		//
		// Constant
		//
		static val constant = 4
		//
		// Static method
		//
		static def aMethod {
		}
	}
[:End:]


### strictfp Modifier

The [:strictfp:] modifier is a keyword that restricts floating-point calculations to ensure portability.
The [:strictfp:] command was originally introduced into Java with the Java virtual machine (JVM) version 1.2
and is available for use on all currently updated Java VMs.

The IEEE standard IEEE 754 specifies a standard method for both floating-point calculations and
storage of floating-point values in either single (32-bit, used in float) or double (64-bit, used in double)
precision, and, for intermediate calculations, also extended precision formats.

Since JVM 1.2, intermediate computations are not limited to the standard 32 bit and 64 bit precisions.
On platforms that can handle other representations e.g. 80-bit double extended on x86 or x86-64 platforms,
those representations can be used, helping to prevent round-off errors and overflows, thereby increasing
precision.

For some applications, a programmer might need every platform to have precisely the same floating-point
behavior, even on platforms that could handle greater precision. However, if this level of precision is not
necessary the VM does not use intermediates by default.

From the VM perspective, turning on this higher precision means the following:


| Precision | Intermediate           |
| --------- | ---------------------- |
| 32 bits   | 64 bits                |
| 64 bits   | 80 bits (if available) |


The [:strictfp:] modifier accomplishes this by truncating all intermediate values to IEEE single precision and
double precision, as occurred in earlier versions of the JVM.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class A {
		[:strictfp](strictfp) def fct { }
	}
[:End:]


### synchronized Modifier

The SARL programming language provides two basic synchronization idioms: synchronized methods and synchronized
statements.
The synchronized statements are described in the
[general reference](./general/Synchronization.md).
This section is about synchronized methods.

To make a method synchronized, simply add the [:synchronizedmodifier:] modifier to its declaration (see
example below).

Synchronized methods enable a simple strategy for preventing thread interference and memory
consistency errors: if an object is visible to more than one thread, all reads or writes to that
object's variables are done through synchronized methods.
(An important exception: final fields, which cannot be modified after the object is constructed, 
can be safely read through non-synchronized methods, once the object is constructed)
This strategy is effective, but can present problems with liveness.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class SynchronizedCounter {
		var c = 0
	
		[:synchronizedmodifier](synchronized) def increment {
			c++
		}
	
		synchronized def decrement {
			c--
		}
	
		synchronized def value {
			return c
		}
	}
[:End:]


### transient Modifier

Java and SARL provide a mechanism, called object serialization where an object can be represented as
a sequence of bytes that includes the object's data as well as information about the object's type and
the types of data stored in the object.

The [:transientmodifier:] modifier is a keyword used as a field modifier.
When a field is declared transient, it would not be serialized even if the type to which it belongs
is serialized.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	class A {
		[:transientmodifier](transient) var field : int = 4
	}
[:End:]


### volatile Modifier

While the [:volatilemodifier:] modifier itself comes from the C programming language, it has a completely
different meaning in Java, and then in SARL. This may not help in growing an understanding of it, 
googling for volatile could lead to different results. Let's take a quick side step and see what
volatile means in C first.

In the C language the compiler ordinarily assumes that variables cannot change value by themselves.
While this makes sense as default behavior, sometimes a variable may represent a location that
can be changed (like a hardware register). Using a volatile variable instructs the compiler
not to apply these optimizations.

Back to Java and SARL. The meaning of volatile in C would be useless in Java. The JVM uses
native libraries to interact with the OS and hardware. Further more, it is simply impossible
to point Java variables to specific addresses, so variables actually won't change value by themselves.

However, the value of variables on the JVM can be changed by different threads. By default the
compiler assumes that variables won't change in other threads. Hence it can apply optimizations such
as reordering memory operations and caching the variable in a CPU register. Using a volatile variable
instructs the compiler not to apply these optimizations. This guarantees that a reading thread
always reads the variable from memory (or from a shared cache), never from a local cache.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
		class A {
			[:volatilemodifier](volatile) var field : int = 4
		}
[:End:]


## References

This documentation is based on documentations from the Xtext and Xtend projects, and from the Java tutorials.
Thank you to the contributors to these documents.

* [Xtend](https://www.eclipse.org/xtend/documentation.html)
* [Xtext](https://www.eclipse.org/Xtext/documentation.html)
* [Java Tutorials](https://docs.oracle.com/javase/tutorial/)



[:Include:](../legal.inc)
