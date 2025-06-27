## Class

[:Outline:]

Objects are structures that contain both data and procedures. Classes are definitions for the data format and
available procedures for a given type or class of objects. They may also contain data and
procedures (known as class methods) themselves.


## Define a Class

For defining a class, you could use the [:classkw:] keyword. The following example defines the class [:classname:].

The members of the class, i.e. the fields, methods and constructors must be written between the braces that are
following the class declaration.

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	[:classkw](class) [:classname](MyClass) {  }
[:End:]


## Class Inheritance

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


## Define a Generic Class

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


## Constructor Definition

An SARL class can define any number of constructors. Unlike Java, you do not have to repeat the name of the
class over and over again, but use the keyword [:newkw:] to declare a constructor.

Constructors can also delegate to other constructors using `[:thiskw!](args...)` in their first line.

If the super class does not define a no-argument constructor, you have to explicitly call
one using `[:superkw!](args...)` as the first expression in the body of the constructor.

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
[synthetic functions](../expr/SyntheticFunctions.md).

## Static Constructor Definition

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
	  
	  [:staticmodifier](static) [:newkw!] {
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



## Field Definition

A field could be declared into a class following the [variable declaration syntax](../expr/VarDecls.md).

A field may be declared with the [:staticmodifier:] modifier. In this case, it becomes a static field, or class variable.
A static field is a variable defined in a class of which a single copy exists, regardless of how many instances of the class exist.
A static field is not an instance variable. It is a special type of class attribute (or class property, field, or data member).


## Method Definition

A method could be declared into a class following the [function declaration syntax](../expr/FuncDecls.md).
The overriding of an inhertited method is explained in section ["Method Overriding"](#method-overriding).

A method may be declared with the [:staticmodifier:] modifier. In this case, it becomes a static method, or class method.
A static method is a function that is not associated to a specific instance of the class.



## Modifiers

Modifiers are used to modify declarations of types and type members. This section introduces the modifiers for the class.
The modifiers are usually written before the keyword for defining the class.

The complete description of the modifiers' semantic is available on [this page](./Modifiers.md).


### Top Class Modifiers

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


### Nested Class Modifiers

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
		[:strictfp](strictfp) static class NestedClass7 {
		}
	}
[:End:]


### Field Modifiers

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


### Method Modifiers

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


### Constructor Modifiers

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


## Method Overriding

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
> according to the [property access syntax](../expr/MemberAccess.md#property-syntax))
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


## Accessing Local Variables of the Enclosing Scope

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

[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
