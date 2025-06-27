# Definition of All the Supported Modifiers

[:Outline:]

In this section, the semantic of the different modifiers is explained.


## abstract Modifier

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


## Access Modifiers: public, protected, package, private


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


## dispatch Modifier

Generally, method resolution and binding is done statically at compile time.
Method calls are bound based on the static types of arguments.

Sometimes this is not what you want. Especially in the context of extension methods
you would like to have polymorphic behavior.

The [:dispatchmodifier:] modifier permits defining a [dispatch method](../expr/FuncDecls.md#dispatch-function).
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


## extension Modifier

This modifier enables to mark a field, a formal parameter, or a local variable as
an [extension provider](../expr/Extension.md).

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


## final Modifier

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


## native Modifier

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


## static Modifier

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


## strictfp Modifier

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


## synchronized Modifier

The SARL programming language provides two basic synchronization idioms: synchronized methods and synchronized
statements.
The synchronized statements are described in the
[general reference](../expr/Synchronization.md).
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


## transient Modifier

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


## volatile Modifier

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

[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
