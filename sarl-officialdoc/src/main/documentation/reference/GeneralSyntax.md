# General Syntax Reference

[:Outline:]

This document describes the general syntax of the SARL Language. 


## Java Interoperability

SARL, like Java, is a statically typed language. In fact, it completely supports 
Java's type system, including the primitive types like _int_ or _boolean_, 
arrays and all the Java classes, interfaces, enumerations and annotations that reside 
on the class path.

Java generic types are fully supported as well: you can define type parameters on 
methods and classes and pass type arguments to generic types just as you are 
used to from Java. The type system and its conformance and casting rules are 
implemented as defined in the
[Java Language Specification](https://docs.oracle.com/javase/specs/jls/se8/html/).

One of the problems with Java is that you are forced to write type signatures 
over and over again. That is why so many people do not like static typing. 
But this is in fact not a problem of static typing, but simply a problem with 
Java. Although SARL is statically typed just like Java, you rarely have to 
write types down because they can be computed from the context.

In addition to Java's auto-boxing to convert primitives to their corresponding wrapper 
types (e.g. _int_ is automatically converted to _Integer_ when needed), there are 
additional conversion rules in SARL: arrays are automatically converted to
`List<ComponentType>` and vice versa.

[:Fact:]{typeof(java.util.List)}

Resembling and supporting every aspect of Java's type system ensures that there is 
no impedance mismatch between Java and SARL. __This means that SARL and Java are 
100% interoperable__. There are no exceptional cases and you do not have to 
think in two worlds. You can invoke SARL code from Java and vice versa without any
surprises or hassles.	


## Name Syntax

In SARL, the names of the features (agents, variables, fields, etc.)
cannot be one of the keywords of SARL or Java.
For example, since [:eventkw:] is a keyword in SARL, the following is illegal:

[:Failure:]
	package io.sarl.docs.reference.gsr
	[:On]
	import io.sarl.[:eventkw](event).ActionEvent
[:End:] 

To solve this problem (since some names come from Java, and
this language has different keywords than SARL), it
is possible to prefix the name fragment with the character `^`:

[:Success:]
	package io.sarl.docs.reference.gsr
	[:On]
	import io.sarl.^[:eventkw!].ActionEvent
[:End:] 


## Statement Syntax

In SARL, statements are instructions that will be executed.
A statement may be one of the elements described in the rest of this document.

As compared to programming languages as Java and C++, there is no need to terminate
a statement with the `;` character.
Use of this statement terminator is optional, and used if you want to
put multiple statements on a line. To continue a statement on the next
line, you end the line with the `\` character.

For instance, the two following lines are equivalent:

[:Success:]
	package io.sarl.docs.reference.gsr
	class MyType {
		def myaction {
			[:On]
			var myVariable : int = 5
			[:Off]
		}
	}
[:End:] 


[:Success:]
	package io.sarl.docs.reference.gsr
	class MyType {
		def myaction {
			[:On]
			var myVariable : int = 5;
			[:Off]
		}
	}
[:End:] 


## Details on the SARL language elements

In the following pages, you could find details on the different elements of the SARL syntax:

* Structural elements;
	* [Script format](./general/Script.md)
	* [Function declaration](./general/FuncDecls.md)
	* [Synthetic Functions](./general/SyntheticFunctions.md)
* Type system:
	* [Supported Types for Variables and Parameters](./general/Types.md)
* Constant expressions:
	* [Numerical, string, and collection literals](./general/Literals.md)
* Structuring expressions:
	* [Block expression](./general/Block.md)
	* [If-then-else expression](./general/IfExpression.md)
	* [Switch expression](./general/SwitchExpression.md)
	* [Loops](./general/LoopExpression.md)
	* [Lambda expressions](./general/Lambda.md)
* Standard expressions:
	* [Variable and attribute declarations](./general/VarDecls.md)
	* [Operators](./general/Operators.md)
	* [Type casting](./general/Cast.md)
	* [Access to object members](./general/MemberAccess.md)
	* [Extension methods](./general/Extension.md)
	* [Synchronization expression](./general/Synchronization.md)
* Errors and exceptions:
	* [Exceptions](./general/Exception.md)
	* [Assertions](./general/Assertion.md)
* Meta-programming:
	* [Active annotations](./general/ActiveAnnotations.md)



[:Include:](./generalsyntaxref.inc)

[:Include:](../legal.inc)
