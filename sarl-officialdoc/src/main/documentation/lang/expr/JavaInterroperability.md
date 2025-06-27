# Java Interoperability

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


[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
