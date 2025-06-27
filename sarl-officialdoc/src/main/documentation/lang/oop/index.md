# Basic Object-Oriented Programming Support with SARL

[:Outline:]

SARL enables to declare objects with an object-oriented programming approach for convenience to develop complex programs, i.e.
the SARL developer must not learn another language such as Java for programming the objects that are used within the
SARL program.
Because of this convenience, the support of object-oriented programming may be less complete than in main stream object programming
languages.
 
This document describes the basic support of object-oriented programming provided by SARL.
Before reading this document, we recommend that you read
the [Expression Syntax Reference](../expr/index.md).

The support of the object-oriented programming (OOP) statements in SARL is less complete
than a real object-oriented language such as Java.
The basics of OOP are enabled in the SARL scripts. If you need more complex or more
complete support of the OOP, you should use a dedicated language, such as Java,
[Xtend](https://eclipse.org/xtend/), or [Scala](http://www.scala-lang.org/).

> **_Note:_** The SARL Eclipse product includes the tools for programming with the Java and Xtend languages.


## Comparison between SARL and Other Languages

A comparison between SARL and other programming languages could be found [here](../Comparison.md).

## Key Features for Object-Oriented Programming

The SARL language is designed with a set of object-oriented features that enable developers to create modular, reusable, and scalable applications. This introduction covers the fundamental object-oriented concepts supported by SARL.

### Classes

Classes are the cornerstone of object-oriented programming in SARL. They serve as blueprints for creating objects and define the structure and behavior that the objects instantiated from the class will have.

For more detailed information, refer to the [Class documentation](./Class.md).

### Anonymous Classes

Anonymous classes in SARL allow you to define and instantiate a class at the same time, without giving it a name. They are useful for creating one-off implementations of interfaces or extensions of classes.

For more detailed information, refer to the [Anonymous Class documentation](./AnonymousClass.md).

### Interfaces

Interfaces in SARL define contracts that classes can implement. They specify what methods a class must have, without dictating how those methods should be implemented.

For more detailed information, refer to the [Interface documentation](./Interface.md).

### Enumerations

Enumerations (enums) in SARL provide a way to define a set of named constants, making code more readable and type-safe.

For more detailed information, refer to the [Enumeration documentation](./Enumeration.md).

### Annotation Types

Annotations in SARL provide metadata about the code that can be used by compilers, development tools, or runtime environments. They do not directly affect the code they annotate but add an extra layer of information.

For more detailed information, refer to the [Annotation Type documentation](./AnnotationType.md).

### Modifiers

Modifiers in SARL are keywords that can be applied to classes, methods, and fields to alter their accessibility and behavior.
For more detailed information, refer to the [Modifiers documentation](./Modifiers.md).


[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
