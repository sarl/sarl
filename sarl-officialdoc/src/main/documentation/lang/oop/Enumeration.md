# Enumeration

[:Outline:]

An enumeration specifies a list of constant values assigned to a type.

The SARL enumeration is not object-oriented unlike the enumeration in the Java programming language. It means
that you cannot define methods nor attributes in the enumeration.


## Define an Enumeration

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


## Modifiers

Modifiers are used to modify declarations of types and type members. This section introduces the modifiers for the enumeration.
The modifiers are usually written before the keyword for defining the enumeration.

The complete description of the modifiers' semantic is available on [this page](./Modifiers.md).


### Top Enumeration Modifiers

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


### Nested Enumeration Modifiers

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
		[:publicmodifier](public) enum NestedClass1 {
			CST1, CST2
		}
		[:protectedmodifier](protected) enum NestedClass2 {
			CST3, CST4
		}
		[:packagemodifier](package) enum NestedClass3 {
			CST5, CST6
		}
		[:privatemodifier](private) enum NestedClass4 {
			CST7, CST8
		}
		[:staticmodifier](static) enum NestedClass5 {
			CST9, CST10
		}
	}
[:End:]

[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
