# Object Member Invocation

[:Outline:]

This section describes the syntax for using or calling the members of an object.

A simple name can refer to a field, variable, or parameter. In addition, it can point to
a method with zero argument since empty parentheses are optional.

The rest of this section describes particular mechanisms for calling the object members.


## Property Syntax

The SARL language provides a very powerfull mecanism for calling members of an object as
properties of this object.

If there is no field with the given name and also no method with
the name and zero parameters accessible, a simple name binds to a
corresponding Java-Bean getter method if available.
The getter method must have a name starting with one of the strings of
characters `get`, `is`, `has`, followed by the given name.

### Definition of the Properties and the Accessors

In the following example, two fields are defined: [:prop1:] and [:prop2:].
As usual, these properties have a private scope, and the getter and setter functions must
be defined to enable public scope access.
In the example, only the getter and setter functions for [:prop2:] are defined.

[:Success:]
	package io.sarl.docs.reference.gsr
	[:On]
	agent A {
		var [:prop1](prop1) : Object
		var [:prop2](prop2) : Object

		def getProperty2 : Object {
			return this.prop2
		}

		def setProperty2(o : Object) {
			this.prop2 = o
		}
	}
[:End:]


### Read Accesses

Four cases for accessing a property are possible:


| Getter Function Def.    | Access Type     | Example             | Effect                            | 
| ----------------------- | --------------- | ------------------- | --------------------------------- |
| A getter is defined     | Functional      | `this.getProperty2` | Invocation of the getter function |
| A getter is defined     | Property-access | `this.property2`    | Invocation of the getter function |
| A getter is defined     | Field-access    | `this.prop2`        | Read the field if it is visible   |
| A getter is not defined | Field-access    | `this.prop2`        | Read the field if it is visible   |


The two first cases have the same effect: the getter function is called.
But, the second case uses the "property-access" syntax, and it is assumed to be easier to be read in the code.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var prop1 : Object
		var prop2 : Object

		def getProperty2 : Object {
			return this.prop2
		}

		def f1 : Object {
			this.getProperty2
		}
		def f2 : Object {
			this.property2
		}
		def f3 : Object {
			this.prop2
		}
		def f4 : Object {
			this.prop1
		}
	}
[:End:]


### Write Accesses

Four cases for writing a property are possible:


| Setter Function Def.    | Access Type     | Example                | Effect                            | 
| ----------------------- | --------------- | ---------------------- | --------------------------------- |
| A setter is defined     | Functional      | `this.setProperty2(x)` | Invocation of the setter function |
| A setter is defined     | Property-access | `this.property2 = x`   | Invocation of the setter function |
| A setter is defined     | Field-access    | `this.prop2 = x`       | Write the field if it is visible  |
| A setter is not defined | Field-access    | `this.prop2 = x`       | Write the field if it is visible  |


The two first cases have the same effect: the setter function is called.
But, the second case uses the "property-access" syntax, and it is assumed to be easier to be read in the code.
Note that for this latest case, the "variable assignment" syntax is used.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var prop1 : Object
		var prop2 : Object

		def setProperty2(o : Object) {
			this.prop2 = o
		}

		def f1(x : Object) {
			this.setProperty2(x)
		}
		def f2(x : Object) {
			this.property2 = x
		}
		def f3(x : Object) {
			this.prop2 = x
		}
		def f4(x : Object) {
			this.prop1 = x
		}
	}
[:End:]


## Static Access to Members

To access a static field or method you can use the recommended Java syntax or the more explicit double colon `::`.
That means, the following expressions are pairwise equivalent:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var a = Integer::TYPE
		var b = Integer.TYPE
		[:Off]
	}
[:End:]


## Null-Safe Feature Call

Checking for null references can make code very unreadable. 
The variable [:myref:] is assumed to be declared.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var [:myref](myRef) = "abc"
		def examples : int {
			[:On]
			if ([:myref!] !== null) [:myref!].length() else 0
			[:Off]
		}
	}
[:End:]


In many situations, it is correct for an expression to return a default value (`null`, `0`...) if a receiver was `null`.
SARL includes the safe navigation operator [:sfop:]. to do the null-check test and make such code better readable.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var myRef = "abc"
		def examples : int {
			[:On]
			[:myref!][:sfop](?.)length()
			[:Off]
		}
	}
[:End:]


## Call to an Inherited Method

When it is possible to extend an existing type, the methods can be overridden.
In this case, the [:super:] keyword invoks the inherited implementation of the method
from the overriding method.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent SuperA {
		def anAction {
		}
	}
	agent A extends SuperA {
		[:On]
		def anAction {
			// Call the inherited implementation
			[:super](super).anAction
		}
		[:Off]
	}
[:End:]


## Call to a Constructor

Constructor calls correspond to the calls of a constructor function for an object.


### Instance Creation
Constructor calls have the same syntax as in Java. The only difference is that empty parentheses are optional.
If type arguments are omitted, they will be inferred from the current context similar to Java's
diamond operator on generic method and constructor call.

[:Success:]
	package io.sarl.docs.reference.gsr
	import java.util.ArrayList
	agent A {
		[:On]
		var a = new Integer(345)
		var b = new ArrayList<Integer>()
		var c = new ArrayList<Integer>
		[:Off]
	}
[:End:]


###	Inherited Constructor

In the implementation of a constructor, it is possible to call one of the inherited constructors.
The syntax is similar to Java: the (:super:] keyword is used to represent the inherited constructor.

> **_Important Note:_** We recommend that you include the parentheses when invoking the default constructor
> of the super type because, in some cases, typing [:super:] alone (without the parenthesis pair)
> has no effect, and that is an error.

[:Success:]
	package io.sarl.docs.reference.gsr
	import io.sarl.lang.core.Address
	event E1
	event E2 extends E1 {
		[:On]
		new {
			super() // Call the inherited default constructor
		}
		new (param : Address) {
			super(param) // Call the inherited constructor with a parameter
		}
		[:Off]
	}
[:End:]



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
