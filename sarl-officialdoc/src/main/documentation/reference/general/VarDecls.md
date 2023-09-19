# Variable and Field Declaration

[:Outline:]

Variables and fields can be declared in SARL. To declare a variable or a field, you must specify if it is a value or a
variable (see below for details), its name, and optionally its type and its initial value.

The variable/value declaration follows the syntax:

```text
[:varkw](var) NAME [: TYPE] [= INITIAL VALUE]
[:valkw](val) NAME [: TYPE] [= INITIAL VALUE]
```

Shadowing variables from outer scopes is not allowed, the only exception is the implicit variable `[:it](it)`.


## Variable vs. Value Declaration

A variable declaration starting with the keyword [:valkw:] denotes a value, which is essentially a final, unsettable variable.

[:Failure:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Object {
			val a = 5
			a = 7
		}
	}
[:End:]


The variable needs to be declared with the keyword [:varkw:], which stands for 'variable', if its value can change.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Object {
			var a = 5
			a = 7
		}
	}
[:End:]


Variables declared outside a lambda expression using the [:varkw:] or [:valkw:] keyword are accessible from within the
lambda expressions.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action1 : () => int {
			var a = 5
			[ a + 5 ]
		}
		def action2 : () => int {
			val a = 5
			[ a + 5 ]
		}
	}
[:End:]


Fields declared outside a lambda expression using the [:varkw:] keyword or the [:valkw:] keyword are
accessible from within the lambda expressions.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var a = 5
		def action : () => int {
			[ a + 5 ]
		}
	}
[:End:]


## Typing

The type of the variable itself can either be explicitly declared or it can be inferred from the initializer expression.

In the following example, the type of the variable is explicitly given:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Object {
			[:On]
			var a : [:vartype](String) = "abc"
			[:Off]
			a
		}
	}
[:End:]


In the following example, the type of the variable is inferred to [:vartype:]:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Object {
			[:On]
			var a = "abc"
			[:Off]
			a
		}
	}
[:End:]


## Implicit Variables: this and it

Like in Java the current object is bound to the keyword `this`. This allows for implicit field access or method invocations.

You can use the variable name `it` to get the same behavior for any variable or parameter.
Moreover, the variable `it` is allowed to be shadowed. This is especially useful when used together with lambda
expressions.

When you type a name that doesn't resolve to a variable in the local scope, the compiler tries to find a field
with the same name on the `it` object, then in the `this` object.


[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
