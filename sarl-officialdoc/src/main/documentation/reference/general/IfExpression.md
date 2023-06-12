# If-Then-Else Expressions

[:Outline:]

An if-expression is used to choose between two different values based on a predicate.


## Standard If-Then-Else Syntax

The following results in either the value [:e1](e1) or [:e2](e2) depending on whether the predicate [:p](e1 !== null) evaluates to
`true` or `false`.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var [:e1!] : Object
		var [:e2!] : Object
		def example1 : Object {
			[:On]
			if ([:p!]) 
				[:e1!]
			else
				[:e2!]
			[:Off]
		}
	}
[:End:]


## Optional Else Part

The else part is optional, which is a shorthand for an else branch that returns the
default value of the current type.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var [:e1!] : Object
		var [:e2!] : Object
		def example1 : Object {
			[:On]
			if ([:p!]) [:e1!]
			[:Off]
		}
	}
[:End:]


## Conditional Operator

Sometimes, it is useful to put a if-then-else expression inside another expression.
This syntax is known as the [conditional operator, inline if, or ternary if](https://en.wikipedia.org/wiki/%3F:)
in many programming languages.
In programming languages such as Java or C/C++, this conditional operator has the syntax:

```java
a ? b : c
```

It evaluates to `b` if the value of `a` is true, otherwise to `c`.

In SARL, this specific syntax is not supported. The standard if-then-else expression (explained above) is
an expression. It means that it could be included into another expression like all the other expressions.
Consequently, there is no need of a specific syntax for the conditional operator in SARL.
The following example is the SARL equivelant of the Java conditional operator:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var a : boolean
		var b : Object
		var c : Object
		def example1 : Object {
			[:On]
			if (a) b else c 
			[:Off]
		}
	}
[:End:]


You can use `if` expressions deeply nested within expressions:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var e1 : Object
		var e2 : Object
		def example1 {
			[:On]
			val name = if ([:p!]) [:e1!] + ' ' + [:e2!] else [:e2!]
			[:Off]
		}
	}
[:End:]


[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
