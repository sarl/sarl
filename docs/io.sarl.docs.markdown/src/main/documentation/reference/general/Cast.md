# Typecasting

Typecasting is an explicit type conversion, also known as type coercion, is a type conversion by the compiler.

## Syntax

The typecasting of an expression to a specific type must be done with the [:as:] keyword. This keywords must
be typed after the expression and before the casting type.

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent A {
				var something : Number = new Integer(123)
				var a = [:On]
					[:a](something) [:as](as) [:integer](Integer)[:Off]
				
				//Do the convertion from a number literal to an Integer object
				var b = [:On]
					[:b](56) as [:integer!][:Off]
			}
		[:End:]

In the first example, the [:a:] expression is casted to [:integer:].
In the second example, the [:b:] expression is casted to [:integer:] too.

## Implicit Conversions

Coming soon.



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
