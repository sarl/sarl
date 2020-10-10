# Switch Expression

[:Outline:]

A switch statement is a type of selection control mechanism used to allow the value of a variable or expression
to change the control flow of program execution via a multiway branch.

Switch statements come in two main variants: a structured switch, as in Pascal, which takes exactly one branch, 
and an unstructured switch, as in C, which functions as a type of goto.
** The SARL language uses structured switchs. **

> **_Important Note:_** The switch expression is very different from Java's switch statement. The use of switch is
> not limited to certain values, but can be used for any object reference.
> The operator `==` or its Java-equivalent `Object.equals(Object)` are
> used to compare the value in the case with the one you are switching over.

## Switch Cases

Consider the following example.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var [:mainexpr](myString) = "abc"
		def example1 : String {
			[:On]
			[:switchkw](switch) [:mainexpr!] {
			[:case](case) 'some' : [:returnexpr]("It's some string.")
			[:case!] 'other' : "another string."
			}
			[:Off]
		}
	}
[:End:]


The main expression [:mainexpr:] is evaluated first and then compared to each 
[:case:] sequentially. If the case expression is of type boolean, the case matches 
if the expression evaluates to `true`. If it is not of type boolean it is 
compared to the value of the main expression using the operator `==`.

If a case is a match, the case expression after the colon is evaluated and is 
the result of the whole switch expression, e.g. [:returnexpr:].
Note that there is no need for a `break` keyword; as in Java the case following
the matching case is never evaluated, due to the structured nature of the switch
statement in SARL.

The main expression, i.e. the parameter of [:switchkw:] can also be a computed value instead 
of a field or variable.

> **_Important Note:_** A case must contains an expression. If you want to do nothing
> for a given case, put an empty block after the colon character.

## Default Case

If none of the cases is matching, the [:default:] case matches automatically if it is specified
in the code.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var myString = "abc"
		def example1 : String {
			[:On]
			[:switchkw!] [:mainexpr!] {
			[:case!] 'some' : [:returnexpr!]
			[:case!] 'other' : "another string."
			[:default](default): "default string"
			}
			[:Off]
		}
	}
[:End:]


## Case Guards

Consider the following example.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var myString = "abc"
		def example1 : String {
			[:On]
			[:switchkw!] [:mainexpr!] {
			[:case!] 'some' : [:returnexpr]("It's some string.")
			[:case!] 'other' : "another string."
			[:case!] [:caseguard]{myString.length > 5} : "It's a long string."
			}
			[:Off]
		}
	}
[:End:]


The expression [:caseguard:] is the guard of the case.
The associated case matches only if the guard expression is evaluated to `true`.
In the example above, the third case matches only if the value of [:mainexpr:] has length
greater than 5.


## Type Guards

In addition to the case guard described in the previous section, you can specify a type
guard.

The case only matches if the switch value conforms to a given type.
A case with both a type guard and a predicate only matches if both conditions match.
If the switch value is a field, parameter or variable, it is automatically casted
to the given type within the predicate and the case body.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var myString = "abc"
		def example1 : String {
			[:On]
			[:switchkw!] [:mainexpr!] {
			String [:case!] [:mainexpr!].length == 5 : "It's string of length 5."
			String : "a string."
			}
			[:Off]
		}
	}
[:End:]


## Fall Through

You can have multiple type guards and cases separated with a comma, to
have all of them share the same then part.

In the following example, the value [:value1:] is replied if one of the two first cases matches.
Otherwise, the value associated to the default case is used.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var myString = "abc"
		def example1 : String {
			[:On]
			[:switchkw!] [:mainexpr!] {
			case [:mainexpr!].length==5,
			case 'some' : "[:value1](a string)"
			default: "Default"
			}
			[:Off]
		}
	}
[:End:]


[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
