# Loop Expressions

[:Outline:]

SARL provides four types of loop statements.


## For-Each Loop

The for loop is used to execute a certain expression for each element of an array or an instance of `Iterable`.

The for's variable is local and final, hence cannot be updated.

The type of a for loop is `void`. The type of the local variable can be inferred from the
iterable or array that is processed, e.g. in the following example [:v:] is of type [:type:].

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			[:On]
			var tab : [:type](String)[]

			for ([:v](v) : tab) {
				println(v)
			}
			[:Off]
		}
	}
[:End:]


You could specify the expected type for the local variable with the [:askw:] following the local variable:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			var tab : String[]
			[:On]
			for (v [:askw](as) String : tab) {
				println(v)
			}
			[:Off]
		}
	}
[:End:]


## Traditional Java For Loop

The traditional for loop is very similar to the one known from Java, or even C.
When executed, it first executes the init-expression, where local variables can be
declared. Next the predicate is executed and if it evaluates to `true`, the
body-expression is executed. On any subsequent iterations the update-expression
is executed instead of the init-expression. This happens until the predicate
returns `false`. The type of a for loop is `void`.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			[:On]
			for (var i = 0; i<123; i++) {
				println(i)
			}
			[:Off]
		}
	}
[:End:]


## While Loop

A while loop is used to execute a certain expression unless the predicate is evaluated to
`false`. The type of a while loop is `void`.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			[:On]
			var i = 0
			while (i<123) {
				println(i)
				i++
			}
			[:Off]
		}
	}
[:End:]


## Do-While Loop

A while loop is used to execute a certain expression unless the predicate is evaluated 
to `false`. The difference to the while loop is that the execution starts by 
executing the block once before evaluating the predicate for the first time. 
The type of a while loop is `void`.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			[:On]
			var i = 0
			do {
				println(i)
				i++
			}
			while (i<123)
			[:Off]
		}
	}
[:End:]


## Breaking a loop

The [:break:] keyword is provides for breaking the enclosing loop.
When this keyword is run, the control flow exits for the nearest
enclosing loop, and run the statement that is just following the loop
expression in the sequence of instructions.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			var tab : String[]
			[:On]
			for (v : tab) {
				if (v == 1) {
					[:break](break)
				}
			}
			[:Off]
		}
	}
[:End:]


## Jump to the next iteration

The [:continue:] keyword is provides for stopping the execution of the
current iteration into loop, and jumping to the next iteration.
When this keyword is run, the control flow jumps to the next iteration
for the nearest enclosing loop, and run the statement that is just at
the beginning of the loop's block expression.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			var tab : String[]
			[:On]
			for (v : tab) {
				if (v == 1) {
					[:continue](continue)
				}
			}
			[:Off]
		}
	}
[:End:]



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
