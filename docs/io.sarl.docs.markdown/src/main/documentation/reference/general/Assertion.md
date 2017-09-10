# Assertion Support

An assertion is a statement that a predicate is expected to always be true at that point in the code.
If an assertion evaluates to false at run time, an assertion failure results, which typically causes
the program to crash, or to throw an assertion exception.

## Assert Statement

SARL supports assertions in the source code by the [:assertkw:] keyword.

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent A {
				def example {
					var someCondition : boolean
					[:On]
					[:assertkw](assert) [:someCondition](someCondition)
					[:Off]
				}
			}
		[:End:]

The [:someCondition:] expression is the Boolean expression that is dynamically evaluated.
For example, in the following code, the two first [:assertkw:] have their conditions evaluated to true, and do not stop the program.
The third [:assertkw:] condition is evaluated to false. It causes a stop of the program.

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent A {
				def example {
					var x : int
					[:On]
					x = 1
					[:assertkw!] x > 0
					x++
					[:assertkw!] x > 1
					[:assertkw!] x <= 1
					[:Off]
				}
			}
		[:End:]

## Error Message

Sometimes, it is useful to give an explanation about the failure.
The [:assertkw:] keyword accepts a string of character that is the message given when the program has crashed.
The message string follows the condition, with a coma character between them.

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent A {
				def example {
					var someCondition : boolean
					[:On]
					[:assertkw](assert) [:someCondition](someCondition), "the failure explanation is here"
					[:Off]
				}
			}
		[:End:]

[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
