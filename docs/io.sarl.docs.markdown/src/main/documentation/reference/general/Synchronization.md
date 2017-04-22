# Synchronization Expression

The synchonization expression does the same as it does in Java (see
[Java Language Specification](http://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.19)).
The only difference is that in SARL it is an expression and can therefore be used at more places. 


## Standard Syntax

The synchronization statement can be used as in Java:

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent A {
				def example : Object {
					[:On]
						var lock = new Object
						[:sync](synchronized) (lock) {
							println("Hello")
						}
					[:Off]
				}
			}
		[:End:]


## Expression Syntax

Because the [:sync:] keyword is an expression, it is possible to write synchronized code inside another expression.

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent A {
				def example : Object {
					[:On]
						var lock = new Object
						val name = synchronized (lock) { 
								"Hello" 
						}
						println(name)
					[:Off]
				}
			}
		[:End:]



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
