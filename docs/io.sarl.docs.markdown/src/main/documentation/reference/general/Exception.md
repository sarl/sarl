# Exception Support

SARL supports exception throwing and catching. The mechanism is similar to the one in Java.

For a description of the exceptions that may be thrown by a function,
please see [how to declare exceptions in a function prototype](./FuncDecls.md#declare_exceptions_in_the_function_prototype).


## Throwing Exceptions

Throwing objects of type `Throwable` and the [:throw:] keyword have the same semantics and syntax as in Java; see
[Java Language Specification](http://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.18). 

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent A {
				def example {
					[:On]
					[:throw](throw) new IllegalArgumentException("explanation")
					[:Off]
				}
			}
		[:End:]


## Try, Catch, Finally

The try-catch-finally expression is used to handle exceptional situations. 
Checked exceptions are treated like runtime exceptions and only optionally 
validated. You can, but do not have to, catch them as they will be silently thrown. 

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent A {
				def example {
					[:On]
					try {
						throw new RuntimeException()
					} catch(e : Exception) {
						// Handle the exception
					} finally {
						// Do this block after the try block (if no exception thrown), 
						// the matched catch block (if an exception was catched),
						// or before exiting the function (if an exception was thrown
						// but not catched).
					}
					[:Off]
				}
			}
		[:End:]


## Try-Catch as an Expression

For try-catch, the argument is an expression. Consequently, you can
write code like the following and do not have to rely on
non-final variables: 

		[:Success:]
			package io.sarl.docs.reference.gsr
				import java.io.IOException
			agent A {
				[:On]
					def readFromFile : String { } 
					def example {
						val name =	try {
										readFromFile
									} catch (e : IOException) {
										"unknown"
									}
						println(name)
					}
				[:Off]
			}
		[:End:]



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
