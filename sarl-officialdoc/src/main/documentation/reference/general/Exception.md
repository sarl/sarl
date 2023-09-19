# Exception Support

SARL supports exception throwing and catching. The mechanism is similar to the one in Java.

For a description of the exceptions that may be thrown by a function,
please see [how to declare exceptions in a function prototype](./FuncDecls.md#declare-exceptions-in-the-function-prototype).


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


## The try-with-resources Statement

The try-with-resources statement is a try statement that declares one or more resources. A resource
is an object that must be closed after the program is finished with it. The try-with-resources
statement ensures that each resource is closed at the end of the statement. Any object that
implements `java.lang.AutoCloseable`, which includes all objects which implement `java.io.Closeable`,
can be used as a resource.

The following example reads the first line from a file. It uses an instance of [:bufferedreadertype:]
to read data from the file. [:bufferedreadertype:] is a resource that must be closed after the
program is finished with it:

[:Success:]
	package io.sarl.docs.reference.gsr
	import java.io.BufferedReader
	import java.io.FileReader
	class A {
		[:On]
		static def readFirstLineFromFile(path : String) : String {
			[:trykw](try) (var br = new [:bufferedreadertype](BufferedReader)(new FileReader(path))) {
				return br.readLine
			}
		}
		[:Off]
	}
[:End:]



In this example, the resource declared in the try-with-resources statement is a [:bufferedreadertype:].
The declaration statement appears within parentheses immediately after the [:trykw:] keyword.
The class [:bufferedreadertype:] implements the interface `java.lang.AutoCloseable`.
Because the [:bufferedreadertype:] instance is declared in a try-with-resource statement, it will be
closed regardless of whether the try statement completes normally or abruptly (as a result of the method
`[:bufferedreadertype!].readLine` throwing an `IOException`).



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
