# Lambda Expressions

[:Outline:]

A lambda expression is basically a piece of code, which is wrapped 
in an object to pass it around. As a Java developer it is best to 
think of a lambda expression as an anonymous class with a single 
method.


## Basic Definition

A lambda expression is code surrounded by square brackets (inspired from Smalltalk).
Like a method declaration, a lambda expression may declare parameters.

[:Success:]
	package io.sarl.docs.reference.gsr
	import java.awt.^event.ActionEvent
	import javax.swing.JTextField
	agent A {
		def example {
			val textField = new JTextField
			// Define a lambda expression that take an ActionEvent as parameter.
			// It is defined as a function of type: (ActionEvent) => void
			textField.addActionListener(
				[:On]
				[ e : [:actionevent](ActionEvent) [:tube](|)
					textField.text = "Something happened!" + e.toString
				][:Off])
		}
	}
[:End:]


The lambda above has one parameter called [:e:] which is of type [:actionevent:].
The code after the [:tube:] operator is the internal code of the lambda expression.


## Inferred Parameter Type

You do not have to specify the type explicitly because it can be inferred from the context.
For example, when using inferred type, the code in the previous section becomes:

[:Success:]
	package io.sarl.docs.reference.gsr
	import java.awt.^event.ActionEvent
	import javax.swing.JTextField
	agent A {
		def example {
			val textField = new JTextField
			// Define a lambda expression that take an ActionEvent as parameter.
			// It is defined as a function of type: (ActionEvent) => void
			textField.addActionListener(
				[:On]
				[ [:e](e) |
					textField.text = "Something happened!" + e.toString
				][:Off])
		}
	}
[:End:]


The type of [:e:] is inferred according to its usage.


## Implicit Parameters

### Case of a single parameter: it

As lambdas with one parameter are a common case, there  a special short hand notation
for these parameters, which is to leave the declaration including the vertical bar out.

The name of the single parameter becomes [:it:].

[:Success:]
	package io.sarl.docs.reference.gsr
	import java.awt.^event.ActionEvent
	import javax.swing.JTextField
	agent A {
		def example {
			val textField = new JTextField
			// Define a lambda expression that take an ActionEvent as parameter.
			// It is defined as a function of type: (ActionEvent) => void
			textField.addActionListener(
				[:On]
				[
					textField.text = "Something happened!" + [:it](it).toString
				][:Off])
			
		}
	}
[:End:]


### Case of multiple parameters

When a lambda has multiple parameters, and no name is provided by the SARL developer, 
the compiler generates default names for each of the formal parameters.
The implicit name for the first parameter is `$0`, `$1` for the second,
`$2` for the third, etc.

Let the following interface declaration:

[:Success:]
    package io.sarl.docs.reference.gsr
    [:On]
    interface [:MyInterfacename](MyInterface) {
        def [:MyInterfacenamefct](myfct)(a : int, b : int, c : int)
    }
    [:Off]
[:End:]


The following code is a lambda expression implementing the [:MyInterfacename:] interface.
Because the [:MyInterfacenamefct:] function has four formal parameters, the lambda expression 
has four parameters with implicit the following implicit names: `$0`, `$1`, and `$2`.

[:Success:]
    package io.sarl.docs.reference.gsr
    interface MyInterface {
        def myfct(a : int, b : int, c : int) : int
    }
    agent A {
        def called(p : MyInterface) {}
        def example {
            called(
            [:On]
            [
                $0 + $1 + $2
            ]
            [:Off])
        }
    }
[:End:]


## Empty List of Parameters

A lambda expression with zero arguments is written with a bar after the opening bracket:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			[:On]
			val runnable : Runnable = [ |
					println("Hello I'm executed!")
				]
			[:Off]
		}
	}
[:End:]


## Short notation of a Lambda Expression

When the lamda expression is passed to a single-parameter function, the brackets that are enclosing the lambda
declaration could be missed. It specific syntax is called the short notation of a lambda expression. 

Consider the following definition of a function with lambda expression as a single parameter:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		def aFunction(aParameter : (Object) => Object) {
		}
		[:Off]
	}
[:End:]


The call to the previously defined function with the short notation is:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def aFunction(aParameter : (Object) => Object) {
		}
		def caller {
			[:On]
			aFunction(a | new Object)
			[:Off]
		}
	}
[:End:]


This call is equivalent to the following one, with the standard notation:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def aFunction(aParameter : (Object) => Object) {
		}
		def caller {
			[:On]
			aFunction([a | new Object])
			[:Off]
		}
	}
[:End:]


As for the standard notation, the parameter and the bar could be missed when their is no formal parameter.


## Lambda as the Last Parameter of a Method

When the last argument of a method call is a lambda, it can be passed right after the argument list.

For instance if you want to sort some strings by their length, you could use either of the following two examples.
The first example uses the notation with the lambda expression inside the argument list.

[:Success:]
	package io.sarl.docs.reference.gsr
	import java.util.List
	import java.util.Collections
	agent A {
		def example {
			[:On]
			var t : List<String>
			// Lambda expression is written outside the parenthesis
			Collections.sort(t, [ a, b | a.length - b.length ])
			[:Off]
		}
	}
[:End:]


The second example uses the notation with the lambda expression outside the argument list.

[:Success:]
	package io.sarl.docs.reference.gsr
	import java.util.List
	import java.util.Collections
	agent A {
		def example {
			[:On]
			var t : List<String>
			// Lambda expression is written outside the parenthesis
			Collections.sort(t) [ a, b | a.length - b.length ]
			[:Off]
		}
	}
[:End:]


## Type of a Lambda Expression

### Pure SARL Notation

Because SARL is a strongly typed programming language, each lambda expression has a type.
The syntax for specifying the type of a lambda is: `(parameter types) [:mapto](=>) return type`

The following example defines a variable [:f:], which is a lambda taking one parameter of [:strtype:], and
returning a value of [:booltype:]. 

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var [:f](f) : ([:strtype]{String}) [:mapto!] [:booltype](Boolean)
		[:Off]
	}
[:End:]


If the lambda is a procedure, i.e. it has no return type, then the return type to specify must be `void`.


### Java-like Notation

The SARL lambda expressions are mapped to the Java types defined in `Functions` or `Procedures`.
		[:Fact:]{typeof(org.eclipse.xtext.xbase.lib.Functions)}
		[:Fact:]{typeof(org.eclipse.xtext.xbase.lib.Procedures)}
These two Java interfaces contains the definitions of inner interfaces for function/procedure with
different numbers of parameters.

For example, the SARL erasure `(String) [:mapto!] Boolean` may be written with the Java notation:

[:Success:]
	package io.sarl.docs.reference.gsr
	import org.eclipse.xtext.xbase.lib.Functions
	agent A {
		[:On]
		// Same type of function.
		var f2 : Functions.[:f1](Function1)<? super String,? extends Boolean>
		[:Off]
	}
[:End:]


The [:f1:] interface represents a function with a single formal parameter (with the type equals to the first generic parameter),
and returning a value of type equals to the second generic parameter.
Depending of the number of parameters, you may use `Function0` to `Function5`, or `Procedure0` to `Procedure6`. 



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
