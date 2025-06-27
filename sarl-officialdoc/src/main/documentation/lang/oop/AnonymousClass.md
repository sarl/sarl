# Anonymous Class

[:Outline:]

Anonymous classes enable you to make your code more concise. They enable you to declare and instantiate a
class at the same time. They are like local classes except that they do not have a name. Use them if you
need to use a local class only once.


## Declaring Anonymous Classes

While local classes are class declarations, anonymous classes are expressions, which means that you define
the class in another expression.

The following example, [:anontype:], uses anonymous classes in the
initialization statements of the local variables [:frenchfield:] and [:spanishfield:]:

[:Success:]
	package io.sarl.docs.reference.oop
	[:On]
	interface HelloWorld {
	    def greetSomeone(someone : String)
	}

	class [:anontype](HelloWorldAnonymousClasses) {
	    def sayHello {
	        var [:frenchfield](frenchGreeting) = new HelloWorld {
	            var name = "tout le monde"
	            def greetSomeone(someone : String) {
	                name = someone
	                println("Salut " + name)
	            }
	        }
	
	        var [:spanishfield](spanishGreeting) = new HelloWorld {
	            var name = "mundo"
	            def greetSomeone(someone : String) {
	                name = someone
	                println("Hola, " + name)
	            }
	        }

	        frenchGreeting.greetSomeone("Marc")
	        spanishGreeting.greetSomeone("Marco")
	    }
	}
[:End:]


## Syntax of Anonymous Classes

As mentioned previously, an anonymous class is an expression.
The syntax of an anonymous class expression is like the invocation of a constructor,
except that there is a class definition contained in a block of code.

Consider the instantiation of the frenchGreeting object ion the code below.

The anonymous class expression consists of the following:

* The [:newkw:] operator
* The name of an interface to implement or a class to extend. In this example, the anonymous class is implementing the interface [:anontype2:].
* Parentheses that contain the arguments to a constructor, just like a normal class instance creation expression. When you implement an interface, there is no constructor, so you do not need to put parameters, as in this example.
* A body, which is a class declaration body. More specifically, in the body, method declarations are allowed but statements are not.

Because an anonymous class definition is an expression, it must be part of a statement.
In this example, the anonymous class expression is part of the statement that instantiates
the [:frenchfield2:] object.

[:Success:]
	package io.sarl.docs.reference.oop
    interface [:anontype2](HelloWorld) {
        def greetSomeone(someone : String)
    }
	class HelloWorldAnonymousClasses {
	    def sayHello {
	[:On]
			var [:frenchfield2](frenchGreeting) = [:newkw](new) HelloWorld {
			    var name = "tout le monde"
			    def greetSomeone(someone : String) {
			        name = someone
			        println("Salut " + name)
			    }
			}
	[:Off]
		}
	}
[:End:]


## Accessing Local Variables of the Enclosing Scope, and Declaring and Accessing Members of the Anonymous Class

Anonymous classes can capture variables; they have the same access to
local variables of the enclosing scope:

* An anonymous class has access to the members of its enclosing class.
* An anonymous class cannot access local variables in its enclosing scope that are not declared as final or effectively final.
* A declaration of a type (such as a variable) in an anonymous class shadows any other declarations in the enclosing scope that have the same name.

Anonymous classes have restrictions with respect to their members:

* You cannot declare static initializers or member interfaces in an anonymous class.
* An anonymous class can have static members provided that they are constant variables.

> **_Note:_** You can declare the following in anonymous classes: fields, extra methods
> (even if they do not implement any methods of the supertype), instance initializers, 
> local classes. However, you cannot declare constructors in an anonymous class.

[:Success:]
	package io.sarl.docs.reference.oop
    interface HelloWorld {
        def greetSomeone(someone : String)
    }
	[:On]
	class HelloWorldAnonymousClasses {
		var name : String
		def sayHello {
			var frenchGreeting = new HelloWorld {
				def greetSomeone(someone : String) {
					name = someone
					println("Salut " + name)
				}
			}
		}
	}
[:End:]

[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
