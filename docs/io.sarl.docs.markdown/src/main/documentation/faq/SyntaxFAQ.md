# FAQ on the SARL language Syntax

[:Outline:]

## General Syntax

### Can I use the same syntax as in Java for number literals?

__No__.

When a decimal point is written in the literal,
the fractional part and the mantissa part must
be specify also, even if these parts are equal
to zero.  Consequently:

* [:number1:] is correct;
* [:number2:] is incorrect;
* [:number3:] is correct;
* [:number4:] is incorrect.

		[:Success:]
			package io.sarl.docs.faq.syntax
			agent A {
				def action : double {
					var a = [:number1](123.0)
					return a
				}
			}
		[:End:]
		[:Success:]
			package io.sarl.docs.faq.syntax
			agent A {
				def action : double {
					var a = [:number2](0.123)
					return a
				}
			}
		[:End:]
		[:Failure:]
			package io.sarl.docs.faq.syntax
			agent A {
				def action : double {
					var a = [:number3](123.)
					return a
				}
			}
		[:End:]
		[:Failure:]
			package io.sarl.docs.faq.syntax
			agent A {
				def action : double {
					var a = [:number4](.123)
					return a
				}
			}
		[:End:]


### Why can I not put a string in the package name?
			
It is not allowed to put a SARL keyword, such as
[:agent:], in the name of a package.

But, if you prefix with the [:hat:] character the string
that corresponds to a keyword, then it is possible
to obtain a package name with one of its components
equals to a SARL keyword:

		[:Success:]
			package io.sarl.docs.faq.syntax.[:hat](^)[:agent](agent)
		[:End:]


### Is the 'val' keyword defining a constant?

__Yes and No__.

Indeed, the [:valkw:] keyword defines a name that it could be initialized only once time.
It is similar to the `final` modifier of the Java language.

Consider the example below: two values are defined, `a` and `b`.
The `a` variable is a real constant because it has a raw type and it is initialized.
The `b` variable is not a real constant because it is a reference to an object.
The reference is constant, *but* the referred object is not. Consequently, it is still
possible to call the setters of `b`. 

		[:Success:]
			package io.sarl.docs.faq.syntax
			agent A {
				[:On][:valkw](val) a : int = 4
				val b : Object = new Object[:Off]
			}
		[:End:]


### Why cannot use the syntax 'a[0]' for arrays?

In SARL, the array type may be written with the classic array syntax, such as
`int[]`, or the object-oriented syntax, such as `List<Integer>`.

SARL considers that the each array is a list of something.
Consequently, retrieving the values of the array must be done with `get(int)`.

		[:Success:]
			package io.sarl.docs.faq.syntax
			import java.util.List
			agent A {
				def action : boolean {
					[:On]var a : Integer[] = #[1, 2, 3]
					var b : List<Integer> = newArrayList(1, 2, 3)
					
					a.get(0) == b.get(0)[:Off]
				}
			}
		[:End:]


### Why can I not use the '<>' notation for generic parameters?

In SARL, the empty generic parameter list, written `<>` is
not supported: a generic type expression must be written between them.

		[:Failure:]
			package io.sarl.docs.faq.syntax
			import java.util.List
			import java.util.ArrayList
			agent A {
				var a : List<Integer> = new ArrayList<>()
			}
		[:End:]

For solving this problem, two choices: i) add a type expression between
< and >; ii) remove the generic parameter list.

		[:Success:]
			package io.sarl.docs.faq.syntax
			import java.util.List
			import java.util.ArrayList
			agent A {
				[:On]var firstSolution : List<Integer> = new ArrayList<Integer>
				var secondSolution : List<Integer> = new ArrayList[:Off]
			}
		[:End:]


### How can I create instances of anonymous classes?

In SARL, the creation of anonymous classes (interface implementation...)
must be done with a closure.

Consider the definition of the following interface:

		[:Success:][:On]
	       interface MyInterface {
	            def myfunction(parameter : Object) : void
	       }
		[:End:]

The on-the-fly definition and instantiation of an instance of this interface,
a.k.a. anonymous class definition in the Java community, could be written is SARL
with the following closure:

		[:Success:]
			package io.sarl.docs.faq.syntax
			import java.util.List
 			interface MyInterface {
 				def myfunction(parameter : Object) : void
 			}
			agent A {
				def action : void {
					[:On]var instance : MyInterface
					instance = [ parameter | /* The code of myfunction() */ ] [:Off]
				}
			}
		[:End:]


### Java syntax for anonymous classes is allowed

In SARL, it is recommended tp create anonymous classes (interface implementation...)
must be done with a closure (see previous question).

The Java-based syntax for defining an anonymous class's instance is allowed, but not recommended
in the SARL language. It means that the following code is valid:

		[:Success:]
			package io.sarl.docs.faq.syntax
			import java.util.List
 			interface MyInterface {
 				def myfunction(parameter : Object) : void
 			}
			agent A {
				def action : void {
					[:On]var instance = new MyInterface() {
						def myfunction(parameter : Object) {
							/* The code of myfunction() */
						}
					}[:Off]
				}
			}
		[:End:]


### Ambiguous call to capacity function

When the calling a capacity function, the SARL compiler complains with an "ambiguous call" error.
In the code below, the function [:myfct:] is defined in the capacities [:c1:] and [:c2:].
The call to [:myfct:] in the agent definition is the place where the error occurs.

		[:Failure:]
			package io.sarl.docs.faq.syntax
			event Initialize
			[:On]capacity [:c1](C1) {
				def [:myfct](myfunction)
				def myfunction2
			}
			capacity [:c2](C2) {
				def myfunction
				def myfunction3
			}
			agent MyAgent {
				[:useskw](uses) C1, C2
				on Initialize {
				    myfunction
				    myfunction2
				    myfunction3
				}
			}
		[:End:]

This error is standard because the functions of the capacities [:c1:] and [:c2:] are implicitly accessible
in the scope of the agent definition, see [:useskw:] keyword definition. The SARL compiler is then unable
to determine that is the function to call.


For solving this issue, the developer must explicitly call the correct version of [:myfct:] by
getting the capacity. The following code is the correct call to the function if the function in the
capacity [:c1:] should be called:

		[:Success:]
			package io.sarl.docs.faq.syntax
			event Initialize
			capacity C1 {
				def myfunction
				def myfunction2
			}
			capacity C2 {
				def myfunction
				def myfunction3
			}
			agent MyAgent {
				[:useskw](uses) C1, C2
				on Initialize {
					[:On]getSkill(C1).myfunction[:Off]
				    myfunction2
				    myfunction3
				}
			}
		[:End:]


[:Include:](../legal.inc)

