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
* [:number2:] is correct;
* [:number3:] is incorrect;
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


### Why can not a static field be defined in an agent type declaration (agent, skill, behavior)?

This is a design choice given that our entities are agents and as such they should not
"share" data unless done explicitly in an agent-oriented manner, for example via
resources or communication channels.
Having static fields in agents or skills would break the "independency" of agents,
also known as their autonomy.

It is most probable that such static data can be seen as a resource outside the skill
or agent, and as such it should be managed outside it (for example by using the artifact
meta-model).



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

### Why is an error or a warning put on the occurrence keyword?


Consider this code:

        [:Success:]
            package io.sarl.docs.faq.syntax
            import java.util.UUID
	        import java.util.Map
            event CarArrivedPercept {
                var car : UUID
                var floor : int
            }
            class CarDescription {
                def getFloor : int {0}
                def setFloor(n : int) {}
            }
            agent X {
                var cars : Map<UUID, CarDescription>
                [:On]
                on CarArrivedPercept {
                    cars.[:getfct](get)([:occcar](occurrence.car)).floor = [:occkw](occurrence).floor
                }
                [:Off]
            }
        [:End:]

We know that [:occkw:] is static, so cannot be changed. However, in the above code,
[:occcar:], is not being changed/assigned, but just used to refer to another entity
where assignment is performed.
However, SARL compiler will think that [:occcar:] may be changed due to a border effect
of the [:getfct:], and complain with warning.

Consider this code:

        [:Failure:]
            package io.sarl.docs.faq.syntax
            import java.util.UUID
            import java.util.Map
            event CarArrivedPercept {
                var car : UUID
                var floor : int
            }
            class CarDescription {
                def getFloor : int {0}
                def setFloor(n : int) {}
            }
            agent X {
                var cars : Map<UUID, CarDescription>
                [:On]
                on CarArrivedPercept {
                    [:occfloorerror](occurrence.floor = 1)
                }
                [:Off]
            }
        [:End:]

The line [:occfloorerror:] generates an error because in this case the SARL compiler
is sure that the [:occkw:] instance is changed.


In order to avoid the warning above, you could write the code as:

        [:Success:]
            package io.sarl.docs.faq.syntax
            import java.util.UUID
            import java.util.Map
            event CarArrivedPercept {
                var car : UUID
                var floor : int
            }
            class CarDescription {
                def getFloor : int {0}
                def setFloor(n : int) {}
            }
            agent X {
                var cars : Map<UUID, CarDescription>
                [:On]
                on CarArrivedPercept {
                    var c = occurrence.car
                    cars.get(c).floor = occurrence.floor
                }
                [:Off]
            }
        [:End:]


### Can we document SARL code for JavaDoc?

Yes. Since the SARL compiler generates valid Java code including the documentation,
you could generate the documentation of your SARL program with the standard javadoc
tool applied on the generated Java files.

Additionnally, you could use a specific Javadoc doclet in order to generate a documentation 
that follows the SARL syntax, intead of the Java syntax.

You could find details on the page dedicated to the [Maven documentation plugin](../tools/APIDocumentation.md). 


### How do I control the log-level of the Logging built-in capacity?

Use `setLogLevel()` of the `Logging` capacity, as explained here in the
[API documentation](http://www.sarl.io/docs/official/reference/bic/Logging.html).

You could also control the general configuration of the log level from the options
of your SARL Run-time Environment, such as [Janus](../tools/Janus.md).


### Equality and identity comparison (`==`, `===`, `!=`, `!==`) in SARL and checking for null: same as Java?

The mapping of the operator from SARL to Java are:

* `a === b` becomes `a == b`
* `a !== b` becomes `a != b`
* `a == b` becomes `a == null ? (b == null) : a.equals(b)`
* `a != b` becomes `!Objects.equals(a,b)`. This is null-safe (part of Google API)
  and the code of the function is `a == b || (a != null && a.equals(b))`.

It is always better to test valid against `null` with the `===` or `!==` operators.

Because the SARL `==` operator is mapped to the Java `equals()` function, and the
`===` and `!==` operators to the Java `==` and `!=` operators, it is better/safer,
and a best practice, to use `===` and `!==` when one of the operands is of primitive
type, e.g. `null`, number constants, primitive type variables. These operators are
not replaced neither `operator_equals` nor `operator_notEquals` within the Java code.

Usually, the SARL compiler generates a warning to push you to use `===` in place of `==`.
But with `null == value`, an ambiguous call error occurs before the warning is generated.
In fact, the SARL compiler tries to find an overloading function for the `==` operator.
Since `null` has not a specific type, the SARL compiler find multiple overloading functions.
Check the [documentation](http://www.sarl.io/docs/official/reference/general/Operators.html#3-comparison-operators) for details on the overloading mechanism of SARL.


### How to return two values?

SARL comes with a `Pair<A,B>` class to build an object for storing two values, nicknamed "key" and "value". It comes useful when a method has
to return two values instead of just one. For example, the following function returns the next floor and direction that an elevator has to serve:

        [:Success:]
            package io.sarl.docs.faq.syntax
            agent X {
                [:On]
                def kb_getNextJob() : Pair<Integer, Double> {
                    //...
                }
                [:Off]
            }
        [:End:]

As of Java 8, and as part of JavaFX, Java provides this `Pair<A,B>` class; check [here](https://www.geeksforgeeks.org/pair-class-in-java/) and
[here](https://docs.oracle.com/javase/8/javafx/api/javafx/util/Pair.html). Note Pairs are different from `Map`, which can be seen as a collection
of Pairs and with a proper key/value semantics.

There exist more advanced implementations of `Pair`, for example from Apache. See [here](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/tuple/package-summary.html),
[here](https://www.baeldung.com/java-pairs) and [here](https://gangmax.me/blog/2017/10/10/how-to-return-multiple-values-from-a-java-method/).

SARL itself have compact syntax do deal with `Pair`, by using `a -> b` to create a `Pair` object `(a,b)`. There are also compact ways of manipulating Collection and Maps.

Check SARL documentation on that [here](../reference/general/Operators.html#collection-operators).



[:Include:](../legal.inc)

