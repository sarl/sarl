# Cast and Type Conversions

Because SARL is statically-typed at compile time, after a variable is declared, it cannot be declared again or assigned a
value of another type unless that type is implicitly convertible to the variable's type.
For example, the `String` cannot be implicitly converted to `int`. Therefore, after you declare [:ivar:] as an [:inttype:], you
cannot assign the string [:hellostr:] to it, as the following code shows:

[:Failure:]
	package io.sarl.docs.reference.gsr
	class A {
		def fct {
			[:On]
			var [:ivar](i) : [:inttype](int)
			i = [:hellostr]("Hello") // Error: "Cannot cast String to int"
			[:Off]
		}
	}
[:End:]


However, you might sometimes need to copy a value into a variable or method parameter of another type.
For example, you might have an integer variable that you need to pass to a method whose parameter is typed as `double`.
Or you might need to assign a class variable to a variable of an interface type. These kinds of operations are
called *type conversions*.
In SARL, you can perform the following kinds of conversions:

* **Implicit conversions:** No special syntax is required because the conversion is type safe and no data will be lost.
  Examples include conversions from smaller to larger number types, and conversions from derived classes to base classes.
* **Explicit conversions (casts):** Explicit conversions require a casting operator. Casting is required when information
  might be lost in the conversion, or when the conversion might not succeed for other reasons. Typical examples include
  numeric conversion to a type that has less precision or a smaller range, and conversion of a base-class instance to a
  derived class.
* **User-defined conversions:** User-defined conversions are performed by special methods that you can define to enable
  explicit and implicit conversions between custom types that do not have a base class–derived class relationship.
  For more information, see [Conversion Operators](#conversion-operators).


## Implicit Conversions

For built-in types, e.g. numeric types, an implicit conversion can be made when the value to be stored can fit into
the variable without being truncated or rounded off. For example, a variable of type long (64-bit integer) 
can store any value that an int (32-bit integer) can store. In the following example, the compiler implicitly
converts the value of [:numvar:] on the right to a type long before assigning it to [:bignumvar:].

[:Success:]
	package io.sarl.docs.reference.gsr
	class A {
		def fct : long {
			[:On]
			var [:numvar](num) : int = 2147483647
			var [:bignumvar](bigNum) : long = num
			[:Off]
			return bigNum
		}
	}
[:End:]


The following table shows the predefined implicit conversions between SARL types.


| From          | To                                                                                                       |
| ------------- | -------------------------------------------------------------------------------------------------------- |
| T[]           | List<T>                                                                                                  |
| boolean       | Boolean                                                                                                  |
| Boolean       | boolean                                                                                                  |
| byte          | Byte, short, int, long, float, double                                                                    |
| Byte          | byte, short, int, long, float, double                                                                    |
| char          | Character, String                                                                                        |
| Character     | char, String                                                                                             |
| double        | Double                                                                                                   |
| Double        | double                                                                                                   |
| float         | Float, double                                                                                            |
| Float         | float, double                                                                                            |
| int           | Integer, long, float, double                                                                             |
| Integer       | int, long, float, double                                                                                 |
| List<T>       | T[]                                                                                                      |
| long          | Long, float, double                                                                                      |
| Long          | long, float, double                                                                                      |
| short         | Short, int, long, float, double                                                                          |
| Short         | short, int, long, float, double                                                                          |
| String        | char, Character (if string length is 1)                                                                  |



## Explicit Conversions

However, if a conversion cannot be made without a risk of losing information, the compiler requires that you perform
an explicit conversion, which is called a cast. A cast is a way of explicitly informing the compiler that you
intend to make the conversion and that you are aware that data loss might occur. To perform a cast, specify
the value or variable to be converted, followed by the [:askw:] keyword and the type that you are casting.
The following program casts a [:doubletype:] to an [:inttype:]. The program will not compile without the cast.

[:Success:]
	class Test {
	    static def Main : [:inttype!] {
	    	[:On]
	        var x : [:doubletype](double) = 1234.7
	        var a : [:inttype!]
	        // Cast [:doubletype!] to [:inttype!].
	        a = x [:askw](as) [:inttype!]
	        [:Off]
	    }
	}
[:End:]


[:Failure:]
	class Test {
	    static def Main : [:inttype!] {
	        var x : [:doubletype](double) = 1234.7
	        var a : [:inttype!]
	        // Cast [:doubletype!] to [:inttype!].
	        a = x
	    }
	}
[:End:]


For object types, an explicit cast is required if you need to convert from a base type to a derived type:

[:Success:]
	class Animal {}
	class Giraffe extends Animal {}
	class Test {
	    static def Main : Object {
	    	[:On]
	    	// Create a new derived type
	        var g = new Giraffe
	        
	        // implicit conversion to base type is safe.
	        var a : Animal = g
	        
	        // Explicit conversion is required to cast back to derived type.
	        // Note: This will compile but will throw an exception at run time if the right-side
	        // object is not in fact a Giraffe.
	        var g2 : Giraffe
	        g2 = a as Giraffe
	        [:Off]
	    }
	}
[:End:]


A casting operation between reference types does not change the run-time type of the underlying object; it only changes
the type of the value that is being used as a reference to that object.


The following table shows the predefined explicit conversions that are predefined into 
SARL.


| From          | To                                                                                                       |
| ------------- | -------------------------------------------------------------------------------------------------------- |
| boolean       | String                                                                                                   |
| byte          | byte, short, int, long, float, double, AtomicInteger, AtomicLong, AtomicDouble, BigInteger, BigDecimal   |
| char          | String                                                                                                   |
| double        | byte, short, int, long, float, double, AtomicInteger, AtomicLong, AtomicDouble, BigInteger, BigDecimal   |
| float         | byte, short, int, long, float, double, AtomicInteger, AtomicLong, AtomicDouble, BigInteger, BigDecimal   |
| int           | byte, short, int, long, float, double, AtomicInteger, AtomicLong, AtomicDouble, BigInteger, BigDecimal   |
| long          | byte, short, int, long, float, double, AtomicInteger, AtomicLong, AtomicDouble, BigInteger, BigDecimal   |
| Number        | byte, short, int, long, float, double, AtomicInteger, AtomicLong, AtomicDouble, BigInteger, BigDecimal   |
| Object        | String                                                                                                   |
| short         | byte, short, int, long, float, double, AtomicInteger, AtomicLong, AtomicDouble, BigInteger, BigDecimal   |
| String        | boolean, char, byte, short, int, long, float, double, AtomicInteger,  AtomicLong, AtomicDouble           |
|               | BigInteger, BigDecimal                                                                                   |


## Type Conversion Exceptions at Run Time

In some reference type conversions, the compiler cannot determine whether a cast will be valid. It is possible for
a casting operation that compiles correctly to fail at run time. As shown in the following example, a type cast
that fails at run time will cause an [:classcastexception:] to be thrown.

[:Success:]
	[:On]
	class Animal {}
	
	class [:reptiletype](Reptile) extends Animal {}
	
	class [:mammaltype](Mammal) extends Animal {}
	
	class Test {
	    static def main {
	    	test(new Mammal())
	    }

		static def test(a : Animal) {
			// Cause a [:classcastexception](ClassCastException) at run time
			// because [:mammaltype!] is not convertible to [:reptiletype].
			var r : Reptile
			r = a as Reptile
		}
	}
    [:Off]
[:End:]


SARL provides the [:instanceofoperator:] operator to enable you to test for compatibility before actually performing a cast.

The [:instanceofoperator:] operator evaluates type compatibility at runtime. It determines whether an object instance or
the result of an expression can be converted to a specified type. It has the syntax:

[:Success:]
	class type { }
	class Test {
	    static def main(expr : Object) {
	    	[:On]
	    	[:exprvar](expr) [:instanceofoperator](instanceof) [:typevar](type)
	    	[:Off]
	    }
	}
[:End:]


where [:exprvar:] is an expression that evaluates to an instance of some type, and [:typevar:] is the name of the
type to which the result of [:exprvar:] is to be converted. The [:instanceofoperator:] operator is `true` if
[:exprvar:] is non-null and the object that results from evaluating the expression can be converted to
[:typevar:]; otherwise, it returns `false`.


## Conversion Operators

SARL enables programmers to declare conversions on classes or basic types so that classes or basic types can
be converted to and/or from other classes or basic types. Conversions are associated to the `as`
type casting operator. When the compiler cannot proceed an implicit nor explicit casting, it tries
to find within the current code scope the definition of a casting operator function.
Depending on the category of the type to cast to, the name of this casting operator function is different:

* **Object type:** the casting operator function's name must have the prefix `to` followed by the simple name
  (first letter upper case) of the cast type, e.g. `toString`.
* **Basic type:** the casting operator function's name must start with the name of the basic type (all lower
  case) following by the post-fix `Value`, e.g. `intValue`.

Additionnally, the return type of the casting operator function must be the cast type, and not a sub-type of the
type specified as right operand of the casting operator.

A single parameter may be specified or not. If it is specified, it is the expression to cast.
It means that the type of the formal parameter is the expected type of the expression.
If the formal parameter is omitted, the current object (`this`) is assumed to be converted.

In the following example, the function [:tointegerfct:] is defined for converting a [:typetype:] to [:integertype:].
When the expression [:castexpr1:] is evaluated by the compiler, the function [:tointegerfct:] is discovered and used
for proceeding the cast.
 
[:Success:]
	[:On]
	class [:typetype](Type) {
		def [:tointegerfct](toInteger) : [:integertype](Integer) { 0 }
	}
	class Test {
	    def main {
	    	var obj : Type
	    	var value = [:castexpr1](obj as Integer)
	    }
	}
	[:Off]
[:End:]


In the second example below, the function [:tointegerfct:] is defined in the same class as the one where the cast operator is
defined.
When the expression [:castexpr1:] is evaluated by the compiler, the function [:tointegerfct:] is discovered and used
for proceeding the cast.
 
[:Success:]
	[:On]
	class Type {
	}
	class Test {
	    def main {
	    	var obj : Type
	    	var value = obj as Integer
	    }
		def [:tointegerfct!](v : Type) : [:integertype](Integer) { 0 }
	}
	[:Off]
[:End:]


Any function that is declared into the scope of the cast operator, and following the rules that are described above, may
be a candidate for being the cast operator function.



The two previous example have an object type as the cast type. The two following examples have the basic type [:inttype:].
The declarations of the [:tointegerfct:] are replaced by declarations of [:intvaluefct:].

[:Success:]
	[:On]
	class [:typetype](Type) {
		def [:intvaluefct](intValue) : [:inttype!] { 0 }
	}
	class Test {
	    def main {
	    	var obj : Type
	    	var value = obj as int
	    }
	}
	[:Off]
[:End:]


[:Success:]
	[:On]
	class Type {
	}
	class Test {
	    def main {
	    	var obj : Type
	    	var value = obj as int
	    }
		def [:intvaluefct!](v : Type) : [:inttype!] { 0 }
	}
	[:Off]
[:End:]


[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
