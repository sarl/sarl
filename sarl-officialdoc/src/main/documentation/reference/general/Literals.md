# Literals

[:Outline:]

A literal denotes a fixed, unchangeable value. Literals for strings, numbers, booleans, null, and Java types are
supported, as well as literals for unmodifiable collection types like lists, sets, maps, and arrays.


## String Literals

A string literal is of type [:stringtype:]. String literals are enclosed in a pair of single quotes
or double quotes. Special characters can be quoted with a backslash or defined using unicode
notation.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var a = 'Hello World!'
		var b = "Hello World!"
		var c = 'Hello "World!"'
		var d = "Hello \"World!\""
		var [:evar](e) = "Hello 
					World!"
		[:Off]
	}
[:End:]

[:Fact:]{'Hello world' instanceof [:stringtype](String)}

> **_Note:_** Unlike Java, SARL strings can span multiple lines, as ilustrated by the variable [:evar:] above.


## Character Literals

Character literals use the same notation as String literals. If a single-character String literal is
used in a context where a primitive char or the wrapper type `Character` or `char` is expected,
the compiler will treat the literal as a `Character` or `char` value.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var a : char = 'a'
		var b : char = "b"
		[:Off]
	}
[:End:]


## Number Literals


### General Notation

SARL supports the same number literals as Java with two exceptions: there is no notation
for specifying octal numbers, and if you put the dot character in a number, you must
specify both the integer and fractional parts.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var a = 42		// Decimal
		var b = [:hexexample](0xbeef)	// Hexadecimal
		var c = [:decexample](077)		// Decimal 77, NOT octal
		var d = [:ratexample](0.1)		// The leading zero must be specified
		var e = 1.0		// The trailing zero must be specified
		[:Off]
	}
[:End:]


| Type of Integer | Description                   | Example        |
| --------------- | ----------------------------- | -------------- |
| Hexadecimal     | Prefix is `0x`                | [:hexexample:] |
| Octal           | Not supported                 |                |
| Decimal         | No prefix, no fractional part | [:decexample:] |
| Rational        | No prefix, fractional part    | [:ratexample:] |



### Integer suffixes

Post-fixing an integer literal may change its type:

* no suffix is for `int` number,
* suffix `l` or [:longsuffix:] is for `long` number (uppercase is less likely to be confused with `1`), and
* suffix [:bigintsuffix:] or `BI` is for `BigInteger` number.

Examples:
[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var anInteger = 1234
		
		var aLong = 1234[:longsuffix](L)
		
		var aBigInteger = 1234[:bigintsuffix](bi)
		[:Off]
	}
[:End:]


## Floating-point-value suffixes

Post-fixing a floating-point literal may change its type:

* no suffix is for `double` number,
* suffix [:doublesuffix:] or `D` is for `double` number,
* suffix [:floatsuffix:] or `F` is for `float` number, and
* suffix [:bigdoublesuffix:] or `BD` is for `BigDecimal` number. 

Examples:
[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var aDouble = 1234.0
		
		var anotherDouble = 5678[:doublesuffix](d)
		
		var aFloat = 1234.0[:floatsuffix](f)
		
		var anotherFloat = 5678[:floatsuffix!]
		
		var aBigDecimal = 1234[:bigdoublesuffix](bd)
		[:Off]
	}
[:End:]


### Large Numbers

As in Java 7, you can separate digits using `_` for better readability of large numbers.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		// underscore is ignored, L for long
		var a = 12_345_678L
		[:Off]
	}
[:End:]


## Boolean Literals

There are two boolean literals, `true` and `false`, which correspond to their Java counterpart of type `boolean`.


## Null Literal

The null pointer literal `null` has exactly the same semantics as in Java: a reference to nothing.


## Type Literals

A type literal is the fully qualified name of a type in the SARL language.
The syntax for type literals is generally the plain name of the type. Nested types use the delimiter `.`.
The following example is the type literal for the [:agenttype:] type.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Class<?> {
			[:On]
			io.sarl.lang.core.[:agenttype](Agent)
			[:Off]
		}
	}
[:End:]

To disambiguate the type expression with the `.` operator for invoking a function, the type literal may
also be specified using the [:typeof:] operator.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Class<?> {
			[:On]
			[:typeof](typeof)(io.sarl.lang.core.[:agenttype!])
			[:Off]
		}
	}
[:End:]

Consequently, it is possible to access the members of a type reflectively by using its plain name.
In the following example, the fields declared in the [:stringtype:] class are provided:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Object {
			[:On]
			[:stringtype!].getDeclaredFields
			[:Off]
		}
	}
[:End:]


##	Collection Literals

The collection literals permit to specify collections of values.


### Mutable Collections

It is easy to create instances of collections since the methods in `[:collectionliterals](CollectionLiterals)` are automatically imported.
They permit you to create instances of collections from the JDK.

[:ShowType:](org.eclipse.xtext.xbase.lib.CollectionLiterals)


In the following example, an array-based list with two strings of characters and a linked hash map with two pairs are created. 

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var myList = newArrayList('Hello', 'world')

		var myMap = newLinkedHashMap('a' -> 1, 'b' -> 2)
		[:Off]
	}
[:End:]


### Array Literals

Java arrays can be created either using a literal as described in the next section, or a new array with a fixed size.


#### Array Creation

The methods from `[:arraylisterals](ArrayLiterals)` are automatically included.

[:ShowType:](org.eclipse.xtext.xbase.lib.ArrayLiterals)


This utility class provides a collection of methods, such as `[:arraylisterals:].[:newarraysize!](int)`
for creating array literals.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		// variable a contains a array of size 400 which contains Objects.
		var a : String[] = [:newarraysize](newArrayOfSize)(400)

		// variable b contains a array of size 200 which contains int values.
		var b : int[] = newIntArrayOfSize(200)
		[:Off]
	}
[:End:]


#### Array Element Accessors

Retrieving and setting values of arrays is done through the extension methods `[:arrayget!](int)` and
`[:arrayset!](int, T)`.
The first parameter of these two functions are the position of the element to get or set.
As with Java, the index of the elements in the array starts with `0`.
The second parameter of [:arrayset!] is the value to put at the specified position in the array.

The method [:arraylength:] is available for retrieving the size of the array.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action {
			[:On]
			var a = #['Hello', 'world', '!']

			// variable b contains the second element of the array a: 'world'.
			var b = a.[:arrayget](get)(1)

			// change the first element in the array a.
			a.[:arrayset](set)(0, 'New Element')

			// variable c contains the size of the array a: 3.
			var c = a.[:arraylength](length)
			[:Off]
		}
	}
[:End:]



#### Array to List

Arrays are automatically converted to lists when needed. This is similar to the boxing and unboxing features
provided by Java to convert between the primitives and their respective object types.
This convertion is not done by copying the elements of array.
The [Facade design pattern] is used for creating a specific [:listtype:] implementation that uses the
original array as its internal backed data structure.
This method is similar to the `Arrays.asList` function provided by the Java API.
		[:Fact:]{typeof(java.util.Arrays).shouldHaveMethod("asList(java.lang.Object[]) : java.util.List")}  

In the following example, an array is defined and converted to a list.

[:Success:]
	package io.sarl.docs.reference.gsr
	import java.util.List
	agent A {
		[:On]
		val myArray : int[] = #[1,2,3]

		val myList : [:listtype](List)<Integer> = myArray
		[:Off]
	}
[:End:]


### Immutable Collections

In addition to the functions described in the previous section, SARL supports collection literals to create
immutable collections and arrays, depending on the target type. There are three types of immutable
collections: array, set, and hash table.

An immutable array of values is specified with the [:imar1:] and [:imar2:] delimiters.  

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var a = [:imar1](#[) 'Hello', 'World' [:imar2](])
		[:Off]
	}
[:End:]


An immutable set of values is specified with the [:imst1:] and [:imst2:] delimiters.  

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var b = [:imst1](#{) 'Hello', 'World' [:imst2](})
		[:Off]
	}
[:End:]


An immutable map of pairs is specified with the [:immp1:] and [:immp2:] delimiters, as for immutable sets, but
the values are pairs, using the [:pairop:].  

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		var c = [:immp1](#{) 'a' [:pairop](->) 1 ,'b' [:pairop!] 2 [:immp2](})
		[:Off]
	}
[:End:]



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
