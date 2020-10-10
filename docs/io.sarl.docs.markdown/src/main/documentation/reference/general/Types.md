# Supported Types for Variables and Parameters

[:Outline:]


The SARL programming language is statically-typed, which means that all variables and parameters must first be declared before
they can be used. This involves stating the variable's type and name, as you've already seen:

[:Success:]
	class X {
		def fct {
			[:On]
			var [:varname](gear) : [:inttype](int) = [:initvalue](1)
			[:Off]
		}
	}
[:End:]

		
Doing so tells your program that a field named [:varname:] exists, holds numerical data, and has an initial
value of [:initvalue:]. A variable's data type determines the values it may contain, plus the operations that
may be performed on it. In addition to [:inttype:], the SARL programming language supports other data types:

* Eight primitive data types, and
* Object data types (class, enumeration, annotation, etc.).


## Primitive Types

A primitive type is predefined by the language and is named by a reserved keyword. Primitive values do not
share state with other primitive values. The eight primitive data types supported by the SARL programming language are:


| Type      | Object Equivalent (Wrapper) | Definition                                            |
| --------- | --------------------------- | ----------------------------------------------------- |
| `byte`    | `java.lang.Byte`            | The `byte` data type is an 8-bit signed two's complement integer. It has a minimum value of -128 and a maximum value of 127 (inclusive). The byte data type can be useful for saving memory in large arrays, where the memory savings actually matters. They can also be used in place of `int` where their limits help to clarify your code; the fact that a variable's range is limited can serve as a form of documentation. |
| `short`   | `java.lang.Short`           | The `short` data type is a 16-bit signed two's complement integer. It has a minimum value of -32,768 and a maximum value of 32,767 (inclusive). As with byte, the same guidelines apply: you can use a short to save memory in large arrays, in situations where the memory savings actually matters. |
| `int`     | `java.lang.Integer`         | By default, the `int` data type is a 32-bit signed two's complement integer, which has a minimum value of -2^31 and a maximum value of 2^(31)-1. In SARL, you can use the `int` data type to represent an unsigned 32-bit integer, which has a minimum value of 0 and a maximum value of 2^(32)-1. Use the `Integer` class to use `int` data type as an unsigned integer. Static methods like `compareUnsigned`, `divideUnsigned` etc have been added to the `Integer` class to support the arithmetic operations for unsigned integers. |
| `long`    | `java.lang.Long`            | The `long` data type is a 64-bit two's complement integer. The signed `long` has a minimum value of -2^63 and a maximum value of 2^(63)-1. In SARL, you can use the `long` data type to represent an unsigned 64-bit long, which has a minimum value of 0 and a maximum value of 2^(64)-1. Use this data type when you need a range of values wider than those provided by `int`. The `Long` class also contains methods like `compareUnsigned`, `divideUnsigned` etc to support arithmetic operations for unsigned long. |
| `float`   | `java.lang.Float`           | The `float` data type is a single-precision 32-bit IEEE 754 floating point. Its range of values is beyond the scope of this discussion, but is specified in the Floating-Point Types, Formats, and Values section of the SARL Language Specification. As with the recommendations for `byte` and `short`, use a `float` (instead of `double`) if you need to save memory in large arrays of floating point numbers. This data type should never be used for precise values, such as currency. For that, you will need to use the `java.math.BigDecimal` class Numbers and Strings covers `BigDecimal` and other useful classes provided by the SARL platform. |
| `double`  | `java.lang.Double`          | The `double` data type is a double-precision 64-bit IEEE 754 floating point. Its range of values is beyond the scope of this discussion, but is specified in the Floating-Point Types, Formats, and Values section of the SARL Language Specification. For decimal values, this data type is generally the default choice. As mentioned above, this data type should never be used for precise values, such as currency. |
| `boolean` | `java.lang.Boolean`         | The `boolean` data type has only two possible values: `true` and `false`. Use this data type for simple flags that track `true`/`false` conditions. This data type represents one bit of information, but its "size" isn't something that's precisely defined. |
| `char`    | `java.lang.Character`       | The `char` data type is a single 16-bit Unicode character. It has a minimum value of `\u0000` (or 0)  and a maximum value of `\uffff` (or 65,535 inclusive). |


## String of Characters

In addition to the eight primitive data types listed above, the SARL  programming language also provides special
support for character strings via the `java.lang.String` class. Enclosing your character string within (double or simple) quotes
will automatically create a new `String` object; for example, `var s : String = "this is a string"`.

String objects are immutable, which means that once created, their values cannot be changed. The `String` class is not technically
a primitive data type, but considering the special support given to it by the language, you'll probably
tend to think of it as such.

## Default Values of Primitive Types

It's not always necessary to assign a value when a field is declared. Fields that are declared but not initialized
will be set to a reasonable default by the compiler. Generally speaking, this default will be zero or null,
depending on the data type. Relying on such default values, however, is generally considered bad programming style.

The following chart summarizes the default values for the above data types.


| Data Type              | Default Value |
| ---------------------- | ------------- |
| `byte`                 | `0`           |
| `int`                  | `0`           |
| `long`                 | `0`           |
| `float`                | `0`           |
| `double`               | `0`           |
| `char`                 | `0`           |
| `boolean`	             | `false`       |
| `String` or any object | `null`        |


## Immutable Types

All objects in SARL has an internal state (composed of the values of the different attributes, transitively).
As SARL is a strongly-typed language, each object is of a given type.
This type is said to be immutable when the internal state of a object of this type remains constant after it has been entirely created.
In other words, the values of the object's attributes cannot be changed after the object's construction.
 
Several types are always immutable in SARL (mostly from the Java API itself):

| Immutable Types |
| --------------- |
[:Dynamic:]{
	io.sarl.lang.typesystem.DefaultImmutableTypeValidator.IMMUTABLE_TYPES.sort.toVerticalStringMatrix.renderToMarkdown
}

Additionally, you could define your own data type that is assumed to be immutable.
This definition is based on the usage of a specific annotation that is attached to your type.
The complete list of annotations that mark the types as immutable is:

| Annotations for Immutable Types |
| ------------------------------- |
[:Dynamic:]{
	io.sarl.lang.typesystem.DefaultImmutableTypeValidator.IMMUTABLE_TYPE_ANNOTATIONS.sort.toVerticalStringMatrix.renderToMarkdown
}

For illustration, the [:dataannon:] annotation, that is described in the [active annotation documentation](./ActiveAnnotations.md#2-data),
could be used as below:

[:Success:]
	import org.eclipse.xtend.lib.annotations.Data 
	[:On]
	[:dataannon](@Data) class Person {
	  [:valkw](val) firstName : String
	  val lastName : String
	}
[:End:]


[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
