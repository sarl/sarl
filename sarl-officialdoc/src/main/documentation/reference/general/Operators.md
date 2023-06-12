# Operators

[:Outline:]

SARL supports a collection of operators. Most of them are infix operators, and several are postfix operators.


## Assignments

The assignment operators are listed below. Local variables and fields can be assigned using the `=` operator.
Compound assignment operators (`+=`, `-=`, `*=`, `/=`, and `%=`) can be used as a shorthand for the assignment
of a binary expression. They work automatically when the corresponding infix operator is declared.


| Operator | Operator Semantic                       |
| -------- | --------------------------------------- |
| a = b    | Set the variable a with the value of b. | [:Fact:]${var a : int = 5; a = 6; a} == 6$
| a += b   | Alias to: `a = a + b`                   | [:Fact:]${var a : int = 5; a += 6; a} == 11$
| a -= b   | Alias to: `a = a - b`                   | [:Fact:]${var a : int = 5; a -= 6; a} == -1$
| a *= b   | Alias to: `a = a * b`                   | [:Fact:]${var a : int = 5; a *= 6; a} == 30$
| a /= b   | Alias to: `a = a / b`                   | [:Fact:]${var a : int = 5; a /= 6; a} == 0$
| a %= b   | Alias to: `a = a % b`                   | [:Fact:]${var a : int = 5; a %= 6; a} == 5$


> **_Note:_** The assignment operator is the only one operator that cannot be overridden yet.
> See the [operator overloading section](#operator-overloading) for details.


## Arithmetic operators

The arithmetic operators are listed below and take numbers as operands. 
There are either unary (one operand) or binary (two operands).


| Operator | Function Name        | Operator Semantic                                            |
| -------- | -------------------- | ------------------------------------------------------------ |
| a + b    | operator\_plus       | Add a and b.                                                 | [:Fact:]$(1 + 2) == 3$
| a - b    | operator\_minus      | Subtract b to a. Binary operator.                            | [:Fact:]$(1 - 2) == -1$
| a * b    | operator\_multiply   | Multiply a by b.                                             | [:Fact:]$(1 * 2) == 2$
| a / b    | operator\_divide     | Divide a by b.                                               | [:Fact:]$(6 / 2) == 3$
| a % b    | operator\_modulo     | Modulo of the division of a by b.                            | [:Fact:]$(1 % 2) == 1$
| a \*\* b | operator\_power      | Compute the power b of a.                                    | [:Fact:]$(2 ** 3) == 8$
| - a      | operator\_minus      | Negate the value of a. Unary operator.                       | [:Fact:]$(- 2) == -2$
| a ++     | operator\_plusPlus   | Increment a by 1, reply the value before the incrementation. | [:Fact:]${var a=2; a++; a} == 3$
| a --     | operator\_moinsMoins | Decrement a by 1, reply the value before the decrementation. | [:Fact:]${var a=2; a--; a} == 1$


Each operator has an associated function name. This function contains
the concrete implementation of the operational semantic of the
operator. This function can be redefined as explained in the 
[operator overloading section](#operator-overloading).


## Comparison operators


### Comparison operators on primitive types

The comparison operators on primitive types are listed below.


| Operator      | Function Name               | Operator Semantic                                                                |
| ------------- | --------------------------- | -------------------------------------------------------------------------------- |
| a == b        | operator\_equals            | Test if a and b are equal.                                                       | [:Fact:]$2 == 2$
| a != b        | operator\_notEquals         | Test if a and b are not equal.                                                   | [:Fact:]$2 != 3$
| a === b       | operator\_tripleEquals      | Test if a and b are equal.                                                       | [:Fact:]$2 === 2$
| a !== b       | operator\_tripleNotEquals   | Test if a and b are not equal.                                                   | [:Fact:]$2 !== 3$
| a &lt; b      | operator\_lessThan          | Test if a is lower than b (a, b cannot be boolean).                              | [:Fact:]$2 < 3$
| a &gt; b      | operator\_greaterThan       | Test if a is greater than b (a, b cannot be boolean).                            | [:Fact:]$3 > 2$
| a &lt;= b     | operator\_lessEqualsThan    | Test if a is lower than or equal to b (a, b cannot be boolean).                  | [:Fact:]$2 <= 3$
| a &gt;= b     | operator\_greaterEqualsThan | Test if a is greater than or equal to b (a, b cannot be boolean).                | [:Fact:]$3 >= 2$
| a &lt;=&gt; b | operator\_spaceship         | Replies a negative value if a &lt; b, a positive value if a &gt; b, otherwise 0. | [:Fact:]$(2 <=> 3) < 0$


Each operator has an associated function name. This function contains
the concrete implementation of the operational semantic of the
operator. This function can be redefined as explained in the 
operator overloading section](#operator-overloading).


### Comparison operators on objects

The comparison operators on objects are listed below.

| Operator      | Function Name               | Operator Semantic                                                                |
| ------------- | --------------------------- | -------------------------------------------------------------------------------- |
| a == b        | operator\_equals            | Test if a and b are equal.                                                       | [:Fact:]$'a' == 'a'$
| a != b        | operator\_notEquals         | Test if a and b are not equal.                                                   | [:Fact:]$'a' != 'b'$
| a === b       | operator\_tripleEquals      | Test if a and b are equal.                                                       | [:Fact:]$var a='a'; a === a$
| a !== b       | operator\_tripleNotEquals   | Test if a and b are not equal.                                                   | [:Fact:]$'a' !== 'a'$
| a &lt; b      | operator\_lessThan          | Test if a is lower than b (a must be Comparable).                                | [:Fact:]$'a' < 'b'$
| a &gt; b      | operator\_greaterThan       | Test if a is greater than b (a must be Comparable).                              | [:Fact:]$'b' > 'a'$
| a &lt;= b     | operator\_lessEqualsThan    | Test if a is lower than or equal to b (a must be Comparable).                    | [:Fact:]$'a' <= 'b'$
| a &gt;= b     | operator\_greaterEqualsThan | Test if a is greater than or equal to b (a must be Comparable).                  | [:Fact:]$'b' >= 'a'$
| a &lt;=&gt; b | operator\_spaceship         | Replies a negative value if a &lt; b, a positive value if a &gt; b, otherwise 0. | [:Fact:]$('a' <=> 'b') < 0$

Each operator has an associated function name. This function contains
the concrete implementation of the operational semantic of the
operator. This function can be redefined as explained in the
operator overloading section](#operator-overloading).


## Boolean Operators

The boolean operators are listed below. Each operator takes one or two boolean values as operands, and
replies the boolean value resulting from the operational semantic of the operator. 


| Operator         | Function Name  | Operator Semantic          |
| ---------------- | -------------- | -------------------------- |
| a &#124;&#124; b | operator\_or   | If a then true else b.     | [:Fact:]$true || false$
| a &amp;&amp; b   | operator\_and  | If a then b else false.    | [:Fact:]$true && true$
| ! a              | operator\_not  | If a then false else true. | [:Fact:]$!false$


Each operator has an associated function name. This function contains
the concrete implementation of the operational semantic of the
operator. This function can be redefined as explained in the 
[operator overloading section](#operator-overloading).


## Bitwise Operators

The bit operators are listed below. The bit operators apply operations on the bits that represent
a numeric value.


| Operator         | Function Name               | Operator Semantic                                                 |
| ---------------- | --------------------------- | ----------------------------------------------------------------- |
| a &lt;&lt; b     | operator\_doubleLessThan    | Shift the signed bit representation of a to the left by b units.  | [:Fact:]$(1 << 3) == 8$
| a &gt;&gt; b     | operator\_doubleGreaterThan | Shift the signed bit representation of a to the left by b units.  | [:Fact:]$(-8 >> 3) == -1$
| a &lt;&lt;&lt; b | operator\_tripleLessThan    | Not supported.                                                    |
| a &gt;&gt;&gt; b | operator\_tripleGreaterThan | Shift the unsigned bit representation of a to the left by b units | [:Fact:]$(-8 >>> 3) == 536870911$


[:Failure:]
	package io.sarl.docs.reference.gsr
	class X {
		def myfct {
			var a = 5
			var b = 6
			var c = (a <<< b)
		}
	}
[:End:]


Each operator has an associated function name. This function contains
the concrete implementation of the operational semantic of the
operator. This function can be redefined as explained in the 
[operator overloading section](#operator-overloading).

Additional bitwise operators are available into the SARL library, but not associated to any operator:


| Operator         | Operator Semantic                     |
| ---------------- | ------------------------------------- |
| a.bitwiseAnd(b)  | Do a bit-per-bit AND operation.       | [:Fact:]$1.bitwiseAnd(2)$
| a.bitwiseOr(b)   | Do a bit-per-bit OR operation.        | [:Fact:]$1.bitwiseOr(2)$
| a.bitwiseXor(b)  | Do a bit-per-bit XOR operation.       | [:Fact:]$1.bitwiseXor(2)$
| a.bitwiseNot     | Do a bit-per-bit NEGATION operation.  | [:Fact:]$1.bitwiseNot$



## String Operators

The string operators are listed below. These operators are dedicated to strings of characters.


| Operator | Function Name  | Operator Semantic                                  |
| ---------| -------------- | -------------------------------------------------- |
| a + b    | operator\_plus | Concatenate the string representations of a and b. | [:Fact:]$('a' + 'b') == 'ab'$



Each operator has an associated function name. This function contains
the concrete implementation of the operational semantic of the
operator. This function can be redefined as explained in the 
[operator overloading section](#operator-overloading).


## Number Range operators

This section presents a collection of operators that define ranges of values.


| Operator   | Function Name                  | Operator Semantic                                                               |
| ---------- | ------------------------------ | ------------------------------------------------------------------------------- |
| a .. b     | operator\_upTo                 | Create a list of integer values from a (inclusive) to b (inclusive).            \
                                                `1..5` is the range from 1 to 5 with 1 &lt;= x &lt;= 5.                         \
                                                [:Fact:]$1..5$                                                                  \
                                                `5..1` is the range from 5 to 1 with 5 &gt;= x &gt;= 1.                         \
                                                [:Fact:]$5..1$                                                                  \
                                                The type of this expression is [:integerrange:].                                \
                                                [:Fact:]$typeof(org.eclipse.xtext.xbase.lib.[:integerrange](IntegerRange))$     |
| a &gt;.. b | operator\_greaterThanDoubleDot | Create a list of integer values from a (exclusive) to b (inclusive).            \
                                                `5&gt;..1` is the range from 4 to 1 with 5 &gt; x &gt;= 1.                      \
                                                [:Fact:]$1>..5$                                                                 \
                                                `1&gt;..5` is the empty range since the constraint is wrong 1 &gt; x &gt;= 5.   \
                                                [:Fact:]$5>..1$                                                                 \
                                                See [Xtext](https://bugs.eclipse.org/bugs/show_bug.cgi?id=443258) for           \
                                                discussion on the operational semantics of this operator.                       \
                                                The type of this expression is [:exclusiverange:].                              \
                                                [:Fact:]$typeof(org.eclipse.xtext.xbase.lib.[:exclusiverange](ExclusiveRange))$ |
| a ..&lt; b | operator\_doubleDotLessThan    | Create a list of integer values from a (inclusive) to b (exclusive).            \
                                                `1..&lt;5` is the range from 1 to 5 with 1 &lt;= x &lt; 5.                      \
                                                `5..&lt;1` is the empty range since the constraint is wrong 5 &lt;= x &lt; 1.   \
                                                See [Xtext](https://bugs.eclipse.org/bugs/show_bug.cgi?id=443258) for           \
                                                discussion on the operational semantics of this operator.                       \
                                                The type of this expression is [:exclusiverange:].                              |


Each operator has an associated function name. This function contains
the concrete implementation of the operational semantic of the
operator. This function can be redefined as explained in the 
[operator overloading section](#operator-overloading).


## Collection Operators

The collection operators are listed below. These operators are dedicated to the collections (lists, sets, maps...)
Most of the time, the first operand is the collection on which the operator must be applied.


| Operator              | Function Name      | Operator Semantic                                                                          |
| --------------------- | ------------------ | ------------------------------------------------------------------------------------------ |
| [:c](c) += [:e](e)    | operator\_add      | Equivalent to: `c.add(e)`                                                                  |
| c -= e                | operator\_remove   | Equivalent to: `c.remove(e)`                                                               |
| [:c1](c1) + [:c2](c2) | operator\_plus     | Create a collection that is containing the elements of the collections [:c1:] and [:c2:].  |
| [:m](m) + [:p](p)     | operator\_plus     | Create a map of type `Map<A,B>` that is containing the elements of the map [:m:]           \
                                               and the new pair [:p:] of type `Pair<A,B>`.                                                |
| m - p                 | operator\_moins    | Create a map of type `Map<A,B>` that is containing the elements of the map [:m:], except   \
                                               the pair [:p:] of type `Pair<A,B>`.                                                        |
| a -&gt; b             | operator\_mappedTo | Create an instance of `Pair<A,B>` where `A` and `B` are the types of a and b respectively. |


[:Fact:]$var c = newArrayList; c += 'a'; c == #['a']$
[:Fact:]$var c = newArrayList('a', 'b'); c -= 'a'; c == #['b']$
[:Fact:]$var c1 = #['a']; var c2 = #['b']; var d = c1 + c2; d.similarTo(#['a', 'b'])$
[:Fact:]$var m = #{4 -> 'a'}; var p = 5 -> 'b'; var d = m + p; d.similarTo(#{4 -> 'a', 5 -> 'b'})$
[:Fact:]$var m = #{4 -> 'a', 5 -> 'b'}; var p = 5 -> 'b'; var d = m - p; d.similarTo(#{4 -> 'a'})$
[:Fact:]$var pair = 4 -> 'a'; pair instanceof org.eclipse.xtext.xbase.lib.Pair$
[:Fact:]$var pair = 4 -> 'a'; pair.key == 4$
[:Fact:]$var pair = 4 -> 'a'; pair.value == 'a'$


Each operator has an associated function name. This function contains
the concrete implementation of the operational semantic of the
operator. This function can be redefined as explained in the 
[operator overloading section](#operator-overloading).


## Other operators

This section presents a collection of operators that are not related to the categories in the previous sections.


| Operator               | Function Name         | Operator Semantic                                                                 |
| ---------------------- | --------------------- | --------------------------------------------------------------------------------- |
| a ?: b                 | operator\_elvis       | If a is not null then a else b.                                                   |
| a [:mapto](=&gt;) b    | operator\_doubleArrow | Used as a  'with'- or 'let'-operation. It allows you to bind an object to a local \
                                                   scope in order to do something on it. b must be a lambda expression.              |
| a &lt;&gt; b           | operator\_diamond     | Not yet supported.                                                                |

[:Fact:]$(null ?: 'a') == "a"$
[:Fact:]$('b' ?: 'a') == "b"$
[:Failure:]
	package io.sarl.docs.reference.gsr
	class X {
		def myfct {
			var a = 5
			var b = 6
			var c = (a <> b)
		}
	}
[:End:]


Each operator has an associated function name. This function contains
the concrete implementation of the operational semantic of the
operator. This function can be redefined as explained in the 
[operator overloading section](#operator-overloading).

For an example of the [:mapto:] operator, consider the class `Person` with two attributes inside: [:attr1:] and [:attr2:].
The creation of an instance of `Person` could be done with:

[:Failure:]
	package io.sarl.docs.reference.gsr
	class X {
		def myfct : Object {
			[:On]
			new Person => [
				[:attr1](firstName) = 'Han'[:semicolon](;)
				[:attr2](lastName) = 'Solo'
			]
			[:Off]
		}
	}
[:End:]


> **_Note:_** Note how the [:semicolon:] allows two expressions on one line.

In this example, the instance of Person is created and passed to the
lambda expression. Inside this expression, the new Person instance is accessible with the `it`
reserved pseudo-variable, which does not need to be typed out since it is the default object in
lambda expression. The lambda expression replies the value of `it`.


## Operator Precedence

The following table lists the precedence and associativity of SARL operators. Operators are listed top to bottom,
in ascending precedence, i.e. from the lower priority to the higher priority.


| Operators                                           | Associativity   |
| --------------------------------------------------- | --------------- |
| =                                                   | right to left   |
| ||                                                  | left to right   |
| &&                                                  | left to right   |
| ==, !=, ===, !==                                    | left to right   |
| >=, <=, <, >                                        | left to right   |
| instanceof                                          | not associative |
| <=>, <>, .., >.., ..<, ->, =>, ?:, >>, <<, >>>, <<< | left to right   |
| +, -                                                | left to right   |
| *, /, %                                             | left to right   |
| as                                                  | left to right   |
| **                                                  | left to right   |
| !, - (unary), + (unrary)                            | right to left   |
| ++, --                                              | not associative |


[:Fact:]{
	#[
		#[ "$v=$i" ],
		#[ "$i||$i" ],
		#[ "$i&&$i" ],
		#[ "$i==$i", "$i!=$i", "$i===$i", "$i!==$i" ],
		#[ "$i>=$i", "$i<=$i", "$i<$i", "$i>$i" ],
		#[ "$o instanceof $t" ],
		#[ "$i<=>$i", "$i<>$i", "$i..$i", "$i>..$i", "$i..<$i", "$i->$i", "$i=>$i", "$i?:$i", "$i>>$i", "$i<<$i", "$i>>>$i", "$i<<<$i" ],
		#[ "$i+$i", "$i-$i" ],
		#[ "$i*$i", "$i/$i", "$i%$i" ],
		#[ "$L as $t" ],
		#[ "$i**$i" ],
		#[ "!$R", "-$R", "+$R" ],
		#[ "$v++", "$v--" ]
	].validateOperatorOrder
}


When parsing an expression, an operator which is listed on some row of the table above with a precedence will
be bound tighter (as if by parentheses) to its arguments than any operator that is listed on a row further 
above it with a lower precedence.

For example, the expressions `c << a == b` and `-p++` are parsed as `(c << a) == b` and `-(p++)`, and not as
`c << (a == b)` or `(-p)++`.

Operators that have the same precedence are bound to their arguments in the direction of their associativity.
For example, the expression `a = b = c` is parsed as `a = (b = c)`, and not as `(a = b) = c` because of
right-to-left associativity of assignment, but `a + b - c` is parsed `(a + b) - c` and not `a + (b - c)`
because of left-to-right associativity of addition and subtraction.
Associativity specification is redundant for unary operators and is only shown for completeness:
unary postfix operators always associate left-to-right.
Note that the associativity is meaningful for member access operators, even though they are grouped with
unary postfix operators: `a.b++` is parsed `(a.b)++` and not `a.(b++)`.
Operator precedence is unaffected by operator overloading.


## Operator Overloading

In SARL, it is easy to overload or re-define an existing operator.

You should define the operator mapping function (see the previous sections for a comprehensive list of them).

The following example, the addition operator [:plusop] for two [:pairtype:] objects is defined.
The function that is defining the operator must be named with the [:operatorprefix:] prefix, and have one parameter
for each operand associated with the operator. In the example, the addition of two pairs [:pair1:] and [:pair2:].
gives the pair (a,d).
 
[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		[:On]
		def [:operatorprefix](operator_)plus(
					a : [:pairtype](Pair)<Integer,Integer>,
					b : Pair<Integer,Integer>) : Pair<Integer,Integer> {
			return new Pair(a.key, b.value)
		}

		def example {
			var x = [:pair1](1 -> 3)
			var y = [:pair2](4 -> 5)
			
			// Old-fashion-style call to the overloaded operator
			var z1 = operator_plus(x, y)
			
			// Operator-style call to the overloaded operator
			var z2 = x [:plusop](+) y 
			
			// z1 == (1 -> 5)
			println(z1.toString)
			
			// z2 == (1 -> 5)
			println(z2.toString)
		}
		[:Off]
	}
[:End:]

In addition to the overloading of the operators that are described on this page, it 
is possible to overload the casting operator. See [details](./Cast.md).


[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
