# Block Expressions

The block expression consists of a sequence of
expressions. 


## General Syntax

A block expression is surrounded by curly braces. The expressions in a block can be terminated
by an optional semicolon.

```text
[:beginblock!]
	<sequence of expressions>
[:endblock!]
```

## Type and Value of a Block Expression

The value of the last expression in the block is returned as the value of the
complete block.

The type of a block is the returned type (i.e., of the last expression). Empty blocks return
`null` and have the type `Object`.

In the following example, the block type is [:blocktype:] because the last expressions in all the
possible execution paths are all of type [:blocktype:].

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var greeting = "abc"
		def block : [:blocktype](String) [:On][:beginblock]({)
			var x = greeting
			if (x == "Hello") {
				x + " World!" 
			} else {
				x
			}
		[:endblock](})[:Off]
	}
[:End:]



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
