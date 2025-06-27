# Statement Syntax

In SARL, statements are instructions that will be executed.
A statement may be one of the elements described in the rest of this document.

As compared to programming languages as Java and C++, there is no need to terminate
a statement with the `;` character.
Use of this statement terminator is optional, and used if you want to
put multiple statements on a line. To continue a statement on the next
line, you end the line with the `\` character.

For instance, the two following lines are equivalent:

[:Success:]
	package io.sarl.docs.reference.gsr
	class MyType {
		def myaction {
			[:On]
			var myVariable : int = 5
			[:Off]
		}
	}
[:End:] 


[:Success:]
	package io.sarl.docs.reference.gsr
	class MyType {
		def myaction {
			[:On]
			var myVariable : int = 5;
			[:Off]
		}
	}
[:End:] 

[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
