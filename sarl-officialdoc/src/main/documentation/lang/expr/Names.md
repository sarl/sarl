# Name Syntax

In SARL, the names of the features (agents, variables, fields, etc.)
cannot be one of the keywords of SARL or Java.
For example, since [:eventkw:] is a keyword in SARL, the following is illegal:

[:Failure:]
	package io.sarl.docs.reference.gsr
	[:On]
	import io.sarl.[:eventkw](event).ActionEvent
[:End:] 

To solve this problem (since some names come from Java, and
this language has different keywords than SARL), it
is possible to prefix the name fragment with the character `^`:

[:Success:]
	package io.sarl.docs.reference.gsr
	[:On]
	import io.sarl.^[:eventkw!].ActionEvent
[:End:] 


[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
