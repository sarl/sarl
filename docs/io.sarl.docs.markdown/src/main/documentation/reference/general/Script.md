# Script Format

[:Outline:]

A script is a text file containing SARL code. Each script must follow the format:

```text
<package declaration>
<imports>
<top-level features>
```


## Package Declaration

To structure your software, it is common to put scripts in different packages (as Java does for the classes).

The keyword [:packagekw:] permits you to define the name of the package associated with a
SARL file. Consequently, all the features defined in the script are contained in this package,
and their names are qualified with the name of the package.

The package's name also affects the generation of the Java files implementing the SARL script. Indeed,
all the Java files are generated in a folder with the name of the package.

In the following example, the qualified name of an agent defined in the file is [:packagename:].

[:Success:]
	[:On]
	[:packagekw](package) [:packagename](io.sarl.docs.reference.gsr)
	[:Off]
	agent A { }
[:End:]

> **_Note:_** If the [:packagekw:] keyword is not used, the default package will be used. The default
> package has an empty name. It is recommended in the SARL Best Practices to specify a package's
> name. 


## Import Directive

The _imports_ section of a SARL script is dedicated to declaring imported classes.
Each time you want to use a feature defined in another package, (a different file)
you include it with the [:importkw:] directive.

> **_Note:_** This directive works in a similar way as in the Java language.

The [:importkw:] keyword is followed by the qualified name of the feature to import.
In the following code, the first directive imports [:importvalue1:].

Optionally, you can import all the features defined by a package.
This can be done by replacing the name of the feature by the
wildcard character [:importvalue3:]. The second import directive is an example of the inclusion of
all the classes defined in [:importvalue2:].

[:Success:]
	package io.sarl.docs.reference.gsr
	[:On]
	[:importkw](import) [:importvalue1](java.util.List)
	import [:importvalue2](java.net).[:importvalue3](*)
	[:Off]
	agent A { }
[:End:]


## Static Import Directive

Sometimes, it is necessary to import a class to access its statically defined functions,
i.e., a function that can be called without any associated object's instance.

To do this, you may invoke the static function with the fully qualified name of the
class. For example, the function [:maxfct:] is invoked with this syntax in the example below.

[:Success:]
	package io.sarl.docs.reference.gsr
	import java.util.Collection
	[:On]
	import java.util.Collections

	//...

	[:Off]
	agent A {
		def example {
			[:On]
			var col : Collection<Integer>
			Collections::[:maxfct](max)(col)
			[:Off]
		}
	}
[:End:]


As an alternative, the static-import mechanism permits you to reference the function directly.
A static import is specified with the [:statickw:] keyword just after the [:importkw:] keyword.
The following identifier must be a fully qualified name of one or more functions (with the
wildcard character).

In the example below, all the functions defined in [:collectiontype:] are imported.
Following this import, it is possible to invoke a static function with its unqualified
name, as the call to [:maxfct:] below.

[:Success:]
	package io.sarl.docs.reference.gsr
	import java.util.Collection
	[:On]
	import [:statickw](static) [:collectiontype](java.util.Collections).*

	//...

	[:Off]
	agent A {
		def example {
			[:On]
			var col : Collection<Integer>
			[:maxfct!](col)
			[:Off]
		}
	}
[:End:]


## Top-Level Features

Most of a SARL script consists of the definitions of top-level features. These features are the
core concepts of SARL, such as [:agentkw:], [:eventkw:], or [:capacitykw:].
All these top-level features are documented in their own reference documents.

[:Success:]
	[:On]
	package io.sarl.docs.reference.gsr
	[:agentkw](agent) MyAgent {
	}
	
	[:eventkw](event) MyEvent
	
	[:capacitykw](capacity) MyCapacity {
	}
[:End:]


Additionally, it is possible to write object-oriented statements with
the SARL syntax for [:classkw:], [:interfacekw:], [:enumkw:], and [:annotationkw:].
The inclusion of these object-oriented statements will help you to write your
application with a single language: SARL. The object-oriented programmation support
is described in the [reference documentation](../OOP.md).

[:Success:]
	[:On]
	package io.sarl.docs.reference.gsr
	
	[:classkw](class) MyClass {
	}
	
	[:interfacekw](interface) MyInterface {
	}
	
	[:enumkw](enum) MyEnum {
		CONSTANT
	}
	
	[:annotationkw](annotation) MyAnnotation {
	}
[:End:]



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
