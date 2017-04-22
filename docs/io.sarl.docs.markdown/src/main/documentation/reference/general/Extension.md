# Extension Methods and Available Extensions

Extension methods allow you to add new methods to existing types without modifying them. This is really
helpful as they can greatly improve the readability. Extension methods use a simple syntactic trick:
the first parameter of a method can either be passed in after opening the parentheses or before the
method call. For example, given a method:

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent X {
				[:On]
				def removeVowels (s : String) {
					s.replaceAll("[aeiouyAEIOUY]", "")
				}
				[:Off]
			}
		[:End:]

We can call this method either like in Java or C++ languages:

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent X {
				def removeVowels (s : String) {
					s.replaceAll("[aeiouyAEIOUY]", "")
				}
				def action {
					[:On]
					removeVowels("Hello")
					[:Off]
				}
			}
		[:End:]


or as an extension method of String:

		[:Success:]
			package io.sarl.docs.reference.gsr
			agent X {
				def removeVowels (s : String) {
					s.replaceAll("[aeiouyAEIOUY]", "")
				}
				def action {
					[:On]
					"Hello".removeVowels
					[:Off]
				}
			}
		[:End:]


## Import static extension methods

You can import static methods as extensions, and directly call the imported static methods.

In the following example, the `[:sort!](List)` function is provided by the [:collections:] utility class.
This function is invoked with the extension method syntax.

		[:Success:]
			package io.sarl.docs.reference.gsr
			[:On]
			import static extension java.util.[:collections](Collections).*
			agent MyAgent {
				def example {
					val colors : String[] = #["red", "blue", "green"]
					colors.[:sort](sort) 
				}
			}
		[:End:]


## Local extension methods

All visible non-static methods of the current class and its super types are automatically available as extensions.

In the following example, the [:hasoneelement:] function is invoked with the extension method syntax.

		[:Success:]
			package io.sarl.docs.reference.gsr
			import java.util.List
			agent A {
				[:On]
				// Define an extension method for List
				def [:hasoneelement](hasOneElement)(list : List<?>) : boolean {
					list.size == 1
				}
				
				// Invoke the extension method
				def example : boolean {
					newArrayList("red").hasOneElement
				}
				[:Off]
			}
		[:End:]


## Extension Provider

By adding the [:extensionkw:] keyword to a field, a local variable, or a parameter declaration, its
instance methods become extension methods.

<caution>The [:extensionkw:] keyword must be always written just before the [:valkw:] or
[:varkw:] keywords for fields and local variables, or before the parameter's name.</caution>

<caution>In the case of an extension field, you must give the type of the field because the type resolved
of SARL cannot infer the type of the field.</caution>

In the following example, the extension provider is a field. The function `[:contains!](Object)`
is called with the extension method syntax. The extension provider is the field [:list:].
Because of the extension method, the call to [:contains:] is equivalent to `[:list!].[:contains!]([:value!])`.


		[:Success:]
			package io.sarl.docs.reference.gsr
			import java.util.ArrayList
			[:On]
			class ExtensionProviderExamples {
				[:extensionkw](extension) [:varkw](var) [:list](list) : ArrayList<String> = newArrayList
				
				def extensionFieldExample(value : String) : boolean {
					[:value](value).[:contains](contains)
				}
			}
		[:End:]

In the following example, the extension provider is the formal parameter [:o:].
Because of the extension method, the call to [:contains:] is equivalent to `[:o!].[:contains!]([:value!])`.

		[:Success:]
			package io.sarl.docs.reference.gsr
			import java.util.ArrayList
			[:On]
			class ExtensionProviderExamples {
				[:On]
				def extensionParameterExample(value : String, extension [:o](o) : ArrayList<String>) : boolean {
					[:value!].[:contains!]
				}
				[:Off]
			}
		[:End:]


In the following example, the extension provider is the local variable [:o:].
Because of the extension method, the call to [:contains:] is equivalent to `[:o!].[:contains!]([:value!])`.

		[:Success:]
			package io.sarl.docs.reference.gsr
			import java.util.ArrayList
			class ExtensionProviderExamples {
				[:On]
				def extensionLocalVariableExample(value : String) : boolean {
					extension [:valkw](val) [:o!] : ArrayList<String> = newArrayList
					[:value!].[:contains!]
				}
				[:Off]
			}
		[:End:]


## Imported Extensions

The following sections describe the extensions (in alphabetical order) that are automatically imported in a SARL script.
The extensions are described into categories.


### Collection Category

The [:arrayext:] class extends the class `Array` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:arrayext]$ArrayExtensions$)

The [:colext:] class extends the class `Collection` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:colext]$CollectionExtensions$)

The [:iterext:] class extends the class `Iterable` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:iterext]$IterableExtensions$)

The [:iterext2:] class extends the class `Iterator` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:iterext2]$IteratorExtensions$)

The [:listext:] class extends the class `List` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:listext]$ListExtensions$)

The [:mapext:] and [:sarlmapext:] classes extend the class `Map` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:mapext]$MapExtensions$)

		[:ShowType:](io.sarl.lang.scoping.batch.[:sarlmapext]$SARLMapExtensions$)


### Number Category

The [:bdecext:] class extends the class `BigDecimal` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:bdecext]$BigDecimalExtensions$)

The [:bintext:] class extends the class `BigInteger` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:bintext]$BigIntegerExtensions$)

The [:byteext:] class extends the class `Byte` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:byteext]$ByteExtensions$)

The [:dblext:] class extends the class `Double` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:dblext]$DoubleExtensions$)

The [:fltext:] class extends the class `Float` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:fltext]$FloatExtensions$)

The [:intext:] class extends the class `Integer` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:intext]$IntegerExtensions$)

The [:longext:] class extends the class `Long` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:longext]$LongExtensions$)

The [:shortext:] class extends the class `Short` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:shortext]$ShortExtensions$)


### Primitive Type Category

The [:boolext:] class extends the class `Boolean` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:boolext]$BooleanExtensions$)

The [:charext:] class extends the class `Character` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:charext]$CharacterExtensions$)


### Object Type Category

The [:compext:] class extends the class `Comparable` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:compext]$ComparableExtensions$)

The [:objext:] class extends the class `Object` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:objext]$ObjectExtensions$)

The [:strext:] class extends the class `String` with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:strext]$StringExtensions$)


### Functions and Procedure Category

The [:funcext:] class extends the function type with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:funcext]$FunctionExtensions$)

The [:procext:] class extends the procedure type with the following functions:

		[:ShowType:](org.eclipse.xtext.xbase.lib.[:procext]$ProcedureExtensions$)


### Time Category

The [:timeext:] class extends the number classes with the following time-based functions:

		[:ShowType:](io.sarl.lang.scoping.batch.[:timeext]$SARLTimeExtensions$)




[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
