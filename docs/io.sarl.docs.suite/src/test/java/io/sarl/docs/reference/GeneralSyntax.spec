/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors and authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.docs.reference

import com.google.inject.Inject
import io.sarl.docs.utils.SARLParser
import io.sarl.docs.utils.SARLSpecCreator
import io.sarl.lang.sarl.SarlAction
import io.sarl.lang.sarl.SarlAgent
import io.sarl.lang.sarl.SarlEvent
import java.math.BigDecimal
import java.math.BigInteger
import java.util.List
import java.util.Map
import org.eclipse.xtend.core.xtend.XtendConstructor
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.XBlockExpression
import org.eclipse.xtext.xbase.XbasePackage
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse

/* @outline
 * 
 * <p>This document describes the general syntax of the SARL Language. 
 */
@CreateWith(SARLSpecCreator)
describe "General Syntax Reference" {

	@Inject extension SARLParser
	@Inject extension ValidationTestHelper
	
	/* SARL, like Java, is a statically typed language. In fact, it completely supports 
	 * Java's type system, including the primitive types like _int_ or _boolean_, 
	 * arrays and all the Java classes, interfaces, enumerations and annotations that reside 
	 * on the class path.
	 * 
	 * <p>Java generic types are fully supported as well: you can define type parameters on 
	 * methods and classes and pass type arguments to generic types just as you are 
	 * used to from Java. The type system and its conformance and casting rules are 
	 * implemented as defined in the
	 * [Java Language Specification](http://docs.oracle.com/javase/specs/jls/se5.0/html/conversions.html).
	 * 
	 * <p>One of the problems with Java is that you are forced to write type signatures 
	 * over and over again. That is why so many people do not like static typing. 
	 * But this is in fact not a problem of static typing, but simply a problem with 
	 * Java. Although SARL is statically typed just like Java, you rarely have to 
	 * write types down because they can be computed from the context.
	 * 
	 * <p>In addition to Java's auto-boxing to convert primitives to their corresponding wrapper 
	 * types (e.g. _int_ is automatically converted to _Integer_ when needed), there are 
	 * additional conversion rules in SARL: arrays are automatically converted to
	 * `List<ComponentType>` and vice versa.
	 *
	 * <p>Resembling and supporting every aspect of Java's type system ensures that there is 
	 * no impedance mismatch between Java and SARL. __This means that SARL and Java are 
	 * 100% interoperable__. There are no exceptional cases and you do not have to 
	 * think in two worlds. You can invoke SARL code from Java and vice versa without any
	 * surprises or hassles.	
	 * 
	 * @filter(.*)
	 */
	fact "Java Interoperability" {
		typeof(List) should not be _
	}

	/* In SARL, the names of the features (agents, variables, fields, etc.)
	 * cannot be one of the keywords of SARL or Java.
	 * For example, it is forbidden to type:
	 * 
	 *     import io.sarl.event.ActionEvent 
	 *
	 *
	 * <p>Indeed, the name fragment `event` corresponds to a keyword
	 * of SARL.
	 * 
	 * <p>For solving this problem (since some names come from Java, and
	 * this language has not the same set of keywords than SARL), it
	 * is possible to prefix the name fragment with the character `^`:
	 * 
	 *     import io.sarl.^event.ActionEvent 
	 * 
	 * @filter(.*)
	 */
	fact "Name Syntax" {
		'''package io.sarl.event.ActionEvent'''.parseWithError
		
		var model = '''package io.sarl.^event.ActionEvent'''.parseSuccessfully
		
		model should havePackage "io.sarl.event.ActionEvent"
	}

	/* In SARL, the statements are the instructions that must be executed.
	 * The statement may be one of the elements described in the rest of
	 * this document.
	 * 
	 * <p>In the opposite as programming languages as Java and C++, there is no
	 * need to terminate a statement with the ```;``` character.
	 * But, you are still able to put it in your code.
	 * 
	 * <p>For instance, the two following lines are equivalent:
	 * 
	 *     var myVariable : int = 5
	 *
	 *     var myVariable : int = 5;
	 * 
	 * @filter(.*)
	 */
	fact "Statement Syntax" {
		'''var myVariable : int = 5'''.parseSuccessfully(
			"agent A1 {",
			"}"
		)
		'''var myVariable : int = 5;'''.parseSuccessfully(
			"agent A1 {",
			"}"
		)
	}

	/* A script is a file in which you must type the SARL code.
	 * Each script must follow the format:
	 * 
	 *     <package declaration>
	 *     <imports>
	 *     <top-level features>
	 */
	describe "Script Format" {

		/*
		 * For structuring your software, it is convenient to put the scripts
		 * in different packages (as Java does for the classes).
		 * 
		 * <p>The keyword `package` permits to define the name of
		 * the package associated to a SARL file. Consequently, all
		 * the features defined in the script are defined in this package,
		 * and their names are qualified with the name of the package.
		 * 
		 * <p>The package's name has also a consequence in the generation of
		 * the Java files behind the SARL script. Indeed, all the
		 * Java files are generated in a folder, which is matching the
		 * name of the package.
		 * 
		 * <p>In the following example, the qualified name of the agent is
		 * `io.sarl.docs.reference.gsr`.
		 * 
		 * <note> If the 
		 * `package` keyword is not used, the default package will
		 * be used. The default package has an empty name.
		 * It is recommended in the SARL Best Practices to specify a package's
		 * name.</note> 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Package Declaration" {
			var model = '''
				package io.sarl.docs.reference.gsr
				'''.parseSuccessfully(
				// TEXT
				"agent A {}")
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
			
			model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should haveNbElements 0
			]
		}	 
		
		/* The _imports_ part of a SARL script is dedicated to the 
		 * declaration of the imported classes.
		 * Each time you want to use a feature defined in another package
		 * than the one of your SARL script, you should include it
		 * with the `import` directive.
		 * 
		 * <note>This directive 
		 * works in a similar way as in the Java language.</note>
		 * 
		 * <p>The `import` keyword is followed by the qualified name
		 * of the feature to import. In the following code, it is illustrated
		 * by the first directive.
		 * 
		 * <p>Optionally, you could import all the features defined inside a package.
		 * This could be done by replacing the name of the feature by the
		 * wildcard character `*`. The second import directive is
		 * an example of the inclusion of all the classes defined in
		 * `java.net`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Import Directive" {
			var model = '''
			import java.util.List
			import java.net.*
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT
				"agent A {}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr";
				it should haveNbImports 2;
				it should importClass "java.util.List";
				it should importClassesFrom "java.net";
				it should haveNbElements 1;
			]
			
			model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 0
			]
		}
	
		/* Sometimes, it is mandatory to import a class for accessing its
		 * statically defined functions, i.e. a function that can be
		 * called without any associated object's instance.
		 * 
		 * <p>In this case, the name of the static function is qualified
		 * by the fully qualified name of the class. For example, 
		 * the function `max()` is invoked with this
		 * syntax, in the example below.
		 * 
		 * <p>However, if there is plenty of invocations to static
		 * methods in your source code, the static-import mechanism
		 * permits to make the code more readable by removing the
		 * fully qualified name of the classes, in which the called
		 * functions are defined.
		 * 
		 * <p>A static import is specify with the `static`
		 * keyword just after the `import` keyword.
		 * The following identifier must be a fully qualified name of
		 * one or more functions (with the wildcard character).
		 * In the example below, all the functions defined in
		 * `java.util.Arrays` are imported.
		 * <pre><code>import static java.util.Arrays.*</code></pre>
		 * 
		 * <p>Then,
		 * it is possible to invoke one of them by typing its
		 * name, as the call to `toString(int[])` below.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Static Import Directive" {
			var model = '''
				def example {
					var col : Collection<Integer>
					var tab : int[]

					// Explicit call to a static method
					println( Collections::max(col) )
					
					// Short hand for calling a static method, when statically imported
					println( toString(tab) )
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.util.Collection
				import java.util.Collections
				import static java.util.Arrays.*			
				agent A {",
				// TEXT 
				"}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 3
				it should importClass "java.util.Collection"
				it should importClass "java.util.Collections"
				it should importMembers "java.util.Arrays"
				it should haveNbElements 1
			]
			
			var a = model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 1
			]
			
			var sig = (a as SarlAgent).members.get(0) => [
				it should beAction "example";
				it should reply _;
				it should haveNbParameters 0
				it should beVariadic false
			]
			
			// Do not test the block content since it should be validated by the Xbase library.
			(sig as SarlAction).expression should be XBlockExpression
		}	 

		/* A large part of a SARL script contains the definitions of
		 * the top-level features. These features are the core concepts
		 * of SARL, such as `agent`, `event`, or
		 * `capacity`.
		 * All these top-level features are documented in their own
		 * reference document.
		 *
		 * <p>Additionally, it is possible to write object-oriented statements with
		 * the SARL syntax: `class`, `interface`, `enum`, `annotation`.
		 * The inclusion of the object-oriented statements will help you to
		 * write your application  with a single language.
		 * This support is described in the
		 * [reference documentation](./BasicObjectOrientedProgrammingSupportSpec.html)
		 * on the object-oriented programming in SARL.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Top-Level Features" {
			"./BasicObjectOrientedProgrammingSupportSpec.html" should beAccessibleFrom this
			//
			var model = '''
			event E {
			}

			capacity C {
			}
			
			skill S implements C {
			}

			behavior B {
			}

			agent A {
			}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 5
			]
			
			model.xtendTypes.get(0) => [
				it should beEvent "E"
				it should extend _
				it should haveNbElements 0
			]
			
			model.xtendTypes.get(1) => [
				it should beCapacity "C"
				it should extend _
				it should haveNbElements 0
			]
			
			model.xtendTypes.get(2) => [
				it should beSkill "S"
				it should extend _
				it should haveNbImplements 1
				it should implement #["io.sarl.docs.reference.gsr.C"]
				it should haveNbElements 0
			]
			
			model.xtendTypes.get(3) => [
				it should beBehavior "B"
				it should extend _
				it should haveNbElements 0
			]
			
			model.xtendTypes.get(4) => [
				it should be SarlAgent "A"
				it should extend _
				it should haveNbElements 0
			]
		}	 

	}

	/* A literal denotes a fixed, unchangeable value. Literals for 
	 * strings, numbers, booleans, null and Java types are 
	 * supported as well as literals for unmodifiable collection 
	 * types like lists, sets and maps or literals for arrays.
	 */	
	describe "Literals"{
	
		/* A string literal is of type `String`. 
		 * String literals are enclosed in a pair of single quotes 
		 * or double quotes. Special characters can be 
		 * quoted with a backslash or defined using unicode 
		 * notation.
		 * Contrary to Java, strings can span multiple lines.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "String Literals"{
			var model = '''
				var a = 'Hello World!'
				var b = "Hello World!"
				var c = 'Hello "World!"'
				var d = "Hello \"World!\""
				var e = "Hello 
							World!"
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
			
			var a = model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 5
			]

			(a as SarlAgent).members.get(0) => [
				it should beVariable "a"
				it should haveType _;
				it should haveInitialValue "Hello World!"
			]

			(a as SarlAgent).members.get(1) => [
				it should beVariable "b"
				it should haveType _
				it should haveInitialValue "Hello World!"
			]

			(a as SarlAgent).members.get(2) => [
				it should beVariable "c"
				it should haveType _
				it should haveInitialValue "Hello \"World!\""
			]

			(a as SarlAgent).members.get(3) => [
				it should beVariable "d"
				it should haveType _
				it should haveInitialValue "Hello \"World!\""
			]

			(a as SarlAgent).members.get(4) => [
				it should beVariable "e"
				it should haveType _
				it should haveInitialValue "Hello \n			World!"
			]
		}
	
		/* Character literals use the same notation as String literals. 
		 * If a single character literal is used in a context where a 
		 * primitive char or the wrapper type `Character` is expected, 
		 * the compiler will treat the literal as a value 
		 * or instance.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Character Literals"{
			var model = '''
				var a : char = 'a'
				var b : char = "b"
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
			
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 2
			]) as SarlAgent
			
			a.members.get(0) => [
				it should beVariable "a"
				it should haveType "char"
				it should haveInitialValue 'a'
			]
			
			a.members.get(1) => [
				it should beVariable "b"
				it should haveType "char"
				it should haveInitialValue 'b'
			]
		}
			
		/* SARL supports roughly the same number literals as Java.
		 * There are two exceptions: there is no notation for specifying octal numbers, and 
		 * if you put the dot character in a number, you must specify the fractional and mantissa parts.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Number Literals"{
			var model = '''
				var a = 42		// Decimal
				var b = 0xbeef	// Hexadecimal
				var c = 077		// Decimal 77, NOT octal
				var d = 0.1		// The leading zero must be specified
				var e = 1.0		// The trailing zero must be specified
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
			
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 5
			]) as SarlAgent
			
			a.members.get(0) => [
				it should beVariable "a"
				it should haveType _
				it should haveInitialValue (42 as Object)
			]
			
			a.members.get(1) => [
				it should beVariable "b"
				it should haveType _
				it should haveInitialValue (0xbeef as Object)
			]

			a.members.get(2) => [
				it should beVariable "c"
				it should haveType _
				it should haveInitialValue (77 as Object)
			]

			a.members.get(3) => [
				it should beVariable "d"
				it should haveType _
				it should haveInitialValue (0.1 as Object)
			]

			a.members.get(4) => [
				it should beVariable "e"
				it should haveType _
				it should haveInitialValue (1.0 as Object)
			]
		}

		/* As in Java 7, you can separate digits using `_` for
		 * better readability of large numbers.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Large Numbers"{
			var model = '''
				var a = 123_456_78l
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
			
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 1
			]) as SarlAgent
			
			a.members.get(0) => [
				it should beVariable "a"
				it should haveType _
				it should haveInitialValue (12345678l as Object)
			]
		}

		/* Post-fixing an integer literal may change its type:
		 * no suffix is for `int`,
		 * suffix `L` is for `long`, and
		 * suffix `BI` is for `BigInteger`. 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Integer suffixes"{
			var model = '''
				var anInteger = 1234
				var aLong = 1234l
				var aBigInteger = 1234bi
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]

			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 3
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "anInteger"
				it should haveType _
				it should haveInitialValue (1234 as Object)
			]

			a.members.get(1) => [
				it should beVariable "aLong"
				it should haveType _
				it should haveInitialValue (1234l as Object)
			]

			a.members.get(2) => [
				it should beVariable "aBigInteger"
				it should haveType _
				it should haveInitialValue new BigInteger("1234")
			]
		}

		/* Post-fixing a floating-point literal may change its type:
		 * no suffix is for `double`,
		 * suffix `D` is for `double`,
		 * suffix `F` is for `float`, and
		 * suffix `BD` is for `BigDecimal`. 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Floating-point-value suffixes"{
			var model = '''
				var aDouble = 1234.0
				var anotherDouble = 5678d
				var aFloat = 1234.0f
				var anotherFloat = 5678f
				var aBigDecimal = 1234bd
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]

			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 5
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "aDouble"
				it should haveType _
				it should haveInitialValue (1234.0 as Object)
			]

			a.members.get(1) => [
				it should beVariable "anotherDouble"
				it should haveType _
				it should haveInitialValue (5678d as Object)
			]

			a.members.get(2) => [
				it should beVariable "aFloat"
				it should haveType _
				it should haveInitialValue (1234f as Object)
			]

			a.members.get(3) => [
				it should beVariable "anotherFloat"
				it should haveType _
				it should haveInitialValue (5678f as Object)
			]

			a.members.get(4) => [
				it should beVariable "aBigDecimal"
				it should haveType _
				it should haveInitialValue new BigDecimal("1234")
			]
		}

		/* There are two boolean literals, `true` and `false`
		 * which correspond to their Java counterpart of type `boolean`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Boolean Literals"{
			var model = '''
				var a = true
				var b = false
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr			
				agent A {",
				// TEXT
				"}"
			)
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]

			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 2
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "a"
				it should haveType _
				it should haveInitialValue (true as Object)
			]

			a.members.get(1) => [
				it should beVariable "b"
				it should haveType _
				it should haveInitialValue(false as Object)
			]
		}
	
		/* The null pointer literal `null` has exactly the same
		 * semantics as in Java.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Null Literals"{
			var model = '''
				var a = null
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]

			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 1
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "a"
				it should haveType _;
				it should haveInitialValue "null"
			]
		}
	
		/* The syntax for type literals is generally the plain name of the 
		 * type. Nested types use the delimiter `.`.
		 * 
		 * <p>To disambiguate the expression, type literals may also be specified 
		 * using the keyword `typeof`.
		 * 
		 * <p>Consequently it is possible to access the members of a type 
		 * reflectively by using its plain name.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Type Literals"{
			var model = '''
				// the variable a contains the Java type String.class
				var a = String
				// the variable b contains the Java type Integer.class
				var b = typeof(Integer)
				// the variable c contains the list of the fields 
				// that are declared in the Java type String.class
				var c = String.getDeclaredFields()
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]

			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 3
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "a"
				it should haveType _
				it should haveInitialValue "java.lang.String"
			]

			a.members.get(1) => [
				it should beVariable "b"
				it should haveType _
				it should haveInitialValue "java.lang.Integer"
			]

			a.members.get(2) => [
				it should beVariable "c"
				it should haveType _;
				it should haveInitialValue "java.lang.Class.getDeclaredFields"
			]
		}
	
	}
	
	/* 
	 */
	describe "Collection Literals"{

	   /* It is easy to create instances of collections since the methods in 
		* `CollectionLiterals` are automatically imported.
		* They permit to create instances of the collections from the JDK.
		* 
		* @filter(.* = '''|'''|.parseSuccessfully.*)
		*/
		fact "Collection Creation"{
			var model = '''
				var myList = newArrayList('Hello', 'world')
				var myMap = newLinkedHashMap('a' -> 1, 'b' -> 2)
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]

			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 2
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "myList"
				it should haveType _
				it should haveInitialValue "org.eclipse.xtext.xbase.lib.CollectionLiterals.newArrayList"
			]

			a.members.get(1) => [
				it should beVariable "myMap"
				it should haveType _
				it should haveInitialValue "org.eclipse.xtext.xbase.lib.CollectionLiterals.newLinkedHashMap"
			]
		}
		
		 /* In addition, SARL supports collection literals to create 
		* immutable collections and arrays, depending on the 
		* target type. There are three types of immutable
		* collections: array, set, and hash table.
		* 
		* @filter(.* = '''|'''|.parseSuccessfully.*)
		*/
		fact "Immutable Collections"{
			var model = '''
				// the variable a contains an immutable array.
				var a = #['Hello','World']
				// the variable b contains an immutable set.
				var b = #{'Hello','World'}
				// the variable c contains an immutable hash table.
				var c = #{'a' -> 1 ,'b' ->2}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]

			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 3
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "a"
				it should haveType _
				it should haveInitialValue #['Hello','World']
			]

			a.members.get(1) => [
				it should beVariable "b"
				it should haveType _
				it should haveInitialValue #{'Hello','World'}
			]

			a.members.get(2) => [
				it should beVariable "c"
				it should haveType _
				it should haveInitialValue #{'a' -> 1 ,'b' ->2}
			]
		}
	
	}

	/* Java arrays can be created either using a literal as described in 
	 * the previous section, or if it should be a new array with a 
	 * fixed size.
	 */
	describe "Array Literals"{

		/* The methods from `ArrayLiterals` are automatically
		 * included. This utility class provides a collection of methods,
		 * such as `ArrayLiterals.newArrayOfSize(int)` for
		 * creating array literals.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */		
		fact "Array Creation"{
			var model = '''
				// variable a contains a array of size 400 which contains Objects.
				var a : String[] = newArrayOfSize(400)
				// variable b contains a array of size 200 which contains int values.
				var b : int[] = newIntArrayOfSize(200)
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]

			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 2
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "a"
				it should haveType "java.lang.String[]"
				it should haveInitialValue "org.eclipse.xtext.xbase.lib.ArrayLiterals.newArrayOfSize"
			]

			a.members.get(1) => [
				it should beVariable "b"
				it should haveType "int[]"
				it should haveInitialValue "org.eclipse.xtext.xbase.lib.ArrayLiterals.newIntArrayOfSize"
			]
		}
		
		/* Retrieving and setting values of arrays is done through the extension 
		 * methods `get(int)` and `set(int, T)`.
		 * As for Java, the index of the elements in the array starts with `0`. 
		 * 
		 * <p>The method `length` is available for retrieving the size of the array.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */		
		fact "Array Getter and Setter"{
			var model = '''
				var a = #['Hello', 'world', '!']
				// variable b contains the second element of the array a: 'world'.
				var b = a.get(1)
				// variable c contains the size of the array a: 3.
				var c = a.length
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]

			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 3
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "a"
				it should haveType _
				it should haveInitialValue #['Hello', 'world', '!']
			]

			a.members.get(1) => [
				it should beVariable "b"
				it should haveType _
				it should haveInitialValue "java.util.List.get"
			]

			a.members.get(2) => [
				it should beVariable "c"
				it should haveType _
				it should haveInitialValue"org.eclipse.xtext.xbase.lib.ArrayExtensions.length"
			]
		}

		/* Arrays are automatically converted to lists 
		 * when needed. It is similar to the boxing and unboxing features
		 * provided by Java, between the primitives and their respective object
		 * types.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */		
		fact "Array to List"{
			var model = '''
				val myArray : int[] = #[1,2,3]
				val myList : List<Integer> = myArray
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr			
				import java.util.List
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 1
				it should importClass "java.util.List"
				it should haveNbElements 1
			]

			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 2
			]) as SarlAgent

			a.members.get(0) => [
				it should beValue "myArray"
				it should haveType "int[]"
				it should haveInitialValue #[1,2,3]
			]

			a.members.get(1) => [
				it should beValue "myList"
				it should haveType "java.util.List<java.lang.Integer>"
				it should haveInitialValue "io.sarl.docs.reference.gsr.A.myArray"
			]
		}

	}
	
	/* The cast of an expression to a specific type must be done with
	 * the `as` keyword. This keywords must be typed after the expression,
	 * but before the casting type.
	 * 
	 * <p>The conformance rules for type casts are defined in the
	 * [Java Language Specification](http://docs.oracle.com/javase/specs/jls/se5.0/html/conversions.html#5.5).
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*) 
	 */
	fact "Type Cast" {
		var model = '''
			// the variable something is of type Number.
			var something : Number = new Integer(123)
			// the variable a contains the value of the variable something
			// but casted to Integer
			var a = something as Integer
			
			//Do the convertion from a number literal to an Integer object
			var b = 56 as Integer
		'''.parseSuccessfully(
			"package io.sarl.docs.reference.gsr		
			agent A {",
			// TEXT
			"}"
		)

		model => [
			it should havePackage "io.sarl.docs.reference.gsr"
			it should haveNbImports 0
			it should haveNbElements 1
		]

		var a = (model.xtendTypes.get(0) => [
			it should beAgent "A"
			it should extend _
			it should haveNbElements 3
		]) as SarlAgent

		a.members.get(0) => [
			it should beVariable "something"
			it should haveType "java.lang.Number"
			it should haveInitialValue "java.lang.Integer.Integer"
		]

		a.members.get(1) => [
			it should beVariable "a"
			it should haveType _
			it should haveInitialValue "java.lang.Integer"
		]

		a.members.get(2) => [
			it should beVariable "b"
			it should haveType _
			it should haveInitialValue (56 as Object)
		]
	}

	/* SARL supports a collection of operators. Most of them are infix operators,
	 * and several are postfix operators.
	 */
	describe "Operators" {
		
		/** The arithmetic operators are listed below.
		 * The arithmetic operators take numbers as operands. They could
		 * be unary (one operand) or binary (two operands).
		 * 
		 * <p>Each operator has an associated function name. This function contains
		 * the concrete implementation of the operational semantic of the
		 * operator. This function could be redefined as it is explained in the 
		 * [operator overloading section](#Operator_Overloading).
		 * 
		 * <table><thead>
		 * <tr><th>Operator</th><th>Function Name</th><th>Operator Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a + b</td><td>operator_plus</td><td>Add a and b.</td></tr>
		 * <tr><td>a - b</td><td>operator_minus (binary)</td><td>Subtract b to a.</td></tr>
		 * <tr><td>a * b</td><td>operator_multiply</td><td> Multiply a by b.</td></tr>
		 * <tr><td>a / b</td><td>operator_divide</td><td>Divide a by b.</td></tr>
		 * <tr><td>a % b</td><td>operator_modulo</td><td>Modulo of the division of a by b.</td></tr>
		 * <tr><td>a ** b</td><td>operator_power</td><td>Compute the power b of a.</td></tr>
		 * <tr><td>- a</td><td>operator_minus (unary</td><td>Negate the value of a.</td></tr>
		 * <tr><td>a ++</td><td>operator_plusPlus</td><td>Increment a by 1, reply the value before the incrementation.</td></tr>
		 * <tr><td>a --</td><td>operator_moinsMoins</td><td>Decrement a by 1, reply the value before the decrementation.</td></tr>
		 * </tbody></table>
		 *
		 * @filter(.*) 
		 */
		fact "Arithmetic operators" {
			"#Operator_Overloading" should beAccessibleFrom this
			
			"1 + 2".toInt should be 3
			"1 - 2".toInt should be -1
			"1 * 2".toInt should be 2
			"4 / 2".toInt should be 2
			"3 % 2".toInt should be 1
			"3 ** 2".toInt should be 9
			"var a : int = 7\na++".toInt should be 7
			"var a : int = 7\na++\na".toInt should be 8
			"var a : int = 7\na--".toInt should be 7
			"var a : int = 7\na--\na".toInt should be 6
		}

		/** The comparison operators primitive types are listed below.
		 * 
		 * <p>Each operator has an associated function name. This function contains
		 * the concrete implementation of the operational semantic of the
		 * operator. This function could be redefined as it is explained in the 
		 * [operator overloading section](#Operator_Overloading).
		 * 
		 * <table><thead>
		 * <tr><th>Operator</th><th>Function Name</th><th>Operator Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a == b</td><td>operator_equals</td><td>Test if a and b are equal.</td></tr>
		 * <tr><td>a != b</td><td>operator_notEquals</td><td>Test if a and b are not equal.</td></tr>
		 * <tr><td>a === b</td><td>operator_tripleEquals</td><td>Test if a and b are equal.</td></tr>
		 * <tr><td>a !== b</td><td>operator_tripleNotEquals</td><td>Test if a and b are not equal.</td></tr>
		 * <tr><td>a &lt; b</td><td>operator_lessThan</td><td>Test if a is lower than b (a, b cannot be boolean).</td></tr>
		 * <tr><td>a &gt; b</td><td>operator_greaterThan</td><td>Test if a is greater than b (a, b cannot be boolean).</td></tr>
		 * <tr><td>a &lt;= b</td><td>operator_lessEqualsThan</td><td>Test if a is lower than or equal to b (a, b cannot be
		 *                           boolean).</td></tr>
		 * <tr><td>a &gt;= b</td><td>operator_greaterEqualsThan</td><td>Test if a is greater than or equal to b (a, b cannot be
		 *                           boolean).</td></tr>
		 * <tr><td>a &lt;=&gt; b</td><td>operator_spaceship</td><td>Replies a negative value if a &lt; b, a positive value if
		 *                               a &gt; b, otherwise 0.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Comparison operators on primitive types" {
			"#Operator_Overloading" should beAccessibleFrom this

			"1 == 2".toBool should be false
			"true == false".toBool should be false
			
			"1 != 2".toBool should be true
			"true != false".toBool should be true
			
			"1 === 2".toBool should be false
			"true === false".toBool should be false
			
			"1 !== 2".toBool should be true
			"true !== false".toBool should be true

			"1 < 2".toBool should be true
			"1 > 2".toBool should be false
			"1 <= 2".toBool should be true
			"1 >= 2".toBool should be false

			"1 <=> 2".toInt should be [ it < 0 ]
			"2 <=> 1".toInt should be [ it > 0 ]
			"2 <=> 2".toInt should be [ it == 0 ]
		}

		/** The comparison operators on objects are listed below.
		 * 
		 * <p>Each operator has an associated function name. This function contains
		 * the concrete implementation of the operational semantic of the
		 * operator. This function could be redefined as it is explained in the 
		 * [operator overloading section](#Operator_Overloading).
		 * 
		 * <table><thead>
		 * <tr><th>Operator</th><th>Function Name</th><th>Operator Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a == b</td><td>operator_equals</td><td>Determine if a and b are equal. If a is Comparable then it is
		 *                        equivalent to `a.compareTo(b) == 0` else `a.equals(b)`. This operator is null-safe.</td></tr>
		 * <tr><td>a != b</td><td>operator_notEquals</td><td>Determine if a and b are not equal. If a is Comparable then it is
		 *                        equivalent to `a.compareTo(b) != 0` else `!a.equals(b)` This operator is null-safe.</td></tr>
		 * <tr><td>a === b</td><td>operator_tripleEquals</td><td>Test if a and b are the same object reference. This operator
		 *                        is not null-safe.</td></tr>
		 * <tr><td>a !== b</td><td>operator_tripleNotEquals</td><td>Test if a and b are not the same object reference. This
		 *                        operator is not null-safe.</td></tr>
		 * <tr><td>a &lt; b</td><td>operator_lessThan</td><td>Test if a is lower than b (a must be Comparable).</td></tr>
		 * <tr><td>a &gt; b</td><td>operator_greaterThan</td><td>Test if a is greater than b (a must be Comparable).</td></tr>
		 * <tr><td>a &lt;= b</td><td>operator_lessEqualsThan</td><td>Test if a is lower than or equal to b (a must be
		 *                           Comparable).</td></tr>
		 * <tr><td>a &gt;= b</td><td>operator_greaterEqualsThan</td><td>Test if a is greater than or equal to b (a must be
		 *                           Comparable).</td></tr>
		 * <tr><td>a &lt;=&gt; b</td><td>operator_spaceship</td><td>Replies a negative value if a &lt; b, a positive value if
		 *                               a &gt; b, otherwise 0 (a must be Comparable).</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Comparison operators on objects" {
			"#Operator_Overloading" should beAccessibleFrom this

			"'a' == 'a'".toBool should be true
			"'a' != 'a'".toBool should be false
			"'a' === 'a'".toBool should be false
			"'a' !== 'a'".toBool should be true
			"'a' < 'a'".toBool should be false
			"'a' <= 'a'".toBool should be true
			"'a' > 'a'".toBool should be false
			"'a' >= 'a'".toBool should be true
			"new Integer(1) <=> new Integer(2)".toInt should be [ it < 0 ]
			"new Integer(2) <=> new Integer(1)".toInt should be [ it > 0 ]
			"new Integer(2) <=> new Integer(2)".toInt should be [ it == 0 ]
		}

		/** The boolean operators are listed below.
		 * Each operator takes one or two boolean values as operands, and
		 * replies a boolean value resulting of the operational semantic of the
		 * operator. 
		 * 
		 * <p>Each operator has an associated function name. This function contains
		 * the concrete implementation of the operational semantic of the
		 * operator. This function could be redefined as it is explained in the 
		 * [operator overloading section](#Operator_Overloading).
		 * 
		 * <table><thead>
		 * <tr><th>Operator</th><th>Function Name</th><th>Operator Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a || b</td><td>operator_or</td><td>If a then true else b.</td></tr>
		 * <tr><td>a &amp;&amp; b</td><td>operator_and</td><td>If a then b else false.</td></tr>
		 * <tr><td>! a</td><td>operator_not</td><td>If a then false else true.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Boolean Operators" {
			"#Operator_Overloading" should beAccessibleFrom this

			"true || false".toBool should be true
			"true && false".toBool should be false
			"!true".toBool should be false
			"!false".toBool should be true
		}

		/** The bit operators are listed below.
		 * The bit operators apply operations on the bits that are representing
		 * a numeric value.
		 * 
		 * <p>Each operator has an associated function name. This function contains
		 * the concrete implementation of the operational semantic of the
		 * operator. This function could be redefined as it is explained in the 
		 * [operator overloading section](#Operator_Overloading).
		 * 
		 * <table><thead>
		 * <tr><th>Operator</th><th>Function Name</th><th>Operator Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a &lt;&lt; b</td><td>operator_doubleLessThan</td><td>Shift the signed bit representation of a to the left
		 *                              by b units.</td></tr>
		 * <tr><td>a &gt;&gt; b</td><td>operator_doubleGreaterThan</td><td>Shift the signed bit representation of a to the
		 *                              left by b units.</td></tr>
		 * <tr><td>a &lt;&lt;&lt; b</td><td>operator_tripleLessThan</td><td>Not supported.</td></tr>
		 * <tr><td>a &gt;&gt;&gt; b</td><td>operator_tripleGreaterThan</td><td>Shift the unsigned bit representation of a to
		 *                                  the left by b units.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Bitwise Operators" {
			"#Operator_Overloading" should beAccessibleFrom this

			"1 << 3".toInt should be 8
			"-1 << 3".toInt should be -8

			"8 >> 3".toInt should be 1
			"-8 >> 3".toInt should be -1

			var expr = "1 <<< 3".expression(false)
			expr.assertError(
					XbasePackage.Literals::XBINARY_OPERATION,
					"org.eclipse.xtext.diagnostics.Diagnostic.Linking")
			expr = "-1 <<< 3".expression(false)
			expr.assertError(
					XbasePackage.Literals::XBINARY_OPERATION,
					"org.eclipse.xtext.diagnostics.Diagnostic.Linking")

			"8 >>> 3".toInt should be 1
			"-8 >>> 3".toInt should be 536870911
		}

		/** The string operators are listed below.
		 * These operators are dedicated to strings of characters.
		 * 
		 * <p>Each operator has an associated function name. This function contains
		 * the concrete implementation of the operational semantic of the
		 * operator. This function could be redefined as it is explained in the 
		 * [operator overloading section](#Operator_Overloading).
		 * 
		 * <table><thead>
		 * <tr><th>Operator</th><th>Function Name</th><th>Operator Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a + b</td><td>operator_plus</td><td>Concatenate the string representations of a and b.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "String Operators" {
			"#Operator_Overloading" should beAccessibleFrom this

			"'a' + 'b'".toStr should be "ab"
			"1 + 'b'".toStr should be "1b"
			"'a' + 1".toStr should be "a1"
		}

		/** The collection operators are listed below.
		 * These operators are dedicated to the collections (lists, sets, maps...)
		 * Most of the time, the first operand is the collection on which the
		 * operator must be applied. 
		 * 
		 * <p>Each operator has an associated function name. This function contains
		 * the concrete implementation of the operational semantic of the
		 * operator. This function could be redefined as it is explained in the 
		 * [operator overloading section](#Operator_Overloading).
		 * 
		 * <table><thead>
		 * <tr><th>Operator</th><th>Function Name</th><th>Operator Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>c += e</td><td>operator_add</td><td>Equivalent to: <code>c.add(e)</code></td></tr>
		 * <tr><td>c -= e</td><td>operator_remove</td><td>Equivalent to: <code>c.remove(e)<code></td></tr>
		 * <tr><td>c1 + c2</td><td>operator_plus</td><td>Create a collection that is containing the elements of the
		 *                                               collections <code>c1</code> and <code>c2</code>.</td></tr>
		 * <tr><td>m + p</td><td>operator_plus</td><td>Create a map of type <code>Map&lt;A,B&gt;</code>
		 *                                             that is containing the elements of the
		 *                                             map <code>m</code> and the new pair <code>p</code> of type
		 *                                             <code>Pair&lt;A,B&gt;</code>.</td></tr>
		 * <tr><td>m - p</td><td>operator_plus</td><td>Create a map of type <code>Map&lt;A,B&gt;</code>
		 *                                             that is containing the elements of the
		 *                                             map <code>m</code>, except the pair <code>p</code> of type
		 *                                             <code>Pair&lt;A,B&gt;</code>.</td></tr>
		 * <tr><td>a -&gt; b</td><td>operator_mappedTo</td><td>Create an instance of <code>Pair&lt;A,B&gt;</code> where
		 *                           <code>A</code> and <code>B</code> are the types of a and b respectively.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Collection Operators" {
			"#Operator_Overloading" should beAccessibleFrom this

			var pair = to("4 -> 'a'", typeof(Pair))			
			pair.key should be 4
			pair.value should be 'a'

			"var c = newArrayList; c += 3".toBool should be true
			"var c = newArrayList; c -= 3".toBool should be false

			"var c = newHashSet; c += 3".toBool should be true
			"var c = newHashSet; c -= 3".toBool should be false

			"var c = newHashMap(5->'b'); c += (4 -> 'a')".toStr should be nullValue
			"var c = newHashMap(5->'b'); c += (5 -> 'a')".toStr should be 'b'
			"var c = newHashMap(5->'b', 4->'c'); c -= 4".toStr should be 'c'

			"var c1=newArrayList(1,2); var c2=newArrayList(3,4); var r = (c1 + c2); r".to(typeof(Iterable)).iterator should iterate #[1,2,3,4]

			"var c1=newHashMap(5->'b'); var c2=newHashMap(4->'a'); var r = (c1 + c2); r".to(typeof(Map)) should be #{4->'a',5->'b'}

			"var c=newHashMap(5->'b'); var r = (c + (4->'a')); r".to(typeof(Map)) should be #{4->'a',5->'b'}
		}

		/** The assignment operators are listed below.
		 * Local variables and fields can be assigned using the `=` operator.
		 * Compound assignment operators (`+=`, `-=`, `*=`, `/=`,
		 * `%=`) can be used as a shorthand for the assignment of a binary expression.
		 * They work automatically when the corresponding infix operator is declared.
		 * 
		 * <table><thead>
		 * <tr><th>Operator</th><th>Operator Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a = b</td><td>Set the variable a with the value of b.</td></tr>
		 * <tr><td>a += b</td><td>Equivalent to: <code>a = a + b</code></td></tr>
		 * <tr><td>a -= b</td><td>Equivalent to: <code>a = a - b</code></td></tr>
		 * <tr><td>a *= b</td><td>Equivalent to: <code>a = a * b</code></td></tr>
		 * <tr><td>a /= b</td><td>Equivalent to: <code>a = a / b</code></td></tr>
		 * <tr><td>a %= b</td><td> Equivalent to: <code>a = a % b</code></td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Assignments" {
			"var a : int = 5\na = 6\na".toInt should be 6
			"var a : int = 5\na += 6\na".toInt should be 11
			"var a : int = 5\na -= 6\na".toInt should be -1
			"var a : int = 5\na *= 6\na".toInt should be 30
			"var a : int = 5\na /= 6\na".toInt should be 0
			"var a : int = 5\na %= 6\na".toInt should be 5
		}

		/** This section presents a collection of operators that permit
		 * to define ranges of values.
		 * 
		 * <p>Each operator has an associated function name. This function contains
		 * the concrete implementation of the operational semantic of the
		 * operator. This function could be redefined as it is explained in the 
		 * [operator overloading section](#Operator_Overloading).
		 * 
		 * <table><thead>
		 * <tr><th>Operator</th><th>Function Name</th><th>Operator Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a .. b</td><td>operator_upTo</td><td>Create a list of integer values from a (inclusive) to b (inclusive).<br/>
		 *                        <code>1..5</code> is the range from 1 to 5 with 1 &lt;= x &lt;= 5.<br/><code>5..1</code> is the
		 *                        range from 5 to 1 with 5 &gt;= x &gt;= 1.<br/>The type of this expression is
		 *                        IntegerRange.</td></tr>
		 * <tr><td>a &gt;.. b</td><td>operator_greaterThanDoubleDot</td><td>Create a list of integer values from a (exclusive) to
		 *                            b (inclusive).<br/><code>5&gt;..1</code> is the range from 4 to 1 with
		 *                            5 &gt; x &gt;= 1.<br/>
		 *                            <code>1&gt;..5</code> is the empty range since the constraint is wrong 1 &gt; x &gt;= 5.<br/>
		 *                            See <a href="https://bugs.eclipse.org/bugs/show_bug.cgi?id=443258">Xtext</a> for
		 *                            discussion on the operational semantics of this operator.<br/>The type of this expression
		 *                            is ExclusiveRange.</td></tr>
		 * <tr><td>a ..&lt; b</td><td>operator_doubleDotLessThan</td><td>Create a list of integer values from a (inclusive) to
		 *                            b (exclusive).<br/><code>1..&lt;5</code> is the range from 1 to 5 with
		 *                            1 &lt;= x &lt; 5.<br/>
		 *                            <code>5..&lt;1</code> is the empty range since the constraint is wrong 5 &lt;= x &lt; 1.<br/>
		 *                            See <a href="https://bugs.eclipse.org/bugs/show_bug.cgi?id=443258">Xtext</a> for
		 *                            discussion on the operational semantics of this operator.<br/>The type of this expression
		 *                            is ExclusiveRange.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Range operators" {
			"#Operator_Overloading" should beAccessibleFrom this

			var r1 = "1 .. 4".to(typeof(IntegerRange))
			r1.iterator() should iterate #[1, 2, 3, 4]
			r1 = "4 .. 1".to(typeof(IntegerRange))
			r1.iterator() should iterate #[4, 3, 2, 1]
			
			var r2 = "5 >.. 1".to(typeof(ExclusiveRange))
			r2.iterator() should iterate #[4,3,2,1]
			r2 = "0 >.. 0".to(typeof(ExclusiveRange))
			r2.iterator() should iterate #[]
			r2 = "5 >.. -3".to(typeof(ExclusiveRange))
			r2.iterator() should iterate #[4,3,2,1,0,-1,-2,-3]

			var r3 = "1 ..< 5".to(typeof(ExclusiveRange))
			r3.iterator() should iterate #[1,2,3,4]
			r3 = "0 ..< 0".to(typeof(ExclusiveRange))
			r3.iterator() should iterate #[]
			r3 = "-3 ..< 5".to(typeof(ExclusiveRange))
			r3.iterator() should iterate #[-3,-2,-1,0,1,2,3,4]

			//
			// Special cases that have a not-so-easy semantic, from my point of view (SG).
			//
			
			var r4 = "1 >.. 5".to(typeof(ExclusiveRange))
			r4.iterator() should iterate #[] // not: #[2,3,4,5]
			r4 = "-3 >.. 5".to(typeof(ExclusiveRange))
			r4.iterator() should iterate #[] // not: #[-2,-1,0,1,2,3,4,5]
			
			var r5 = "5 ..< 1".to(typeof(ExclusiveRange))
			r5.iterator() should iterate #[] // not: #[5,4,3,2]
			r5 = "5 ..< -3".to(typeof(ExclusiveRange))
			r5.iterator() should iterate #[] // not: #[5,4,3,2,1,0,-1,-2]
		}

		/** This section presents a collection of operators that are not
		 * related to the categories in the previous sections.
		 * 
		 * <p>Each operator has an associated function name. This function contains
		 * the concrete implementation of the operational semantic of the
		 * operator. This function could be redefined as it is explained in the 
		 * [operator overloading section](#Operator_Overloading).
		 * 
		 * <table><thead>
		 * <tr><th>Operator</th><th>Function Name</th><th>Operator Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a ?: b</td><td>operator_elvis</td><td>If a is not null then a else b.</td></tr>
		 * <tr><td>a =&gt; b</td><td>operator_doubleArrow</td><td>Used as a 'with'- or 'let'-operation. It allows to bind an
		 *                           object to a local scope in order to do something on it. b must be a lambda
		 *                           expression.</td></tr>
		 * <tr><td>a &lt;&gt; b</td><td>operator_diamond</td><td>Not yet supported.</td></tr>
		 * </tbody></table>
		 * 
		 * <p>For illustrating an usage of the `=>` operator, consider the class `Person`
		 * with two attributes inside: `firstName` and `lastName`.
		 * The creation of an instance of `Person` could be done with:
		 * <pre><code>new Person => [
		 *   firstName = 'Han'
		 *   lastName = 'Solo'
		 * ]</code></pre>
		 * In this example, the instance of Person is created and passed to the
		 * lambda expression. In this expression, it is accessible with the `it`
		 * reserved variable, which does not need to be typed out since it is
		 * the default object in lambda expression. The lambda expression replies
		 * the value of `it`.
		 *
		 * @filter(.*) 
		 */
		fact "Other operators" {
			"#Operator_Overloading" should beAccessibleFrom this

			"null ?: 'a'".toStr should be "a"

			"'b' ?: 'a'".toStr should be "b"

			var expr = "1 <> 3".expression(false)
			expr.assertError(
					XbasePackage.Literals::XBINARY_OPERATION,
					"org.eclipse.xtext.diagnostics.Diagnostic.Linking")
		}

		/* In SARL, it is easy to overload an existing operator or
		 * to re-define the algorithm of one.
		 * 
		 * <p>You should define the operator mapping function (see the
		 * previous sections for a comprehensive list of them).
		 * 
		 * <p>Below, the addition operator `+` between two `Pair` is defined.
		 * The function that is defining the operator must have
		 * a name with the `operator_` prefix, and one parameter
		 * for each operand associated to the operator.
		 * In the example, the addition of two pairs (a,b) and (c,d)
		 * gives the pair (a,d).
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Operator Overloading"{
			var model = '''
				def operator_plus(
							a : Pair<Integer,Integer>,
							b : Pair<Integer,Integer>) : Pair<Integer,Integer> {
					return new Pair(a.key, b.value)
				}
				def example {
					var x = new Pair(1,3)
					var y = new Pair(4,5)
					var z1 = operator_plus(x, y) // Call the overloaded operator
					var z2 = x + y // Call the overloaded operator
					// z1.key == 1
					// z1.value == 5
					println(z1.toString)
					// z2.key == 1
					// z2.value == 5
					println(z2.toString)
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
	
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 2
			]) as SarlAgent
	
			a.members.get(0) => [
				it should beAction "operator_plus"
				it should reply "org.eclipse.xtext.xbase.lib.Pair<java.lang.Integer, java.lang.Integer>"
				it should haveNbParameters 2
				it should beVariadic false
				(it as SarlAction).parameters.get(0) => [
					it should beParameter "a"
					it should haveType "org.eclipse.xtext.xbase.lib.Pair<java.lang.Integer, java.lang.Integer>"
					it should haveDefaultValue _
				]
				(it as SarlAction).parameters.get(1) => [
					it should beParameter "b"
					it should haveType "org.eclipse.xtext.xbase.lib.Pair<java.lang.Integer, java.lang.Integer>"
					it should haveDefaultValue _
				]
			]

			a.members.get(1) => [
				it should beAction "example"
				it should reply _
				it should haveNbParameters 0
				it should beVariadic false
			]
		}

	}

	/* The block expression allows to have imperative code sequences. 
	 * It consists of a sequence of expressions. The value of the last 
	 * expression in the block is the value of the complete block. 
	 * The type of a block is also the type of the last expression. 
	 * Empty blocks return `null` and have the type `Object`.
	 * 
	 * <p>A block expression is surrounded by curly braces. The expressions in a block can be terminated by an optional semicolon.
	 * 
	 * @filter(.* = '''|'''|.parseSuccessfully.*) 
	 */
	fact "Blocks" {
		'''
			// The block of the following function as a value of the same type as x,
			// which is after inferring String.
			def block : String {
				var x = greeting;
				if (x.equals("Hello ")) {
					x + "World!" 
				} else {
					x
				}
			}
		'''.parseSuccessfully(
			"package io.sarl.docs.reference.gsr
			agent A {
				var greeting = \"abc\"",
			// TEXT
			"}"
		)
	}

	/* Variables and Fields can be declared in SARL.
	 * For declaring a variable or a field, you must specify if it is a value or a
	 * variable (see below for details), its name, and optionally its type and its
	 * initial value.
	 * 
	 * <p>The variable/value declaration follows the syntax:
	 * 
	 *     var NAME [: TYPE] [= INITIAL VALUE]
	 *     val NAME [: TYPE] [= INITIAL VALUE]
	 * 
	 * <p>Shadowing variables from outer scopes is not allowed, the only exception is the 
	 * implicit variable `it`.
	 * 
	 */
	describe "Variable and Field Declarations" {
		
		/* A variable declaration starting with the keyword `val` denotes 
		 * a value, which is essentially a final, unsettable variable.
		 * 
		 * <p>The variable needs to be declared with the keyword `var`, which 
		 * stands for 'variable' if it should be allowed to reassign its value.
		 * 
		 * <p>Variables declared outside a lambda expression using the `var` keyword
		 * are not accessible from within the lambda expressions. Those declared with the
		 * `val` keyword are accessible.
		 * 
		 * <p>Fields declared outside a lambda expression using the `var` keyword
		 * or the `val` keyword are accessible from within the lambda expressions.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Variable vs. Value Declaration"{
			var model = '''
					val max = 100
					var i = 0
					while (i < max) {
						println("Hi there!")
						i = i + 1
					}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					def example {",
				// TEXT
				"} }"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
	
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 1
			]) as SarlAgent
	
			a.members.get(0) => [
				it should beAction "example"
				it should reply _
				it should haveNbParameters 0
				it should beVariadic false
			]
		}

		/* The type of the variable itself can either be explicitly declared or it can be 
		 * inferred from the initializer expression.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Typing"{
			var model = '''
				// Explicit type
				var a : String = "abc"
				// Inferred type
				var b = "abc"
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}")

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
	
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 2
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "a"
				it should haveType "java.lang.String"
				it should haveInitialValue "abc"
			]

			a.members.get(1) => [
				it should beVariable "b"
				it should haveType _
				it should haveInitialValue "abc"
			]
		}

		/* Like in Java the current object is bound to the keyword `this`.
		 * This allows for either qualified field access or method invocations.
		 * 
		 * <p>You can use the variable name `it` to get the same behavior for
		 * any variable or parameter.
		 * Moreover, the variable `it` is that it is allowed to
		 * be shadowed. This is especially useful when used together with lambda
		 * expressions.
		 * 
		 * <p>It means that if you type a name, the compiler tries to find a member
		 * with the same name on the `it` object, then in the `this` object.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Implicit Variables this and it"{
			'''
			agent A {
				var a = 35
				def example : int {
					this.a = 123
					
					val it = new String("abc")
					return length // translates to 'it.length()'
				}
			}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT
				""
			)
		}

	}

	/* A function, or method, or action, is a named block of code that could be invoked.
	 *
	 * <p>A function declaration starts with the keyword `def`.
	 * This declaration can only occur in top-level features
	 * (`agent`, `skill`, etc.)
	 */
	describe "Function Declarations" {
		
		/* 
		 * The standard function declaration follows the syntax:
		 * 
		 *      def NAME [([PARAMETER, PARAMETER, PARAMETER...])] [: RETURN TYPE] [BLOCK]
		 *
		 * 
		 * <note> The parameters are implicitly declared with the keyword `val`.</note>
		 * 
		 * <p>The following code gives examples of function declarations:
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Standard Declarations"{
			var model = '''
				// No parameter.
				// Return type: void
				def action1 {
				}
				// No parameter.
				// Return type: int
				def action2 : int {
					return 0
				}
				// Parameter 1, named 'a', of type int.
				// Return type: void
				def action3(a : int) {
				}
				// Parameter 1, named 'a', of type int.
				// Parameter 2, named 'b', of type String.
				// Return type: void
				def action4(a : int, b : String) {
				}
				// Parameter 1, named 'a', of type int.
				// Return type: double
				def action5(a : int) : double {
					return 0
				}
				// Parameter 1, named 'a', of type int.
				// Parameter 2, named 'b', of type String.
				// Return type: String
				def action6(a : int, b : String) : String {
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
	
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 6
			]) as SarlAgent

			a.members.get(0) => [
				it should beAction "action1"
				it should reply _
				it should haveNbParameters 0
				it should beVariadic false
			]

			a.members.get(1) => [
				it should beAction "action2"
				it should reply "int"
				it should haveNbParameters 0
				it should beVariadic false
			]

			a.members.get(2) => [
				it should beAction "action3"
				it should reply _
				it should haveNbParameters 1
				it should beVariadic false
				(it as SarlAction).parameters.get(0) => [
					it should beParameter "a"
					it should haveType "int"
					it should haveDefaultValue _
				]
			]

			a.members.get(3) => [
				it should beAction "action4"
				it should reply _
				it should haveNbParameters 2
				it should beVariadic false
				(it as SarlAction).parameters.get(0) => [
					it should beParameter "a"
					it should haveType "int"
					it should haveDefaultValue _
				]
				(it as SarlAction).parameters.get(1) => [
					it should beParameter "b"
					it should haveType "java.lang.String"
					it should haveDefaultValue _
				]
			]

			a.members.get(4) => [
				it should beAction "action5"
				it should reply "double"
				it should haveNbParameters 1
				it should beVariadic false
				(it as SarlAction).parameters.get(0) => [
					it should beParameter "a"
					it should haveType "int"
					it should haveDefaultValue _
				]
			]

			a.members.get(5) => [
				it should beAction "action6"
				it should reply "java.lang.String"
				it should haveNbParameters 2
				it should beVariadic false
				(it as SarlAction).parameters.get(0) => [
					it should beParameter "a"
					it should haveType "int"
					it should haveDefaultValue _
				]
				(it as SarlAction).parameters.get(1) => [
					it should beParameter "b"
					it should haveType "java.lang.String"
					it should haveDefaultValue _
				]
			]
		}

		/* The section "[Exception Support](#ExceptionSupport)" shows how to write an exception handler
		 * in the code. Sometimes, it is appropriate for code to catch exceptions that can occur within it.
		 * In other cases, however, it is better to let a method further up the call stack handle the exception.
		 * 
		 * <p>If a function doesn't catch the checked exceptions that can occur within it, the function could
		 * specify that it can throw these exceptions.
		 * <note>This specification is optional since the SARL compiler determines the
		 * exceptions that are not catched, and assumes that they are implicitly thrown outside the function.
		 *
		 * <p>The declaration of the thrown exceptions is done with the <code>throws</code> keyword, followed
		 * by a list of thrown exception types. This declaration must be put between the list of formal
		 * parameters and the function's code.
		 *
		 * <p>In the following example, the function <code>myaction</code> is defined without formal parameter
		 * and returned value. This function indicates to its caller that it could throw an exception of
		 * type <code>IllegalStateException</code>. 
		 *  
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		 fact "Declare exceptions in the function prototype" {
			var model = '''
				def myaction throws IllegalStateException {
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
	
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 1
			]) as SarlAgent

			a.members.get(0) => [
				it should beAction "myaction"
				it should reply _
				it should haveNbParameters 0
				it should beVariadic false
				it should throwException 'java.lang.IllegalStateException'
			]
		 }

		/* 
		 * Generic functions are methods that introduce their own type parameters.
		 * This is similar to declaring a
		 * [generic type](./BasicObjectOrientedProgrammingSupportSpec.html#Define_a_Generic_Class),
		 * but the type parameter's scope
		 * is limited to the function where it is declared. 
		 * Static and non-static generic functions are allowed.
		 * 
		 * <p>You can write a single generic method declaration that can be called with arguments of
		 * different types. Based on the types of the arguments passed to the generic method,
		 * the compiler handles each method call appropriately. Following are the rules to define
		 * generic functions: <ul>
		 * <li>All generic method declarations have a type parameter section written with
		 *     the "with" or the bracket syntax.</li>
		 * <li>Each type parameter section contains one or more type parameters separated by commas. 
		 *     A type parameter, also known as a type variable, is an identifier that specifies a generic
		 *     type name.</li>
		 * <li>The type parameters can be used to declare the return type and act as placeholders for
		 *     the types of the arguments passed to the generic method, which are known as actual type
		 *     arguments.</li>
		 * </ul>
		 * 
		 * <p>A generic method's body is declared like that of any other method.
		 * 
		 * <note>Type parameters can represent only reference types, not primitive types
		 * (like `int`, `double` and `char`).</note>
		 
		 * <p>Two syntaxes are allowed for defining the type parameters of the actions:
		 * the "with" syntax, and the bracket syntax.
		 */
		describe "Generic Function"{
			
			/**  
			 * The "with" syntax for a generic function includes a type parameter, after the `with`
			 * keyword, between the function's return type and the function's body.
			 *
			 * <p>In the following example, the function specifies a type <code>T</code>, which is used both
			 * as type for the element parameter and the generic type of the Collection.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Definition with \"with\"" {
				// Verify the URL in the upper section text.
				"./BasicObjectOrientedProgrammingSupportSpec.html" should beAccessibleFrom this
				//	
				var model = '''
					def addAndReturn(element : T, collection : Collection<T>) : T with T {
					    collection.add(element);
					    return element;
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.gsr
					import java.util.Collection
					agent A {",
					// TEXT
					"}"
				)
	
				model => [
					it should havePackage "io.sarl.docs.reference.gsr"
					it should haveNbImports 1
					it should haveNbElements 1
				]
		
				var a = (model.xtendTypes.get(0) => [
					it should beAgent "A"
					it should extend _
					it should haveNbElements 1
				]) as SarlAgent
	
				a.members.get(0) => [
					it should beAction "addAndReturn"
					it should reply "T"
					it should haveNbParameters 2
					it should beVariadic false
					(it as SarlAction).parameters.get(0) => [
						it should beParameter "element"
						it should haveType "T"
						it should haveDefaultValue _
					]
					(it as SarlAction).parameters.get(1) => [
						it should beParameter "collection"
						it should haveType "java.util.Collection<T>"
						it should haveDefaultValue _
					]
				]
			}

			/**  
			 * The bracket syntax for a generic function includes a type parameter, inside angle brackets, and 
			 * appears before the function's name.
			 * 
			 * <p>In the following example, the function specifies a type <code>T</code>, which is used both
			 * as type for the element parameter and the generic type of the Collection.
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Definition with Brackets" {
				var model = '''
					def <T> addAndReturn(element : T, collection : Collection<T>) : T {
					    collection.add(element);
					    return element;
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.gsr
					import java.util.Collection
					agent A {",
					// TEXT
					"}"
				)
	
				model => [
					it should havePackage "io.sarl.docs.reference.gsr"
					it should haveNbImports 1
					it should haveNbElements 1
				]
		
				var a = (model.xtendTypes.get(0) => [
					it should beAgent "A"
					it should extend _
					it should haveNbElements 1
				]) as SarlAgent
	
				a.members.get(0) => [
					it should beAction "addAndReturn"
					it should reply "T"
					it should haveNbParameters 2
					it should beVariadic false
					(it as SarlAction).parameters.get(0) => [
						it should beParameter "element"
						it should haveType "T"
						it should haveDefaultValue _
					]
					(it as SarlAction).parameters.get(1) => [
						it should beParameter "collection"
						it should haveType "java.util.Collection<T>"
						it should haveDefaultValue _
					]
				]
			}

			/*
			 * There may be times when you'll want to restrict the kinds of types that are allowed to be
			 * passed to a type parameter. For example, a method that operates on numbers might only want
			 * to accept instances of Number or its subclasses. This is what bounded type parameters
			 * are for.
			 *
			 * <p>To declare a bounded type parameter, list the type parameter's name, followed by the 
			 * `extends` keyword, followed by its upper bound. 
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			 fact "Bounded Type Parameters" {
				var model = '''
					def print(value : T) with T extends Number {
					    System.out.println("Type = " + value.getClass)
					    System.out.println("Value = " + value)
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.gsr
					import java.util.Collection
					agent A {",
					// TEXT
					"}"
				)
	
				model => [
					it should havePackage "io.sarl.docs.reference.gsr"
					it should haveNbImports 1
					it should haveNbElements 1
				]
		
				var a = (model.xtendTypes.get(0) => [
					it should beAgent "A"
					it should extend _
					it should haveNbElements 1
				]) as SarlAgent
	
				a.members.get(0) => [
					it should beAction "print"
					it should reply _
					it should haveNbParameters 1
					it should beVariadic false
					(it as SarlAction).parameters.get(0) => [
						it should beParameter "value"
						it should haveType "T"
						it should haveDefaultValue _
					]
				]
			 }

		}

		/* A variadic function is a function of indefinite arity: 
		 * one which accepts a variable number of arguments.
		 * 
		 * <p>SARL enables to define the last parameter of a function
		 * as variadic with the operator `*`.
		 * This operator has an informal meaning similar to the
		 * cardinality in UML: zero to many.
		 * 
		 * <p>In other languages, such as Java and C++, the variadic
		 * operator is `...`
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Variadic Function"{
			var model = '''
				// Function with indefinite number of integers as parameters
				def action1(v : int*) { }
				// Function which takes a boolean, a double and an indefinite 
				// number of integers as parameters
				def action2(a : boolean, b : double, c : int*) { }
				
				// Calls
				def calls {
					action1()
					action1(1)
					action1(1, 3)
					action2(true, 3.0)
					action2(true, 3.0, 1)
					action2(true, 3.0, 1, 5)
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
	
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 3
			]) as SarlAgent

			a.members.get(0) => [
				it should beAction "action1"
				it should reply _
				it should haveNbParameters 1
				it should beVariadic true
				(it as SarlAction).parameters.get(0) => [
					it should beParameter "v"
					it should haveType "int"
					it should haveDefaultValue _
				]
			]

			a.members.get(1) => [
				it should beAction "action2"
				it should reply _
				it should haveNbParameters 3
				it should beVariadic true
				(it as SarlAction).parameters.get(0) => [
					it should beParameter "a"
					it should haveType "boolean"
					it should haveDefaultValue _
				]
				(it as SarlAction).parameters.get(1) => [
					it should beParameter "b"
					it should haveType "double"
					it should haveDefaultValue _
				]
				(it as SarlAction).parameters.get(2) => [
					it should beParameter "c"
					it should haveType "int"
					it should haveDefaultValue _
				]
			]

			a.members.get(2) => [
				it should beAction "calls"
				it should reply _
				it should haveNbParameters 0
				it should beVariadic false
			]
		}

		/* SARL allows to specify a default value for a formal parameter.
		 * 
		 * <p>When a default value is specified, it means that the caller of
		 * the action can skip to pass a value for the corresponding argument.
		 * And, when the function is run, the default value is given to the
		 * skipped argument.
		 * 
		 * <importantnote> In SARL, 
		 * if a formal parameter has a default value, the following formal 
		 * parameters do not need to have default value as well. This is a major
		 * difference with the default values in the C++ language for instance. 
		 * </importantnote>
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Default Value for the Formal Parameters"{
			var model = '''
				// Function with one parameter with a default value.
				def action1(v : int = 5) { }
				// Function which takes a boolean, a double and an integer as parameters.
				// The first and third parameters have default values. 
				def action2(a : boolean=true, b : double, c : int=7) { }
				
				// Calls
				def calls {
					// v == 1
					action1(1)
					// v == 5
					action1()
					// a == true, b == 3.0, c == 1
					action2(true, 3.0, 1)
					// a == false, b == 4.0, c == 7
					action2(false, 4.0)
					// a == true, b == 7.0, c == 56
					action2(7.0, 56)
					// a == true, b == 9.0, c == 7
					action2(9.0)
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
	
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 3
			]) as SarlAgent

			a.members.get(0) => [
				it should beAction "action1"
				it should reply _
				it should haveNbParameters 1
				it should beVariadic false
				(it as SarlAction).parameters.get(0) => [
					it should beParameter "v"
					it should haveType "int"
					it should haveDefaultValue "5"
				]
			]

			a.members.get(1) => [
				it should beAction "action2"
				it should reply _
				it should haveNbParameters 3
				it should beVariadic false
				(it as SarlAction).parameters.get(0) => [
					it should beParameter "a"
					it should haveType "boolean"
					it should haveDefaultValue "true"
				]
				(it as SarlAction).parameters.get(1) => [
					it should beParameter "b"
					it should haveType "double"
					it should haveDefaultValue _
				]
				(it as SarlAction).parameters.get(2) => [
					it should beParameter "c"
					it should haveType "int"
					it should haveDefaultValue "7"
				]
			]

			a.members.get(2) => [
				it should beAction "calls"
				it should reply _
				it should haveNbParameters 0
				it should beVariadic false
			]
		}

		/* It is possible to mix the variadic parameter and the default values,
		 * except that the variadic parameter cannot have a default value. 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Mixing Variadic Parameter and Default Values"{
			var model = '''
				def action(v : int = 5, a : float*) { }
				
				// Calls
				def calls {
					// v == 1, a == #[]
					action(1)
					// v == 5, a == #[]
					action()
					// v == 1, a == #[3.5, 6.45]
					action(1, 3.5f, 6.45f)
					// v == 5, a == #[3.5, 6.45]
					action(3.5f, 6.45f)
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
	
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 2
			]) as SarlAgent

			a.members.get(0) => [
				it should beAction "action"
				it should reply _
				it should haveNbParameters 2
				it should beVariadic true
				(it as SarlAction).parameters.get(0) => [
					it should beParameter "v"
					it should haveType "int"
					it should haveDefaultValue "5"
				]
				(it as SarlAction).parameters.get(1) => [
					it should beParameter "a"
					it should haveType "float"
					it should haveDefaultValue _
				]
			]
		}

	}

	/* This section describes the syntax for using or calling the members
	 * of an object.
	 * 
	 * <p>A simple name can refer to a field, variable or parameter. 
	 * In addition, it can point to a method with zero argument since 
	 * empty parentheses are optional.
	 * 
	 * <p>The rest of this section describes particular mechanisms for
	 * calling the object members.
	 */
	describe "Object Member Invocation" {
		
		/* The SARL language provides a very powerfull mecanism for calling members of an object as
		 * properties of this object.
		 * 
		 * <p>Indeed, if there is no field with the given name and also no method with 
		 * the name and zero parameters accessible, a simple name binds to a 
		 * corresponding Java-Bean getter method if available.
		 * The getter method must have a name starting with one of the strings of
		 * characters <code>"get"</code>, <code>"is"</code>, <code>"has"</code>, followed
		 * by the given name.
		 * 
		 * <p>In the following example, two fields are defined: <code>prop1</code> and <code>prop2</code>.
		 * As usual, these properties have a private scope, and the getter and setter functions must
		 * be defined for enabling public scope access.
		 * In the example, only the getter and setter functions for <code>prop2</code> are defined.
		 * 
		 * <p>In the function <code>getters</code>, four examples of calls are given:<ol>
		 * <li><code>this.prop1</code>: this expression accesses directly to the field <code>prop1</code>;</li>
		 * <li><code>this.prop2</code>: this expression accesses directly to the field <code>prop2</code>;</li>
		 * <li><code>this.getProperty2</code>: this expression calls the getter function;</li>
		 * <li><code>this.property2</code>: since there is no field with the name <code>property2</code>, and
		 * a function is defined with the prefix <code>"get"</code> and the name, then the getter is called.</li>
		 * </ol>
		 * This last example illustrates the well-known property-access syntax.
		 * 
		 * <p>In the function <code>setters</code>, four examples of calls are given:<ol>
		 * <li><code>this.prop1</code>: this expression sets directly to the field <code>prop1</code>;</li>
		 * <li><code>this.prop2</code>: this expression sets directly to the field <code>prop2</code>;</li>
		 * <li><code>this.setProperty2</code>: this expression calls the setter function;</li>
		 * <li><code>this.property2</code>: since there is no field with the name <code>property2</code>, and
		 * a function is defined with the prefix <code>"set"</code> and the name, then the setter is called.</li>
		 * </ol>
		 * This last example also illustrates the well-known property-access syntax.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Property Syntax"{
			'''
			agent A {
				var prop1 : Object
				var prop2 : Object
				def getProperty2 : Object {
					return this.prop2
				}
				def setProperty2(o : Object) {
					this.prop2 = o
				}
				
				def getters {
					// Direct access to the property
					println(this.prop1)
					println(this.prop2)
					// Use the getter
					println(this.getProperty2)
					println(this.property2)
				}
				
				def setters {
					// Direct access to the property
					this.prop1 = 4
					this.prop2 = new Object
					// Use the setter
					this.setProperty2(new Object)
					this.property2 = new Object
				}
			}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT
				""
			)
		}

		/* For accessing a static field or method you can use the recommended 
		 * Java syntax or the more explicit double colon `::`. 
		 * That means, the following expressions are pairwise equivalent:
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Static Access to Members"{
			var model = '''
				var a = Integer::TYPE
				var b = Integer.TYPE
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)

			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 1
			]
	
			var a = (model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 2
			]) as SarlAgent

			a.members.get(0) => [
				it should beVariable "a"
				it should haveType _
				it should haveInitialValue "java.lang.Integer.TYPE"
			]

			a.members.get(1) => [
				it should beVariable "b"
				it should haveType _
				it should haveInitialValue "java.lang.Integer.TYPE"
			]
		}

		/* Checking for null references can make code very unreadable. 
		 * In many situations, it is correct for an expression to return `null`
		 * if a receiver was `null`.
		 * 
		 * <p>SARL supports the safe navigation operator `?`. to make such code
		 * better readable.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Null-Safe Feature Call"{
			'''
					// First expression
					if (myRef != null) myRef.length()
					// Second expression, equivalent to the first expression
					myRef?.length()
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var myRef = \"abc\"
					def examples {",
				// TEXT
				"} }"
			)
		}

		/* When it is possible to extend an existing type,
		 * the methods can be overridden.
		 * In this case, the `super` keyword
		 * permits invoking the inherited implementation of the method from
		 * the overriding method.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Inherited Method"{
			var model = '''
				def anAction {
					// Call the inherited implementation
					super.anAction
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					def anAction {
					}
				}
				agent B extends A {",
				// TEXT
				"}"
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.gsr"
				it should haveNbImports 0
				it should haveNbElements 2
			]
	
			model.xtendTypes.get(0) => [
				it should beAgent "A"
				it should extend _
				it should haveNbElements 1
				(it as SarlAgent).members.get(0) => [
					it should beAction "anAction"
					it should reply _
					it should haveNbParameters 0
					it should beVariadic false
				]
			]

			model.xtendTypes.get(1) => [
				it should beAgent "B"
				it should extend "io.sarl.docs.reference.gsr.A"
				it should haveNbElements 1
				(it as SarlAgent).members.get(0) => [
					it should beAction "anAction"
					it should reply _
					it should haveNbParameters 0
					it should beVariadic false
				]
			]
		}

		/* Constructor calls correspond to the calls of a constructor function for
		 * an object.
		 */ 
		describe "Constructor Call" {
			
			/* Constructor calls have the same syntax as in Java. 
			 * The only difference is that empty parentheses are optional.
			 * If type arguments are omitted, they will be inferred from the current context similar to Java's 
			 * diamond operator on generic method and constructor call.
			 *  
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Instance Creation" {
				var model = '''
					var a = new Integer(345)
					var b = new ArrayList<Integer>()
					var c = new ArrayList<Integer>
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.gsr
					import java.util.ArrayList
					agent A {",
					// TEXT
					"}"
				)
	
				model => [
					it should havePackage "io.sarl.docs.reference.gsr"
					it should haveNbImports 1
					it should importClass "java.util.ArrayList"
					it should haveNbElements 1
				]
		
				var a = (model.xtendTypes.get(0) => [
					it should beAgent "A"
					it should extend _
					it should haveNbElements 3
				]) as SarlAgent
	
				a.members.get(0) => [
					it should beVariable "a"
					it should haveType _
					it should haveInitialValue "java.lang.Integer.Integer"
				]
	
				a.members.get(1) => [
					it should beVariable "b"
					it should haveType _
					it should haveInitialValue "java.util.ArrayList.ArrayList"
				]
				
				a.members.get(2) => [
					it should beVariable "c"
					it should haveType _
					it should haveInitialValue "java.util.ArrayList.ArrayList"
				]
			}
	
			/* In the implementation of a constructor, it is possible to
			 * call one of the inherited constructors.
			 * The syntax is similar to Java: the `super` keyword
			 * is used to represent the inherited constructor.
			 * 
			 * <importantnote> It is recommended typing the
			 * two parentheses when invoking the default constructor of the super type.
			 * Indeed, in some cases, typing `super` causes no side-effect that is an error.
			 * </importantnote>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*) 
			 */
			fact "Inherited Constructor" {
				var model = '''
				new () {
					super() // Call the inherited default constructor
				}
				new (param : Address) {
					super(param) // Call the inherited constructor with a parameter
				}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.gsr
					import io.sarl.lang.core.Address
					event E1
					event E2 extends E1 {",
					// TEXT
					"}"
				)
				
				model => [
					it should havePackage "io.sarl.docs.reference.gsr"
					it should haveNbImports 1
					it should importClass "io.sarl.lang.core.Address"
					it should haveNbElements 2
				]
		
				model.xtendTypes.get(0) => [
					it should beEvent "E1"
					it should extend _
					it should haveNbElements 0
				]
	
				model.xtendTypes.get(1) => [
					it should beEvent "E2"
					it should extend "io.sarl.docs.reference.gsr.E1"
					it should haveNbElements 2
					(it as SarlEvent).members.get(0) => [
						it should beConstructor _
						it should haveNbParameters 0
						it should beVariadic false 
					] 
					(it as SarlEvent).members.get(1) => [
						it should beConstructor _
						it should haveNbParameters 1
						it should beVariadic false
						(it as XtendConstructor).parameters.get(0) => [
							it should beParameter "param"
							it should haveType "io.sarl.lang.core.Address"
							it should haveDefaultValue _
						]
					] 
				]
			}
	
		}

	}
	
	/*
	 * Extension methods allow adding new methods to existing 
	 * types without modifying them. This is really 
	 * helpful as they can greatly improve the readability. They
	 * use a simple syntactic trick: the first parameter of a method
	 * can either be passed in after opening the parentheses or before the 
	 * method call. For example, given a method:
	 *  
	 *     def removeVowels (s : String) {
	 *         s.replaceAll("[aeiouyAEIOUY]", "")
	 *     }
	 *
	 * 
	 * <p>We can call this method either like in Java:
	 *  
	 *     removeVowels("Hello")
	 *
	 * 
	 * <p>or as an extension method of String:
	 *  
	 *     "Hello".removeVowels
	 */
	describe "Extension Methods"{
		
		/*
		 * You can import static methods as extensions, directly call the 
		 * imported static methods on our list objects:
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Import static extension methods"{
			parseSuccessfully("
			package io.sarl.docs.reference.gsr
			class C1 {
				def removeVowels (s : String) {
					s.replaceAll(\"[aziouyAEIOUY]+\", \"\")
				}
				def caller1 {
					removeVowels(\"Hello\")
				}
				def caller2 {
					\"Hello\".removeVowels
				}
			}
			")
			//
			'''
			import static extension java.util.Collections.*
			agent A {
				def example {
					val colors : String[] = #["red", "blue", "green"]
					colors.sort // sort is implemented by Collections#sort(List<T>)
				}
			}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT
				""
			)
		}

		/*
		 * All visible non-static methods of the current class and its super 
		 * types are automatically available as extensions.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Local extension methods"{
			'''
				// Define an extension method for List
				def hasOneElement(list : List<?>) : boolean {
					list.size == 1
				}
				// Invoke the extension method
				def example : boolean {
					newArrayList("red").hasOneElement
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.util.List
				agent A {",
				// TEXT,
				"}")
		}
		
		/*
		 * By adding the `extension` keyword to a field, a local variable or a parameter declaration, its
		 * instance methods become extension methods.
		 * 
		 * <caution>The `extension` keyword must be always written just before the `var`|`var` keywords for
		 * fields and local variables, or before the parameter's name.</caution>
		 * 
		 * <caution>In the case of an extension field, you must give the type of the field. Indeed, the type resolved
		 * of SARL cannot infer the type of the field yet.</caution>
		 * 
		 * <p>In the following example, three functions are defined for illustrating the three types of
		 * extension providers.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Extension Provider" {
			'''
				class ExtensionProviderExamples {
					//
					// Example of an extension provider on a class field.
					//
					extension var list : ArrayList<String> = newArrayList
					def extensionFieldExample(value : String) : boolean {
						value.contains // calls this.list.contains(value)
					}
					//
					// Example of an extension provider on a method parameter.
					//
					def extensionParameterExample(value : String, extension o : ArrayList<String>) : boolean {
						value.contains // calls o.contains(value)
					}
					//
					// Example of an extension provider on a local variable.
					//
					def extensionLocalVariableExample(value : String) : boolean {
						extension var o : ArrayList<String> = newArrayList
						value.contains // calls o.contains(value)
					}
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.util.ArrayList",
				// TEXT,
				"")
		}

		/* The `String` class is extended with the following functions, where
		 * s is a `String`.
		 * 
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>s.isNullOrEmpty()</td><td>Replies if s is null or empty.</td></tr>
		 * <tr><td>s.toFirstLower()</td><td>Replies a copy of s in which the first letter is lowercase.</td></tr>
		 * <tr><td>s.toFirstUpper()</td><td>Replies a copy of s in which the first letter is uppercase.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "String extension" {
			'''
			package io.sarl.docs.reference.gsr
			agent A {
				var s : String
				def example0 : boolean {
					s.isNullOrEmpty
				}
				def example1 : String {
					s.toFirstLower
				}
				def example2 : String {
					s.toFirstUpper
				}
			}
			'''.parseSuccessfully
		}
	
		/* The integer numbers have extension functions below, where
		 * a and b are numbers.
		 * 
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a.bitwiseAnd(b)</td><td>The bitwise <code>and</code> operation. This is the equivalent
		 *                                 to the java <code>&</code> operator.</td></tr>
		 * <tr><td>a.bitwiseNot</td><td>The bitwise complement operation. This is the equivalent to the
		 *                              java <code>~</code> operator.</td></tr>
		 * <tr><td>a.bitwiseOr(b)</td><td>The bitwise <code>or</code> operation. This is the equivalent
		 *                                 to the java <code>|</code> operator.</td></tr>
		 * <tr><td>a.bitwiseXor(b)</td><td>The bitwise <code>xor</code> operation. This is the equivalent
		 *                                 to the java <code>^</code> operator.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Integer and Long extensions" {
			'''
			package io.sarl.docs.reference.gsr
			agent A {
				var a : Long
				var b : Long
				def example0 : Object {
					a.bitwiseAnd(b)
				}
				def example1 : Object {
					a.bitwiseNot
				}
				def example2 : Object {
					a.bitwiseOr(b)
				}
				def example3 : Object {
					a.bitwiseXor(b)
				}
			}
			'''.parseSuccessfully
		}

		/* The `Boolean` class is extended with the following functions, where
		 * a and b are `Boolean` objects.
		 * 
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a.xor(b)</td><td>A logical <code>xor</code>. This is the equivalent to the java
		 *                          <code>^</code> operator.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Boolean extension" {
			'''
			package io.sarl.docs.reference.gsr
			agent A {
				var a : boolean
				var b : boolean
				def example0 : Object {
					a.xor(b)
				}
			}
			'''.parseSuccessfully
		}

		/* The `Object` class is extended with the following functions, where
		 * a and b are an `Object`.
		 * 
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>a.identityEquals(b)</td><td>Equivalent to: <code>a === b</code></td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Object extension" {
			'''
			package io.sarl.docs.reference.gsr
			agent A {
				var a : Object
				var b : Object
				def example0 : Object {
					a.identityEquals(b)
				}
			}
			'''.parseSuccessfully
		}

		/* The `Iterable` classes is extended with the following functions, where
		 * i is an instance of `Iterable`, and ii is an instanceof of `Iterable<Iterable>`.
		 *  
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>i.drop(n)</td><td>Returns a view on this iterable object that provides all elements except the first n entries.</td></tr>
		 * <tr><td>i.dropWhile [e | predicate]</td><td>Returns an Iterable containing all elements starting from the first
		 *                                             element for which the drop-predicate returned false. The resulting
		 *                                             Iterable is a lazily computed view, so any modifications to the
		 *                                             underlying Iterators will be reflected on iteration.</td></tr>
		 * <tr><td>i.elementsEqual(i2)</td><td>Returns true if the elements of the two iterables, or one iterator and one iterable are equal.</td></tr>
		 * <tr><td>i.exists [e | predicate]</td><td>Returns true if one or more elements in iterable satisfy the predicate.</td></tr>
		 * <tr><td>i.filter [e | predicate]<br/>
		 *         i.filter(type)</td><td>Returns the elements of i that satisfy a predicate or the given type.</td></tr>
		 * <tr><td>i.filterNull</td><td>Returns a new iterable filtering any null references.</td></tr>
		 * <tr><td>i.findFirst [e | predicate]</td><td>Finds the first element in the given iterable that
		 *                                             fulfills the predicate.</td></tr>
		 * <tr><td>i.findLast [e | predicate]</td><td>Finds the last element in the given iterable that
		 *                                            fulfills the predicate.</td></tr>
		 * <tr><td>ii.flatten</td><td>Combines multiple iterables into a single iterable. The returned iterable has an
		 *                           iterator that traverses the elements of each iterable in inputs.</td></tr>
		 * <tr><td>i.fold(seed) [e,l | statements]</td><td>Applies the combinator function to all elements of the iterable in turn.
		 *                                             e is the current itered element. l is the last computed result.
		 *                                             Replies a new value for the combination result.
		 *                                             More formally, given an iterable {@code [a, b, c, d]} and a function
		 *                                             {@code f}, the result of fold is
		 *                                             <code>f(f(f(f(seed, a), b), c), d)</code>.</td></tr>
		 * <tr><td>i.forall [e | predicate]</td><td>Returns true if every element in iterable satisfies the predicate.</td></tr>
		 * <tr><td>i.forEach [e | statements]<br/>
		 *         i.forEach [e,c | statements]</td><td>Applies the given procedure for each element
		 *                                            of the given iterable. c is the number of the loop.</td></tr>
		 * <tr><td>i.groupBy [e | key]</td><td>Returns a map for which the values is a collection of lists, where the
		 *                                     elements in the list will appear in the order as they appeared in the
		 *                                     iterable. Each key is the product of invoking the supplied  function
		 *                                     on its corresponding value. So a key of that map groups a list of
		 *                                     values for which the function produced exactly that key.</td></tr>
		 * <tr><td>i.head</td><td>Returns the first element in the given iterable or null if empty.</td></tr>
		 * <tr><td>i.indexed</td><td>Returns an Iterable of Pairs where the n-th pair is created by taking the n-th element of
		 *                           the source as the value and its 0-based index as the key.</td></tr>
		 * <tr><td>i.isEmpty</td><td>Determines if the given iterable contains no elements.</td></tr>
		 * <tr><td>i.isNullOrEmpty</td><td>Determines if the given iterable is <code>null</code> or contains no elements.</td></tr>
		 * <tr><td>i.join<br/>
		 *         i.join(sep) [e| statement]<br/>
		 *         i.join(prefix,sep,postfix) [e| statement]</td><td>Returns the concatenated string representation of the
		 *                                          elements in the given iterable. sep is the separator between the
		 *                                          elements. prefix and postfix are added to the final result.
		 *                                          The function is computing the string representation of the elements.</td></tr>
		 * <tr><td>i.last</td><td>Returns the last element in the given iterable or null if empty.</td></tr>
		 * <tr><td>i.map [e | transformation]</td><td>Returns a iterable that performs the given transformation for
		 *                                            each element of original when requested.
		 *                                            The mapping is done lazily. That is, subsequent
		 *                                            iterations of the elements in the list will
		 *                                            repeatedly apply the transformation. The returned list is
		 *                                            a transformed view of original; changes to original will
		 *                                            be reflected in the returned iterator and vice versa.</td></tr>
		 * <tr><td>i.max<br/>
		 *         i.max(comparator)<br/>
		 *         i.maxBy [e | comparable]</td><td>Returns the maximal value in the iterable. If the comparator
		 *                                        or the function is provided, it is used for comparing the
		 *                                        elements of the iterable. Otherwise the natural ordering of
		 *                                        the elements is used.</td></tr>
		 * <tr><td>i.min<br/>
		 *         i.min(comparator)<br/>
		 *         i.minBy [e | comparable]</td><td>Returns the minimal value in the iterable. If the comparator
		 *                                        or the function is provided, it is used for comparing the
		 *                                        elements of the iterable. Otherwise the natural ordering of
		 *                                        the elements is used.</td></tr>
		 * <tr><td>i.reduce [e,l | statements]</td><td>Applies the combinator function to all elements of the iterable in turn.
		 *                                             e is the current itered element. l is the last computed result.
		 *                                             Replies a new value for the comination result.
		 *                                             More formally, given an iterable `[a, b, c, d]` and a function
		 *                                             {@code f}, the result of reduce is
		 *                                             <code>f(f(f(a, b), c), d)</code>.</td></tr>
		 * <tr><td>i.size</td><td>Returns the number of elements in the iterable.</td></tr>
		 * <tr><td>i.sort<br/>
		 *         i.sortWith(comparator)<br/>
		 *         i.sortBy [e | comparable]</td><td>Creates a sorted list that contains the items of the given iterable.
		 *                                        If the comparator or the function is provided, it is used for comparing the
		 *                                        elements of the iterable. Otherwise the natural ordering of
		 *                                        the elements is used.</td></tr>
		 * <tr><td>i.tail</td><td>Returns a view on this iterable that contains all the elements except the first.</td></tr>
		 * <tr><td>i.take(n)</td><td>Returns a view on this iterable that provides at most the first n entries.</td></tr>
		 * <tr><td>i.takeWhile [e | predicate]</td><td>Returns an Iterable containing all elements starting from the head of
		 *                                             the source up to and excluding the first element that violates the
		 *                                             predicate. The resulting Iterable is a lazily computed view, so
		 *                                             any modifications to the underlying Iterables will be reflected
		 *                                             on iteration.</td></tr>
		 * <tr><td>i.toInvertedMap [e | value]</td><td>Returns a map for which the values are computed by the given function, and each
		 *                                 key is an element in the given iterable. If the iterable contains equal keys more than
		 *                                 once, the last one will be contained in the map. The map is computed eagerly. That
		 *                                 is, subsequent changes in the keys are not reflected by the map.</td></tr>
		 * <tr><td>i.toList</td><td>Wraps an iterable in a List.</td></tr>
		 * <tr><td>i.toMap [e | key]</td><td>Returns a map for which the keys are computed by the given function, and each
		 *                                 value is an element in the given iterable.</td></tr>
		 * <tr><td>i.toSet</td><td>Wraps an iterable in a Set.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Iterable extension" {
			'''
			package io.sarl.docs.reference.gsr
			import java.util.Comparator
			import java.util.List
			import java.util.Map
			import java.util.Set
			agent A {
				var i : Iterable<Integer>
				var ii : Iterable<Iterable<Integer>>
				def doNothing {}
				def example0 : Object {
					var r : Iterable<Integer>
					r = i.drop(5)
				}
				def example1 : Object {
					var r : Iterable<Integer>
					r = i.dropWhile [e | true]
				}
				def example2_a : Object {
					var r : boolean
					var i2 : Iterable<Integer>
					r = i.elementsEqual(i2)
				}
				def example2_b : Object {
					var r : boolean
					var i2 : Iterable<Integer>
					r = i.elementsEqual(i2)
				}
				def example3 : Object {
					var r : boolean
					r = i.exists [e | e == 5]
				}
				def example4_a : Object {
					var r : Iterable<Integer>
					r = i.filter [e | e == 5]
				}
				def example4_b : Object {
					var r : Iterable<Number>
					r = i.filter(typeof(Number))
				}
				def example5 : Object {
					var r : Iterable<Integer>
					r = i.filterNull
				}
				def example6 : Object {
					var r : Integer
					r = i.findFirst [ e | e == 1 ]
				}
				def example7 : Object {
					var r : Integer
					r = i.findLast [ e | e == 1 ]
				}
				def example8 : Object {
					var r : Iterable<Integer>
					r = ii.flatten
				}
				def example9 : Object {
					var r : Integer
					r = i.fold(4) [e,res | e + res]
				}
				def example10 : Object {
					var r : boolean
					r = i.forall [e | e == 5]
				}
				def example11_a {
					i.forEach [e | doNothing]
				}
				def example11_b {
					i.forEach [e,c | doNothing]
				}
				def example12 : Object {
					var r : Map<String,List<Integer>>
					r = i.groupBy [e | e.toString]
				}
				def example13 : Object {
					var r : Integer
					r = i.head
				}
				def example14 : Object {
					var r : Iterable<Pair<Integer,Integer>>
					r = i.indexed
				}
				def example15 : Object {
					var r : boolean
					r = i.isEmpty
				}
				def example16 : Object {
					var r : boolean
					r = i.isNullOrEmpty
				}
				def example17_a : Object {
					var s : String
					s = i.join
				}
				def example17_b : Object {
					var s : String
					s = i.join("-") [e | e.toString]
				}
				def example17_c : Object {
					var s : String
					s = i.join("{","-","}") [e | e.toString]
				}
				def example18 : Object {
					var r : Integer
					r = i.last
				}
				def example19 : Object {
					var r : Iterable<Double>
					r = i.map [e | e.doubleValue]
				}
				def example20_a : Object {
					var r : Integer
					r = i.max
				}
				def example20_b : Object {
					var r : Integer
					var c : Comparator<Integer>
					r = i.max(c)
				}
				def example20_c : Object {
					var r : Integer
					r = i.maxBy[e | e]
				}
				def example21_a : Object {
					var r : Integer
					r = i.min
				}
				def example22_b : Object {
					var r : Integer
					var c : Comparator<Integer>
					r = i.min(c)
				}
				def example22_c : Object {
					var r : Integer
					r = i.minBy [e | e]
				}
				def example23 : Object {
					var r : Integer
					r = i.reduce [e,res | e + res]
				}
				def example24 : Object {
					var r : int
					r = i.size
				}
				def example25_a : Object {
					var r : List<Integer>
					r = i.sort
				}
				def example25_b : Object {
					var r : List<Integer>
					var c : Comparator<Integer>
					r = i.sortWith(c)
				}
				def example25_c : Object {
					var r : List<Integer>
					r = i.sortBy [e | e]
				}
				def example26 : Object {
					var r : Iterable<Integer>
					r = i.tail
				}
				def example27 : Object {
					var r : Iterable<Integer>
					r = i.take(5)
				}
				def example28 : Object {
					var r : Iterable<Integer>
					r = i.takeWhile [e | true]
				}
				def example29 : Object {
					var r : Map<Integer,String>
					r = i.toInvertedMap [e | e.toString]
				}
				def example30 : Object {
					var r : Map<String,Integer>
					r = i.toMap [e | e.toString]
				}
				def example31 : Object {
					var r : List<Integer>
					r = i.toList
				}
				def example32 : Object {
					var r : Set<Integer>
					r = i.toSet
				}
			}
			'''.parseSuccessfully
		}

		/* The `Iterator` classes is extended with a collection of functions.
		 * 
		 * <p>The same functions as for the `Iterable` class are provided (see above), except
		 * `flatten`, `sort`, `sortBy`, `sortWith`.
		 * 
		 * <p>Additionnaly, the following functions extends the iterator type, where
		 * i is an instance of `Iterator`.
		 *  
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>i.toIterable</td><td>Wraps an iterator in an Iterable.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Iterator extension" {
			'''
			package io.sarl.docs.reference.gsr
			import java.util.Iterator
			agent A {
				var i : Iterator<Integer>
				def example0 : Object {
					var r : Iterable<Integer>
					r = i.toIterable
				}
			}
			'''.parseSuccessfully
		}

		/* The `List` data structure is extended with the following functions, where
		 * l is a `List`.
		 * 
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>l.immutableCopy</td><td>Returns an immutable copy of the specified list.</td></tr>
		 * <tr><td>l.unmodifiableView</td><td>Returns an unmodifiable view of the specified list.</td></tr>
		 * <tr><td>l.map [e | transformation]</td><td>Returns a list that performs the given transformation for
		 *                                            each element of original when requested.
		 *                                            The mapping is done lazily. That is, subsequent
		 *                                            iterations of the elements in the list will
		 *                                            repeatedly apply the transformation. The returned list is
		 *                                            a transformed view of original; changes to original will
		 *                                            be reflected in the returned list and vice versa.</td></tr>
		 * <tr><td>l.reverse</td><td>Reverses the order of the elements in the specified list. The list itself
		 *                           will be modified.</td></tr>
		 * <tr><td>l.reverseView</td><td>Provides a reverse view on the given list which is especially useful
		 *                               to traverse a list backwards in a for-each loop. The list itself is
		 *                               not modified by calling this method.</td></tr>
		 * <tr><td>l.sortInplace<br/>
		 *         l.sortInplace(comparator)</td><td>
		 *                               Sorts the specified list itself into ascending order. It the
		 *                               comparator is given, it is used for comparing the elements.
		 *                               Otherwise, the natural ordering of its elements is use.</td></tr>
		 * <tr><td>l.sortInplaceBy [e | comparable_value]</td><td>Sorts the specified list itself according to the order
		 *                                             induced by applying a key function to each element which
		 *                                             yields a comparable criteria.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "List extension" {
			'''
			package io.sarl.docs.reference.gsr
			import java.util.List
			import java.util.Comparator
			agent A {
				var l : List<Integer>
				var r : List<Integer>
				var o : Object
				def example0 : Object {
					r = l.map [e | e + 1]
				}
				def example1 : Object {
					r = l.reverse
				}
				def example2 : Object {
					r = l.reverseView
				}
				def example3 : Object {
					r = l.sortInplace
				}
				def example4 : Object {
					var c : Comparator<Integer>
					r = l.sortInplace(c)
				}
				def example5 : Object {
					r = l.sortInplaceBy [e | e]
				}
				def example6 : Object {
					r = l.immutableCopy
				}
				def example7 : Object {
					r = l.unmodifiableView
				}
			}
			'''.parseSuccessfully
		}

		/* The `Set` data structure is extended with the following functions, where
		 * s is a `Set`.
		 * 
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>s.immutableCopy</td><td>Returns an immutable copy of the specified set.</td></tr>
		 * <tr><td>s.unmodifiableView</td><td>Returns an unmodifiable view of the specified set.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Set extension" {
			'''
			package io.sarl.docs.reference.gsr
			import java.util.Set
			agent A {
				var s : Set<Integer>
				var r : Set<Integer>
				def example0 : Object {
					r = s.immutableCopy
				}
				def example1 : Object {
					r = s.unmodifiableView
				}
			}
			'''.parseSuccessfully
		}

		/* The `Map` data structure is extended with the following functions, where
		 * m, m1 are instances of `Map`. p is an instance of `Pair`. k is a key instance, and
		 * ks is an iterable on keys. 
		 * 
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>m-&gt;k</td><td>Replies the value associated to the given key in the map.</td></tr>
		 * <tr><td>m + m1</td><td>Merge the two maps for obtaining a new map.</td></tr>
		 * <tr><td>m + p</td><td>Add the entries of the input map into the output map.</td></tr>
		 * <tr><td>m += m1</td><td>Add the entries of the input map into the output map.
		 *                                    If a key in the input map already exists in the output map,
		 *                                    its value is replaced in the output map by the value from
		 *                                    the input map.</td></tr>
		 * <tr><td>m += p</td><td>Add the given pair into the map. If the pair key already exists in the map, 
		 *                        its value is replaced by the value in the pair, and the old value in the
		 *                        map is returned.</td></tr>
		 * <tr><td>m - k</td><td>Replies in a new map the elements of the given map except the pair with the given key.</td></tr>
		 * <tr><td>m - ks</td><td>Replies the elements of the given map except the pairs with the given keys.</td></tr>
		 * <tr><td>m - m1</td><td>Replies the elements of the left map without the pairs in the right map.</td></tr>
		 * <tr><td>m -= k</td><td>emove a key (and the associated value) from the given map.</td></tr>
		 * <tr><td>m -= ks</td><td>Remove the pairs that have there key equals to the values in the given iterable.</td></tr>
		 * <tr><td>m.filter [k,v | boolean value]</td><td>Returns a filtered live view on top of the original map.
		 *                                                Changes to one affect the other.
		 *                                                The replied map contains the elements of m for which
		 *                                                the given function has replied true.</td></tr>
		 * <tr><td>m.forEach [k,v | statements]<br/>
		 *         m.forEach [k,v,c:int | statements]</td><td>Applies the given procedure for each key-value pair
		 *                                              of the given map. c is the loop counter, starting with
		 *                                              0 for the first pair.</td></tr>
		 * <tr><td>m.immutableCopy</td><td>Returns an immutable copy of the specified map.</td></tr>
		 * <tr><td>m.mapValues [v | transformation]</td><td>Returns a map that performs the given transformation
		 *                                                  for each value of original when requested.
		 *                                                  The mapping is done lazily. That is, subsequent access of
		 *                                                  the values in the map will repeatedly apply the
		 *                                                  transformation. Characteristics of the original map, 
		 *                                                  such as iteration order, are left intact. Changes in the
		 *                                                  original map are reflected in the result map. The results
		 *                                                  supports removal if the original map supports removal.</td></tr>
		 * <tr><td>m.union(m1)</td><td>Replies a view on the merging of the two given maps.</td></tr>
		 * <tr><td>m.unmodifiableView</td><td>Returns an unmodifiable view of the specified map.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Map extension" {
			'''
			package io.sarl.docs.reference.gsr
			import static io.sarl.lang.scoping.batch.SARLMapExtensions.*
			import java.util.Map
			agent A {
				def checkMapsTo(m : Map<String, Integer>, k : String) : Integer {
					m->k
				}
				def checkPlusMap(m1 : Map<String, Integer>, m2 : Map<String, Integer>) : Map<String, Integer> {
					m1 + m2
				}
				def checkPlusPair(m : Map<String, Integer>, p : Pair<String, Integer>) : Map<String, Integer> {
					m + p
				}
				def checkAddMap(m1 : Map<String, Integer>, m2 : Map<String, Integer>) : void {
					m1 += m2
				}
				def checkAddPair(m : Map<String, Integer>, p : Pair<String, Integer>) : void {
					m += p
				}
				def checkMinusPair(m : Map<String, Integer>, k : String) : Map<String, Integer> {
					m - k
				}
				def checkMinusPairs(m : Map<String, Integer>, ks : Iterable<String>) : Map<String, Integer> {
					m - ks
				}
				def checkMinusMap(m1 : Map<String, Integer>, m2 : Map<String, Integer>) : Map<String, Integer> {
					m1 - m2
				}
				def checkRemovePair(m : Map<String, Integer>, k : String) : void {
					m -= k
				}
				def checkRemovePairs(m : Map<String, Integer>, ks : Iterable<String>) : void {
					m -= ks
				}
				def checkFilter(m : Map<String, Integer>) : Map<String, Integer> {
					m.filter [k,v | true]
				}
				def checkForEach(m : Map<String, Integer>) : void {
					val i = new StringBuffer
					m.forEach [k,v | i.append(v)]
				}
				def checkImmutableCopy(m : Map<String, Integer>) : Map<String, Integer> {
					m.immutableCopy
				}
				def checkMapValues(m : Map<String, Integer>) : Map<String, Integer> {
					m.mapValues [it + 1]
				}
				def checkUnion(m1 : Map<String, Integer>, m2 : Map<String, Integer>) : Map<String, Integer> {
					m1.union(m2)
				}
				def checkUnmodifiableView(m : Map<String, Integer>) : Map<String, Integer> {
					m.unmodifiableView
				}
			}
			'''.parseSuccessfully
		}

		/* A procedure is a lambda expression replying nothing. For example, the following
		 * code is defining a procedure without parameter:
		 *
		 * 		var proc : () => void
		 * 		proc = [ statements ]
		 * 
		 * <p>A function is a lambda expression replying a value. For example, the following
		 * code is defining a function without parameter and replying an integer:
		 *
		 * 		var func : () => int
		 * 		func = [ 1 ]
		 * 
		 * <p>For the procedures/functions with 1 to 6 formal parameters, the following function is defined below,
		 * where f is the procedure/function and p is the first parameter of proc.
		 * 
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>f.curry(p)</td><td>If f has n formal parameters, the curly function replies
		 *                               a procedure/function with (n-1) parameters. This replied
		 *                               procedure/function
		 *                               is calling f with p as the first argument, and the
		 *                               arguments of the replied procedure/function are passed to f.<br/>
		 *                               Below, the lines 1 and 3 are equivalent:
		 *                               <pre><code>
		 *                               proc(1, 2, 3, 4)
		 *                               var cproc = proc.curly(1)
		 *                               cproc(2, 3, 4)
		 *                               </code></pre></td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Procedure and Function extensions" {
			'''
			package io.sarl.docs.reference.gsr
			import org.eclipse.xtext.xbase.lib.Procedures
			import org.eclipse.xtext.xbase.lib.Functions
			agent A {
				var n : Integer
				def example0_0 {
					var cproc : Procedures.Procedure1<Integer>
					cproc.curry(n).apply
				}
				def example0_1 {
					var cproc : Procedures.Procedure2<Integer,Integer>
					cproc.curry(n).apply(1)
				}
				def example0_2 {
					var cproc : Procedures.Procedure3<Integer,Integer,Integer>
					cproc.curry(n).apply(1,2)
				}
				def example0_3 {
					var cproc : Procedures.Procedure4<Integer,Integer,Integer,Integer>
					cproc.curry(n).apply(1,2,3)
				}
				def example0_4 {
					var cproc : Procedures.Procedure5<Integer,Integer,Integer,Integer,Integer>
					cproc.curry(n).apply(1,2,3,4)
				}
				def example0_5 {
					var cproc : Procedures.Procedure6<Integer,Integer,Integer,Integer,Integer,Integer>
					cproc.curry(n).apply(1,2,3,4,5)
				}
				def example1_0 : Integer {
					var cfunc : Functions.Function1<Integer,Integer>
					cfunc.curry(n).apply
				}
				def example1_1 : Integer {
					var cfunc : Functions.Function2<Integer,Integer,Integer>
					cfunc.curry(n).apply(1)
				}
				def example1_2 : Integer {
					var cfunc : Functions.Function3<Integer,Integer,Integer,Integer>
					cfunc.curry(n).apply(1,2)
				}
				def example1_3 : Integer {
					var cfunc : Functions.Function4<Integer,Integer,Integer,Integer,Integer>
					cfunc.curry(n).apply(1,2,3)
				}
				def example1_4 : Integer {
					var cfunc : Functions.Function5<Integer,Integer,Integer,Integer,Integer,Integer>
					cfunc.curry(n).apply(1,2,3,4)
				}
				def example1_5 : Integer {
					var cfunc : Functions.Function6<Integer,Integer,Integer,Integer,Integer,Integer,Integer>
					cfunc.curry(n).apply(1,2,3,4,5)
				}
			}
			'''.parseSuccessfully
		}

		/* The SARL language provides extension functions for computing times and durations.
		 * In the following table, x is a long integer number.
		 * 
		 * <table><thead>
		 * <tr><th>Method</th><th>Semantic</th></tr>
		 * </thead><tbody>
		 * <tr><td>x.milliseconds</td><td>Convert x milliseconds to milliseconds.</td></tr>
		 * <tr><td>x.seconds</td><td>Convert x seconds to milliseconds.</td></tr>
		 * <tr><td>x.minutes</td><td>Convert x minutes to milliseconds.</td></tr>
		 * <tr><td>x.hours</td><td>Convert x hours to milliseconds.</td></tr>
		 * <tr><td>x.weeks</td><td>Convert x weeks to milliseconds.</td></tr>
		 * </tbody></table>
		 * 
		 * @filter(.*) 
		 */
		fact "Time computation extension" {
			'''
			package io.sarl.docs.reference.gsr
			import java.util.Set
			agent A {
				def example0 : Object {
					1234.milliseconds
				}
				def example1 : Object {
					1234.seconds
				}
				def example2 : Object {
					1234.minutes
				}
				def example3 : Object {
					1234.hours
				}
				def example4 : Object {
					1234.weeks
				}
			}
			'''.parseSuccessfully
		}

	}

	/* A lambda expression is basically a piece of code, which is wrapped 
	 * in an object to pass it around. As a Java developer it is best to 
	 * think of a lambda expression as an anonymous class with a single 
	 * method.
	 */
	describe "Lambda Expressions" {
		
		/* That is the code above can be written in SARL for creating a lambda exception.
		 * A lambda expression is surrounded by square brackets (inspired from Smalltalk).
		 * Lambda expression like a method declares parameters. 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Basic Definition"{
			'''
					val textField = new JTextField
					// Define a lambda expression that take an ActionEvent as parameter
					// It is the definition of a function of type: (ActionEvent) => void
					textField.addActionListener([ e : ActionEvent |
							textField.text = "Something happened!" + e.toString
						])
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.awt.^event.ActionEvent
				import javax.swing.JTextField
				agent A {
					def example {",
				// TEXT
				"} }"
			)
		}
		
		/* The lambda above has one parameter called e which is of type `ActionEvent`.
		 *
		 * <p>You do not have to specify the type explicitly because it can be inferred from the context.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Inferred Parameter Type"{
			'''
					val textField = new JTextField
					// Define a lambda expression that take an ActionEvent as parameter
					// It is the definition of a function of type: (ActionEvent) => void
					textField.addActionListener([ e |
							textField.text = "Something happened!" + e.toString
						])
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				import javax.swing.JTextField
				agent A {
					def example {",
				// TEXT
				"} }"
			)
		}

		/* As lambdas with one parameter are a common case, there is a special short hand notation 
		 * for these parameters, which is to leave the declaration including the vertical bar out. 
		 * The name of the single parameter becomes `it`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Implicit Parameters: it"{
			'''
					val textField = new JTextField
					// Define a lambda expression that take an ActionEvent as parameter
					// It is the definition of a function of type: (ActionEvent) => void
					textField.addActionListener([
							textField.text = "Something happened!" + it.toString
						])
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				import javax.swing.JTextField
				agent A {
					def example {",
				// TEXT
				"} }"
			)
		}

		/* A lambda expression with zero arguments is written like this (note the bar after the opening bracket):
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Empty List of Parameters"{
			'''
					val runnable : Runnable = [ |
							println("Hello I'm executed!")
						]
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					def example {",
				// TEXT
				"} }"
			)
		}
		
		/* When the last argument of a method call is a lambda, it can be passed right after 
		 * the argument list.
		 * 
		 * <p>For instance if you want to sort some strings by their length, you could write
		 * the following two codes.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Lambda as the Last Parameter of a Method"{
			'''
				var t : List<String>
				def example1 {
					// Lambda expression is written outside the parenthesis
					Collections.sort(t) [ a, b |
								a.length - b.length
								]
				}
				def example2 {
					// Lambda expression is written inside the parenthesis
					Collections.sort(t, [ a, b |
								a.length - b.length
								]
					)
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.util.List
				import java.util.Collections
				agent A {",
				// TEXT
				"}"
			)
		}

		/* The type of a lambda will be one of the inner types found in `Functions`
		 * or `Procedures`. It is a procedure if the return type is `void`,
		 * otherwise it is a function.
		 * 
		 * <p>The syntax for specifying the type of a lambda is: `(parameter types) => return type`
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Typing"{
			'''
				// Define a function f, which takes one parameter of 
				// typeString, and returning a value of type String. 
				var f1 : (String) => String
				
				// Same type of function.
				var f2 : Function1<? super String,? extends String>
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				import org.eclipse.xtext.xbase.lib.Functions.Function1
				agent A {",
				// TEXT
				"}"
			)
		}
	}
	
	/* An if-expression is used to choose between two different values based on a predicate.
	 */
	describe "If Expression" {
		
		/* Results in either the value e1 or e2 depending on whether the predicate p evaluates to 
		 * `true` or `false`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Classic Syntax"{
			'''
					if (e1 !== null) e1 else e2
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var e1 : Object
					var e2 : Object
					def example1 : Object {",
				// TEXT
				"} }"
			)
		}
		
		/* The else part is optional, which is a shorthand for an else branch that returns the 
		 * default value of the current type.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Optional Else Part"{
			'''
					if (e1 !== null) e1 /* else null */
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var e1 : Object
					var e2 : Object
					def example1 : Object {",
				// TEXT
				"} }"
			)
		}

		/* While the if expression has the syntax of Java's if statement it behaves more 
		 * like Java's ternary operator (`predicate ? thenPart : elsePart`),
		 * because it is an expression and returns a value. 
		 * Consequently, you can use if expressions deeply nested within expressions:
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Inlined If Expression"{
			'''
					val name = if (e1 != null) e1 + ' ' + e2 else e2
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var e1 : Object
					var e2 : Object
					def example {",
				// TEXT
				"} }"
			)
		}

	}

	/* The switch expression is very different from Java's switch statement. The use of switch is 
	 * not limited to certain values, but can be used for any object reference. 
	 * The operator `==` or its Java-equivalent `Object.equals(Object)` are 
	 * used to compare the value in the case with the one you are switching over.
	 */
	describe "Switch Expression" {
		
		/* Consider the following example.
		 * the main expression `myString` is evaluated first and then compared to each 
		 * case sequentially. If the case expression is of type boolean, the case matches 
		 * if the expression evaluates to `true`. If it is not of type boolean it is 
		 * compared to the value of the main expression using the operator `==`.
		 * 
		 * <p>If a case is a match, the case expression after the colon is evaluated and is 
		 * the result of the whole switch expression. Note that there is no need for 
		 * a `break` keyword, as in Java: the case following the matching case is
		 * never evaluated.
		 * 
		 * <p>The main expression (parameter of `switch`) can also be a computed value instead 
		 * of a field or variable.
		 * 
		 * <importantnote> 
		 * A case must contains an expression. If you want to do nothing
		 * for a given case, put an empty block.</importantnote>
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Cases"{
			'''
				def example1 : String {
					switch myString {
					case myString.length > 5 : "a long string."
					case 'some' : "It's some string."
					default : "It's another short string."
					}
				}
				def computeString : String {
					return myString
				}
				def example2 : String {
					switch computeString {
					case 'some' : "It's some string."
					default : "It's another short string: "
					}
				}
				def example3 {
					switch myString {
					case 'some' : println("It's some string.")
					default : { }
					}
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var myString = \"abc\"",
				// TEXT
				"}"
			)
		}
		
		/* In addition to the case guard, you can specify a type 
		 * guard.
		 * The case only matches if the switch value conforms to a given type.
		 * A case with both a type guard and a predicate only matches if both conditions match. 
		 * If the switch value is a field, parameter or variable, it is automatically casted 
		 * to the given type within the predicate and the case body.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Type Guards"{
			'''
					switch myString {
					String case myString.length==5 : "It's string of length 5."
					String : "a string."
					}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var myString : Object
					def example2 : String {",
				// TEXT
				"} }"
			)
		}

		/* You can have multiple type guards and cases separated with a comma, to
		 * have all of them share the same then part.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Fall Through"{
			'''
					switch myString {
					case myString.length==5,
					case 'some' : println("a string.")
					default: println("Default")
					}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var myString : String
					def example {",
				// TEXT
				"} }"
			)
		}

	}

	/* SARL provides four types of loop statements.
	 */
	describe "Loops" {

		/* The for loop is used to execute a certain expression for each 
		 * element of an array or an instance of `Iterable`. 
		 * 
		 * <p>The for's variable is local and final, hence cannot be updated.
		 * 
		 * <p>The type of a for loop is `void`. The type of the local
		 * variable can be inferred from the 
		 * iterable or array that is processed.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "For Loop"{
			'''
						var tab : String[]
						// The type of the local variable is inferred
						for (v : tab) {
							println(v)
						}
						// The type of the local variable is explicit
						for (v as String : tab) {
							println(v)
						}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}
		
		/* The traditional for loop is very similar to the one known from Java, or even C. 
		 * When executed, it first executes the init-expression, where local variables can be 
		 * declared. Next the predicate is executed and if it evaluates to `true`, the 
		 * body-expression is executed. On any subsequent iterations the update-expression
		 * is executed instead of the init-expression. This happens until the predicate
		 * returns `false`. The type of a for loop is `void`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Traditional Java For Loop" {
			'''
						for (var i = 0; i<123; i++) {
							println(i)
						}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}

		/* A while loop is used to execute a certain expression unless the predicate is evaluated 
		 * to `false`. The type of a while loop is `void`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "While Loop" {
			'''
						var i = 0
						while (i<123) {
							println(i)
							i++
						}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}
	
		/* A while loop is used to execute a certain expression unless the predicate is evaluated 
		 * to `false`. The difference to the while loop is that the execution starts by 
		 * executing the block once before evaluating the predicate for the first time. 
		 * The type of a while loop is `void`.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Do-While Loop" {
			'''
						var i = 0
						do {
							println(i)
							i++
						}
						while (i<123)
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}

	}

	/* SARL supports exception throwing and catching.
	 * The mechanism is similar to the one of Java.
	 * 
	 * <p>For defining the exceptions that may be thrown by a function,
	 * please see [how to declare exceptions in a function prototype](#DeclareExceptionsInTheFunctionPrototype).
	 */
	describe "Exception Support" {
		
		/* Throwing objects of type `Throwable` and the `throw` keyword have the same semantics
		 * and syntax as in Java, see 
		 * [Java Language Specification](http://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.18). 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Throwing Exceptions"{
			'''
						throw new IllegalArgumentException("explanation")
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
			"#DeclareExceptionsInTheFunctionPrototype" should beAccessibleFrom this
			"http://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.18" should beURL _
		}
		
		/* The try-catch-finally expression is used to handle exceptional situations. 
		 * Checked exceptions are treated like runtime exceptions and only optionally 
		 * validated. You can, but do not have to, catch them as they will be silently thrown. 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Try, Catch, Finally"{
			'''
						try {
							throw new RuntimeException()
						}
						catch(e : Exception) {
							// Handle the exception
						}
						finally {
							// Do this block after the try block (if no exception thrown), 
							// the matched catch block (if an exception was catched),
							// or before exiting the function (if an exception was thrown
							// but not catched).
						}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}

		/* For try-catch, the argument is an expression. Consequently, you can
		 * write code like the following and do not have to rely on
		 * non-final variables: 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "Try-Catch as an Expression"{
			'''
					def readFromFile : String { } 
					def example {
						val name =	try {
										readFromFile
									} catch (e : IOException) {
										"unknown"
									}
						println(name)
					}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.io.IOException
				agent A {",
				// TEXT
				"}"
			)
		}

	}

	/* The synchonized expression does the same as it does in Java (see 
	 * [Java Language Specification](http://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.19)).
	 * The only difference is 
	 * that in SARL it is an expression and can therefore be used at
	 * more places. 
	 */
	describe "Synchronized Expression"{

		/* The synchronization statement can be used as in Java:
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */		
		fact "Classic Syntax" {
			'''
					var lock = new Object
					synchronized(lock) {
						println("Hello")
					}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					def example : Object {",
				// TEXT
				"} }"
			)
		}
	
		/* Because the synchronization keyword is related to an expression,
		 * it is possible to write synchronized code inside another expression.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */		
		fact "Expression Syntax" {
			'''
					var lock = new Object
					val name = synchronized(lock) { 
							"Hello" 
						}
					println(name)
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					def example {",
				// TEXT
				"} }"
			)
		}

	}
	
	/* This documentation is based on documentations from the Xtext and Xtend projects.
	 * Thank you to the contributors to these documents.
	 * 
	 * * [Xtend](https://www.eclipse.org/xtend/documentation.html)
	 * * [Xtext](https://www.eclipse.org/Xtext/documentation.html)
	 *
	 * @filter(.*) 
	 */
	fact "References" {
		true
	}

	/* Specification: SARL General-purpose Agent-Oriented Programming Language ("Specification")<br/>
	 * Version: %sarlspecversion%<br/>
	 * Status: %sarlspecreleasestatus%<br/>
	 * Release: %sarlspecreleasedate%
	 * 
	 * 
	 * <p>Copyright &copy; %copyrightdate% %copyrighters%. All rights reserved.
	 * 
	 * <p>Licensed under the Apache License, Version 2.0;
	 * you may not use this file except in compliance with the License.
	 * You may obtain a copy of the [License](http://www.apache.org/licenses/LICENSE-2.0).
	 *
	 * @filter(.*) 
	 */
	fact "Legal Notice" {
		// The checks are valid only if the macro replacements were done.
		// The replacements are done by Maven.
		// So, Eclipse Junit tools do not make the replacements.
		System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.").assumeFalse
		//
		"%sarlversion%" should startWith "%sarlspecversion%"
		("%sarlspecreleasestatus%" == "Stable Release"
			|| "%sarlspecreleasestatus%" == "Draft Release") should be true
		"%sarlspecreleasedate%" should beDate "YYYY-mm-dd"
		"%copyrightdate%" should beNumber "0000";
		("%copyrighters%".empty || "%copyrighters%".startsWith("%")) should be false
	}

}
