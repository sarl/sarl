/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
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
import java.math.BigDecimal
import java.math.BigInteger
import org.eclipse.xtext.xbase.XBlockExpression
import org.eclipse.xtext.xbase.XBooleanLiteral
import org.eclipse.xtext.xbase.XCastedExpression
import org.eclipse.xtext.xbase.XConstructorCall
import org.eclipse.xtext.xbase.XFeatureCall
import org.eclipse.xtext.xbase.XListLiteral
import org.eclipse.xtext.xbase.XMemberFeatureCall
import org.eclipse.xtext.xbase.XNullLiteral
import org.eclipse.xtext.xbase.XNumberLiteral
import org.eclipse.xtext.xbase.XSetLiteral
import org.eclipse.xtext.xbase.XStringLiteral
import org.eclipse.xtext.xbase.XTypeLiteral
import org.jnario.runner.CreateWith

/*
 * This document describes the general syntax of the SARL Language. 
 * While we will use the agent definition, it is also valid for other 
 * concepts. Please see the specific Reference documentation 
 * for details.
 */
@CreateWith(SARLSpecCreator)
describe "General Syntax Reference" {

	@Inject extension SARLParser
	
	/* SARL, like Java, is a statically typed language. In fact it completely supports 
	 * Java's type system, including the primitive types like int or boolean, 
	 * arrays and all the Java classes, interfaces, enums and annotations that reside 
	 * on the class path.
	 * 
	 * Java generics are fully supported as well: You can define type parameters on 
	 * methods and classes and pass type arguments to generic types just as you are 
	 * used to from Java. The type system and its conformance and casting rules are 
	 * implemented as defined in the
	 * [Java Language Specification](http://docs.oracle.com/javase/specs/jls/se5.0/html/conversions.html).
	 * 
	 * One of the problems with Java is that you are forced to write type signatures 
	 * over and over again. That is why so many people do not like static typing. 
	 * But this is in fact not a problem of static typing but simply a problem with 
	 * Java. Although SARL is statically typed just like Java, you rarely have to 
	 * write types down because they can be computed from the context.
	 * 
	 * In addition to Java's autoboxing to convert primitives to their corresponding wrapper 
	 * types (e.g. int is automatically converted to Integer when needed), there are 
	 * additional conversion rules in SARL: arrays are automatically converted to
	 * <code>List&lt;ComponentType&gt;</code> and vice versa.
	 *
	 * Resembling and supporting every aspect of Java's type system ensures that there is 
	 * no impedance mismatch between Java and SARL. __This means that SARL and Java are 
	 * 100% interoperable__. There are no exceptional cases and you do not have to 
	 * think in two worlds. You can invoke SARL code from Java and vice versa without any
	 * surprises or hassles.	
	 */
	describe "Java Interoperability" {
		
	}

	/* In SARL, the names of the features (agents, variables, fields, etc.)
	 * cannot be one of the keywords of SARL or Java.
	 * For example, it is forbidden to type:<pre><code>
	 * import java.awt.event.ActionEvent 
	 * </code></pre>
	 * Indeed, the name fragment <code>event</code> corresponds to a kerword
	 * of sarl.
	 * 
	 * For solving this problem (since some names comes from Java, and
	 * this language has not the same set of keywords than SARL), it
	 * is possible to prefix the name fragment with the character <code>^</code>:<pre><code>
	 * import java.awt.^event.ActionEvent 
	 * </code></pre>
	 */
	describe "Name Syntax" {
		
	}

	/* In SARL, each script must follow the format:<pre><code>
	 * &lt;package definition&gt;
	 * &lt;imports&gt;
	 * &lt;top-level features&gt;
	 * </code></pre>
	 */
	describe "Script Format" {

		/*
		 * For structuring your software, it is convenient to put the scripts
		 * in different packages (as Java does for the classes).
		 * 
		 * The keyword <code>package</code> permits to define the name of
		 * the package associated to a SARL file. Consequently, all
		 * the features defined in the script defined in this package,
		 * and their names are qualified with the name of the package.
		 * 
		 * The package's name has also a consequency in the generation of
		 * the Java files associated to the SARL script. Indeed, all the
		 * Java files are generated in a folder, which is matching the
		 * name of the package.
		 * 
		 * In the following example, the qualified name of the agent is
		 * <code>io.sarl.documentation.example.A</code>.
		 * 
		 * <span class="label label-warning">Important</span> If the 
		 * <code>package</code> keyword is not used, the default package will
		 * be used. The default package has an empty name.
		 * It is recommended by the SARL Best Practices to specify a package's
		 * name always. 
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Package Declaration" {
			var model = '''
				package io.sarl.docs.reference.gsr
				'''.parsesSuccessfully(
				// TEXT
				"agent A {}")
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(0)
		}	 
		
		/* The section part of a SARL script is dedicated to the 
		 * declaration of the imported classes.
		 * Each time you want to use a feature defined in another package
		 * than the one of your SARL script, you shoumd include it
		 * with the <code>import</code> directive.
		 * 
		 * <span class="label label-warning">Important</span> This directive 
		 * works in the same way as in the Java language.
		 * 
		 * The <code>import</code> keyword is following by the qualified name
		 * of the feature to import. In the following code, it is illustrated
		 * by the first directive.
		 * 
		 * Optionnally, you could import all the features defined inside a package.
		 * This could be done by replacing the names of the features with the
		 * wildcard character <code>*</code>. The second import directive is
		 * an example of the inclusion of all the classes defined in
		 * <code>java.net</code>.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Import Directive" {
			var model = '''
			import java.util.List
			import java.net.*
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT
				"agent A {}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustHaveImport(0, "java.util.List", false, false, false)
			model.mustHaveTopElements(1)
			model.elements.get(0).mustBeAgent("A", null)
			
			model.mustHaveImports(2)
			model.mustHaveImport(0, "java.util.List", false, false, false)
			model.mustHaveImport(1, "java.net", false, true, false)
			model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(0)
		}
	
		/* Sometimes, it is mandatory to import a class for accessing its
		 * statically defined functions, i.e. a function that can be
		 * called without any associated object's instance.
		 * 
		 * In this case, the name of the static function is qualified
		 * by the fully qualifed name of the class. For example, 
		 * the function <code>max()</code> is invoked with this
		 * syntax, in the example below.
		 * 
		 * However, if there is plenty of invocations to static
		 * methods in your source code, the static-import mechanism
		 * permits to make the code more readable by removing the
		 * fully qualified name of the clases, in which the called
		 * functions are defined.
		 * 
		 * A static import is specifiy with the <code>static</code>
		 * keyword just after the <code>import</code> keyword.
		 * The following identifier must be a fully qualifed name of
		 * one or more functions (with the wildcard characted).
		 * In the example below, all the functions defined in
		 * <code>java.util.Arrays</code> are imported.
		 * <pre><code>
		 * import static java.util.Arrays.*
		 * </code></pre>
		 * Then,
		 * it is possible to invoked one of them by entering its
		 * name, as the call to <code>toString(int[])</code> below.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.util.Collection
				import java.util.Collections
				import static java.util.Arrays.*			
				agent A {",
				// TEXT 
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustHaveImports(3)
			model.mustHaveImport(0, "java.util.Collection", false, false, false)
			model.mustHaveImport(1, "java.util.Collections", false, false, false)
			model.mustHaveImport(2, "java.util.Arrays", true, true, false)
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(1)
			a.features.get(0).mustBeAction("example", null, 0, false).body.mustBe(XBlockExpression)
			// Do not test the block content since it should be validated by the Xbase library.
		}	 

		/* A large part of a SARL script contains the definitions of
		 * the top-level features. These features are core concepts
		 * of SARL, such as <code>agent</code>, <code>event</code>, or
		 * <code>capacity</code>.
		 * 
		 * All these top-level features are documented in there own
		 * Reference.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Top-Level Features" {
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT
				""
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(5)
			model.elements.get(0).mustBeEvent("E", null).mustHaveFeatures(0)
			model.elements.get(1).mustBeCapacity("C").mustHaveFeatures(0)
			model.elements.get(2).mustBeSkill("S", null, "io.sarl.docs.reference.gsr.C").mustHaveFeatures(0)
			model.elements.get(3).mustBeBehavior("B", null).mustHaveFeatures(0)
			model.elements.get(4).mustBeAgent("A", null).mustHaveFeatures(0)
		}	 

	}

	/* A literal denotes a fixed, unchangeable value. Literals for 
	 * strings, numbers, booleans, null and Java types are 
	 * supported as well as literals for unmodifiable collection 
	 * types like lists, sets and maps or literals for arrays.
	 */	
	describe "Literals"{
	
		/* A string literal is of type <code>String</code>. 
		 * String literals are enclosed in a pair of single quotes 
		 * or double quotes. Special characters can be 
		 * quoted with a backslash or defined using unicode 
		 * notation.
		 * Contrary to Java, strings can span multiple lines.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "String Literals"{
			var model = '''
				var a = 'Hello World!'
				var b = "Hello World!"
				var c = 'Hello "World!"'
				var d = "Hello \"World!\""
				var e = "Hello 
							World!"
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(5)
			a.features.get(0).mustBeAttribute(true, "a", null, true).initialValue.mustBe(XStringLiteral).mustBeEqual("Hello World!")
			a.features.get(1).mustBeAttribute(true, "b", null, true).initialValue.mustBe(XStringLiteral).mustBeEqual("Hello World!")
			a.features.get(2).mustBeAttribute(true, "c", null, true).initialValue.mustBe(XStringLiteral).mustBeEqual("Hello \"World!\"")
			a.features.get(3).mustBeAttribute(true, "d", null, true).initialValue.mustBe(XStringLiteral).mustBeEqual("Hello \"World!\"")
			a.features.get(4).mustBeAttribute(true, "e", null, true).initialValue.mustBe(XStringLiteral).mustBeEqual("Hello \n			World!")
		}
	
		/* Character literals use the same notation as String literals. 
		 * If a single character literal is used in a context where a 
		 * primitive char or the wrapper type <code>Character</code> is expected, 
		 * the compiler will treat the literal as such a value 
		 * or instance.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Character Literals"{
			var model = '''
				var a : char = 'a'
				var b : char = "b"
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(2)
			a.features.get(0).mustBeAttribute(true, "a", "char", true).initialValue.mustBe(XStringLiteral).mustBeEqual('a')
			a.features.get(1).mustBeAttribute(true, "b", "char", true).initialValue.mustBe(XStringLiteral).mustBeEqual('b')
		}
			
		/* SARL supports roughly the same number literals as Java.
		 * There is two exceptions: there is no notation for specifying octal numbers, and 
		 * if you put the dot character in a number, you must specify the fractional and mantissa parts.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Number Literals"{
			var model = '''
				var a = 42		// Decimal
				var b = 0xbeef	// Hexadecimal
				var c = 077		// Decimal 77, NOT octal
				var d = 0.1		// The leading zero must be specified
				var e = 1.0		// The trailing zero must be specified
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(5)
			a.features.get(0).mustBeAttribute(true, "a", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(42)
			a.features.get(1).mustBeAttribute(true, "b", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(0xbeef)
			a.features.get(2).mustBeAttribute(true, "c", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(77)
			a.features.get(3).mustBeAttribute(true, "d", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(0.1)
			a.features.get(4).mustBeAttribute(true, "e", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(1.0)
		}

		/* As in Java 7, you can separate digits using <code>_</code> for
		 * better readability of large numbers.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Large Numbers"{
			var model = '''
				var a = 123_456_78l
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(1)
			a.features.get(0).mustBeAttribute(true, "a", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(12345678l)
		}

		/* Postfixing an integer literal may change its type:
		 * no suffix is for <code>int</code>,
		 * suffix <code>L</code> is for <code>long</code>, and
		 * suffix <code>BI</code> is for <code>BigInteger</code>. 
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Integer suffixes"{
			var model = '''
				var anInteger = 1234
				var aLong = 1234l
				var aBigInteger = 1234bi
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(3)
			a.features.get(0).mustBeAttribute(true, "anInteger", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(1234)
			a.features.get(1).mustBeAttribute(true, "aLong", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(1234l)
			a.features.get(2).mustBeAttribute(true, "aBigInteger", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(new BigInteger("1234"))
		}

		/* Postfixing a floating-point literal may change its type:
		 * no suffix is for <code>double</code>,
		 * suffix <code>D</code> is for <code>double</code>,
		 * suffix <code>F</code> is for <code>float</code>, and
		 * suffix <code>BD</code> is for <code>BigDecimal</code>. 
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Floating-point-value suffixes"{
			var model = '''
				var aDouble = 1234.0
				var anotherDouble = 5678d
				var aFloat = 1234.0f
				var anotherFloat = 5678f
				var aBigDecimal = 1234bd
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(5)
			a.features.get(0).mustBeAttribute(true, "aDouble", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(1234.0)
			a.features.get(1).mustBeAttribute(true, "anotherDouble", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(5678d)
			a.features.get(2).mustBeAttribute(true, "aFloat", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(1234f)
			a.features.get(3).mustBeAttribute(true, "anotherFloat", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(5678f)
			a.features.get(4).mustBeAttribute(true, "aBigDecimal", null, true).initialValue.mustBe(XNumberLiteral).mustBeEqual(new BigDecimal("1234"))
		}

		/* There are two boolean literals, <code>true</code> and <code>false</code>
		 * which correspond to their Java counterpart of type <code>boolean</code>.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Boolean Literals"{
			var model = '''
				var a = true
				var b = false
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr			
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(2)
			a.features.get(0).mustBeAttribute(true, "a", null, true).initialValue.mustBe(XBooleanLiteral).mustBeEqual(true)
			a.features.get(1).mustBeAttribute(true, "b", null, true).initialValue.mustBe(XBooleanLiteral).mustBeEqual(false)
		}
	
		/* The null pointer literal <code>null</code> has exactly the same
		 * semantics as in Java.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Null Literals"{
			var model = '''
				var a = null
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(1)
			a.features.get(0).mustBeAttribute(true, "a", null, true).initialValue.mustBe(XNullLiteral)
		}
	
		/* The syntax for type literals is generally the plain name of the 
		 * type. Nested types use the delimiter <code>'.'</code>.
		 * 
		 * To disambiguate the expression, type literals may also be specified 
		 * using the keyword <code>typeof</code>.
		 * 
		 * Consequently it is possible to access the members of a type 
		 * reflectively by using its plain name.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(3)
			a.features.get(0).mustBeAttribute(true, "a", null, true).initialValue.mustBe(XFeatureCall).mustBeType(String)
			a.features.get(1).mustBeAttribute(true, "b", null, true).initialValue.mustBe(XTypeLiteral).mustBeEqual(Integer)
			a.features.get(2).mustBeAttribute(true, "c", null, true).initialValue.mustBe(XMemberFeatureCall).mustCall("java.lang.Class.getDeclaredFields")
		}
	
	}
	
	/* 
	 */
	describe "Collection Literals"{

		 /* It is easy to create instances of collections since the methods in 
		* <code>CollectionLiterals</code> are automatically imported.
		* They permits to create instances of the collections from the JDK.
		* 
		* @filter(.* = '''|'''|.parsesSuccessfully.*)
		*/
		fact "Collection Creation"{
			var model = '''
				var myList = newArrayList('Hello', 'world')
				var myMap = newLinkedHashMap('a' -> 1, 'b' -> 2)
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(2)
			a.features.get(0).mustBeAttribute(true, "myList", null, true).initialValue.mustBe(XFeatureCall).mustCall("org.eclipse.xtext.xbase.lib.CollectionLiterals.newArrayList")
			a.features.get(1).mustBeAttribute(true, "myMap", null, true).initialValue.mustBe(XFeatureCall).mustCall("org.eclipse.xtext.xbase.lib.CollectionLiterals.newLinkedHashMap")
		}
		
		 /* In addition SARL supports collection literals to create 
		* immutable collections and arrays, depending on the 
		* target type. There is three types of immutable
		* collections: array, set, and hash table.
		* 
		* @filter(.* = '''|'''|.parsesSuccessfully.*)
		*/
		fact "Immutable Collections"{
			var model = '''
				// the variable a contains an immutable array.
				var a = #['Hello','World']
				// the variable b contains an immutable set.
				var b = #{'Hello','World'}
				// the variable c contains an immutable hash table.
				var c = #{'a' -> 1 ,'b' ->2}
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(3)
			a.features.get(0).mustBeAttribute(true, "a", null, true).initialValue.mustBe(XListLiteral)
			a.features.get(1).mustBeAttribute(true, "b", null, true).initialValue.mustBe(XSetLiteral)
			a.features.get(2).mustBeAttribute(true, "c", null, true).initialValue.mustBe(XSetLiteral)
		}
	
	}

	/* Java arrays can be created either using a literal as described in 
	 * the previous section, or if it should be a new array with a 
	 * fixed size.
	 */
	describe "Array Literals"{

		/* The methods from <code>ArrayLiterals</code> is automatically
		 * included. This utility class provides a collection of methods,
		 * such as <code>ArrayLiterals.newArrayOfSize(int)</code>.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */		
		fact "Array Creation"{
			var model = '''
				// variable a contains a array of size 400 which contains Objects.
				var a : String[] = newArrayOfSize(400)
				// variable b contains a array of size 200 which contains int values.
				var b : int[] = newIntArrayOfSize(200)
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(2)
			a.features.get(0).mustBeAttribute(true, "a", "java.lang.String[]", true).initialValue.mustBe(XFeatureCall).mustCall("org.eclipse.xtext.xbase.lib.ArrayLiterals.newArrayOfSize")
			a.features.get(1).mustBeAttribute(true, "b", "int[]", true).initialValue.mustBe(XFeatureCall).mustCall("org.eclipse.xtext.xbase.lib.ArrayLiterals.newIntArrayOfSize")
		}
		
		/* Retrieving and setting values of arrays is done through the extension 
		 * methods <code>get(int)</code> and <code>set(int, T)</code>.
		 * As for Java, the index of the elements in the array starts with <code>0</code>. 
		 * 
		 * The method <code>length</code> is available for retreiving the size of the array.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */		
		fact "Array Getter and Setter"{
			var model = '''
				var a = #['Hello', 'world', '!']
				// variable b contains the second element of the array a: 'world'.
				var b = a.get(1)
				// variable c contains the size of the array a: 3.
				var c = a.length
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(3)
			a.features.get(0).mustBeAttribute(true, "a", null, true).initialValue.mustBe(XListLiteral)
			a.features.get(1).mustBeAttribute(true, "b", null, true).initialValue.mustBe(XMemberFeatureCall).mustCall("java.util.List.get")
			a.features.get(2).mustBeAttribute(true, "c", null, true).initialValue.mustBe(XMemberFeatureCall).mustCall("org.eclipse.xtext.xbase.lib.ArrayExtensions.length")
		}

		/* Arrays are automatically converted to lists 
		 * when needed. It is similar to the boxing and unboxing feature
		 * provided by Java, between primitives and their respective object
		 * types.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */		
		fact "Array to List"{
			var model = '''
				val myArray : int[] = #[1,2,3]
				val myList : List<Integer> = myArray
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr			
				import java.util.List
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustHaveImports(1)
			model.mustHaveImport(0, "java.util.List", false, false, false)
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(2)
			a.features.get(0).mustBeAttribute(false, "myArray", "int[]", true).initialValue.mustBe(XListLiteral)
			a.features.get(1).mustBeAttribute(false, "myList", "java.util.List<java.lang.Integer>", true).initialValue.mustBe(XFeatureCall).mustCall("io.sarl.docs.reference.gsr.A.myArray")
		}

	}
	
	/* The conformance rules for type casts are defined in the
	 * [Java Language Specification](http://docs.oracle.com/javase/specs/jls/se5.0/html/conversions.html#5.5).
	 * 
	 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
		'''.parsesSuccessfully(
			"package io.sarl.docs.reference.gsr		
			agent A {",
			// TEXT
			"}"
		)
		model.mustHavePackage("io.sarl.docs.reference.gsr")
		model.mustNotHaveImport
		model.mustHaveTopElements(1)
		var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(3)
		a.features.get(0).mustBeAttribute(true, "something", "java.lang.Number", true).initialValue.mustBe(XConstructorCall)
		a.features.get(1).mustBeAttribute(true, "a", null, true).initialValue.mustBe(XCastedExpression)
		a.features.get(2).mustBeAttribute(true, "b", null, true).initialValue.mustBe(XCastedExpression)
	}

	/* SARL supports a collection of operators. Most of them are infix operators,
	 * and several are postfix operators.
	 */
	describe "Operators" {
		
		/* Below, it is the complete list of all available operators
		 * (from less prior to most prior), and 
		 * their corresponding method signatures:
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Infix Operators"{
			'''
					// Add the value 3 to the list l.
					l += 3
					l.operator_add(3)
					// Remove the value 18 from the list l.
					l -= 18
					l.operator_remove(18)
					// true if b1 or b2 is true
					r = b1 || b2
					r = b1.operator_or(b2)
					// true if b1 and b2 are true
					r = b1 && b2
					r = b1.operator_and(b2)
					// true if e1 is equal to e2, based on Object::equals(Object), null-value safe.
					r = e1 == e2
					r = e1.operator_equals(e2)
					// true if e1 is not equal to e2, based on Object::equals(Object), null-value safe.
					r = e1 != e2
					r = e1.operator_notEquals(e2)
					// true if e1 is same object as e2, equivalent as the operator == of Java.
					r = e1 === e2
					r = e1.operator_tripleEquals(e2)
					// true if e1 is not same object as e2, equivalent as the operator != of Java.
					r = e1 !== e2
					r = e1.operator_tripleNotEquals(e2)
					// true if e1 is lower than e2.
					r = e1 < e2
					r = e1.operator_lessThan(e2)
					// true if e1 is greater than e2.
					r = e1 > e2
					r = e1.operator_greaterThan(e2)
					// true if e1 is lower than or equal to e2.
					r = e1 <= e2
					r = e1.operator_lessEqualsThan(e2)
					// true if e1 is greater than or equal to e2.
					r = e1 >= e2
					r = e1.operator_greaterEqualsThan(e2)
					// Create a Pair with e1 and e2.
					r = e1 -> e2
					r = e1.operator_mappedTo(e2)
					// Define a range of values from e1, inclusive, to e2, inclusive.
					r = e1 .. e2
					r = e1.operator_upTo(e2)
					// Define a range of values from e1, exclusive, to e2, inclusive.
					r = e1 >.. e2
					r = e1.operator_greaterThanDoubleDot(e2)
					// Define a range of values from e1, inclusive, to e2, exclusive.
					r = e1 ..< e2
					r = e1.operator_doubleDotLessThan(e2)
					// Bind the object e1 to the procedure p. p must be a 
					// procedure that takes e1 as parameter. The value of this operator is e1.
					r = e1 => p
					r = e1.operator_doubleArrow(p)
					// Shift left e1 by e2.
					r = e1 << e2
					r = e1.operator_doubleLessThan(e2)
					// Shift right e1 by e2.
					r = e1 >> e2
					r = e1.operator_doubleGreaterThan(e2)
					// 
					//r = e1 <<< e2
					//r = e1.operator_tripleLessThan(e2)
					// Unsigned shift right e1 by e2.
					r = e1 >>> e2
					r = e1.operator_tripleGreaterThan(e2)
					//
					//r = e1 <> e2
					//r = e1.operator_diamond(e2)
					// e1 if e1 is not null, or e2 if e1 is null.
					r = o1 ?: o2
					r = e1.operator_elvis(e2)
					// negative value if e1 is strictly lower than e2,
					// zero if e1 is equal to e2,
					// positive value if e2 is strictly greater than e2.
					r = e1 <=> e2
					r = e1.operator_spaceship(e2)
					// addition of e1 and e2
					r = e1 + e2
					r = e1.operator_plus(e2)
					// substract e2 to e1
					r = e1 - e2
					r = e1.operator_minus(e2)
					// multiply e1 and e2
					r = e1 * e2
					r = e1.operator_multiply(e2)
					// divide e1 by e2
					r = e1 / e2
					r = e1.operator_divide(e2)
					// modulo of the division of e1 by e2
					r = e1 % e2
					r = e1.operator_modulo(e2)
					// e1 power e2
					r = e1 ** e2
					r = e1.operator_power(e2)
					// boolean negation
					r = !b1
					r = b1.operator_not()
					// unary minus sign
					r = -e1
					r = e1.operator_minus()
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var r : Object
					val l = newArrayList()
					var b1 : boolean
					var b2 : boolean
					var e1 = 4
					var e2 = 18
					var o1 = new Object
					var o2 = new Object
					var p
					
					def examples {",
				// TEXT
				"} }"
			)
		}

		/* The two postfix operators <code>++</code> and <code>--</code> are supported.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Postfix Operators"{
			'''
					// Increment e1 by 1.
					e1++
					e1.operator_plusPlus()
					// Decrement e1 by 1.
					e1--
					e1.operator_minusMinus()
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var e1 : int
					
					def examples {",
				// TEXT
				"} }"
			)
		}

		/* Local variables and fields can be assigned using the <code>=</code> operator.
		 * 
		 * Compound assignment operators (@{code +=}, @{code -=}, @{code *=}, @{code /=},
		 * @{code %=}) can be used as a shorthand for the assignment of a binary expression.
		 * They work automatically when the corresponding infix operator is declared.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Assignments"{
			'''
					a = 345
					a += 45	// equivalent to a = a + 45
					a -= 24	// equivalent to a = a - 24
					a *= 7	// equivalent to a = a * 7
					a /= 5	// equivalent to a = a / 5
					a %= 9	// equivalent to a = a % 9
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var a = 34
					
					def aFunction {",
				// TEXT
				"} }"
			)
		}

		/* In SARL, it is easy to overload an existing operator or
		 * to define the algorithm of one.
		 * You should define the operator mapping function.
		 * Below the addition between two <code>Pair</code> is defined.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Operator Overloading"{
			var model = '''
				def operator_plus(a : Pair<Integer,Integer>, b : Pair<Integer,Integer>) : Pair<Integer,Integer> {
					return new Pair(a.key, b.value)
				}
				def example {
					var x = new Pair(1,3)
					var y = new Pair(4,5)
					var z1 = operator_plus(x, y) // Call the overloaded operator
					var z2 = x + y // Call the overloaded operator
					// z.key == 1
					// z.value == 5
					println(z1.toString)
					println(z2.toString)
				}
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(2)
			var act1 = a.features.get(0).mustBeAction("operator_plus", "org.eclipse.xtext.xbase.lib.Pair<java.lang.Integer, java.lang.Integer>", 2, false)
			act1.mustHaveParameter(0, "a", "org.eclipse.xtext.xbase.lib.Pair<java.lang.Integer, java.lang.Integer>", false)
			act1.mustHaveParameter(1, "b", "org.eclipse.xtext.xbase.lib.Pair<java.lang.Integer, java.lang.Integer>", false)
			a.features.get(1).mustBeAction("example", null, 0, false)
		}

	}

	/* The block expression allows to have imperative code sequences. 
	 * It consists of a sequence of expressions. The value of the last 
	 * expression in the block is the value of the complete block. 
	 * The type of a block is also the type of the last expression. 
	 * Empty blocks return <code>null</code> and have the type <code>Object</code>.
	 * 
	 * A block expression is surrounded by curly braces. The expressions in a block can be terminated by an optional semicolon.
	 * 
	 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
		'''.parsesSuccessfully(
			"package io.sarl.docs.reference.gsr
			agent A {
				var greeting = \"abc\"",
			// TEXT
			"}"
		)
	}

	/* Variables and Fields can be declared in SARL.
	 * They declaring a variable or a field, you must specify if it is a value or a
	 * variable (see below for details), its name, and optionally its type and its
	 * initial value.
	 * 
	 * Shadowing variables from outer scopes is not allowed, the only exception is the 
	 * implicit variable <code>it</code>.
	 * 
	 */
	describe "Field and Variable Declarations" {
		
		/* A variable declaration starting with the keyword <code>val</code> denotes 
		 * a value, which is essentially a final, unsettable variable.
		 * 
		 * The variable needs to be declared with the keyword <code>var</code>, which 
		 * stands for 'variable' if it should be allowed to reassign its value.
		 * 
		 * Variables declared outside of a lambda expression using the <code>var</code> keyword
		 * are not accessible from within the lambda expressions. Those declared with the
		 * <code>val</code> keyword are accessible.
		 * 
		 * Fields declared outside of a lambda expression using the <code>var</code> keyword
		 * or the <code>val</code> keyword are accessible from within the lambda expressions.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Variable vs. Value Declaration"{
			var model = '''
					val max = 100
					var i = 0
					while (i < max) {
						println("Hi there!")
						i = i + 1
					}
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					def example {",
				// TEXT
				"} }"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(1)
			a.features.get(0).mustBeAction("example", null, 0, false)
		}

		/* The type of the variable itself can either be explicitly declared or it can be 
		 * inferred from the initializer expression.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Typing"{
			var model = '''
				// Explicit type
				var a : String = "abc"
				// Inferred type
				var b = "abc"
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}")
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(2)
			a.features.get(0).mustBeAttribute(true, "a", "java.lang.String", true)
			a.features.get(1).mustBeAttribute(true, "b", null, true)
		}

	}

	/* A function, or method, or action, is a named block of code that could be invoked.
	 *
	 * A function declaration starts with the keyword <code>def</code>.
	 * This declaration can only occur in top-level features
	 * (<code>agent</code>, <code>skill</code>, etc.)
	 *
	 * The functiondeclaration follows the syntax:<pre><code>
	 * def NAME [([PARAMETER, PARAMETER, PARAMETER...])] [: RETURN TYPE] [BLOCK]
	 * </code></pre>
	 * 
	 * <span class="label label-warning">Important</span> The parameters are implicitly declared with the keyword <code>val</code>.
	 */
	describe "Function Declarations" {
		
		/* 
		 * 
		 * The following code gives examples of function declarations:
		 *
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(6)
			a.features.get(0).mustBeAction("action1", null, 0, false)
			a.features.get(1).mustBeAction("action2", "int", 0, false)
			var act1 = a.features.get(2).mustBeAction("action3", null, 1, false)
			act1.mustHaveParameter(0, "a", "int", false)
			var act2 = a.features.get(3).mustBeAction("action4", null, 2, false)
			act2.mustHaveParameter(0, "a", "int", false)
			act2.mustHaveParameter(1, "b", "java.lang.String", false)
			var act3 = a.features.get(4).mustBeAction("action5", "double", 1, false)
			act3.mustHaveParameter(0, "a", "int", false)
			var act4 = a.features.get(5).mustBeAction("action6", "java.lang.String", 2, false)
			act4.mustHaveParameter(0, "a", "int", false)
			act4.mustHaveParameter(1, "b", "java.lang.String", false)
		}

		/* A variadic function is a function of indefinite arity: 
		 * one which accepts a variable number of arguments.
		 * 
		 * SARL enables to define the last parameter of a function
		 * as variadic with the operator <code>*</code>.
		 * This operator has an informal meaning similar to the
		 * cardinality in UML: zero to many.
		 * 
		 * In other languages, such as Java and C++, the variadic
		 * operator is <code>...</code>
		 *
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(3)
			var act1 = a.features.get(0).mustBeAction("action1", null, 1, true)
			act1.mustHaveParameter(0, "v", "int", false)
			var act2 = a.features.get(1).mustBeAction("action2", null, 3, true)
			act2.mustHaveParameter(0, "a", "boolean", false)
			act2.mustHaveParameter(1, "b", "double", false)
			act2.mustHaveParameter(2, "c", "int", false)
			a.features.get(2).mustBeAction("calls", null, 0, false)
		}

		/* SARL allows to specify a default value for a formal parameter.
		 * 
		 * When a default value is specified, it means that the caller of
		 * the action can skip to pass a value for the corresponding argument.
		 * And, when the function is run, the default value is given to the
		 * skiped argument.
		 * 
		 * In SARL, if a formal parameter has a default value, the following formal 
		 * parameters do not need to have default value also. This is a major
		 * difference with the default values inthe C++ language for instance. 
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(3)
			var act1 = a.features.get(0).mustBeAction("action1", null, 1, false)
			act1.mustHaveParameter(0, "v", "int", true)
			var act2 = a.features.get(1).mustBeAction("action2", null, 3, false)
			act2.mustHaveParameter(0, "a", "boolean", true)
			act2.mustHaveParameter(1, "b", "double", false)
			act2.mustHaveParameter(2, "c", "int", true)
			a.features.get(2).mustBeAction("calls", null, 0, false)
		}

		/* It is possible to mix the variadic parameter and the default values,
		 * except that the variadic parameter cannot have a default value. 
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(2)
			var act1 = a.features.get(0).mustBeAction("action", null, 2, true)
			act1.mustHaveParameter(0, "v", "int", true)
			act1.mustHaveParameter(1, "a", "float", false)
		}

	}

	/* A simple name can refer to a field, variable or parameter. 
	 * In addition it can point to a method with zero arguments, since 
	 * empty parentheses are optional.
	 */
	describe "Field Access and Method Invocation" {
		
		/* If there is no field with the given name and also no method with 
		 * the name and zero parameters accessible, a simple name binds to a 
		 * corresponding Java-Bean getter method if available:
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Property Access"{
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
					this.setProperty2 = new Object
					this.property2 = new Object
				}
			}
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT
				""
			)
		}

		/* Like in Java the current object is bound to the keyword <code>this</code>.
		 * This allows for either qualified field access or method invocations.
		 * 
		 * You can use the variable name <code>it</code> to get the same behavior for
		 * any variable or parameter.
		 * Another speciality of the variable <code>it</code> is that it is allowed to
		 * be shadowed. This is especially useful when used together with lambda
		 * expressions.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT
				""
			)
		}

		/* For accessing a static field or method you can use the recommended 
		 * Java syntax or the more explicit double colon <code>::</code>. 
		 * That means, the following epxressions are pairwise equivalent:
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Static Access"{
			var model = '''
				var a = Integer::TYPE
				var b = Integer.TYPE
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(1)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(2)
			a.features.get(0).mustBeAttribute(true, "a", null, true).initialValue.mustBe(XMemberFeatureCall).mustCall("java.lang.Integer.TYPE")
			a.features.get(1).mustBeAttribute(true, "b", null, true).initialValue.mustBe(XMemberFeatureCall).mustCall("java.lang.Integer.TYPE")
		}

		/* Checking for null references can make code very unreadable. 
		 * In many situations it is ok for an expression to return <code>null</code>
		 * if a receiver was <code>null</code>.
		 * 
		 * SARL supports the safe navigation operator <code>?</code>. to make such code
		 * better readable.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Null-Safe Feature Call"{
			'''
					// First expression
					if (myRef != null) myRef.length()
					// Second expression, equivalent to the first expression
					myRef?.length()
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var myRef = \"abc\"
					def examples {",
				// TEXT
				"} }"
			)
		}

		/* When it is possible to extend an existing type,
		 * the methods can be overriden.
		 * In this case, the <code>super</code> keyword
		 * permits to invoke the inherited implementation of the method from
		 * the overriding method.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Inherited Method"{
			var model = '''
				def anAction {
					// Call the inherited implementation
					super.anAction
				}
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					def anAction {
					}
				}
				agent B extends A {",
				// TEXT
				"}"
			)
			model.mustHavePackage("io.sarl.docs.reference.gsr")
			model.mustNotHaveImport
			model.mustHaveTopElements(2)
			var a = model.elements.get(0).mustBeAgent("A", null).mustHaveFeatures(1)
			a.features.get(0).mustBeAction("anAction", null, 0, false)
			var b = model.elements.get(1).mustBeAgent("B", "io.sarl.docs.reference.gsr.A").mustHaveFeatures(1)
			b.features.get(0).mustBeAction("anAction", null, 0, false)
		}

	}

	/* Constructor calls corresponds to the calls of a constructor function for
	 * an object.
	 */ 
	describe "Constructor Call" {
		
		/* Constructor calls have the same syntax as in Java. 
		 * The only difference is that empty parentheses are optional.
		 * If type arguments are omitted, they will be inferred from the current context similar to Java's 
		 * diamond operator on generic method and constructor call.
		 *  
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Instance Creation" {
			'''
				var a = new Integer(345)
				var b = new ArrayList<Integer>()
				var c = new ArrayList<Integer>
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.util.ArrayList
				agent A {",
				// TEXT
				"}"
			)
		}

		/* In the implementation of a constructor, it is possible to
		 * call one of the inherited constructors.
		 * The syntax is similar to Java: the <code>super</code> keyword
		 * is used to represent the inherited constructor.
		 * <pre><code>
		 * new () {
		 *     super // Call the inherited default constructor
		 * }
		 * new (param : Address) {
		 *     super(param) // Call the inherited constructor with a parameter
		 * }
		 * </code></pre>
		 */
		fact "Inherited Constructor" {
			"nothing to test"
		}

	}
	
	/*
	 * Extension methods allow adding new methods to existing 
	 * types without modifying them. This is really 
	 * helpful as they can greatly improve the readability. They
	 * use a simple syntactic trick: the first parameter of a method
	 * can either be passed in after opening the parentheses or before the 
	 * method call. For example, given a method: <pre><code> 
	 * def removeVowels (String s){
	 *     s.replaceAll("[aeiouAEIOU]", "")
	 * }
	 * </code></pre>
	 * 
	 * We can call this method either like in Java: <pre><code> 
	 * removeVowels("Hello")
	 * </code></pre>
	 * 
	 * or as an extension method of String: <pre></code> 
	 * "Hello".removeVowels
	 * </code></pre>
	 */
	describe "Extension Methods"{
 
		/*
		 * You can import static methods as extensions, for example, when we import: <pre></code>
		 * import static extension java.util.Collections.&#42;
		 * </code></pre>
		 * we can directly call the imported static methods on our list objects:
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Import Static Extension Methods"{
			'''
			import static extension java.util.Collections.*
			agent A {
				def example {
					val colors : String[] = #["red", "blue", "green"]
					colors.sort // sort is implemented by Collections#sort(List<T>)
				}
			}
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT
				""
			)
		}

		/*
		 * All visible non-static methods of the current class and its super 
		 * types are automatically available as extensions.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Local extension methods."{
			'''
				// Define an extension method for List
				def hasOneElement(list : List<?>) : boolean {
					list.size == 1
				}
				// Invoke the extension method
				def example : boolean {
					newArrayList("red").hasOneElement
				}
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.util.List
				agent A {",
				// TEXT,
				"}")
		}
		
	}

	/* A lambda expression is basically a piece of code, which is wrapped 
	 * in an object to pass it around. As a Java developer it is best to 
	 * think of a lambda expression as an anonymous class with a single 
	 * method.
	 */
	describe "Lambda Expressions" {
		
		/* That is the code above can be written in SARL for creating a lamda exception.
		 * A lambda expression is surrounded by square brackets (inspired from Smalltalk).
		 * Lambda expression like a method declares parameters. 
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Basic Definition"{
			'''
					val textField = new JTextField
					// Define a lambda expression that take an ActionEvent as parameter
					// It is the definition of a function of type: (ActionEvent) => void
					textField.addActionListener([ e : ActionEvent |
							textField.text = "Something happened!" + e.toString
						])
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.awt.^event.ActionEvent
				import javax.swing.JTextField
				agent A {
					def example {",
				// TEXT
				"} }"
			)
		}
		
		/* The lambda above has one parameter called e which is of type <code>ActionEvent</code>.
		 *
		 * You do not have to specify the type explicitly because it can be inferred from the context.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Inferred Parameter Type"{
			'''
					val textField = new JTextField
					// Define a lambda expression that take an ActionEvent as parameter
					// It is the definition of a function of type: (ActionEvent) => void
					textField.addActionListener([ e |
							textField.text = "Something happened!" + e.toString
						])
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				import javax.swing.JTextField
				agent A {
					def example {",
				// TEXT
				"} }"
			)
		}

		/* As lambdas with one parameter are a common case, there is a special short hand notation 
		 * for them, which is to leave the declaration including the vertical bar out. 
		 * The name of the single variable will be <code>it</code>.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Implicit Parameters: it"{
			'''
					val textField = new JTextField
					// Define a lambda expression that take an ActionEvent as parameter
					// It is the definition of a function of type: (ActionEvent) => void
					textField.addActionListener([
							textField.text = "Something happened!" + it.toString
						])
			'''.parsesSuccessfully(
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
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Empty List of Parameters"{
			'''
					val runnable : Runnable = [ |
							println("Hello I'm executed!")
							]
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					def example {",
				// TEXT
				"} }"
			)
		}
		
		/* When a method call's last parameter is a lambda it can be passed right after 
		 * the parameter list.
		 * 
		 * For instance if you want to sort some strings by their length, you could write
		 * the following two codes.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Lambda as the Last Parameter of a Method"{
			'''
				var t : List<String>
				def example1 {
					Collections.sort(t) [ a, b |
								a.length - b.length
								]
				}
				def example2 {
					Collections.sort(t, [ a, b |
								a.length - b.length
								]
					)
				}
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				import java.util.List
				import java.util.Collections
				agent A {",
				// TEXT
				"}"
			)
		}

		/* The type of a lambda will be one of the inner types found in <code>Functions</code>
		 * or <code>Procedures</code>. It is a procedure if the return type is <code>void</code>,
		 * otherwise it is a function.
		 * 
		 * The syntax for specifying the type of a lambda is: <code>(parameter types) => return type</code>
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Typing"{
			'''
				// Define a function f, which takes one parameter of 
				// typeString, and returning a value of type String. 
				var f1 : (String) => String
				
				// Same type of function.
				var f2 : Function1<? super String,? extends String>
			'''.parsesSuccessfully(
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
		 * <code>true</code> or <code>false</code>.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Classic Syntax"{
			'''
					if (e1 !== null) e1 else e2
			'''.parsesSuccessfully(
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
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Optional Else Part"{
			'''
					if (e1 !== null) e1 /* else null */
			'''.parsesSuccessfully(
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
		 * like Java's ternary operator (<code>predicate ? thenPart : elsePart</code>),
		 * because it is an expression and returns a value. 
		 * Consequently, you can use if expressions deeply nested within expressions:
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Inlined If Expression"{
			'''
					val name = if (e1 != null) e1 + ' ' + e2 else e2
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var e1
					var e2
					def example {",
				// TEXT
				"} }"
			)
		}

	}

	/* The switch expression is very different from Java's switch statement. The use of switch is 
	 * not limited to certain values but can be used for any object reference. 
	 * <code>Object.equals(Object)</code> is used to compare the value in the case with the one 
	 * you are switching over.
	 */
	describe "Switch Expression" {
		
		/* Consider the following example.
		 * the main expression <code>myString</code> is evaluated first and then compared to each 
		 * case sequentially. If the case expression is of type boolean, the case matches 
		 * if the expression evaluates to <code>true</code>. If it is not of type boolean it is 
		 * compared to the value of the main expression using <code>Object.equals(Object)</code>.
		 * 
		 * If a case is a match, the case expression after the colon is evaluated and is 
		 * the result of the whole switch expression. Note that there is no need for 
		 * a <code>break</code> keyword, as in Java: the case following the matching case is
		 * never evaluated.
		 * 
		 * The main expression (parameter of <code>switch</code> can also be a computed value instead 
		 * of a field or variable.
		 * 
		 * <span class="label label-warning">Important</span> A case must contains an expression. If you want to do nothing
		 * for a given case, put an empty block.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					var myString = \"abc\"",
				// TEXT
				"}"
			)
		}
		
		/* In addition to the case guard you can specify a type 
		 * guard.
		 * The case only matches if the switch value conforms to a given type.
		 * A case with both a type guard and a predicate only matches if both conditions match. 
		 * If the switch value is a field, parameter or variable, it is automatically casted 
		 * to the given type within the predicate and the case body.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Type Guards"{
			'''
					switch myString {
					String case myString.length==5 : "It's string of length 5."
					String : "a string."
					}
			'''.parsesSuccessfully(
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
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Fall Through"{
			'''
					switch myString {
					case myString.length==5,
					case 'some' : println("a string.")
					default: println("Default")
					}
			'''.parsesSuccessfully(
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
		 * element of an array or an instance of <code>Iterable</code>. 
		 * 
		 * The local variable is final, hence cannot be updated.
		 * 
		 * The type of a for loop is <code>void</code>. The type of the local
		 * variable can be inferred from the 
		 * iterable or array that is processed.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}
		
		/* The traditional for loop is very similar to the one known from Java, or even C. 
		 * When executed, it first executes the init-expression, where local variables can be 
		 * declared. Next the predicate is executed and if it evaluates to <code>true</code>, the 
		 * body-expression is executed. On any subsequent iterations the update-expression
		 * is executed instead of the init-expression. This happens until the predicate
		 * returns <code>false</code>. The type of a for loop is <code>void</code>.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Traditional Java For Loop" {
			'''
						for (var i = 0; i<123; i++) {
							println(i)
						}
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}

		/* A while loop is used to execute a certain expression unless the predicate is evaluated 
		 * to <code>false</code>. The type of a while loop is <code>void</code>.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "While Loop" {
			'''
						var i = 0
						while (i<123) {
							println(i)
							i++
						}
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}
	
		/* A while loop is used to execute a certain expression unless the predicate is evaluated 
		 * to <code>false</code>. The difference to the while loop is that the execution starts by 
		 * executing the block once before evaluating the predicate for the first time. 
		 * The type of a while loop is <code>void</code>.
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Do-While Loop" {
			'''
						var i = 0
						do {
							println(i)
							i++
						}
						while (i<123)
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}

	}

	/* SARL supports exception throwning and catching.
	 * The mechanism is similar to the one of Java.
	 */
	describe "Exception Support" {
		
		/* Throwing objects of type <code>Throwable</code> up the call stack has the same semantics
		 * and syntax as in Java. 
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */
		fact "Throwing Exceptions"{
			'''
						throw new IllegalArgumentException("explanation")
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}
		
		/* The try-catch-finally expression is used to handle exceptional situations. 
		 * Checked exceptions are treated like runtime exceptions and only optionally 
		 * validated. You can but do not have to catch them as they will be silently thrown. 
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
						def example {",
				// TEXT
				"} }"
			)
		}

		/* For try-catch it is again beneficial that it is an expression, because you 
		 * can write code like the following and do not have to rely on non-final variables: 
		 * 
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
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
			'''.parsesSuccessfully(
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
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */		
		fact "Classic Syntax" {
			'''
					var lock = new Object
					synchronized(lock) {
						println("Hello")
					}
			'''.parsesSuccessfully(
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
		 * @filter(.* = '''|'''|.parsesSuccessfully.*) 
		 */		
		fact "Expression Syntax" {
			'''
					var lock = new Object
					val name = synchronized(lock) { 
							"Hello" 
						}
					println(name)
			'''.parsesSuccessfully(
				"package io.sarl.docs.reference.gsr
				agent A {
					def example {",
				// TEXT
				"} }"
			)
		}

	}

}