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
import org.eclipse.xtend.core.xtend.XtendClass
import org.eclipse.xtend.core.xtend.XtendEnumLiteral
import org.eclipse.xtend.core.xtend.XtendField
import org.eclipse.xtend.core.xtend.XtendInterface
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*
import static extension org.junit.Assume.assumeFalse
import org.eclipse.xtext.common.types.JvmVisibility

/**
 * @outline
 *
 * <p>SARL enables to declare objects with an object-oriented programming approach.
 * This document describes the basic support of object-oriented programming provided by SARL.
 * Before reading this document, we recommend that you read
 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html).
 * 
 * <p>The support of the object-oriented programming (OOP) statements in SARL is less complete
 * than a real object-oriented language such as Java.
 * The basics of OOP are enabled in the SARL scripts. If you need more complex or more
 * complete support of the OOP, you should use a dedicated language, such as Java,
 * [Xtend](https://eclipse.org/xtend/), or [Scala](http://www.scala-lang.org/).
 * 
 * <note>The SARL Eclipse product includes the tools for programming with the Java and Xtend languages</note>
 * 
 */
@CreateWith(SARLSpecCreator)
describe "Basic Object-Oriented Programming Support" {

	@Inject extension SARLParser


	/* Java and [Xtend](https://www.eclipse.org/xtend) are both object-oriented programming languages.
	 * As SARL, Xtend is based on the [Xtext](https://www.eclipse.org/Xtext) libraries for compiling to the Java language.
	 *
	 * <p>The main features coming from the Java language are supported by SARL too.
	 * The following table provides the major differences between the SARL, Java and Xtend languages regarding the supports of the object-oriented features.
	 *
	 * <table><thead>
	 * <tr><th></th><th>SARL</th><th>Java</th><th><th>Xtend</th></tr>
	 * </thead><tbody>
	 * <tr><td>Object-oriented enumeration</td>
	 * 			<td>No, only constants could be defined</td>
	 * 			<td>Yes, constants and functions could be defined</td>
	 * 			<td>No, only constants could be defined</td></tr>
	 * <tr><td>Definition of dispatching functions</td>
	 * 			<td>Yes</td>
	 * 			<td>No</td>
	 * 			<td>Yes</td></tr>
	 * <tr><td>Extension methods</td>
	 * 			<td>Yes</td>
	 * 			<td>No</td>
	 * 			<td>Yes</td></tr>
	 * <tr><td>Automatic generation of equals() and hashCode()</td>
	 * 			<td>Yes</td>
	 * 			<td>No</td>
	 * 			<td>No</td></tr>
	 * <tr><td>Automatic generation of clone() when cloneable type</td>
	 * 			<td>Yes</td>
	 * 			<td>No</td>
	 * 			<td>No</td></tr>
	 * <tr><td>Automatic generation of serialVersionUID when serializable type</td>
	 * 			<td>Yes</td>
	 * 			<td>No</td>
	 * 			<td>No</td></tr>
	 * <tr><td>Definition of default values for the formal parameters</td>
	 * 			<td>Yes</td>
	 * 			<td>No</td>
	 * 			<td>No</td></tr>
	 * <tr><td>Definition of lambda expressions</td>
	 * 			<td>Yes</td>
	 * 			<td>Yes</td>
	 * 			<td>Yes</td></tr>
	 * <tr><td>Inference of types</td>
	 * 			<td>Yes</td>
	 * 			<td>No</td>
	 * 			<td>Yes</td></tr>
	 * <tr><td>Operator overriding</td>
	 * 			<td>Yes</td>
	 * 			<td>No</td>
	 * 			<td>Yes</td></tr>
	 * </tbody></table>
	 *
	 * @filter(.*) 
	 */
	fact "Comparison between SARL, Java and Xtend" {
		"https://www.eclipse.org/xtend" should beURL _
		"https://www.eclipse.org/Xtext" should beURL _
	}

	/** Objects are structures that contain both data and procedures.
	 * Classes are definitions for the data format and available procedures
	 * for a given type or class of objects. They may also contain data and
	 * procedures (known as class methods) themselves.
	 */
	describe "Class" {
		
		/* For defining a class, you could use the ```class``` keyword.
		 * The following example defines the class ```MyClass```. 
		 *  
		 * <p>The members of the class, i.e. the fields, methods and constructors must be
		 * written between the braces that are following the class declaration.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Define a Class" {
			// Test the elements of the global documentation.
			"./GeneralSyntaxReferenceSpec.html" should beAccessibleFrom this
			"https://eclipse.org/xtend/" should beURL _
			"http://www.scala-lang.org/" should beURL _
			//
			val model = '''
			class MyClass {  }
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.oop"
				it should haveNbImports 0
				it should haveNbElements 1
			]
			
			model.xtendTypes.get(0) => [
				it should beClass "MyClass"
				it should extend _
				it should implement _
				it should haveNbElements 0
			]
		}

		/* Languages that support classes almost always support inheritance.
		 * This allows classes to be arranged in a hierarchy that represents
		 * "is-a-type-of" relationships.
		 *
		 * <p>For example, class ```Employee``` might inherit from class ```Person```.
		 * All the data and methods available to the parent class also appear in
		 * the child class with the same names.
		 * For example, class ```Person``` might define variables "firstName" and
		 * "lastName" with method "getFullName()". These will also be available
		 * in class Employee, which might add the variables
		 * "position" and "salary".
		 * 
		 * <p>The definition of the inheritance relationship between two classes is
		 * done with the ```extends``` keyword.
		 *  
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Class Inheritance" {
			val model = '''
			class Person {
				
				var firstName : String
				var lastName : String
				
				def getFullName : String {
					this.firstName + " " + this.lastName
				}
			}
			
			
			class Employee extends Person {
				
				var position : String
				
				var salary : float
			
			}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.oop"
				it should haveNbImports 0
				it should haveNbElements 2
			]
			
			model.xtendTypes.get(0) => [
				it should beClass "Person"
				it should extend _
				it should implement _
				it should haveNbElements 3
				
				it.members.get(0) => [
					it should beVariable "firstName"
					it should haveType "java.lang.String"
				]

				it.members.get(1) => [
					it should beVariable "lastName"
					it should haveType "java.lang.String"
				]

				it.members.get(2) => [
					it should beAction "getFullName"
					it should haveNbParameters 0
					it should reply "java.lang.String"
				]
			]

			model.xtendTypes.get(1) => [
				it should beClass "Employee"
				it should extend "io.sarl.docs.reference.oop.Person"
				it should implement _
				it should haveNbElements 2
				
				it.members.get(0) => [
					it should beVariable "position"
					it should haveType "java.lang.String"
				]

				it.members.get(1) => [
					it should beVariable "salary"
					it should haveType "float"
				]
			]
		}

		/*
		 * A generic class declaration looks like a non-generic class declaration, except that the class name
		 * is followed by a type parameter section.
		 * 
		 * <p>The type parameter section of a generic class can have one or more type parameters separated
		 * by commas. These classes are known as parameterized classes or parameterized types
		 * because they accept one or more parameters.
		 *  
		 * <p>There may be times when you'll want to restrict the kinds of types that are allowed to be passed
		 * to a type parameter. For example, a method that operates on numbers might only want to
		 * accept instances of Number or its subclasses. This is what bounded type parameters are for.
		 * To declare a bounded type parameter, list the type parameter's name, followed by: <ul>
		 * <li>the <code>extends</code> keyword, followed by its upper bound; or</li>
		 * <li>the <code>super</code> keyword, followed by its lower bound.</li>
		 * </ul>
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Define a Generic Class" {
			val model = '''
				class AType<T> {
				
					var t : T
				
					def add(t : T) {
						this.t = t
					}
				
					def get : T {
						return this.t
					}
				}
				
				class Vector<T extends Number> {
				
					var x : T
					var y : T
				
					def norm : Vector<? extends Number> {
						var v = new Vector
						var n = Math::sqrt(this.x.floatValue**2 + this.y.floatValue**2)
						v.x = this.x.floatValue/n
						v.y = this.y.floatValue/n
						return v
					}
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.oop"
				it should haveNbImports 0
				it should haveNbElements 2
			]
			
			model.xtendTypes.get(0) => [
				it should beClass "AType"
				it should extend _
				it should implement _
				it should haveNbElements 3
				it should haveNbTypeParameters 1
				(it as XtendClass).typeParameters.get(0) => [
					it should beTypeParameter "T"
					it should beContrainedWith _
				]
				
				it.members.get(0) => [
					it should beVariable "t"
					it should haveType "T"
				]

				it.members.get(1) => [
					it should beAction "add"
					it should haveNbParameters 1
					(it as SarlAction).parameters.get(0) =>[
						it should beParameter "t"
						it should haveType "T" 
					]
					it should reply _
				]

				it.members.get(2) => [
					it should beAction "get"
					it should haveNbParameters 0
					it should reply "T"
				]
			]

			model.xtendTypes.get(1) => [
				it should beClass "Vector"
				it should extend _
				it should implement _
				it should haveNbElements 3
				it should haveNbTypeParameters 1
				(it as XtendClass).typeParameters.get(0) => [
					it should beTypeParameter "T"
					it should beContrainedWith "extends java.lang.Number"
				]
				
				it.members.get(0) => [
					it should beVariable "x"
					it should haveType "T"
				]

				it.members.get(1) => [
					it should beVariable "y"
					it should haveType "T"
				]

				it.members.get(2) => [
					it should beAction "norm"
					it should haveNbParameters 0
					it should reply "io.sarl.docs.reference.oop.Vector<? extends java.lang.Number>"
				]
			]
		}

		/** An SARL class can define any number of constructors.
		 * Unlike Java, you do not have to repeat the name of the class over and over again,
		 * but use the keyword ```new``` to declare a constructor.
		 * 
		 * <p>Constructors can also delegate to other constructors using ```this(args...)```
		 * in their first line.
		 * 
		 * <p>If the super class does not define a no-argument constructor, you have to explicitly call
		 * one using ```super(args...)``` as the first expression in the body of the constructor.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Constructor Definition" {
			'''
				class MyClass extends AnotherClass {
				  new(s : String) {
				    super(s)
				  }
				  
				  new() {
				    this("default")
				  }
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop
				class AnotherClass {
				  new (s : String) { }
				}",
				// TEXT
				""
			)
		}

		/** Modifiers are used to modify declarations of types and type members.
		 * This section introduces the modifiers for the class.
		 * The modifiers are usually written before the keyword for defining the class.
		 * 
		 * <p>The complete description of the modifiers' semantic is available in
		 * <a href="./BasicObjectOrientedProgrammingSupportModifiersSpec.html">this section</a>.
		 */
		describe "Modifiers" {
			
			/** A top class may be declared with one or more modifiers, which affect its runtime behavior: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  the class is accessible from any other type (default);</li>
			 *     <li>`package`: the class is accessible from only the types in the same package.</li>
			 *     </ul></li>
			 * <li>`abstract`: the class is abstract and cannot be instanced.</li>
			 * <li>`final`: avoid to be derived.</li>
			 * <li>`strictfp`: avoid the methods of the class to use intermediate floating number formats.</li>
			 * </ul>
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Top Class Modifiers" {
				'''
					public class TopClass1 {
					}
					package class TopClass2 {
					}
					abstract class TopClass3 {
					}
					final class TopClass4 {
					}
					strictfp class TopClass5 {
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop",
					// TEXT
					""
				)
				// Test URL in the enclosing section text.
				"./BasicObjectOrientedProgrammingSupportModifiersSpec.html" should beAccessibleFrom this
					// Test default visibility
					var visib = "class C1 {}".parse.xtendTypes.get(0)
					visib should beVisibleWith JvmVisibility::PUBLIC
			}

			/** A nested class may be declared with one or more modifiers, which affect its runtime behavior: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  there are no restrictions on accessing the class (default);</li>
			 *     <li>`protected`:  the class is accessible within the same package, and derived classes;</li>
			 *     <li>`package`: the class is accessible only within the same package as its class;</li>
			 *     <li>`private`: the class is accessible only within its class.</li>
			 *     </ul></li>
			 * <li>`abstract`: the class is abstract and cannot be instanced.</li>
			 * <li>`final`: avoid to be derived.</li>
			 * <li>`static`: the inner class do not have access to the non-static members of the enclosing type.</li>
			 * <li>`strictfp`: avoid the methods of the class to use intermediate floating number formats.</li>
			 * </ul>
			 *
			 * <note>Terminology: Nested classes are divided into two categories: static and non-static.
			 * Nested classes that are declared static are called <i>static nested classes</i>.
			 * Non-static nested classes are called <i>inner classes</i>.</note>
			 * 
			 * <note>The modifiers may differ from the previously described, depending on the enclosing type, e.g. agent.</note>
			 *
			 * <caution>Until now, all the nested classes must be declared as static. This restriction may be removed in later versions.</caution>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Nested Class Modifiers" {
				'''
					class EnclosingClass {
						public static class NestedClass1 {
						}
						protected static class NestedClass2 {
						}
						package static class NestedClass3 {
						}
						private static class NestedClass4 {
						}
						abstract static class NestedClass5 {
						}
						final static class NestedClass6 {
						}
						strictfp static class NestedClass7 {
						}
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop",
					// TEXT
					""
				)
				// Test default visibility
				var visib = "class C1 { class C2 {}}".parse.xtendTypes.get(0).members.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

			/** The modifiers for the fields in a class are: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  there are no restrictions on accessing the field;</li>
			 *     <li>`protected`:  the field is accessible within the same package, and derived classes;</li>
			 *     <li>`package`: the field is accessible only within the same package as its class;</li>
			 *     <li>`private`: the field is accessible only within its class (default).</li>
			 *     </ul></li>
			 * <li>`static`: the field is a class field, not an instance field.</li>
			 * <li>`transient`: the field is never serialized.</li>
			 * <li>`volatile`: the field is modified by different threads. It is never cached thread-locally, and synchronized.</li>
			 * </ul>
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Field Modifiers" {
				'''
					public var a : Object;
					protected var b : Object;
					package var c : Object;
					private var d : Object;
					static var e : Object;
					transient var h : Object;
					volatile var g : Object;
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop
					public class MyClass1 {",
					// TEXT
					"}"
				)
				// Test default visibility
				var visib = "class C1 {var field : int}".parse.xtendTypes.get(0).members.get(0)
				visib should beVisibleWith JvmVisibility::PRIVATE
			}

			/** The modifiers for the methods in a class are: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  there are no restrictions on accessing the method (default);</li>
			 *     <li>`protected`:  the method is accessible within the same package, and derived classes;</li>
			 *     <li>`package`: the method is accessible only within the same package as its class;</li>
			 *     <li>`private`: the method is accessible only within its class.</li>
			 *     </ul></li>
			 * <li>`abstract`: the method has no implementation in the class.</li>
			 * <li>`dispatch`: the method provides an implementation for the dispatch method mechanism.</li>
			 * <li>`final`: the method cannot be overridden in derived classes.</li>
			 * <li>`native`: the implementation of the method is inside a native library (DLL, SO, DYNLIB).</li>
			 * <li>`static`: the method is a class method, not an instance method.</li>
			 * <li>`strictfp`: avoid the method to use intermediate floating number formats.</li>
			 * <li>`synchronized`: the method is synchronized on the class instance.</li>
			 * </ul>
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Method Modifiers" {
				'''
					public def fct1 { }
					protected def fct2 { }
					package def fct3 { }
					private def fct4 { }
					abstract def fct5
					final def fct6 { }
					native def fct7
					static def fct8 { }
					strictfp def fct9 { }
					synchronized def fct10 { }
					// Dispatch functions
					dispatch def fct11(p : Integer) { }
					dispatch def fct11(p : Float) { }
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop
					abstract class MyClass1 {",
					// TEXT
					"}"
				)
				// Test default visibility
				var visib = "class C1 {def fct {}}".parse.xtendTypes.get(0).members.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

			/** The modifiers for the constructors of a class are: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  there are no restrictions on accessing the constructor (default);</li>
			 *     <li>`protected`:  the constructor is accessible within the same package, and derived classes;</li>
			 *     <li>`package`: the constructor is accessible only within the same package as its class;</li>
			 *     <li>`private`: the constructor is accessible only within its class.</li>
			 *     </ul></li>
			 * </ul>
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Constructor Modifiers" {
				'''
					public new(p : int) { }
					protected new(p : float) { }
					package new(p : char) { }
					private new(p : boolean) { }
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop
					public class MyClass1 {",
					// TEXT
					"}"
				)
				// Test default visibility
				var visib = "class C1 { new {} }".parse.xtendTypes.get(0).members.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

		}

		/* Method overriding is a language feature that allows a subclass or child class
		 * to provide a specific implementation of a method that is already provided by
		 * one of its super classes or parent classes.
		 *
		 * <p>The implementation in the subclass overrides (replaces) the implementation in
		 * the superclass by providing a method that has same name, same parameters or
		 * signature, and same return type as the method in the parent class.
		 * 
		 * <p>The version of a method that is executed will be determined by the object that is
		 * used to invoke it. If an object of a parent class is used to invoke the method,
		 * then the version in the parent class will be executed, but if an object of the
		 * subclass is used to invoke the method, then the version in the child class
		 * will be executed.
		 *
		 * <p>The following code defines the class ```PersonEx``` as a subclass of ```Person```,
		 * and in which the title (mister, madam, miss) is added.
		 * Then the full name of the person becomes the sequence of the title, first name
		 * and last name.
		 * Since the first name and last name are already sequenced in the function
		 * ```getFullName``` of the superclass, we should override this function for changing
		 * its behavior. The ```override``` keyword is specified for clearly marking this
		 * implementation of ```getFullName``` as an override of the parent's implementation.
		 * 
		 * <note>The return type of the ```getFullName``` method (called with the name ```fullName``,
		 * according to the
		 * [property access syntax](./GeneralSyntaxReferenceObjectMemberInvocationSpec.html#Property_Syntax))
		 * is not specified in the overriding
		 * prototype since it could be inferred by the SARL compiler.</note>  
		 *  
		 * <p>For preventing a function to be overridden, you should add the modifier ```final```
		 * in the signature of the method (as in Java).
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Method Overriding" {
			val model = '''
			class PersonEx extends Person {
				
				var title : String
				
				override getFullName {
					return title + " " + super.fullName
				} 
			}
			'''.parseSuccessfully('''
				package io.sarl.docs.reference.oop
				class Person {
					var firstName : String
					var lastName : String
				
					def getFullName : String {
						this.firstName + " " + this.lastName
					}
				}
				''',
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.oop"
				it should haveNbImports 0
				it should haveNbElements 2
			]
			
			model.xtendTypes.get(0) => [
				it should beClass "Person"
				it should extend _
				it should implement _
				it should haveNbElements 3
				
				it.members.get(0) => [
					it should beVariable "firstName"
					it should haveType "java.lang.String"
				]

				it.members.get(1) => [
					it should beVariable "lastName"
					it should haveType "java.lang.String"
				]

				it.members.get(2) => [
					it should beAction "getFullName"
					it should haveNbParameters 0
					it should reply "java.lang.String"
				]
			]

			model.xtendTypes.get(1) => [
				it should beClass "PersonEx"
				it should extend "io.sarl.docs.reference.oop.Person"
				it should implement _
				it should haveNbElements 2
				
				it.members.get(0) => [
					it should beVariable "title"
					it should haveType "java.lang.String"
				]

				it.members.get(1) => [
					it should beAction "getFullName"
					it should haveNbParameters 0
					it should reply _
					it should haveModifiers "override"
				]
			]

			"./GeneralSyntaxReferenceObjectMemberInvocationSpec.html" should beAccessibleFrom this
		}

		/** Local classes (or nested classes, i.e. classes defined inside an other type) have
		 * the `static` modifier. It means that a nested class cannot have access to the
		 * fields and methods of the instance of the enclosing type.
		 * Only accesses to static members are allowed.
		 *
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Accessing Local Variables of the Enclosing Scope" {
			'''
				class EnclosingClass {
					static var field1 : int
					static def fct1 {
					}

					static class NestedClass {
						def fct2 : int {
							// Explicit type specification for the field access
							EnclosingClass::field1
						}
				
						def fct3 {
							// Implicit type specification for the function's call
							fct1
						}
					}
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop",
				// TEXT
				""
			)
		}

	}

	/** An interface is a description of the actions that an object can do.
	 * For example when you flip a light switch, the light goes on, you don't
	 * care how, just that it does. In object-oriented programming, an
	 * interface is a description of all functions that an object must have
	 * in order to be an "X".
	 * 
	 * <p>The purpose of interfaces is to allow the program to enforce these
	 * properties, and to know that an object of type T (whatever the interface
	 * is) must have functions called X,Y,Z, etc.
	 */
	describe "Interface" {

		/*
		 * In the following example, the ```Light``` interface is defined
		 * with the two methods ```turnOn()``` and ```turnOff()```.
		 *  
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Define an Interface" {
			val model = '''
			interface Light {
				def turnOn
				def turnOff
			}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.oop"
				it should haveNbImports 0
				it should haveNbElements 1
			]
			
			model.xtendTypes.get(0) => [
				it should beInterface "Light"
				it should extend _
				it should haveNbElements 2
				
				it.members.get(0) => [
					it should beActionSignature "turnOn"
					it should haveNbParameters 0
					it should reply _
				]

				it.members.get(1) => [
					it should beActionSignature "turnOff"
					it should haveNbParameters 0
					it should reply _
				]
			]
		}

		/*
		 * It is possible to specialize the definition of an interface.
		 * In the following example, the ```VariableIntensityLight```
		 * interface that is refining the previous ```Light``` interface
		 * and add specific functions.
		 *  
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Interface Inheritance" {
			val model = '''
			interface VariableIntensityLight extends Light {
				def setLightIntensity(intensity : float)
				def getLightIntensity : float
			}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop
				interface Light {
				  def turnOn
				  def turnOff
				}",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.oop"
				it should haveNbImports 0
				it should haveNbElements 2
			]
			
			model.xtendTypes.get(1) => [
				it should beInterface "VariableIntensityLight"
				it should extend "io.sarl.docs.reference.oop.Light"
				it should haveNbElements 2
				
				it.members.get(0) => [
					it should beActionSignature "setLightIntensity"
					it should haveNbParameters 1
					it should reply _;
					(it as SarlAction) => [
						it.parameters.get(0) should beParameter "intensity"
						it.parameters.get(0) should haveType "float"
					]
				]

				it.members.get(1) => [
					it should beActionSignature "getLightIntensity"
					it should haveNbParameters 0
					it should reply "float"
				]
			]
		}

		/*
		 * A generic interface declaration looks like a non-generic interface declaration, except that the interface name
		 * is followed by a type parameter section.
		 * 
		 * <p>The type parameter section of a generic interface can have one or more type parameters separated
		 * by commas. These interfaces are known as parameterized interfaces or parameterized types
		 * because they accept one or more parameters.
		 *  
		 * <p>There may be times when you'll want to restrict the kinds of types that are allowed to be passed
		 * to a type parameter. For example, a method that operates on numbers might only want to
		 * accept instances of Number or its subclasses. This is what bounded type parameters are for.
		 * To declare a bounded type parameter, list the type parameter's name, followed by: <ul>
		 * <li>the <code>extends</code> keyword, followed by its upper bound; or</li>
		 * <li>the <code>super</code> keyword, followed by its lower bound.</li>
		 * </ul>
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Define a Generic Interface" {
			val model = '''
				interface AnInterface<T> {
					def add(t : T)
				
					def get : T
				}
				
				interface Vector<T extends Number> {
					def norm : Vector<? extends Number>
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.oop"
				it should haveNbImports 0
				it should haveNbElements 2
			]
			
			model.xtendTypes.get(0) => [
				it should beInterface "AnInterface"
				it should extend _
				it should haveNbElements 2
				it should haveNbTypeParameters 1
				(it as XtendInterface).typeParameters.get(0) => [
					it should beTypeParameter "T"
					it should beContrainedWith _
				]
				
				it.members.get(0) => [
					it should beActionSignature "add"
					it should haveNbParameters 1
					(it as SarlAction).parameters.get(0) => [
						it should beParameter "t"
						it should haveType "T" 
					]
					it should reply _
				]

				it.members.get(1) => [
					it should beActionSignature "get"
					it should haveNbParameters 0
					it should reply "T"
				]
			]

			model.xtendTypes.get(1) => [
				it should beInterface "Vector"
				it should extend _
				it should haveNbElements 1
				it should haveNbTypeParameters 1
				(it as XtendInterface).typeParameters.get(0) => [
					it should beTypeParameter "T"
					it should beContrainedWith "extends java.lang.Number"
				]
				
				it.members.get(0) => [
					it should beActionSignature "norm"
					it should haveNbParameters 0
					it should reply "io.sarl.docs.reference.oop.Vector<? extends java.lang.Number>"
				]
			]
		}

		/*
		 * A class is able to implement an interface.
		 * The ```implements``` keyword is used for defining
		 * the implementation relationship between a class
		 * and an interface.
		 * The class must provide an implementation of all
		 * the functions defined in the interface.
		 * The only one exception is when the class is abstract.
		 * In this case, the derived classes must implement the
		 * functions of the interface.
		 *  
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Interface Implementation" {
			val model = '''
			class TheLight implements Light {
				var isSwitchedOn = false
				def turnOn {
					this.isSwitchedOn = true
				}
				def turnOff {
					this.isSwitchedOn = false
				}
			}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop
				interface Light {
				  def turnOn
				  def turnOff
				}",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.oop"
				it should haveNbImports 0
				it should haveNbElements 2
			]
			
			model.xtendTypes.get(1) => [
				it should beClass "TheLight"
				it should implement "io.sarl.docs.reference.oop.Light"
				it should haveNbElements 3
				
				it.members.get(0) => [
					it should beVariable "isSwitchedOn"
					it should haveType _;
					(it as XtendField).initialValue should beLiteral "false"
				]

				it.members.get(1) => [
					it should beAction "turnOn"
					it should haveNbParameters 0
					it should reply _
				]

				it.members.get(2) => [
					it should beAction "turnOff"
					it should haveNbParameters 0
					it should reply _
				]
			]
		}

		/** Modifiers are used to modify declarations of types and type members.
		 * This section introduces the modifiers for the interface.
		 * The modifiers are usually written before the keyword for defining the interface.
		 * 
		 * <p>The complete description of the modifiers' semantic is available in
		 * <a href="./BasicObjectOrientedProgrammingSupportModifiersSpec.html">this section</a>.
		 */
		describe "Modifiers" {
			
			/** A top interface may be declared with one or more modifiers, which affect its runtime behavior: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  the class is accessible from any other type (default);</li>
			 *     <li>`package`: the class is accessible from only the types in the same package.</li>
			 *     </ul></li>
			 * <li>`abstract`: the interface is abstract (not needed since all the interfaces are abstract).</li>
			 * <li>`strictfp`: avoid the methods of the implementing classes to use intermediate floating number formats.</li>
			 * </ul>
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Top Interface Modifiers" {
				'''
					public interface TopInterface1 {
					}
					package interface TopInterface2 {
					}
					abstract interface TopInterface3 {
					}
					strictfp interface TopInterface4 {
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop",
					// TEXT
					""
				)
				// Test URL in the enclosing section text.
				"./BasicObjectOrientedProgrammingSupportModifiersSpec.html" should beAccessibleFrom this
				// Test default visibility
				var visib = "interface I1 {}".parse.xtendTypes.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

			/** A nested interface may be declared with one or more modifiers, which affect its runtime behavior: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  there are no restrictions on accessing the interface (public);</li>
			 *     <li>`protected`:  the interface is accessible within the same package, and derived classes;</li>
			 *     <li>`package`: the interface is accessible only within the same package as its class;</li>
			 *     <li>`private`: the interface is accessible only within its class.</li>
			 *     </ul></li>
			 * <li>`abstract`: the interface is abstract (not needed since all the interfaces are abstract).</li>
			 * <li>`static`: the inner interface do not have access to the non-static members of the enclosing type.</li>
			 * <li>`strictfp`: avoid the methods of the interface to use intermediate floating number formats.</li>
			 * </ul>
			 *
			 * <note>Terminology: Nested interfaces are divided into two categories: static and non-static.
			 * Nested interfaces that are declared static are called <i>static nested interfaces</i>.
			 * Non-static nested interfaces are called <i>inner interfaces</i>.</note>
			 * 
			 * <note>The modifiers may differ from the previously described, depending on the enclosing type, e.g. agent.</note>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Nested Interface Modifiers" {
				'''
					class EnclosingClass {
						public interface NestedInterface1 {
						}
						protected interface NestedInterface2 {
						}
						package interface NestedInterface3 {
						}
						private interface NestedInterface4 {
						}
						abstract interface NestedInterface5 {
						}
						static interface NestedInterface6 {
						}
						strictfp interface NestedInterface7 {
						}
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop",
					// TEXT
					""
				)
				var visib = "interface I1 { interface I2 {} }".parse.xtendTypes.get(0).members.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

			/** The modifiers for the fields in an interface are: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  there are no restrictions on accessing the field (default);</li>
			 *     </ul></li>
			 * <li>`static`: the field is a class field, not an instance field (default).</li>
			 * </ul>
			 *
			 * <caution>Only fields defined with `val` can be put in an interface.</caution>
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Field Modifiers" {
				'''
					public val a : Object;
					static val e : Object;
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop
					public interface MyInterface1 {",
					// TEXT
					"}"
				)
				var visib = "interface I1 {var field = 3}".parse.xtendTypes.get(0).members.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

			/** The modifiers for the methods in an interface are: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  there are no restrictions on accessing the method (default);</li>
			 *     </ul></li>
			 * <li>`abstract`: the method is abstract (not needed since all the interface methods are abstract).</li>
			 * </ul>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Method Modifiers" {
				'''
					public def fct1
					abstract def fct5
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop
					interface MyInterface1 {",
					// TEXT
					"}"
				)
				var visib = "interface I1 {def fct}".parse.xtendTypes.get(0).members.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

		}

	}

	/** An enumeration specifies a list of constant values assigned to a type.
	 *
	 * <p>The SARL enumeration is not object-oriented unlike the enumeration
	 * in the Java programming language. It means that you cannot define
	 * methods nor attributes in the enumeration.
	 */
	describe "Enumeration" {

		/* For defining an enumeration, you could use the ```enum``` keyword.
		 * The following example defines the enumeration ```MyEnum``` with two constants: 
		 *  
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Define an Enumeration" {
			val model = '''
			enum MyEnum {
				CONSTANT_1,
				CONSTANT_2
			}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.oop"
				it should haveNbImports 0
				it should haveNbElements 1
			]
			
			model.xtendTypes.get(0) => [
				it should beEnumeration "MyEnum"
				it should haveNbElements 2
				
				it.members.get(0) => [
					it should be typeof(XtendEnumLiteral)
					(it as XtendEnumLiteral).name should be "CONSTANT_1"
				]
	
				it.members.get(1) => [
					it should be typeof(XtendEnumLiteral)
					(it as XtendEnumLiteral).name should be "CONSTANT_2"
				]
			]
		}

		/** Modifiers are used to modify declarations of types and type members.
		 * This section introduces the modifiers for the enumeration.
		 * The modifiers are usually written before the keyword for defining the enumeration.
		 * 
		 * <p>The complete description of the modifiers' semantic is available in
		 * <a href="./BasicObjectOrientedProgrammingSupportModifiersSpec.html">this section</a>.
		 */
		describe "Modifiers" {
			
			/** A top enumeration may be declared with one or more modifiers, which affect its runtime behavior: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  the class is accessible from any other type (default);</li>
			 *     <li>`package`: the class is accessible from only the types in the same package.</li>
			 *     </ul></li>
			 * </ul>
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Top Enumeration Modifiers" {
				'''
					public enum TopEnumeration1 {
						CST1, CST2
					}
					package enum TopEnumeration2 {
						CST3, CST4
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop",
					// TEXT
					""
				)
				// Test URL in the enclosing section text.
				"./BasicObjectOrientedProgrammingSupportModifiersSpec.html" should beAccessibleFrom this
				var visib = "enum E1 { CST1 }".parse.xtendTypes.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

			/** A nested interface may be declared with one or more modifiers, which affect its runtime behavior: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  there are no restrictions on accessing the enumeration (default);</li>
			 *     <li>`protected`:  the enumeration is accessible within the same package, and derived classes;</li>
			 *     <li>`package`: the enumeration is accessible only within the same package as its class;</li>
			 *     <li>`private`: the enumeration is accessible only within its class.</li>
			 *     </ul></li>
			 * <li>`static`: the inner enumeration do not have access to the non-static members of the enclosing type.</li>
			 * </ul>
			 *
			 * <note>Terminology: Nested enumerations are divided into two categories: static and non-static.
			 * Nested enumerations that are declared static are called <i>static nested enumerations</i>.
			 * Non-static nested enumerations are called <i>inner enumerations</i>.</note>
			 * 
			 * <note>The modifiers may differ from the previously described, depending on the enclosing type, e.g. agent.</note>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Nested Enumeration Modifiers" {
				'''
					class EnclosingClass {
						public enum NestedClass1 {
							CST1, CST2
						}
						protected enum NestedClass2 {
							CST3, CST4
						}
						package enum NestedClass3 {
							CST5, CST6
						}
						private enum NestedClass4 {
							CST7, CST8
						}
						static enum NestedClass5 {
							CST9, CST10
						}
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop",
					// TEXT
					""
				)
				var visib = "class C1 { enum E1 { CST1 } }".parse.xtendTypes.get(0).members.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

		}

	}

	/** An annotation is a form of syntactic metadata that can be added to SARL source code.
	 * Annotations can be reflective in that they can be embedded in binary files generated by the
	 * SARL compiler, and may be retained by the Virtual Machine to be made retrievable at run-time.
	 */
	describe "Annotation Type" {

		/* For defining an annotation, you could use the ```annotation``` keyword.
		 *
		 * <p>The following example defines the annotation ```MyAnnotation```.
		 * This annotation defines three parameters:<ul>
		 * <li><code>value</code>, an array of strings of characters, without default value;</li>
		 * <li><code>isTricky</code>, a boolean value, with the default ```false```;</li>
		 * <li><code>lotteryNumbers</code>, an array of integer numbers, with a default value.</li>
		 * </ul>
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Define an Annotation" {
			val model = '''
				annotation MyAnnotation {
				  val value : String[]
				  val isTricky : boolean = false
				  val lotteryNumbers : int[] = #[ 42, 137 ]
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop",
				// TEXT
				""
			)
			
			model => [
				it should havePackage "io.sarl.docs.reference.oop"
				it should haveNbImports 0
				it should haveNbElements 1
			]
			
			model.xtendTypes.get(0) => [
				it should beAnnotation "MyAnnotation"
				it should haveNbElements 3
				
				it.members.get(0) => [
					it should beValue "value"
					it should haveType "java.lang.String[]"
					(it as XtendField).initialValue should be null
				]

				it.members.get(1) => [
					it should beValue "isTricky"
					it should haveType "boolean"
					(it as XtendField).initialValue should beLiteral "false"
				]

				it.members.get(2) => [
					it should beValue "lotteryNumbers"
					it should haveType "int[]"
					(it as XtendField).initialValue should beLiteral #[ 42, 137 ]
				]
			]
		}

		/** Modifiers are used to modify declarations of types and type members.
		 * This section introduces the modifiers for the annotation types.
		 * The modifiers are usually written before the keyword for defining the annotation type.
		 * 
		 * <p>The complete description of the modifiers' semantic is available in
		 * <a href="./BasicObjectOrientedProgrammingSupportModifiersSpec.html">this section</a>.
		 */
		describe "Modifiers" {
			
			/** A top annotation type may be declared with one or more modifiers, which affect its runtime behavior: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  the annotation type is accessible from any other type (default);</li>
			 *     <li>`package`: the annotation type is accessible from only the types in the same package.</li>
			 *     </ul></li>
			 * <li>`abstract`: the annotation type is abstract and cannot be instanced.</li>
			 * </ul>
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Top Annotation Type Modifiers" {
				'''
					public annotation TopAnnotationType1 {
					}
					package annotation TopAnnotationType2 {
					}
					abstract annotation TopAnnotationType3 {
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop",
					// TEXT
					""
				)
				// Test URL in the enclosing section text.
				"./BasicObjectOrientedProgrammingSupportModifiersSpec.html" should beAccessibleFrom this
				var visib = "annotation A1 {}".parse.xtendTypes.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

			/** A nested annotation type may be declared with one or more modifiers, which affect its runtime behavior: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  there are no restrictions on accessing the annotation type (default);</li>
			 *     <li>`protected`:  the annotation type is accessible within the same package, and derived classes;</li>
			 *     <li>`package`: the annotation type is accessible only within the same package as its class;</li>
			 *     <li>`private`: the annotation type is accessible only within its class.</li>
			 *     </ul></li>
			 * <li>`abstract`: the annotation type is abstract and cannot be instanced.</li>
			 * <li>`static`: the inner annotation type do not have access to the non-static members of the enclosing type.</li>
			 * </ul>
			 *
			 * <note>Terminology: Nested annotation types are divided into two categories: static and non-static.
			 * Nested annotation types that are declared static are called <i>static nested annotation types</i>.
			 * Non-static nested annotation types are called <i>inner annotation types</i>.</note>
			 * 
			 * <note>The modifiers may differ from the previously described, depending on the enclosing type, e.g. agent.</note>
			 * 
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Nested Annotation Type Modifiers" {
				'''
					class EnclosingClass {
						public annotation NestedAnnotationType1 {
						}
						protected annotation NestedAnnotationType2 {
						}
						package annotation NestedAnnotationType3 {
						}
						private annotation NestedAnnotationType4 {
						}
						abstract annotation NestedAnnotationType5 {
						}
						static annotation NestedAnnotationType6 {
						}
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop",
					// TEXT
					""
				)
				var visib = "class CA { annotation A1 {} } ".parse.xtendTypes.get(0).members.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

			/** The modifiers for the values in an annotation type are: <ul>
			 * <li>Access modifiers: <ul>
			 *     <li>`public`:  there are no restrictions on accessing the value;</li>
			 *     </ul></li>
			 * <li>`static`: the value is a class value, not an instance value.</li>
			 * </ul>
			 *
			 * @filter(.* = '''|'''|.parseSuccessfully.*)
			 */
			fact "Value Modifiers" {
				'''
					public val val1 : int
					static val val2 : int
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop
					annotation MyAnnotationType1 {",
					// TEXT
					"}"
				)
				var visib = "annotation A1 { val val1 : int}".parse.xtendTypes.get(0).members.get(0)
				visib should beVisibleWith JvmVisibility::PUBLIC
			}

		}

	}

	/** Anonymous classes enable you to make your code more concise.
	 * They enable you to declare and instantiate a class at the same time.
	 * They are like local classes except that they do not have a name.
	 * Use them if you need to use a local class only once.
	 */
	describe "Anonymous Class" {

		/** While local classes are class declarations, anonymous classes are expressions,
		 * which means that you define the class in another expression.
		 * 
		 * <p>The following example, `HelloWorldAnonymousClasses`, uses anonymous classes in the
		 * initialization statements of the local variables `frenchGreeting` and `spanishGreeting`:
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Declaring Anonymous Classes" {
			'''
			interface HelloWorld {
			    def greetSomeone(someone : String)
			}

			class HelloWorldAnonymousClasses {
			    def sayHello {
			        var frenchGreeting = new HelloWorld {
			            var name = "tout le monde"
			            def greetSomeone(someone : String) {
			                name = someone
			                println("Salut " + name)
			            }
			        }
			        
			        var spanishGreeting = new HelloWorld {
			            var name = "mundo"
			            def greetSomeone(someone : String) {
			                name = someone
			                println("Hola, " + name)
			            }
			        }

			        frenchGreeting.greetSomeone("Marc")
			        spanishGreeting.greetSomeone("Marco")
			    }
			}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop",
				// TEXT
				""
			)
		}

		/** As mentioned previously, an anonymous class is an expression.
		 * The syntax of an anonymous class expression is like the invocation of a constructor,
		 * except that there is a class definition contained in a block of code.
		 * 
		 * <p>Consider the instantiation of the frenchGreeting object ion the code below.
		 *
		 * <p>The anonymous class expression consists of the following:<ul>
		 * <li>The `new` operator</li>
		 * <li>The name of an interface to implement or a class to extend. In this example,
		 * the anonymous class is implementing the interface `HelloWorld`.</li>
		 * <li>Parentheses that contain the arguments to a constructor, just like a normal class
		 * instance creation expression.
		 * When you implement an interface, there is no constructor, so you do not need to put parameters,
		 * as in this example.</li>
		 * <li>A body, which is a class declaration body. More specifically, in the body, method
		 * declarations are allowed but statements are not.</li>
		 * </ul>
		 * 
		 * <p>Because an anonymous class definition is an expression, it must be part of a statement.
		 * In this example, the anonymous class expression is part of the statement that instantiates
		 * the frenchGreeting object.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Syntax of Anonymous Classes" {
			'''
				var frenchGreeting = new HelloWorld {
				    var name = "tout le monde"
				    def greetSomeone(someone : String) {
				        name = someone
				        println("Salut " + name)
				    }
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop
			    interface HelloWorld {
			        def greetSomeone(someone : String)
			    }
				class HelloWorldAnonymousClasses {
				    def sayHello {
				",
				// TEXT
				"} }"
			)
		}

		/** Anonymous classes can capture variables; they have the same access to
		 * local variables of the enclosing scope:<ul>
		 * <li>An anonymous class has access to the members of its enclosing class.</li>
		 * <li>An anonymous class cannot access local variables in its enclosing scope that are
		 * not declared as final or effectively final.</li>
		 * <li>A declaration of a type (such as a variable) in an anonymous class shadows any
		 * other declarations in the enclosing scope that have the same name.</li>
		 * </ul>
		 *
		 * <p>Anonymous classes have restrictions with respect to their members:<ul>
		 * <li>You cannot declare static initializers or member interfaces in an anonymous class.</li>
		 * <li>An anonymous class can have static members provided that they are constant variables.</li>
		 * </ul>
		 * 
		 * <li>Note that you can declare the following in anonymous classes: fields, extra methods
		 * (even if they do not implement any methods of the supertype), instance initializers, 
		 * local classes. However, you cannot declare constructors in an anonymous class.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "Accessing Local Variables of the Enclosing Scope, and Declaring and Accessing Members of the Anonymous Class" {
			'''
				class HelloWorldAnonymousClasses {
				  var name : String
				  def sayHello {
				    var frenchGreeting = new HelloWorld {
				      def greetSomeone(someone : String) {
				      	name = someone
				        println("Salut " + name)
				        
				      }
				    }
				  }
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.oop
			    interface HelloWorld {
			        def greetSomeone(someone : String)
			    }
				",
				// TEXT
				""
			)
		}

	}

	/* In this section, the semantic of the different modifiers is explained.
	 */
	describe "Modifiers" {
		
		/*
		 * An abstract type is a type that is declared `abstract` (it may or may not include abstract methods).
		 * Abstract types cannot be instantiated, but they can be derived.
		 *
		 * <p>An abstract method is a method that is declared without an implementation 
		 * (without braces, and followed by a semicolon), like this:
		 * 
		 *       abstract def moveTo(deltaX : double, deltaY : double)
		 *
		 * <p>If a type includes abstract methods, then the type itself must be declared abstract, as in:
		 * 
		 *       abstract class GraphicObject {
		 *         // declare fields
		 *         // declare nonabstract methods
		 *         abstract def draw
		 *       }
		 *
		 * <p>When an abstract type is derived, the subtype usually provides implementations for all of the
		 * abstract methods in its parent type. However, if it does not, then the subtype must also be
		 * declared abstract.
		 *
		 * @filter(.*)
		 */
		fact "abstract Modifier" {
			'''
				package io.sarl.docs.reference.oop
				abstract class GraphicObject {
				   abstract def draw
				}
			'''.parseSuccessfully
		}

		/*
		 * Access level modifiers determine whether other types can use a particular field or invoke
		 * a particular method. There are two levels of access control:
		 * 
		 *  * At the top level in a SARL script.
		 *  * At the member level - inside another type.
		 *
		 * <p>A type may be declared with the modifier `public`, in which case that type is visible to
		 * all types everywhere.
		 * If a type has the modifier `package`, it is visible only within its own package
		 * (packages are named groups of related types).
		 * 
		 * <p>At the member level, you can also use the `public` modifier or `package` modifier just as
		 * with top-level types, and with the same meaning.
		 * For members, there are two additional access modifiers: `private` and `protected`.
		 * The `private` modifier specifies that the member can only be accessed in its own type.
		 * The `protected` modifier specifies that the member can only be accessed within its own package
		 * (as with `package`) and, in addition, by a derived type of its type in another package.
		 *
		 * <p>The following table shows the access to members permitted by each modifier.
		 * 
		 * <table><thead>
		 * <tr><th>Modifier</th><th>From the type</th><th>From the package</th><th>From subtypes</th><th>Other</th></tr>
		 * </thead><tbody>
		 * <tr><td><code>public</code></td><td>yes</td><td>yes</td><td>yes</td><td>yes</td></tr>
		 * <tr><td><code>protected</code></td><td>yes</td><td>yes</td><td>yes</td><td>no</td></tr>
		 * <tr><td><code>package</code></td><td>yes</td><td>yes</td><td>no</td><td>no</td></tr>
		 * <tr><td><code>private</code></td><td>yes</td><td>no</td><td>no</td><td>no</td></tr>
		 * </tbody></table>
		 *
		 * <p>The first column indicates whether the type itself has access to the member defined by the
		 * access level. As you can see, a type always has access to its own members.
		 * The second column indicates whether types in the same package as the type (regardless of their parentage)
		 * have access to the member.
		 * The third column indicates whether subtypes of the type declared outside this package have access to the member.
		 * The fourth column indicates whether all types have access to the member.
		 *
		 * @filter(.*)
		 */
		fact "Access Modifiers: public, protected, package, private" {
			'''
				package io.sarl.docs.reference.oop
				class C1 {
				   public def pubfct {}
				   protected def protfct {}
				   package def packfct {}
				   private def privfct {}
				   
				   def test0 {
				   	pubfct;
				   	protfct;
				   	packfct;
				    privfct;
				   }
				}
				class C2 extends C1 {
				   def test1 {
				   	pubfct;
				   	protfct;
				   	packfct;
				   }
				}
				class C3 {
				   def test3(obj : C1) {
				   	obj.pubfct;
				   	obj.protfct;
				   	obj.packfct;
				   }
				}
			'''.parseSuccessfully
		}

		/*
		 * Generally, method resolution and binding is done statically at compile time.
		 * Method calls are bound based on the static types of arguments.
		 *
		 * <p>Sometimes this is not what you want. Especially in the context of extension methods
		 * you would like to have polymorphic behavior.
		 *
		 * <p>The `dispatch` modifier permits defining a dispatch method.
		 * For a set of visible dispatch methods in the current type hierarchy with the same name and
		 * the same number of arguments, the compiler infers a synthetic dispatcher method.
		 * This dispatcher uses the common super type of all declared arguments.
		 * The method name of the actual dispatch cases is prepended with an underscore and the
		 * visibility of these methods is reducedBy adding the extension keyword to a field, a local variable or a parameter declaration, its
		 * instance methods become extension methods. to protected if they have been defined as
		 * public methods.
		 * Client code always binds to the synthesized dispatcher method.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 */
		fact "dispatch Modifier" {
				'''
					def dispatch printType(x : Number) { 
					  "it's a number" 
					}
					 
					def dispatch printType(x : Integer) { 
					  "it's an int" 
					}

					def clientCode {
						System.out.println(printType(4.5)) // Print "it's a number"
						System.out.println(printType(4))   // Print "it's an int"
					}
				'''.parseSuccessfully(
					"package io.sarl.docs.reference.oop
					class MyClass {",
					// TEXT
					"}"
				)
		}

		/* This modifier enables to mark a field, a formal parameter, or a local variable as
		 * an <a href="./GeneralSyntaxReferenceSpec.html#ExtensionMethods">extension provider</a>.
		 * 
		 * <p>Extension methods allow adding new methods to existing 
		 * types without modifying them. This is really 
		 * helpful as they can greatly improve the readability. They
		 * use a simple syntactic trick: the first parameter of a method
		 * can either be passed in after opening the parentheses or before the 
		 * method call. For example, given a method:
		 *  
		 *     def removeVowels (String s) {
		 *         s.replaceAll("[aeiouAEIOU]", "")
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
		 * 
		 * <p>By adding the `extension` keyword to a field, a local variable or a parameter declaration, its
		 * instance methods become extension methods.
		 * 
		 * <p>In the following example, three functions are defined for illustrating the three types of
		 * extension providers.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "extension Modifier" {
			'''
				class Examples {
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
			"./GeneralSyntaxReferenceSpec.html#ExtensionMethods" should beAccessibleFrom this
		}

		/*
		 * The `final` keyword is used in several different contexts to define an entity which may only be
		 * assigned once.
		 * 
		 * <p>Once a final variable has been assigned, it always contains the same value. If a final variable
		 * holds a reference to an object, then the state of the object may be changed by operations on the
		 * object, but the variable will always refer to the same object.
		 * 
		 * <caution>The SARL compiler complains if you write the `final` modifer in conjonction with the `var`
		 * or `val` modifiers. Indeed, the `val` modifier defines a final variable; and the `var` modifier
		 * defines a no-final variable.</caution>
		 *
		 * <p>This applies also to arrays, because arrays are objects; if a final variable holds a reference to
		 * an array, then the components of the array may be changed by operations on the array, but the variable
		 * will always refer to the same array.
		 * 
		 * <p>A final method cannot be overridden or hidden by subclasses. This is used to prevent unexpected
		 * behavior from a subtype altering a method that may be crucial to the function or consistency of
		 * the type.
		 * 
		 * <p>A final type cannot be derived. Doing this can confer security and efficiency benefits, so many
		 * of the Java standard library classes are final, such as <code>java.lang.System</code> and
		 * <code>java.lang.String</code>. All methods in a final type are implicitly final.
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "final Modifier" {
			'''
				class A {
					//
					// Final field
					//
					val field = 4
					//
					// Final method
					//
					final def cannotBeOverriden {
					}
				}
				final class B {
					// This class cannot be derived
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT,
				"")
		}
		
		/*
		 * The `native` keyword is applied to a method to indicate that the method is implemented in
		 * native code, i.e. outside SARL and Java, using the Java Native Interface.
		 * 
		 * <note>This modifier is provided for enabling agents to access to low-level resources that
		 * are not supported by the Java API. You should not use this modifier if you can use
		 * a higher-level API in place of.</note>
		 * 
		 * @filter(.*) 
		 */
		fact "native Modifier" {
			'''
				package io.sarl.docs.reference.gsr
				class A {
					native def fct
				}
			'''.parseSuccessfully
		}		

		/*
		 * The `static` keyword is used for creating fields and methods that belong to the type, rather than to an
		 * instance of the type.
		 * 
		 * <p>Sometimes, you want to have variables that are common to all instances. This is accomplished with
		 * the `static` modifier. Fields that have the static modifier in their declaration are called static 
		 * fields (or class variables in object-oriented programming). They are associated with the type,
		 * rather than with any instance. Every instance of the type shares a static field, which is in one 
		 * fixed location in memory. Any instance can change the value of a static variable, but static variables
		 * can also be manipulated without creating an instance of the type.
		 * 
		 * <note>Constants are usually defined as final static fields</note>
		 * 
		 * <p>Static methods, which have the `static` modifier in their declarations, should be invoked with the
		 * type name, without the need for creating an instance of the type. 
		 * 
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "static Modifier" {
			'''
				class A {
					//
					// Static field
					//
					static var field : int
					//
					// Constant
					//
					static val constant = 4
					//
					// Static method
					//
					static def aMethod {
					}
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT,
				"")
		}		

		/*
		 * The `strictfp` modifier is a keyword that restricts floating-point calculations to ensure portability.
		 * The `strictfp` command was originally introduced into Java with the Java virtual machine (JVM) version 1.2
		 * and is available for use on all currently updated Java VMs.
		 *
		 * <p>The IEEE standard IEEE 754 specifies a standard method for both floating-point calculations and
		 * storage of floating-point values in either single (32-bit, used in float) or double (64-bit, used in double)
		 * precision, and, for intermediate calculations, also extended precision formats.
		 * 
		 * <p>Since JVM 1.2, intermediate computations are not limited to the standard 32 bit and 64 bit precisions.
		 * On platforms that can handle other representations e.g. 80-bit double extended on x86 or x86-64 platforms,
		 * those representations can be used, helping to prevent round-off errors and overflows, thereby increasing
		 * precision.
		 *
		 * <p>For some applications, a programmer might need every platform to have precisely the same floating-point
		 * behavior, even on platforms that could handle greater precision. However, if this level of precision is not
		 * necessary the VM does not use intermediates by default.
		 * 
		 * <p>From the VM perspective, turning on this higher precision means the following:
		 * <table><thead>
		 * <tr><th>Precision</th><th>Intermediate</th></tr>
		 * </thead><tbody>
		 * <tr><td>32 bits</td><td>64 bits</td></tr>
		 * <tr><td>64 bits</td><td>80 bits (if available)</td></tr>
		 * </tbody></table>
		 * 
		 * The `strictfp` modifier accomplishes this by truncating all intermediate values to IEEE single precision and
		 * double precision, as occurred in earlier versions of the JVM.
		 * 
		 * @filter(.*) 
		 */
		fact "strictfp Modifier" {
			'''
				package io.sarl.docs.reference.gsr
				class A {
					strictfp def fct { }
				}
			'''.parseSuccessfully
		}

		/*
		 * The SARL programming language provides two basic synchronization idioms: synchronized methods and synchronized
		 * statements.
		 * The synchronized statements are described in the
		 * <a href="./GeneralSyntaxReferenceSpec.html#SynchronizedExpression">general reference</a>.
		 * This section is about synchronized methods.
		 * 
		 * <p>To make a method synchronized, simply add the `synchronized` modifier to its declaration (see
		 * example below).
		 * 
		 * <p>Synchronized methods enable a simple strategy for preventing thread interference and memory
		 * consistency errors: if an object is visible to more than one thread, all reads or writes to that
		 * object's variables are done through synchronized methods.
		 * (An important exception: final fields, which cannot be modified after the object is constructed, 
		 * can be safely read through non-synchronized methods, once the object is constructed)
		 * This strategy is effective, but can present problems with liveness.
		 *
		 * @filter(.* = '''|'''|.parseSuccessfully.*) 
		 */
		fact "synchronized Modifier" {
			'''
				class SynchronizedCounter {
					var c = 0
				
					synchronized def increment {
						c++
					}
				
					synchronized def decrement {
						c--
					}
				
					synchronized def value {
						return c
					}
				}
			'''.parseSuccessfully(
				"package io.sarl.docs.reference.gsr",
				// TEXT,
				"")
			"./GeneralSyntaxReferenceSpec.html#SynchronizedExpression" should beAccessibleFrom this
		}		

		/* Java and SARL provide a mechanism, called object serialization where an object can be represented as
		 * a sequence of bytes that includes the object's data as well as information about the object's type and
		 * the types of data stored in the object.
		 * 
		 * The `transient` modifier is a keyword used as a field modifier.
		 * When a field is declared transient, it would not be serialized even if the type to which it belongs
		 * is serialized.
		 * 
		 * @filter(.*) 
		 */
		fact "transient Modifier" {
			'''
				package io.sarl.docs.reference.gsr
				class A {
					transient var field : int = 4
				}
			'''.parseSuccessfully
		}		

		/*
		 * While the `volatile` modifier itself comes from the C programming language, it has a completely
		 * different meaning in `Java`, and then in SARL. This may not help in growing an understanding of it, 
		 * googling for volatile could lead to different results. Let's take a quick side step and see what
		 * volatile means in C first.
		 * 
		 * <p>In the C language the compiler ordinarily assumes that variables cannot change value by themselves.
		 * While this makes sense as default behavior, sometimes a variable may represent a location that
		 * can be changed (like a hardware register). Using a volatile variable instructs the compiler
		 * not to apply these optimizations.
		 *
		 * <p>Back to Java and SARL. The meaning of volatile in C would be useless in Java. The JVM uses
		 * native libraries to interact with the OS and hardware. Further more, it is simply impossible
		 * to point Java variables to specific addresses, so variables actually won't change value by themselves.
		 *
		 * <p>However, the value of variables on the JVM can be changed by different threads. By default the
		 * compiler assumes that variables won't change in other threads. Hence it can apply optimizations such
		 * as reordering memory operations and caching the variable in a CPU register. Using a volatile variable
		 * instructs the compiler not to apply these optimizations. This guarantees that a reading thread
		 * always reads the variable from memory (or from a shared cache), never from a local cache.
		 *
		 * <note>From now, it is better to use
		 * <a href="https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/atomic/package-summary.html">atomic
		 * references</a> in place of volatile variables.</note>
		 * 
		 * @filter(.*) 
		 */
		fact "volatile Modifier" {
			"https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/atomic/package-summary.html" should beURL _
			'''
				package io.sarl.docs.reference.gsr
				class A {
					volatile var field : int = 4
				}
			'''.parseSuccessfully
		}		

	}

	/* This documentation is based on documentations from the Xtext and Xtend projects,
	 * and from the Java tutorials.
	 * Thank you to the contributors to these documents.
	 * 
	 * * [Xtend](https://www.eclipse.org/xtend/documentation.html)
	 * * [Xtext](https://www.eclipse.org/Xtext/documentation.html)
	 * * [Java Tutorials](https://docs.oracle.com/javase/tutorial/)
	 *
	 * @filter(.*) 
	 */
	fact "References" {
		"https://www.eclipse.org/xtend/documentation.html" should beURL _
		"https://www.eclipse.org/Xtext/documentation.html" should beURL _
		"https://docs.oracle.com/javase/tutorial/" should beURL _
	}

	/* Specification: SARL General-purpose Agent-Oriented Programming Language ("Specification")<br/>
	 * Version: %sarlspecversion%<br/>
	 * Status: %sarlspecreleasestatus%<br/>
	 * Release: %sarlspecreleasedate%
	 * 
	 * 
	 * <p>Copyright &copy; %copyrightdate% %copyrighters%.
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
