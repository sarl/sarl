/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
import org.eclipse.xtend.core.xtend.XtendEnumLiteral
import org.eclipse.xtend.core.xtend.XtendField
import org.jnario.runner.CreateWith

import static extension io.sarl.docs.utils.SpecificationTools.*

/**
 * @outline
 *
 * SARL enables to declare objects with an object-oriented programming approach.
 * This document describes the basic support of object-oriented programming provided by SARL.
 * Before reading this document, it is recommended reading
 * the [General Syntax Reference](./GeneralSyntaxReferenceSpec.html).
 * 
 * The support of the object-oriented programming (OOP) statements in SARL is less complete
 * than a real object-oriented language such as Java.
 * The basics of OOP are enabled in the SARL scripts. If you need more complex or more
 * complete support of the OOP, you should use a dedicated language, such as Java,
 * [Xtend](https://eclipse.org/xtend/), or [Scala](http://www.scala-lang.org/).
 * 
 */
@CreateWith(SARLSpecCreator)
describe "Basic Object-Oriented Programming Support" {

	@Inject extension SARLParser

	/** Objects are structures that contain both data and procedures.
	 * Classes are definitions for the data format and available procedures
	 * for a given type or class of object. They may also contain data and
	 * procedures (known as class methods) themselves
	 */
	describe "Class" {
		
		/* For defining a class, you could use the ```class``` keyword.
		 * The following example defines the class ```MyClass```: 
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
		 * For example, class ```Employee``` might inherit from class ```Person```.
		 * All the data and methods available to the parent class also appear in
		 * the child class with the same names.
		 * For example, class ```Person``` might define variables "firstName" and
		 * "lastName" with method "getFullName()". These will also be available
		 * in class Employee, which might add the variables
		 * "position" and "salary".
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

	}

	/** An interface is a description of the actions that an object can do.
	 * For example when you flip a light switch, the light goes on, you don't
	 * care how, just that it does. In object-oriented programming, an
	 * interface is a description of all functions that an object must have
	 * in order to be an "X".
	 * 
	 * The purpose of interfaces is to allow the program to enforce these
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
				"package io.sarl.docs.reference.oop\n"
				+ "interface Light {\n"
				+ "def turnOn\n"
				+ "def turnOff\n"
				+ "}\n",
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
		 * A class is able to implement an interface.
		 * The ```implements``` keyword is used for defining
		 * the implementation relationship between a class
		 * and an interface.
		 * The class must provide an implementation of all
		 * the functions defined in the interface.
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
				"package io.sarl.docs.reference.oop\n"
				+ "interface Light {\n"
				+ "def turnOn\n"
				+ "def turnOff\n"
				+ "}\n",
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

	}

	/** An enumeration specifies a list of constant values assigned to a type.
	 *
	 * The SARL enumeration is not object-oriented unlike the enumeration
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

	}

	/** An annotation is a form of syntactic metadata that can be added to SARL source code.
	 * Annotations can be reflective in that they can be embedded in binary files generated by the
	 * SARL compiler, and may be retained by the Virtual Machine to be made retrievable at run-time.
	 */
	describe "Annotation" {

		/* For defining an annotation, you could use the ```annotation``` keyword.
		 * The following example defines the annotation ```MyAnnotation```: 
		 *  
		 * @filter(.* = '''|'''|.parseSuccessfully.*)
		 * 
		 * This annotation defines three parameters:<ul>
		 * <li><code>value</code>, an array of strings of characters, without default value;</li>
		 * <li><code>isTricky</code>, a boolean value, with the default ```false```;</li>
		 * <li><code>lotteryNumbers</code>, an array of integer numbers, with a default value.</li>
		 * </ul>
		 */
		fact "Define an Annotation" {
			val model = '''
				annotation MyAnnotation {
				  String[] value
				  boolean isTricky = false
				  int[] lotteryNumbers = #[ 42, 137 ]
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

	}

}
