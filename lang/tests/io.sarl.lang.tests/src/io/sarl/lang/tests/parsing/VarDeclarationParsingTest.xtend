/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.parsing

import com.google.inject.Inject
import io.sarl.lang.SARLInjectorProvider
import io.sarl.lang.sarl.Model
import io.sarl.lang.sarl.SarlPackage
import org.eclipse.xtext.common.types.TypesPackage
import org.eclipse.xtext.diagnostics.Diagnostic
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.XbasePackage
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class VarDeclarationParsingTest {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper

	@Test
	def void variableDeclaration_attributeScope_xtend() {
		val mas = '''
			import java.util.List
			agent A1 {
				var List<Integer> list
				var i = 45
				var double j = 45
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"no viable alternative at input 'List'")
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"no viable alternative at input 'double'")
	}

	@Test
	def void variableDeclaration_localScope_xtend() {
		val mas = '''
			agent A1 {
				def myaction {
					var int i
					var j = 45
					var double k = 45
					System.out.println(i)
					System.out.println(j)
					System.out.println(k)
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmIdentifiableElement 'i'")
		mas.assertError(
			XbasePackage::eINSTANCE.XAssignment,
			Diagnostic::LINKING_DIAGNOSTIC,
			"The method k(int) is undefined")
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmIdentifiableElement 'k'")
	}

	@Test
	def void variableDeclaration_attributeScope() {
		val mas = '''
			import java.util.List
			agent A1 {
				var list : List<Integer>
				var i = 45
				var j : double = 45
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void variableDeclaration_localScope() {
		val mas = '''
			import java.util.List
			agent A1 {
				def myaction {
					var i : List<Integer>
					var j = 45
					var k : double = 45
					System.out.println(i)
					System.out.println(j)
					System.out.println(k)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void valueDeclaration_attributeScope_xtend() {
		val mas = '''
			import java.util.List
			agent A1 {
				val List<Integer> list
				val i = 45
				val double j = 45
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.agent,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"no viable alternative at input 'List'")
		mas.assertError(
			SarlPackage::eINSTANCE.agent,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"no viable alternative at input 'double'")
	}

	@Test
	def void valueDeclaration_localScope_xtend() {
		val mas = '''
			agent A1 {
				def myaction {
					val int i
					val j = 45
					val double k = 45
					System.out.println(i)
					System.out.println(j)
					System.out.println(k)
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmIdentifiableElement 'i'")
		mas.assertError(
			XbasePackage::eINSTANCE.XAssignment,
			Diagnostic::LINKING_DIAGNOSTIC,
			"The method k(int) is undefined")
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmIdentifiableElement 'k'")
	}

	@Test
	def void valueDeclaration_attributeScope() {
		val mas = '''
			import java.util.List
			agent A1 {
				val list : List<Integer>
				val i = 45
				val j : double = 45
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void valueDeclaration_localScope() {
		val mas = '''
			agent A1 {
				def myaction {
					val j = 45
					val k : double = 45
					System.out.println(j)
					System.out.println(k)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void forLoop_xtend() {
		val mas = '''
			import java.util.List
			agent A1 {
				var list : List<Integer>
				def myaction {
					for( Number i : list) {
						System.out.println(i)
					}
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage.eINSTANCE.XForLoopExpression,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"extraneous input 'i' expecting ':'")
	}

	@Test
	def void forLoop_inferredType() {
		val mas = '''
			import java.util.List
			agent A1 {
				var list : List<Integer>
				def myaction {
					for( i : list) {
						System.out.println(i)
					}
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void forLoop_explicitType() {
		val mas = '''
			import java.util.List
			agent A1 {
				var list : List<Integer>
				def myaction {
					for( i as Number : list) {
						System.out.println(i)
					}
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void catch_xtend() {
		val mas = '''
			agent A1 {
				def myaction {
					try {
						System.out.println("G")
					}
					catch(Throwable e) {
						System.out.println(e)
					}
				}
			}
		'''.parse
		mas.assertError(
			TypesPackage.eINSTANCE.jvmParameterizedTypeReference,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"missing ':' at 'e'")
	}

	@Test
	def void catch_oneType() {
		val mas = '''
			agent A1 {
				def myaction {
					try {
						System.out.println("G")
					}
					catch(e : Throwable) {
						System.out.println(e)
					}
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void multicatch_xtend() {
		val mas = '''
			agent A1 {
				def myaction {
					try {
						System.out.println("G")
					}
					catch(Exception e) {
						System.out.println(e)
					}
					catch(Throwable e) {
						System.out.println(e)
					}
				}
			}
		'''.parse
		mas.assertError(
			TypesPackage.eINSTANCE.jvmParameterizedTypeReference,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"missing ':' at 'e'")
	}

	@Test
	def void multicatch_oneType() {
		val mas = '''
			agent A1 {
				def myaction {
					try {
						System.out.println("G")
					}
					catch(e : Exception) {
						System.out.println(e)
					}
					catch(e : Throwable) {
						System.out.println(e)
					}
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void closure_xtend() {
		val mas = '''
			agent A1 {
				def mycall(a : int, f : (Number,Number) => int) {
					return a + f.apply
				}
				def myaction {
					mycall(4) [ Float a, Integer b |
						2 * a.floatValue + b.intValue
					]
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage.eINSTANCE.XClosure,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input ',' expecting ']'")
	}

	@Test
	def void closure_twoParams() {
		val mas = '''
			agent A1 {
				def mycall(a : int, f : (Float,Integer) => float) : float {
					return a + f.apply(5.45f, 6)
				}
				def myaction : void {
					mycall(4) [ a : Float, b : Integer |
						2f * a.floatValue + b.intValue
					]
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

}
