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
import io.sarl.lang.sarl.SarlScript
import io.sarl.lang.sarl.SarlPackage
import org.eclipse.xtext.diagnostics.Diagnostic
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Ignore
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import io.sarl.lang.validation.IssueCodes
import org.eclipse.xtext.common.types.TypesPackage

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class AgentParsingTest {
	@Inject extension ParseHelper<SarlScript>
	@Inject extension ValidationTestHelper

	@Test def void testParse() {
		val mas = '''
			package test
			agent A1 {}
			agent A2 {}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test def parseBehaviorDeclaration() {
		val mas = '''
			event E {}
			agent A1 {
				on E {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test def parseBehaviorWithGuard() {
		val mas = '''
			event E {}
			agent A1 {
				on E [ occurrence.source != null] {}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test def parseAgentWithAttributes() {
		val mas = '''
			agent A1 {
				var name : String = "Hello"
				var number : Integer
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test def parseAgentWithConstAttributes() {
		val mas = '''
			
			agent A1 {
				val name : String = "Hello"
				var number : Integer
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test def eventsMustBeDeclared() {
		val mas = '''
			
			agent A1 {
				on E  {}
			}
		'''.parse

		mas.assertError(
			SarlPackage::eINSTANCE.behaviorUnit,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to Event 'E'.")
	}

	@Test @Ignore("not ready yet")
	def constAttributesMustHaveIniatlizer() {
		val mas = '''
			
			agent A1 {
				val name : String = "Hello"
				val number : Integer
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"Constant attribute 'number' must be initialized ."
		)
	}

	@Test
	def capacityMustBeDeclaredBeforeUse() {
		val mas = '''
			agent A1 {
				uses MyCap
			}
		'''.parse		
		mas.assertError(
			SarlPackage::eINSTANCE.capacityUses,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to Capacity 'MyCap'."
		)
	}

	@Test
	def agentCanDeclareCapacityUses() {
		val mas = '''
			capacity MyCap {
				def my_operation
			}
			
			agent A1 {
				uses MyCap
			}
		'''.parse
		mas.assertNoErrors

	}
	
	@Test
	def void multipleActionDefinitionInAgent() {
		val mas = '''
			agent A1 {
				def myaction(a : int, b : int) { }
				def myaction(a : int) { }
				def myaction(a : int) { }
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.action,
			IssueCodes::ACTION_COLLISION,
			"Cannot define many times the same feature in 'A1': myaction(a : int)")
	}

	@Test
	def invalidActionName() {
		val mas = '''
			agent A1 {
				def myaction {
					System.out.println("ok")
				}
				def _handle_myaction {
					System.out.println("ko")
				}
				def myaction2 {
					System.out.println("ok")
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			IssueCodes::INVALID_ACTION_NAME,
			"Invalid action name '_handle_myaction'.")
	}

	@Test
	def invalidAttributeName_0() {
		val mas = '''
			agent A1 {
				var myfield1 = 4.5
				var ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = "String"
				var myfield2 = true
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::INVALID_ATTRIBUTE_NAME,
			"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.")
	}

	@Test
	def invalidAttributeName_1() {
		val mas = '''
			agent A1 {
				val myfield1 = 4.5
				val ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = "String"
				val myfield2 = true
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::INVALID_ATTRIBUTE_NAME,
			"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.")
	}

	@Test
	def void missedFinalFieldInitialization() {
		val mas = '''
			agent A1 {
				val field1 : int = 5
				val field2 : String
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmField,
			org.eclipse.xtext.xbase.validation.IssueCodes::MISSING_INITIALIZATION,
			"The blank final field 'field2' may not have been initialized")
	}
	
	@Test
	def void completeFinalFieldInitialization() {
		val mas = '''
			agent A1 {
				val field1 : int = 5
				val field2 : String = ""
			}
		'''.parse
		mas.assertNoErrors
	}

}
