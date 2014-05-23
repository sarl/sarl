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
import io.sarl.lang.sarl.SarlPackage
import io.sarl.lang.sarl.SarlScript
import io.sarl.lang.validation.IssueCodes
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.junit.runner.RunWith
import org.eclipse.xtext.common.types.TypesPackage

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class BehaviorParsingTest {
	@Inject extension ParseHelper<SarlScript>
	@Inject extension ValidationTestHelper

	@Test
	def void multipleActionDefinition() {
		val mas = '''
			behavior B1 {
				def myaction(a : int, b : int) { }
				def myaction(a : int) { }
				def myaction(a : int) { }
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.action,
			IssueCodes::ACTION_ALREADY_DEFINED,
			"Cannot define many times the same feature in 'B1': myaction(a : int)")
	}

	@Test
	def void multipleVariableDefinition() {
		val mas = '''
			behavior B1 {
				var myfield : int
				var myfield1 : String
				var myfield : double
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::FIELD_ALREADY_DEFINED,
			"Cannot define many times the same feature in 'B1': myfield")
	}

	@Test
	def void multipleValueDefinition() {
		val mas = '''
			behavior B1 {
				val myfield : int = 4
				val myfield1 : String = ""
				val myfield : double = 5
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::FIELD_ALREADY_DEFINED,
			"Cannot define many times the same feature in 'B1': myfield")
	}

	@Test
	def invalidActionName() {
		val mas = '''
			behavior B1 {
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
			behavior B1 {
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
			behavior B1 {
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
			behavior B1 {
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
			behavior B1 {
				val field1 : int = 5
				val field2 : String = ""
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void fieldNameShadowing() {
		val mas = '''
			behavior B1 {
				val field1 : int = 5
				def myaction(a : int) { }
			}
			behavior B2 extends B1 {
				val field1 : int = 5
				def myaction(a : int) { }
			}
		'''.parse
		mas.assertWarning(
			TypesPackage::eINSTANCE.jvmField,
			IssueCodes::FIELD_NAME_SHADOWING,
			"The field 'field1' in 'B2' is hidding the inherited field 'B1.field1'.")
	}

}
