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
import org.eclipse.xtext.common.types.TypesPackage
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.junit.runner.RunWith

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class EventParsingTest {
	@Inject extension ParseHelper<SarlScript>
	@Inject extension ValidationTestHelper
	
	@Test
	def void missedFinalFieldInitialization() {
		val mas = '''
			event E1 {
				val field1 : int = 5
				val field2 : String
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			org.eclipse.xtext.xbase.validation.IssueCodes::MISSING_INITIALIZATION,
			"The blank final field 'field2' may not have been initialized")
	}
	
	@Test
	def void completeFinalFieldInitialization() {
		val mas = '''
			event E1 {
				val field1 : int = 5
				val field2 : String = ""
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def invalidAttributeName_0() {
		val mas = '''
			event E1 {
				var myfield1 = 4.5
				var ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = "String"
				var myfield2 = true
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::INVALID_MEMBER_NAME,
			"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.")
	}

	@Test
	def invalidAttributeName_1() {
		val mas = '''
			event E1 {
				val myfield1 = 4.5
				val ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = "String"
				val myfield2 = true
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::INVALID_MEMBER_NAME,
			"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.")
	}

	@Test
	def void multipleVariableDefinition() {
		val mas = '''
			event E1 {
				var myfield : int
				var myfield1 : String
				var myfield : double
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::DUPLICATE_FIELD,
			"Duplicate field in 'E1': myfield")
	}

	@Test
	def void multipleValueDefinition() {
		val mas = '''
			event E1 {
				val myfield : int = 4
				val myfield1 : String = ""
				val myfield : double = 5
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::DUPLICATE_FIELD,
			"Duplicate field in 'E1': myfield")
	}

	@Test
	def void fieldNameShadowing() {
		val mas = '''
			event E0 {
				val field1 : int = 5
				val field2 : int = 6
			}
			event E1 extends E0 {
				val field1 : int = 5
			}
		'''.parse
		mas.assertWarning(
			SarlPackage::eINSTANCE.attribute,
			org.eclipse.xtext.xbase.validation.IssueCodes::VARIABLE_NAME_SHADOWING,
			"The field 'field1' in 'E1' is hidding the inherited field 'E0.field1'.")
	}

	@Test
	def void invalidExtend() {
		val mas = '''
			capacity C1 {
			}
			event E1 extends C1 {
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Invalid super-type: 'C1'. Only the type 'io.sarl.lang.core.Event' and one of its subtypes are allowed for 'E1'")
	}

}
