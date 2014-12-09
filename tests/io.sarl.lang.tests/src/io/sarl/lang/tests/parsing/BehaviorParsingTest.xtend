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
import org.eclipse.xtext.xbase.XbasePackage
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
			IssueCodes::DUPLICATE_METHOD,
			"Duplicate action in 'B1': myaction(a : int)")
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
			IssueCodes::DUPLICATE_FIELD,
			"Duplicate field in 'B1': myfield")
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
			IssueCodes::DUPLICATE_FIELD,
			"Duplicate field in 'B1': myfield")
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
			SarlPackage::eINSTANCE.action,
			IssueCodes::INVALID_MEMBER_NAME,
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
			IssueCodes::INVALID_MEMBER_NAME,
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
			IssueCodes::INVALID_MEMBER_NAME,
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
			TypesPackage::eINSTANCE.jvmConstructor,
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
			SarlPackage.eINSTANCE.attribute,
			org.eclipse.xtext.xbase.validation.IssueCodes::VARIABLE_NAME_SHADOWING,
			"The field 'field1' in 'B2' is hidding the inherited field 'B1.field1'.")
	}

	@Test
	def void incompatibleReturnType_0() {
		val mas = '''
			behavior B1 {
				def myaction(a : int) : int {
					return 0
				}
			}
			behavior B2 extends B1 {
				def myaction(a : int) : float {
					return 0f
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.action,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
			"Incompatible return type between 'float' and 'int' for myaction(int).")
	}

	@Test
	def void incompatibleReturnType_1() {
		val mas = '''
			behavior B1 {
				def myaction(a : int) {
					// void
				}
			}
			behavior B2 extends B1 {
				def myaction(a : int) : int {
					return 0
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.action,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
			"Incompatible return type between 'int' and 'void' for myaction(int).")
	}

	@Test
	def void incompatibleReturnType_2() {
		val mas = '''
			behavior B1 {
				def myaction(a : int) : int {
					return 0
				}
			}
			behavior B2 extends B1 {
				def myaction(a : int) {
					// void
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.action,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
			"Incompatible return type between 'void' and 'int' for myaction(int).")
	}

	@Test
	def void compatibleReturnType_0() {
		val mas = '''
			behavior B1 {
				def myaction(a : int) : Number {
					return 0.0
				}
			}
			behavior B2 extends B1 {
				def myaction(a : int) : Double {
					return 0.0
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void compatibleReturnType_1() {
		val mas = '''
			behavior B1 {
				def myaction(a : int) : float {
					return 0f
				}
			}
			behavior B2 extends B1 {
				def myaction(a : int) : float {
					return 0f
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void invalidExtend_0() {
		val mas = '''
			capacity C1 {
			}
			behavior B1 extends C1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.behavior,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Invalid supertype. Expecting: class")
	}

	@Test
	def void invalidExtend_1() {
		val mas = '''
			agent A1 {
			}
			behavior B3 extends A1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.behavior,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Supertype must be of type 'io.sarl.lang.core.Behavior'")
	}

	@Test
	def void invalidExtend_2() {
		val mas = '''
			behavior B1 extends B1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.behavior,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'B1' is inconsistent")
	}

	@Test
	def void invalidExtend_3() {
		val mas = '''
			behavior B1 extends B2 {
			}
			behavior B2 extends B1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.behavior,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'B1' is inconsistent")
	}

	@Test
	def void invalidExtend_4() {
		val mas = '''
			behavior B1 extends B2 {
			}
			behavior B2 extends B1 {
			}
			behavior B3 extends B2 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.behavior,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'B1' is inconsistent")
	}

	@Test
	def void invalidExtend_5() {
		val mas = '''
			behavior B3 extends B2 {
			}
			behavior B2 extends B1 {
			}
			behavior B1 extends B2 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.behavior,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'B3' is inconsistent")
	}

	@Test
	def void multipleParameterNames() {
		val mas = '''
			behavior B1 {
				def myaction(a : int, b : int, c : int, b : int) {
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			org.eclipse.xtext.xbase.validation.IssueCodes::VARIABLE_NAME_SHADOWING,
			"Duplicate local variable b")
	}

	@Test
	def void duplicateTypeNames() {
		val mas = '''
			package io.sarl.test
			behavior B1 {
			}
			behavior B2 {
			}
			behavior B1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.behavior,
			IssueCodes::DUPLICATE_TYPE_NAME,
			"Duplicate definition of the type 'io.sarl.test.B1'")
	}
	
	@Test
	def void validImplicitSuperConstructor() {
		val mas = '''
			package io.sarl.test
			behavior B1 {
			}
			behavior B2 extends B1 {
				new (a : int) {
					super(null)
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void missedImplicitSuperConstructor_1() {
		val mas = '''
			package io.sarl.test
			behavior B1 {
			}
			behavior B2 extends B1 {
				new (a : int) {
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.constructor,
			IssueCodes::MISSING_CONSTRUCTOR,
			"Undefined default constructor in the super-type")
	}

	@Test
	def void missedImplicitSuperConstructor_2() {
		val mas = '''
			package io.sarl.test
			behavior B1 {
				new (a : int) {
					super(null)
				}
			}
			behavior B2 extends B1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.behavior,
			IssueCodes::MISSING_CONSTRUCTOR,
			"The constructor B1(io.sarl.lang.core.Agent) is undefined.")
	}

	@Test
	def void missedImplicitSuperConstructor_3() {
		val mas = '''
			package io.sarl.test
			behavior B1 {
				new (a : int) {
					super(null)
				}
			}
			behavior B2 extends B1 {
				new (a : int) {
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.constructor,
			IssueCodes::MISSING_CONSTRUCTOR,
			"Undefined default constructor in the super-type")
	}

	@Test
	def void invalidArgumentTypeToSuperConstructor() {
		val mas = '''
			package io.sarl.test
			behavior B1 {
				new (a : int) {
					super(null)
				}
			}
			behavior B2 extends B1 {
				new (a : int) {
					super("")
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XStringLiteral,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from String to int")
	}

	@Test
	def multipleCapacityUses_0() {
		val mas = '''
			capacity C1 {}
			capacity C2 {}
			behavior B1 {
				uses C1, C2, C1
				def testFct { }
			}
		'''.parse

		mas.assertWarning(
			SarlPackage::eINSTANCE.capacityUses,
			IssueCodes::REDUNDANT_CAPACITY_USE,
			"Redundant use of the capacity 'C1'")
	}

	@Test
	def multipleCapacityUses_1() {
		val mas = '''
			capacity C1 {}
			capacity C2 {}
			behavior B1 {
				uses C2
				def testFct { }
				uses C2, C1
			}
		'''.parse

		mas.assertWarning(
			SarlPackage::eINSTANCE.capacityUses,
			IssueCodes::REDUNDANT_CAPACITY_USE,
			"Redundant use of the capacity 'C2'")
	}

}
