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
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class CapacityParsingTest {
	@Inject extension ParseHelper<SarlScript>
	@Inject extension ValidationTestHelper
	
	@Test
	def void testCapacityDirectImplementation() {
		val mas = '''
			import io.sarl.lang.core.Capacity
			skill S1 implements Capacity {
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH,
			"Invalid implemented type: 'io.sarl.lang.core.Capacity'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'")
	}

	@Test
	def void multipleActionDefinitionInCapacity() {
		val mas = '''
			capacity C1 {
				def myaction(a : int, b : int)
				def myaction(a : int)
				def myaction(a : int)
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			IssueCodes::DUPLICATE_METHOD,
			"Duplicate action in 'C1': myaction(a : int)")
	}

	@Test
	def void multipleActionDefinitionInSkill() {
		val mas = '''
			capacity C1 { }
			skill S1 implements C1 {
				def myaction(a : int, b : int) { }
				def myaction(a : int) { }
				def myaction(a : int) { }
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			IssueCodes::DUPLICATE_METHOD,
			"Duplicate action in 'S1': myaction(a : int)")
	}
	
	@Test
	def void multipleVariableDefinitionInSkill() {
		val mas = '''
			capacity C1 { }
			skill S1 implements C1 {
				var myfield : int
				var myfield1 : String
				var myfield : double
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::DUPLICATE_FIELD,
			"Duplicate field in 'S1': myfield")
	}

	@Test
	def void multipleValueDefinitionInSkill() {
		val mas = '''
			capacity C1 { }
			skill S1 implements C1 {
				val myfield : int = 4
				val myfield1 : String = ""
				val myfield : double = 5
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::DUPLICATE_FIELD,
			"Duplicate field in 'S1': myfield")
	}

	@Test
	def invalidActionNameInCapacity() {
		val mas = '''
			capacity C1 {
				def myaction
				def _handle_myaction
				def myaction2
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			IssueCodes::INVALID_MEMBER_NAME,
			"Invalid action name '_handle_myaction'.")
	}

	@Test
	def invalidActionNameInSkill() {
		val mas = '''
			capacity C1 { }
			skill S1 implements C1 {
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
			IssueCodes::INVALID_MEMBER_NAME,
			"Invalid action name '_handle_myaction'.")
	}

	@Test
	def void missedFinalFieldInitialization() {
		val mas = '''
			capacity C1 { }
			skill S1 implements C1 {
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
			capacity C1 { }
			skill S1 implements C1 {
				val field1 : int = 5
				val field2 : String = ""
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void fieldNameShadowingInSkill() {
		val mas = '''
			capacity C1 { }
			capacity C2 { }
			skill S1 implements C1 {
				val field1 : int = 5
				def myaction(a : int) { }
			}
			skill S2 extends S1 implements C2 {
				val field1 : int = 5
				def myaction(a : int) { }
			}
		'''.parse
		mas.assertWarning(
			TypesPackage::eINSTANCE.jvmField,
			org.eclipse.xtext.xbase.validation.IssueCodes::VARIABLE_NAME_SHADOWING,
			"The field 'field1' in 'S2' is hidding the inherited field 'S1.field1'.")
	}

	@Test
	def void redundantCapacity_fromSuperType() {
		val mas = '''
			capacity C1 {}
			capacity C2 {}
			skill S1 implements C1 { }
			skill S2 extends S1 implements C2, C1 { }
		'''.parse
		mas.assertWarning(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
			"The feature 'C1' is already implemented by the super type 'S1'.")
	}

	@Test
	def void redundantCapacity_duplicate() {
		val mas = '''
			capacity C1 {}
			capacity C2 {}
			capacity C3 {}
			skill S1 implements C1 { }
			skill S2 extends S1 implements C2, C3, C2 { }
		'''.parse
		mas.assertWarning(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
			"The feature 'C2' is already implemented by the preceding interface 'C2'.")
	}

	@Test
	def void redundantCapacity_fromPreviousCapacity() {
		val mas = '''
			capacity C1 {}
			capacity C2 {}
			capacity C3 extends C2 {}
			skill S1 implements C1 { }
			skill S2 extends S1 implements C3, C2 { }
		'''.parse
		mas.assertWarning(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
			"The feature 'C2' is already implemented by the preceding interface 'C3'.")
	}

	@Test
	def void missedActionImplementation_0() {
		val mas = '''
			capacity C1 {
				def myaction1(a : int)
			}
			capacity C2 {
				def myaction2(b : float, c : boolean)
			}
			skill S1 implements C1, C2 {
				def myaction1(x : int) { }
				def myaction2(y : float, z : boolean) { }
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void missedActionImplementation_1() {
		val mas = '''
			capacity C1 {
				def myaction1(a : int)
			}
			capacity C2 {
				def myaction2(b : float, c : boolean)
			}
			skill S1 implements C1, C2 {
				def myaction2(b : float, c : boolean) { }
			}
		'''.parse
		mas.assertError(
			SarlPackage.eINSTANCE.skill,
			IssueCodes::MISSING_METHOD_IMPLEMENTATION,
			"The operation myaction1(int) must be implemented.")
	}

	@Test
	def void missedActionImplementation_2() {
		val mas = '''
			capacity C1 {
				def myaction1(a : int)
			}
			capacity C2 {
				def myaction2(b : float, c : boolean)
			}
			skill S1 implements C1, C2 {
				def myaction1(x : float) { }
				def myaction2(y : float, z : boolean) { }
			}
		'''.parse
		mas.assertError(
			SarlPackage.eINSTANCE.skill,
			IssueCodes::MISSING_METHOD_IMPLEMENTATION,
			"The operation myaction1(int) must be implemented.")
	}

	@Test
	def void incompatibleReturnType_0() {
		val mas = '''
			capacity C1 { }
			capacity C2 { }
			skill S1 implements C1 {
				def myaction(a : int) : int {
					return 0
				}
			}
			skill S2 extends S1 implements C2 {
				def myaction(a : int) : float {
					return 0f
				}
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmOperation,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
			"Incompatible return type between 'float' and 'int' for myaction(int).")
	}

	@Test
	def void incompatibleReturnType_1() {
		val mas = '''
			capacity C1 { }
			capacity C2 { }
			skill S1 implements C1 {
				def myaction(a : int) {
					// void
				}
			}
			skill S2 extends S1 implements C2 {
				def myaction(a : int) : int {
					return 0
				}
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmOperation,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
			"Incompatible return type between 'int' and 'void' for myaction(int).")
	}

	@Test
	def void incompatibleReturnType_2() {
		val mas = '''
			capacity C1 { }
			capacity C2 { }
			skill S1 implements C1 {
				def myaction(a : int) : int {
					return 0
				}
			}
			skill S2 extends S1 implements C2 {
				def myaction(a : int) {
					// void
				}
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmOperation,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
			"Incompatible return type between 'void' and 'int' for myaction(int).")
	}

	@Test
	def void incompatibleReturnType_3() {
		val mas = '''
			capacity C1 {
				def myaction(a : int) : int
			}
			skill S2 implements C1 {
				def myaction(a : int) : float {
					return 0f
				}
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmOperation,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
			"Incompatible return type between 'float' and 'int' for myaction(int).")
	}

	@Test
	def void incompatibleReturnType_4() {
		val mas = '''
			capacity C1 {
				def myaction(a : int) // void
			}
			skill S2 implements C1 {
				def myaction(a : int) : int {
					return 0
				}
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmOperation,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
			"Incompatible return type between 'int' and 'void' for myaction(int).")
	}

	@Test
	def void incompatibleReturnType_5() {
		val mas = '''
			capacity C1 {
				def myaction(a : int) : int
			}
			skill S2 implements C1 {
				def myaction(a : int) {
					// void
				}
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmOperation,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
			"Incompatible return type between 'void' and 'int' for myaction(int).")
	}

	@Test
	def void compatibleReturnType_0() {
		val mas = '''
			capacity C1 { }
			capacity C2 { }
			skill S1 implements C1 {
				def myaction(a : int) : Number {
					return 0.0
				}
			}
			skill S2 extends S1 implements C2 {
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
			capacity C1 { }
			capacity C2 { }
			skill S1 implements C1 {
				def myaction(a : int) : float {
					return 0f
				}
			}
			skill S2 extends S1 implements C2 {
				def myaction(a : int) : float {
					return 0f
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void compatibleReturnType_2() {
		val mas = '''
			capacity C1 {
				def myaction(a : int) : Number
			}
			skill S2 implements C1 {
				def myaction(a : int) : Double {
					return 0.0
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void compatibleReturnType_3() {
		val mas = '''
			capacity C1 {
				def myaction(a : int) : float
			}
			skill S2 implements C1 {
				def myaction(a : int) : float {
					return 0f
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void invalidCapacityTypeForUses() {
		val mas = '''
			capacity C1 {
				def myaction(a : int) : float
			}
			event E1 {
				var abc : int
			}
			behavior B1 {
				uses C1, E1
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH,
			"Invalid type: 'E1'. Only capacities can be used after the keyword 'uses'")
	}

	@Test
	def void invalidCapacityTypeForRequires() {
		val mas = '''
			capacity C1 {
				def myaction(a : int) : float
			}
			event E1 {
				var abc : int
			}
			behavior B1 {
				requires C1, E1
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH,
			"Invalid type: 'E1'. Only capacities can be used after the keyword 'requires'")
	}

	@Test
	def void invalidCapacityExtend() {
		val mas = '''
			agent A1 {
			}
			capacity C1 extends A1 {
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH,
			"Invalid super-type: 'A1'. Only the type 'io.sarl.lang.core.Capacity' and one of its subtypes are allowed for 'C1'")
	}

	@Test
	def void invalidSkillExtend_0() {
		val mas = '''
			capacity C1 {
			}
			agent A1 {
			}
			skill S1 extends A1 implements C1 {
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH,
			"Invalid super-type: 'A1'. Only the type 'io.sarl.lang.core.Skill' and one of its subtypes are allowed for 'S1'")
	}

	@Test
	def void invalidSkillExtend_1() {
		val mas = '''
			behavior B1 {
			}
			skill S1 implements B1 {
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH,
			"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed")
	}

	@Test
	def void inheritance() {
		val mas = '''
			capacity CapTest1 {
				def func1 : int
			}
			capacity CapTest2 extends CapTest1 {
				def func2(a : int)
			}
		'''.parse
		mas.assertNoErrors
	}

}
