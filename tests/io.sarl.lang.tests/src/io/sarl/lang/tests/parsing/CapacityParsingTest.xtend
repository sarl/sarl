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
import org.eclipse.emf.ecore.EClass
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.TypesPackage
import org.eclipse.xtext.diagnostics.Severity
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.validation.Issue
import org.junit.Test
import org.junit.runner.RunWith
import java.util.List
import org.junit.Assert
import io.sarl.tests.api.AbstractSarlTest

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class CapacityParsingTest extends AbstractSarlTest {
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
			IssueCodes::INVALID_IMPLEMENTED_TYPE,
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
			SarlPackage::eINSTANCE.action,
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
			SarlPackage::eINSTANCE.action,
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
			TypesPackage::eINSTANCE.jvmConstructor,
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
			SarlPackage::eINSTANCE.attribute,
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
			SarlPackage::eINSTANCE.skill,
			IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
			"The feature 'C1' is already implemented by the super-type 'S1'.")
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
		mas.assertError(
			SarlPackage::eINSTANCE.skill,
			IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
			"Duplicate implemented feature 'C2'")
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
		mas.assertError(
			SarlPackage::eINSTANCE.skill,
			IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
			"Duplicate implemented feature 'C2'")
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
			SarlPackage::eINSTANCE.action,
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
			SarlPackage::eINSTANCE.action,
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
			SarlPackage::eINSTANCE.action,
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
			SarlPackage::eINSTANCE.action,
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
			SarlPackage::eINSTANCE.action,
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
			SarlPackage::eINSTANCE.action,
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
			IssueCodes::INVALID_CAPACITY_TYPE,
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
			IssueCodes::INVALID_CAPACITY_TYPE,
			"Invalid type: 'E1'. Only capacities can be used after the keyword 'requires'")
	}

	@Test
	def void invalidCapacityExtend_0() {
		val mas = '''
			agent A1 {
			}
			capacity C1 extends A1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Invalid supertype. Expecting: interface")
	}

	@Test
	def void invalidCapacityExtend_1() {
		val mas = '''
			agent A1 {
			}
			capacity C1 {
			}
			capacity C2 extends A1, C1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Invalid supertype. Expecting: interface")
	}

	@Test
	def void invalidCapacityExtend_2() {
		val mas = '''
			agent A1 {
			}
			capacity C1 {
			}
			capacity C2 extends C1, A1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Invalid supertype. Expecting: interface")
	}

	@Test
	def void invalidCapacityExtend_3() {
		val mas = '''
			agent A1 {
			}
			capacity C1 {
			}
			capacity C2 {
			}
			capacity C3 extends A1, C1, C2 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Invalid supertype. Expecting: interface")
	}

	@Test
	def void invalidCapacityExtend_4() {
		val mas = '''
			agent A1 {
			}
			capacity C1 {
			}
			capacity C2 {
			}
			capacity C3 extends C1, A1, C2 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Invalid supertype. Expecting: interface")
	}

	@Test
	def void invalidCapacityExtend_5() {
		val mas = '''
			agent A1 {
			}
			capacity C1 {
			}
			capacity C2 {
			}
			capacity C3 extends C1, C2, A1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Invalid supertype. Expecting: interface")
	}

	@Test
	def void invalidCapacityExtend_6() {
		val mas = '''
			capacity C1 extends java.lang.Cloneable {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Supertype must be of type 'io.sarl.lang.core.Capacity'")
	}

	@Test
	def void invalidCapacityExtend_7() {
		val mas = '''
			capacity C1 {
			}
			capacity C2 extends java.lang.Cloneable, C1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Supertype must be of type 'io.sarl.lang.core.Capacity'")
	}

	@Test
	def void invalidCapacityExtend_8() {
		val mas = '''
			capacity C1 {
			}
			capacity C2 extends C1, java.lang.Cloneable {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Supertype must be of type 'io.sarl.lang.core.Capacity'")
	}

	@Test
	def void invalidCapacityExtend_9() {
		val mas = '''
			capacity C1 {
			}
			capacity C2 {
			}
			capacity C3 extends java.lang.Cloneable, C1, C2 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Supertype must be of type 'io.sarl.lang.core.Capacity'")
	}

	@Test
	def void invalidCapacityExtend_10() {
		val mas = '''
			capacity C1 {
			}
			capacity C2 {
			}
			capacity C3 extends C1, java.lang.Cloneable, C2 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Supertype must be of type 'io.sarl.lang.core.Capacity'")
	}

	@Test
	def void invalidCapacityExtend_11() {
		val mas = '''
			capacity C1 {
			}
			capacity C2 {
			}
			capacity C3 extends C1, C2, java.lang.Cloneable {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Supertype must be of type 'io.sarl.lang.core.Capacity'")
	}

	@Test
	def void invalidCapacityExtend_12() {
		val mas = '''
			capacity C1 extends C1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'C1' is inconsistent")
	}

	@Test
	def void invalidCapacityExtend_13() {
		val mas = '''
			capacity C1 extends C2 {
			}
			capacity C2 extends C1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'C1' is inconsistent")
	}

	@Test
	def void invalidCapacityExtend_14() {
		val mas = '''
			capacity C1 extends C3 {
			}
			capacity C2 extends C1 {
			}
			capacity C3 extends C2 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'C1' is inconsistent")
	}

	@Test
	def void invalidCapacityExtend_15() {
		val mas = '''
			capacity C1 { }
			capacity C2 { }
			capacity C3 extends C1, C2, C3 { }
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'C3' is inconsistent")
	}

	@Test
	def void invalidCapacityExtend_16() {
		val mas = '''
			capacity C1 { }
			capacity C2 { }
			capacity C3 extends C1, C3, C2 { }
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'C3' is inconsistent")
	}

	@Test
	def void invalidCapacityExtend_17() {
		val mas = '''
			capacity C1 { }
			capacity C2 { }
			capacity C3 extends C3, C1, C3 { }
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'C3' is inconsistent")
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
			SarlPackage::eINSTANCE.skill,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Supertype must be of type 'io.sarl.lang.core.Skill'")
	}

	@Test
	def void invalidSkillExtend_1() {
		val mas = '''
			capacity C1 {
			}
			capacity C2 {
			}
			skill S1 extends C1 implements C2 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.skill,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Invalid supertype. Expecting: class")
	}

	@Test
	def void invalidSkillImplement_0() {
		val mas = '''
			behavior B1 {
			}
			skill S1 implements B1 {
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			IssueCodes::INVALID_IMPLEMENTED_TYPE,
			"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed")
	}

	@Test
	def void invalidSkillImplement_1() {
		val mas = '''
			behavior B1 {
			}
			capacity C1 {
			}
			capacity C2 {
			}
			skill S1 implements B1, C1, C2 {
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			IssueCodes::INVALID_IMPLEMENTED_TYPE,
			"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed")
	}

	@Test
	def void invalidSkillImplement_2() {
		val mas = '''
			behavior B1 {
			}
			capacity C1 {
			}
			capacity C2 {
			}
			skill S1 implements C1, B1, C2 {
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			IssueCodes::INVALID_IMPLEMENTED_TYPE,
			"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed")
	}

	@Test
	def void invalidSkillImplement_3() {
		val mas = '''
			behavior B1 {
			}
			capacity C1 {
			}
			capacity C2 {
			}
			skill S1 implements C1, C2, B1 {
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			IssueCodes::INVALID_IMPLEMENTED_TYPE,
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

	@Test
	def void emptyCapacity() {
		val mas = '''
			capacity C1 { }
		'''.parse
		mas.assertWarning(
			SarlPackage::eINSTANCE.capacity,
			IssueCodes::DISCOURAGED_CAPACITY_DEFINITION,
			"Discouraged capacity definition. A capacity without actions defined inside is not useful since it cannot be called by an agent or a behavior.")
	}

	@Test
	def void skillImplementCapacity() {
		val mas = '''
			capacity C1 {
				def myaction
			}
			skill S1 implements C1 {
				def myaction { }
			}
		'''.parse
		mas.assertNoIssues
	}

	@Test
	def void skillExtendSkill() {
		val mas = '''
			capacity C1 {
				def myaction
			}
			skill S1 implements C1 {
				def myaction { }
			}
			skill S2 extends S1 {
				def myaction { }
			}
		'''.parse
		mas.assertNoIssues
	}

	@Test
	def void skillExtendSkillImplementCapacity() {
		val mas = '''
			capacity C1 {
				def myaction
			}
			capacity C2 {
				def myaction2
			}
			skill S1 implements C1 {
				def myaction { }
			}
			skill S2 extends S1 implements C2 {
				def myaction { }
				def myaction2 { }
			}
		'''.parse
		mas.assertNoIssues
	}

	@Test
	def void skillNoExtendSkillNoImplementCapacity() {
		val mas = '''
			skill S1 {
				def myaction { }
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.skill,
			org.eclipse.xtext.xbase.validation.IssueCodes::MISSING_TYPE,
			"Missing implemented type 'io.sarl.lang.core.Capacity' for 'S1'")
	}
	
	/**
	 * FIXME: Issue #260. Move to Xbase.
	 */
	private def StringBuilder getIssuesAsString(EObject model, Iterable<Issue> issues, StringBuilder result) {
		for(issue : issues) {
			var uri = issue.uriToProblem
			result.append(issue.severity)
			result.append(" (")
			result.append(issue.code)
			result.append(") '")
			result.append(issue.message)
			result.append("'")
			if (uri !== null) {
				var eObject = model.eResource.resourceSet.getEObject(uri, true)
				result.append(" on ")
				result.append(eObject.eClass.name)
			}
			result.append("\n")
		}
		return result
	}

	/**
	 * FIXME: Issue #260. Move to Xbase.
	 */
	private def boolean isIssueMessage(Issue issue, String... messageParts) {
		for (String messagePart : messageParts) {
			if (!issue.message.toLowerCase.contains(messagePart.toLowerCase)) {
				return false
			}
		}
		return true
	}
	
	/**
	 * FIXME: Issue #260. Move to Xbase.
	 */
	private def void assertIssue(List<Issue> issues, Severity severity, EObject model,
		EClass objectType, String code, String... messageParts) {
		var iterator = issues.iterator
		while (iterator.hasNext) {
			var issue = iterator.next
			if (issue.code == code && issue.severity == severity) {
				var object = model.eResource().resourceSet.getEObject(issue.uriToProblem, true)
				if (objectType.isInstance(object)) {
					if (issue.isIssueMessage(messageParts)) {
						iterator.remove
						return;
					}
				}
			}
		}
		var message = new StringBuilder("Expected " + severity + " '" + code + "' on " + objectType.name + " but got\n")
		getIssuesAsString(model, issues, message)
		Assert::fail(message.toString)
	}
	
	/**
	 * FIXME: Issue #260. Move to Xbase.
	 */
	def void assertWarning(List<Issue> issues, EObject model, EClass objectType, String code,
			String... messageParts) {
		assertIssue(issues, Severity::WARNING, model, objectType, code, messageParts)
	}
	
	/**
	 * FIXME: Issue #260. Move to Xbase.
	 */
	def void assertNoMoreIssues(List<Issue> issues, EObject model) {
		if (!issues.isEmpty) {
			var message = new StringBuilder("Expecting no issue but got\n")
			getIssuesAsString(model, issues, message)
			Assert::fail(message.toString)
		}
	}

	@Test
	def void agentUnsuedCapacity_0() {
		val mas = '''
			capacity C1 {
				def myfct
			}
			capacity C2 {
				def myfct2
			}
			agent A1 {
				uses C2, C1
				def myaction {
					myfct2
				}
			}
		'''.parse
		var issues = mas.validate
		issues.assertWarning(
			mas,
			SarlPackage::eINSTANCE.capacityUses,
			IssueCodes::UNUSED_AGENT_CAPACITY,
			"The capacity 'C1' is not used")
		issues.assertNoMoreIssues(mas)
	}

	@Test
	def void agentUnsuedCapacity_1() {
		val mas = '''
			capacity C1 {
				def myfct
			}
			capacity C2 {
				def myfct2
			}
			agent A1 {
				uses C2, C1
				def myaction {
				}
			}
		'''.parse
		var issues = mas.validate
		issues.assertWarning(
			mas,
			SarlPackage::eINSTANCE.capacityUses,
			IssueCodes::UNUSED_AGENT_CAPACITY,
			"The capacity 'C1' is not used")
		issues.assertWarning(
			mas,
			SarlPackage::eINSTANCE.capacityUses,
			IssueCodes::UNUSED_AGENT_CAPACITY,
			"The capacity 'C2' is not used")
		issues.assertNoMoreIssues(mas)
	}

	@Test
	def void agentUnsuedCapacity_2() {
		val mas = '''
			capacity C1 {
				def myfct
			}
			capacity C2 {
				def myfct2
			}
			agent A1 {
				uses C2, C1
				def myaction {
					myfct
					myfct2
				}
			}
		'''.parse
		mas.assertNoIssues
	}

	@Test
	def multipleCapacityUses_0() {
		val mas = '''
			capacity C1 {}
			capacity C2 {}
			capacity C3 { def testFct }
			skill S1 implements C3 {
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
			capacity C3 { def testFct }
			skill S1 implements C3 {
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
