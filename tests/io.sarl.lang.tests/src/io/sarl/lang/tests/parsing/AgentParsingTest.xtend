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
import org.eclipse.xtext.diagnostics.Diagnostic
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.XbasePackage
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import org.junit.Ignore
import io.sarl.tests.api.AbstractSarlTest

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class AgentParsingTest extends AbstractSarlTest {
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
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmType 'E'.")
	}

	@Test
	def capacityMustBeDeclaredBeforeUse() {
		val mas = '''
			agent A1 {
				uses MyCap
			}
		'''.parse		
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmType 'MyCap'"
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
			IssueCodes::DUPLICATE_METHOD,
			"Duplicate action in 'A1': myaction(a : int)")
	}

	@Test
	def void multipleVariableDefinition() {
		val mas = '''
			agent A1 {
				var myfield : int
				var myfield1 : String
				var myfield : double
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::DUPLICATE_FIELD,
			"Duplicate field in 'A1': myfield")
	}

	@Test
	def void multipleValueDefinition() {
		val mas = '''
			agent A1 {
				val myfield : int = 4
				val myfield1 : String = ""
				val myfield : double = 5
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			IssueCodes::DUPLICATE_FIELD,
			"Duplicate field in 'A1': myfield")
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
			SarlPackage::eINSTANCE.action,
			IssueCodes::INVALID_MEMBER_NAME,
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
			IssueCodes::INVALID_MEMBER_NAME,
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
			IssueCodes::INVALID_MEMBER_NAME,
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
			TypesPackage::eINSTANCE.jvmConstructor,
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

	@Test
	def void fieldNameShadowing() {
		val mas = '''
			agent A1 {
				val field1 : int = 5
				def myaction(a : int) { }
			}
			agent A2 extends A1 {
				val field1 : int = 5
				def myaction(a : int) { }
			}
		'''.parse
		mas.assertWarning(
			SarlPackage::eINSTANCE.attribute,
			org.eclipse.xtext.xbase.validation.IssueCodes::VARIABLE_NAME_SHADOWING,
			"The field 'field1' in 'A2' is hidding the inherited field 'A1.field1'.")
	}

	@Test
	def void incompatibleReturnType_0() {
		val mas = '''
			agent A1 {
				def myaction(a : int) : int {
					return 0
				}
			}
			agent A2 extends A1 {
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
			agent A1 {
				def myaction(a : int) {
					// void
				}
			}
			agent A2 extends A1 {
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
			agent A1 {
				def myaction(a : int) : int {
					return 0
				}
			}
			agent A2 extends A1 {
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
			agent A1 {
				def myaction(a : int) : Number {
					return 0.0
				}
			}
			agent A2 extends A1 {
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
			agent A1 {
				def myaction(a : int) : float {
					return 0f
				}
			}
			agent A2 extends A1 {
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
			agent A1 extends C1 {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.agent,
			IssueCodes::INVALID_EXTENDED_TYPE,
			"Invalid supertype. Expecting: class")
	}

	@Test
	@Ignore
	def void invalidExtend_1() {
		val mas = '''
			agent A1 extends foo.MockFinalAgent {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.agent,
			IssueCodes::OVERRIDDEN_FINAL_TYPE,
			"Attempt to override final class")
	}

	@Test
	def void invalidFires() {
		val mas = '''
			event E1
			behavior B1 { }
			agent A1 {
				def myaction1 fires E1, B1 { }
			}
		'''.parse
		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			IssueCodes::INVALID_FIRING_EVENT_TYPE,
			"Invalid type: 'B1'. Only events can be used after the keyword 'fires'")
	}

	@Test
	def invalidBehaviorUnit_EventType() {
		val mas = '''
			agent A1 {
				on String {}
			}
		'''.parse

		mas.assertError(
			TypesPackage::eINSTANCE.jvmParameterizedTypeReference,
			org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH,
			"Invalid type: 'java.lang.String'. Only events can be used after the keyword 'on'")
	}

	@Test
	def invalidBehaviorUnit_GuardType() {
		val mas = '''
			event E1
			agent A1 {
				on E1 [ "hello" ] {}
			}
		'''.parse

		mas.assertError(
			XbasePackage::eINSTANCE.XExpression,
			org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from String to boolean")
	}

	@Test
	def trueGuardBehaviorUnit() {
		val mas = '''
			event E1
			agent A1 {
				on E1 [ true ] {}
			}
		'''.parse

		mas.assertWarning(
			XbasePackage::eINSTANCE.XBooleanLiteral,
			IssueCodes::DISCOURAGED_BOOLEAN_EXPRESSION,
			"Discouraged boolean value. The guard is always true.")
	}

	@Test
	def falseGuardBehaviorUnit() {
		val mas = '''
			event E1
			agent A1 {
				on E1 [ false ] {}
			}
		'''.parse

		mas.assertWarning(
			SarlPackage::eINSTANCE.behaviorUnit,
			IssueCodes::UNREACHABLE_BEHAVIOR_UNIT,
			"Dead code. The guard is always false.")
	}

	@Test
	def recursiveAgentExtension_0() {
		val mas = '''
			agent A1 extends A1 {
			}
		'''.parse

		mas.assertError(
			SarlPackage::eINSTANCE.agent,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'A1' is inconsistent")
	}

	@Test
	def recursiveAgentExtension_1() {
		val mas = '''
			agent A1 extends A2 {
			}
			agent A2 extends A1 {
			}
		'''.parse

		mas.assertError(
			SarlPackage::eINSTANCE.agent,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'A1' is inconsistent")
	}

	@Test
	def recursiveAgentExtension_2() {
		val mas = '''
			agent A1 extends A3 {
			}
			agent A2 extends A1 {
			}
			agent A3 extends A2 {
			}
		'''.parse

		mas.assertError(
			SarlPackage::eINSTANCE.agent,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'A1' is inconsistent")
	}

	@Test
	def sequenceAgentDefinition_invalid() {
		val mas = '''
			agent A1 extends A2 {
			}
			agent A2 extends A3 {
			}
			agent A3 {
			}
		'''.parse

		mas.assertError(
			SarlPackage::eINSTANCE.agent,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'A1' is inconsistent")
		mas.assertError(
			SarlPackage::eINSTANCE.agent,
			IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
			"The inheritance hierarchy of 'A2' is inconsistent")
	}

	@Test
	def sequenceAgentDefinition_valid() {
		val mas = '''
			agent A3 {
			}
			agent A2 extends A3 {
			}
			agent A1 extends A2 {
			}
		'''.parse

		mas.assertNoErrors
	}
	
	@Test
	def multipleCapacityUses_0() {
		val mas = '''
			capacity C1 {}
			capacity C2 {}
			agent A1 {
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
			agent A1 {
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
