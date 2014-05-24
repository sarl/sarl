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
import org.eclipse.xtext.diagnostics.Diagnostic
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.validation.IssueCodes
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import org.eclipse.xtext.xbase.XbasePackage
import org.eclipse.xtext.common.types.TypesPackage

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class ArgDefaultValueParsingTest {
	@Inject extension ParseHelper<SarlScript>
	@Inject extension ValidationTestHelper

	@Test
	def void inAgentAction_1p() {
		val mas = '''
			agent A1 {
				def myaction(arg : int=4) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_1p_invalid1() {
		val mas = '''
			agent A1 {
				def myaction(arg : int=4*) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			io.sarl.lang.validation.IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER,
			"A default value cannot be declared for the variadic formal parameter")
	}

	@Test
	def void inAgentAction_1p_invalid2() {
		val mas = '''
			agent A1 {
				def myaction(arg : int*=4) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input '*=' expecting ')'")
	}

	@Test
	def void inAgentAction_5p_0() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_5p_1() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int, arg1 : String="abc", arg2 : int, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_5p_2() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_5p_3() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_5p_4() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String="xyz") {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_5p_0_3() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_5p_0_3_4() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String="def") {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_5p_0_2_4() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String="def") {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_5p_0_1_2_3() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int=4, arg1 : String="ghj", arg2 : int=18, arg3 : int=98, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_5p_0_1_2_3_4() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int=4, arg1 : String="ghj", arg2 : int=18, arg3 : int=98, arg4 : String="klm") {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_3p_vararg_2() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int, arg1 : int, arg2 : int=45*) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			io.sarl.lang.validation.IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER,
			"A default value cannot be declared for the variadic formal parameter")
	}

	@Test
	def void inAgentAction_3p_vararg_1() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int, arg1 : int=45, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_3p_vararg_0() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int=45, arg1 : int, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_3p_vararg_0_1() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_1p() {
		val mas = '''
			behavior B1 {
				new(arg : int=4) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_1p_invalid1() {
		val mas = '''
			behavior B1 {
				new(arg : int=4*) {
					System.out.println(arg)
				}
			}
		'''.parse 
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			io.sarl.lang.validation.IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER,
			"A default value cannot be declared for the variadic formal parameter")
	}

	@Test
	def void inBehaviorConstructor_1p_invalid2() {
		val mas = '''
			behavior B1 {
				new(arg : int*=4) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.constructor,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input '*=' expecting ')'")
	}

	@Test
	def void inBehaviorConstructor_5p_0() {
		val mas = '''
			behavior B1 {
				new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_5p_1() {
		val mas = '''
			behavior B1 {
				new(arg0 : int, arg1 : String="abc", arg2 : int, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_5p_2() {
		val mas = '''
			behavior B1 {
				new(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_5p_3() {
		val mas = '''
			behavior B1 {
				new(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_5p_4() {
		val mas = '''
			behavior B1 {
				new(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String="xyz") {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_5p_0_3() {
		val mas = '''
			behavior B1 {
				new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_5p_0_3_4() {
		val mas = '''
			behavior B1 {
				new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String="def") {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_5p_0_2_4() {
		val mas = '''
			behavior B1 {
				new(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String="def") {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_5p_0_1_2_3() {
		val mas = '''
			behavior B1 {
				new(arg0 : int=4, arg1 : String="ghj", arg2 : int=18, arg3 : int=98, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_5p_0_1_2_3_4() {
		val mas = '''
			behavior B1 {
				new(arg0 : int=4, arg1 : String="ghj", arg2 : int=18, arg3 : int=98, arg4 : String="klm") {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_2() {
		val mas = '''
			behavior B1 {
				new(arg0 : int, arg1 : int, arg2 : int=45*) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			io.sarl.lang.validation.IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER,
			"A default value cannot be declared for the variadic formal parameter")
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_1() {
		val mas = '''
			behavior B1 {
				new(arg0 : int, arg1 : int=45, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_0() {
		val mas = '''
			behavior B1 {
				new(arg0 : int=45, arg1 : int, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_0_1() {
		val mas = '''
			behavior B1 {
				new(arg0 : int=45, arg1 : int=56, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_1p() {
		val mas = '''
			capacity C1 {
				def myaction(arg : int=4)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_1p_invalid1() {
		val mas = '''
			capacity C1 {
				def myaction(arg : int=4*)
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			io.sarl.lang.validation.IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER,
			"A default value cannot be declared for the variadic formal parameter")
	}

	@Test
	def void inCapacity_1p_invalid2() {
		val mas = '''
			capacity C1 {
				def myaction(arg : int*=4)
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input '*=' expecting ')'")
	}

	@Test
	def void inCapacity_5p_0() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_5p_1() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int, arg1 : String="abc", arg2 : int, arg3 : int, arg4 : String)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_5p_2() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_5p_3() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_5p_4() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String="xyz")
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_5p_0_3() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_5p_0_3_4() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String="def")
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_5p_0_2_4() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String="def")
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_5p_0_1_2_3() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int=4, arg1 : String="ghj", arg2 : int=18, arg3 : int=98, arg4 : String)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_5p_0_1_2_3_4() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int=4, arg1 : String="ghj", arg2 : int=18, arg3 : int=98, arg4 : String="klm")
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_3p_vararg_2() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int, arg1 : int, arg2 : int=45*)
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			io.sarl.lang.validation.IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER,
			"A default value cannot be declared for the variadic formal parameter")
	}

	@Test
	def void inCapacity_3p_vararg_1() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int, arg1 : int=45, arg2 : int*)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_3p_vararg_0() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int=45, arg1 : int, arg2 : int*)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacity_3p_vararg_0_1() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inSkill_1p() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg : int=4) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_1p_invalid1() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg : int=4*) {}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			io.sarl.lang.validation.IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER,
			"A default value cannot be declared for the variadic formal parameter")
	}

	@Test
	def void inSkill_1p_invalid2() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg : int*=4) {}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input '*=' expecting ')'")
	}

	@Test
	def void inSkill_5p_0() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_5p_1() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int, arg1 : String="abc", arg2 : int, arg3 : int, arg4 : String) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_5p_2() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_5p_3() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_5p_4() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String="xyz") {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_5p_0_3() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_5p_0_3_4() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String="def") {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_5p_0_2_4() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String="def") {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_5p_0_1_2_3() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int=4, arg1 : String="ghj", arg2 : int=18, arg3 : int=98, arg4 : String) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_5p_0_1_2_3_4() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int=4, arg1 : String="ghj", arg2 : int=18, arg3 : int=98, arg4 : String="klm") {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_3p_vararg_2() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int, arg1 : int, arg2 : int=45*) {}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			io.sarl.lang.validation.IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER,
			"A default value cannot be declared for the variadic formal parameter")
	}

	@Test
	def void inSkill_3p_vararg_1() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int, arg1 : int=45, arg2 : int*) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_3p_vararg_0() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int=45, arg1 : int, arg2 : int*) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkill_3p_vararg_0_1() {
		val mas = '''
			capacity C1 {
				def capAction
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void constructorCast_String2int() {
		val mas = '''
			package io.sarl.test
			behavior B1 {
				new(arg0 : int=45, arg1 : int="S", arg2 : int) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			IssueCodes::INVALID_CAST,
			"Cannot cast from String to int")
	}

	@Test
	def void actionCast_String2int() {
		val mas = '''
			behavior B1 {
				def myaction(arg0 : int=45, arg1 : int="S", arg2 : int) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.formalParameter,
			IssueCodes::INVALID_CAST,
			"Cannot cast from String to int")
	}

	@Test
	def void constructorCast_int2double() {
		val mas = '''
			behavior B1 {
				new(arg0 : int=45, arg1 : double=18, arg2 : int) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void actionCast_int2double() {
		val mas = '''
			behavior B1 {
				def myaction(arg0 : int=45, arg1 : double=18, arg2 : int) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void constructorCast_double2int() {
		val mas = '''
			behavior B1 {
				new(arg0 : int=45, arg1 : int=18.0, arg2 : int) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XNumberLiteral,
			IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from double to int")
	}

	@Test
	def void actionCast_double2int() {
		val mas = '''
			behavior B1 {
				def myaction(arg0 : int=45, arg1 : int=18.0, arg2 : int) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertError(
			XbasePackage::eINSTANCE.XNumberLiteral,
			IssueCodes::INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from double to int")
	}

	@Test
	def void overridingCapacitySkill() {
		val mas = '''
			capacity C1 {
				def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*)
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int, arg1 : int, arg2 : int*) {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void multipleActionDefinitionsInBehavior() {
		val mas = '''
			behavior B1 {
				def myaction(arg0 : int, arg1 : int=42, arg2 : int*) {
					System.out.println("valid")
				}
				def myaction(arg0 : int, arg1 : int*) {
					System.out.println("invalid")
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.action,
			io.sarl.lang.validation.IssueCodes::ACTION_ALREADY_DEFINED,
			"Cannot define many times the same feature in 'B1': myaction(arg0 : int, arg1 : int)")
	}

	@Test
	def void multipleActionDefinitionsInAgent() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int, arg1 : int=42, arg2 : int...) {
					System.out.println("valid")
				}
				def myaction(arg0 : int, arg1 : int...) {
					System.out.println("invalid")
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.action,
			io.sarl.lang.validation.IssueCodes::ACTION_ALREADY_DEFINED,
			"Cannot define many times the same feature in 'A1': myaction(arg0 : int, arg1 : int)")
	}

	@Test
	def void multipleActionDefinitionsInSkill() {
		val mas = '''
			capacity C1 {}
			skill S1 implements C1 {
				def myaction(arg0 : int, arg1 : int=42, arg2 : int...) {
					System.out.println("valid")
				}
				def myaction(arg0 : int, arg1 : int...) {
					System.out.println("invalid")
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.action,
			io.sarl.lang.validation.IssueCodes::ACTION_ALREADY_DEFINED,
			"Cannot define many times the same feature in 'S1': myaction(arg0 : int, arg1 : int)")
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
			io.sarl.lang.validation.IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
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
			io.sarl.lang.validation.IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
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
			io.sarl.lang.validation.IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
			"The feature 'C2' is already implemented by the preceding interface 'C3'.")
	}

}
