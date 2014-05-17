/*
 * $Id$
 * 
 * SARL is an open-source multiagent language.
 * More details on &lt;http://www.sarl.io&gt;
 * Copyright (C) 2013 SARL Core Developers
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see &lt;http://www.gnu.org/licenses/&gt;.
 */
package io.sarl.lang.tests.parsing

import com.google.inject.Inject
import io.sarl.lang.SARLInjectorProvider
import io.sarl.lang.sarl.Model
import io.sarl.lang.sarl.SarlPackage
import org.eclipse.xtext.diagnostics.Diagnostic
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
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
class ArgDefaultValueParsingTest {
	@Inject extension ParseHelper<Model>
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
				def myaction(arg : int=4...) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_1p_invalid2() {
		val mas = '''
			agent A1 {
				def myaction(arg : int...=4) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input '=' expecting ')'")
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
				def myaction(arg0 : int, arg1 : int, arg2 : int=45...) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_3p_vararg_1() {
		val mas = '''
			agent A1 {
				def myaction(arg0 : int, arg1 : int=45, arg2 : int...) {
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
				def myaction(arg0 : int=45, arg1 : int, arg2 : int...) {
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
				def myaction(arg0 : int=45, arg1 : int=56, arg2 : int...) {
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
				new(arg : int=4...) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_1p_invalid2() {
		val mas = '''
			behavior B1 {
				new(arg : int...=4) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.constructor,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input '=' expecting ')'")
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
				new(arg0 : int, arg1 : int, arg2 : int=45...) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_1() {
		val mas = '''
			behavior B1 {
				new(arg0 : int, arg1 : int=45, arg2 : int...) {
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
				new(arg0 : int=45, arg1 : int, arg2 : int...) {
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
				new(arg0 : int=45, arg1 : int=56, arg2 : int...) {
					System.out.println(arg0)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

}
