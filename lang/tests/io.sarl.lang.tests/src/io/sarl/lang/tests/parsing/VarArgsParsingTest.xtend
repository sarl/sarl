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
 * @author $Author: srodriguez$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class VarArgsParsingTest {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper

	@Test
	def void inAgentAction_singleParam() {
		val mas = '''
			agent A1 {
				def myaction(arg : int...) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction() {
		val mas = '''
			agent A1 {
				def myaction(arg1 : char, arg2 : boolean, arg3 : int...) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inAgentAction_invalid() {
		val mas = '''
			agent A1 {
				def myaction(arg1 : char, arg2 : boolean..., arg3 : int) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input ',' expecting ')'")
	}
		
	@Test
	def void inBehaviorAction_singleParam() {
		val mas = '''
			behavior B1 {
				def myaction(arg : int...) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorAction() {
		val mas = '''
			behavior B1 {
				def myaction(arg1 : char, arg2 : boolean, arg3 : int...) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorAction_invalid() {
		val mas = '''
			behavior B1 {
				def myaction(arg1 : char, arg2 : boolean..., arg3 : int) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input ',' expecting ')'")
	}

	@Test
	def void inSkillAction_singleParam() {
		val mas = '''
			capacity C1 {}
			skill S1 implements C1 {
				def myaction(arg : int...) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkillAction() {
		val mas = '''
			capacity C1 {}
			skill S1 implements C1 {
				def myaction(arg1 : char, arg2 : boolean, arg3 : int...) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkillAction_invalid() {
		val mas = '''
			capacity C1 {}
			skill S1 implements C1 {
				def myaction(arg1 : char, arg2 : boolean..., arg3 : int) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input ',' expecting ')'")
	}

	@Test
	def void inCapacityAction_singleParam() {
		val mas = '''
			capacity C1 {
				def myaction(arg : int...)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacityAction() {
		val mas = '''
			capacity C1 {
				def myaction(arg1 : char, arg2 : boolean, arg3 : int...)
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inCapacityAction_invalid() {
		val mas = '''
			capacity C1 {
				def myaction(arg1 : char, arg2 : boolean..., arg3 : int)
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.actionSignature,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input ',' expecting ')'")
	}

	@Test
	def void inEventConstructor_singleParam() {
		val mas = '''
			event E1 {
				new(arg : int...) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inEventConstructor() {
		val mas = '''
			event E1 {
				new (arg1 : char, arg2 : boolean, arg3 : int...) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inEventConstructor_invalid() {
		val mas = '''
			event E1 {
				new (arg1 : char, arg2 : boolean..., arg3 : int) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.constructor,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input ',' expecting ')'")
	}

	@Test
	def void inSkillConstructor_singleParam() {
		val mas = '''
			capacity C1 {}
			skill S1 implements C1 {
				new(arg : int...) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkillConstructor() {
		val mas = '''
			capacity C1 {}
			skill S1 implements C1 {
				new (arg1 : char, arg2 : boolean, arg3 : int...) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test
	def void inSkillConstructor_invalid() {
		val mas = '''
			capacity C1 {}
			skill S1 implements C1 {
				new (arg1 : char, arg2 : boolean..., arg3 : int) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.constructor,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input ',' expecting ')'")
	}

	@Test
	def void inBehaviorConstructor_singleParam() {
		val mas = '''
			behavior B1 {
				new(arg : int...) {
					System.out.println(arg)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor() {
		val mas = '''
			behavior B1 {
				new (arg1 : char, arg2 : boolean, arg3 : int...) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void inBehaviorConstructor_invalid() {
		val mas = '''
			behavior B1 {
				new (arg1 : char, arg2 : boolean..., arg3 : int) {
					System.out.println(arg3)
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.constructor,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"mismatched input ',' expecting ')'")
	}

}
