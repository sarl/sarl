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
class VarArgsParsingTest {
	@Inject extension ParseHelper<SarlScript>
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

	@Test
	def void multipleActionDefinitionsInBehavior_0() {
		val mas = '''
			behavior B1 {
				def myaction(arg0 : int, arg1 : int...) {
					System.out.println("invalid")
				}
				def myaction {
					System.out.println("invalid")
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void multipleActionDefinitionsInBehavior_1() {
		val mas = '''
			behavior B1 {
				def myaction(arg0 : int, arg1 : int...) {
					System.out.println("invalid")
				}
				def myaction(arg0 : int) {
					System.out.println("invalid")
				}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void multipleActionDefinitionsInBehavior_2() {
		val mas = '''
			behavior B1 {
				def myaction(arg0 : int, arg1 : int...) {
					System.out.println("invalid")
				}
				def myaction(arg0 : int, arg1 : int) {
					System.out.println("invalid")
				}
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.action,
			io.sarl.lang.validation.IssueCodes::ACTION_COLLISION,
			"Cannot define many times the same feature in 'B1': myaction(arg0 : int, arg1 : int)")
	}

}
