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
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.Capacity
import io.sarl.lang.sarl.SarlPackage
import io.sarl.lang.sarl.SarlScript
import org.eclipse.xtext.diagnostics.Diagnostic
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*
import io.sarl.lang.validation.IssueCodes
import org.eclipse.xtext.common.types.TypesPackage

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
	
	var SarlScript mas
	var Iterable<Capacity> knownCapacities
	
	@Before
	def void setUp() {
		mas = '''
			package test
			capacity C {
				def op1
			}
			capacity C2 {
				def op2
			}
			capacity C3 {
				def op3
			}
			capacity C4 {
				def op4
			}
			
			skill S implements C {
				def op1 {
					//do Something
				}
			}
			skill S2 implements C2 {
				def op1 {
					//do Something
				}
			}
			event E {}
			agent A {
				on E {
					val shouldChange = true
					if (shouldChange) {
					C.setSkill(S)	
					}else {
					
					 while(true) {
					 C3.op3 	
					 }
					 C3.read 
					
					}
					
				}
				
				on E {
					C2.op1
					
				}
			}
			agent B {
				on E {
					doSomething(C3)
					C4.op4
				}
			}
		'''.parse
		knownCapacities = mas.elements.filter(Capacity)

	}

	@Test def void testParsedElements() {
		assertEquals(9, mas.elements.size)

	}

	def void testErrorFreeExampleCode() {

		mas.assertNoErrors
	}

	@Test def void testAgentFind() {
		val agents = mas.elements.filter(Agent)
		assertEquals(2, agents.size)
		assertEquals("A", agents.head.name)
	}

	@Test def void testFindCapacityReferences() {
		val agents = mas.elements.filter(Agent)
		assertEquals(2, agents.size)

//		assertEquals(3, agents.head.findCapacityReferences(knownCapacities).size)
//		assertEquals(1, agents.last.findCapacityReferences(knownCapacities).size)

	}

	@Test
	def void testCapacityDirectImplementation() {
		val mas = '''
			import io.sarl.lang.sarl.Capacity
			skill S1 implements Capacity {
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.skill,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to InheritingElement 'Capacity'")
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
			IssueCodes::ACTION_COLLISION,
			"Cannot define many times the same feature in 'C1': myaction(a : int)")
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
			IssueCodes::ACTION_COLLISION,
			"Cannot define many times the same feature in 'S1': myaction(a : int)")
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
			IssueCodes::INVALID_ACTION_NAME,
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
			IssueCodes::INVALID_ACTION_NAME,
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

}
