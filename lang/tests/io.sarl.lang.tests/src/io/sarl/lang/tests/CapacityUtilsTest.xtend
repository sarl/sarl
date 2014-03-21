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
package io.sarl.lang.tests

import com.google.inject.Inject
import io.sarl.lang.SARLInjectorProvider
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.Capacity
import io.sarl.lang.sarl.Model
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

/**
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class CapacityUtilsTest {
	@Inject extension ParseHelper<Model>

	@Inject extension ValidationTestHelper
	
	var Model mas
	var Iterable<Capacity> knownCapacities
	
	@Inject extension IQualifiedNameProvider

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


}
