/*
 * $Id$
 * 
 * SARL is an open-source multiagent language.
 * More details on &lt;http://www.sarl.io&gt;
 * Copyright (C) 2014 SARL Core Developers
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
 * @author $Author: Sebastian Rodriguez$
 * @version $Name$ $Revision$ $Date$
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
