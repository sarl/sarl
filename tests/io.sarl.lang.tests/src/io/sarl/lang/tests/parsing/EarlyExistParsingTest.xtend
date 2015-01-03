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
import io.sarl.lang.tests.AbstractSARLTestCase
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.XbasePackage
import org.eclipse.xtext.xbase.validation.IssueCodes
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.assertEquals

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
class EarlyExistParsingTest extends AbstractSARLTestCase {

	@Inject extension ValidationTestHelper
	
	@Test
	def void earlyExistFunction_inAction_lastExpression_0() {
		val mas = '''
			agent A1 {
				def caller {
					foo.EarlyExitFunctionDefinitions::killFunction2
				}
			}
		'''.parseWithCP
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void earlyExistFunction_inAction_lastExpression_1() {
		val mas = '''
			agent A1 {
				def caller {
					var inst = new foo.EarlyExitFunctionDefinitions
					inst.killFunction1
				}
			}
		'''.parseWithCP
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void earlyExistFunction_inAction_penultimateExpression_0() {
		val mas = '''
			agent A1 {
				def caller {
					foo.EarlyExitFunctionDefinitions::killFunction2
					println("Hello")
				}
			}
		'''.parseWithCP
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::UNREACHABLE_CODE,
			77,
			16,
			"Unreachable expression")
	}

	@Test
	def void earlyExistFunction_inAction_penultimateExpression_1() {
		val mas = '''
			agent A1 {
				def caller {
					var inst = new foo.EarlyExitFunctionDefinitions
					inst.killFunction1
					println("Hello")
				}
			}
		'''.parseWithCP
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::UNREACHABLE_CODE,
			98,
			16,
			"Unreachable expression")
	}

	@Test
	def void earlyExistFunction_inIf_0() {
		val mas = '''
			agent A1 {
				def caller {
					if (true) {
						foo.EarlyExitFunctionDefinitions::killFunction2
					} else {
						foo.EarlyExitFunctionDefinitions::killFunction2
					}
					println("Hello")
				}
			}
		'''.parseWithCP
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::UNREACHABLE_CODE,
			158,
			16,
			"Unreachable expression")
	}

	@Test
	def void earlyExistFunction_inIf_1() {
		val mas = '''
			agent A1 {
				def caller {
					if (true) {
						foo.EarlyExitFunctionDefinitions::killFunction2
					} else {
						println("Hello")
					}
					println("Bye bye")
				}
			}
		'''.parseWithCP
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

	@Test
	def void earlyExistFunction_inIf_2() {
		val mas = '''
			agent A1 {
				def caller {
					if (true) {
						println("Hello")
					} else {
						foo.EarlyExitFunctionDefinitions::killFunction2
					}
					println("Bye bye")
				}
			}
		'''.parseWithCP
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
	}

}
