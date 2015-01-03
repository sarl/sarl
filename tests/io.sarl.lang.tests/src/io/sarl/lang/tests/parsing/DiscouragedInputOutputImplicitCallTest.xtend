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
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.XbasePackage
import org.eclipse.xtext.xbase.validation.IssueCodes
import org.junit.Test
import org.junit.runner.RunWith
import io.sarl.tests.api.AbstractSarlTest

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class DiscouragedInputOutputImplicitCallTest extends AbstractSarlTest {

	@Inject
	extension ParseHelper<SarlScript>
	@Inject
	extension ValidationTestHelper

	@Test
	def println_agent_action() {
		val mas = '''
			agent A1 {
				def test {
					println("")
				}
			}
		'''.parse
		mas.assertWarning(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::DISCOURAGED_REFERENCE,
			"Discouraged feature use. The agent's logging capacity should be used in place of the implicitly imported input/output functions")
	}

	@Test
	def println_agent_behaviorUnit() {
		val mas = '''
			event E1 { }
			agent A1 {
				on E1 {
					println("")
				}
			}
		'''.parse
		mas.assertWarning(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::DISCOURAGED_REFERENCE,
			"Discouraged feature use. The agent's logging capacity should be used in place of the implicitly imported input/output functions")
	}

	@Test
	def println_behavior_action() {
		val mas = '''
			behavior B1 {
				def test {
					println("")
				}
			}
		'''.parse
		mas.assertWarning(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::DISCOURAGED_REFERENCE,
			"Discouraged feature use. The agent's logging capacity should be used in place of the implicitly imported input/output functions")
	}

	@Test
	def println_behavior_behaviorUnit() {
		val mas = '''
			event E1 { }
			behavior B1 {
				on E1 {
					println("")
				}
			}
		'''.parse
		mas.assertWarning(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::DISCOURAGED_REFERENCE,
			"Discouraged feature use. The agent's logging capacity should be used in place of the implicitly imported input/output functions")
	}

	@Test
	def println_behavior_constructor() {
		val mas = '''
			import io.sarl.lang.core.Agent
			behavior B1 {
				new (a : Agent) {
					super(a)
					println("")
				}
			}
		'''.parse
		mas.assertWarning(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::DISCOURAGED_REFERENCE,
			"Discouraged feature use. The agent's logging capacity should be used in place of the implicitly imported input/output functions")
	}

	@Test
	def println_skill_action() {
		val mas = '''
			capacity C1 { }
			skill S1 implements C1 {
				def test {
					println("")
				}
			}
		'''.parse
		mas.assertWarning(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::DISCOURAGED_REFERENCE,
			"Discouraged feature use. The agent's logging capacity should be used in place of the implicitly imported input/output functions")
	}

	@Test
	def println_skill_constructor() {
		val mas = '''
			import io.sarl.lang.core.Agent
			capacity C1 { }
			skill S1 implements C1 {
				new (a : Agent) {
					super(a)
					println("")
				}
			}
		'''.parse
		mas.assertWarning(
			XbasePackage::eINSTANCE.XFeatureCall,
			IssueCodes::DISCOURAGED_REFERENCE,
			"Discouraged feature use. The agent's logging capacity should be used in place of the implicitly imported input/output functions")
	}

}
