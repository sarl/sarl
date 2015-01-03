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
import org.junit.Test
import org.junit.runner.RunWith
import io.sarl.lang.sarl.SarlPackage
import org.eclipse.xtext.diagnostics.Diagnostic
import org.eclipse.xtext.xbase.XbasePackage
import org.eclipse.xtext.xbase.validation.IssueCodes
import io.sarl.tests.api.AbstractSarlTest

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class DataTypeParsingTest extends AbstractSarlTest {
	@Inject extension ParseHelper<SarlScript>
	@Inject extension ValidationTestHelper

	@Test
	def void intToDouble() {
		val mas = '''
			agent A1 {
				var internalTime : Double = 0
			}
		'''.parse
		mas.assertError(
			XbasePackage.eINSTANCE.XNumberLiteral,
			IssueCodes::INCOMPATIBLE_TYPES,
			"cannot convert from int to Double")
	}
	
	@Test
	def void doubleToDouble_1() {
		val mas = '''
			agent A1 {
				var internalTime : Double = 0.0
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test
	def void doubleToDouble_2() {
		val mas = '''
			agent A1 {
				var internalTime : Double = 0.
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.agent,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"extraneous input '.' expecting '}'")
	}

	@Test
	def void doubleToDouble_3() {
		val mas = '''
			agent A1 {
				var internalTime : Double = .0
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"no viable alternative at input '.'")
	}

	@Test
	def void doubleToDouble_4() {
		val mas = '''
			agent A1 {
				var internalTime : Double = 0d
			}
		'''.parse
		mas.assertNoErrors
	}

}
