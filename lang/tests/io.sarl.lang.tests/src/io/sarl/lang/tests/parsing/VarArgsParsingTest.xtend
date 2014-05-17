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
import org.eclipse.xtext.xbase.XbasePackage

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
	def void inAgentActionNoPredecessor() {
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
	def void inAgentActionInvalid() {
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
		mas.assertError(
			XbasePackage::eINSTANCE.XFeatureCall,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmIdentifiableElement 'arg3'")
	}
		
}
