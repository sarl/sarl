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
package io.sarl.lang.tests

import org.eclipse.xtext.junit4.XtextRunner
import org.junit.runner.RunWith
import org.eclipse.xtext.junit4.InjectWith
import com.google.inject.Inject
import org.junit.Test
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper

import static org.junit.Assert.*
import io.sarl.lang.sarl.Model
import org.eclipse.xtext.junit4.IInjectorProvider
import io.sarl.lang.SARLInjectorProvider
import io.sarl.lang.sarl.Agent
import org.eclipse.xtext.xbase.XbasePackage
import org.eclipse.xtext.diagnostics.Diagnostic
import io.sarl.lang.sarl.SarlPackage
import org.junit.Ignore

/**
 * @author $Author: Sebastian Rodriguez$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class AgentParsingTest {
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper

	@Test def void testParse() {
		val mas = '''
			package test
			agent A1 {}
			agent A2 {}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test def parseBehaviorDeclaration() {
		val mas = '''
			event E {}
			agent A1 {
				on E {}
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(2, mas.elements.size)
	}

	@Test def parseBehaviorWithGuard() {
		val mas = '''
			event E {}
			agent A1 {
				on E [ occurrence.source != null] {}
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test def parseAgentWithAttributes() {
		val mas = '''
			agent A1 {
				var name : String = "Hello"
				var number : Integer
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test def parseAgentWithConstAttributes() {
		val mas = '''
			
			agent A1 {
				val name : String = "Hello"
				var number : Integer
			}
		'''.parse
		mas.assertNoErrors
	}

	@Test def eventsMustBeDeclared() {
		val mas = '''
			
			agent A1 {
				on E  {}
			}
		'''.parse

		mas.assertError(SarlPackage::eINSTANCE.behaviorUnit, Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to Event 'E'.")
	}

	@Test @Ignore("not ready yet")
	def constAttributesMustHaveIniatlizer() {
		val mas = '''
			
			agent A1 {
				val name : String = "Hello"
				val number : Integer
			}
		'''.parse
		mas.assertError(
			SarlPackage::eINSTANCE.attribute,
			Diagnostic::SYNTAX_DIAGNOSTIC,
			"Constant attribute 'number' must be initialized ."
		)
	}

	@Test
	def capacityMustBeDeclaredBeforeUse() {
		val mas = '''
			agent A1 {
				uses MyCap
			}
		'''.parse		
		mas.assertError(
			SarlPackage::eINSTANCE.capacityUses,
			Diagnostic::LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to Capacity 'MyCap'."
		)
	}

	@Test
	def agentCanDeclareCapacityUses() {
		val mas = '''
			capacity MyCap {
				def my_operation
			}
			
			agent A1 {
				uses MyCap
			}
		'''.parse
		mas.assertNoErrors

	}
	

}
