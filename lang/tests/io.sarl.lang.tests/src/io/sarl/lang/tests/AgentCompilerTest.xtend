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
import org.eclipse.xtext.junit4.XtextRunner
import org.junit.runner.RunWith
import org.eclipse.xtext.junit4.InjectWith
import io.sarl.lang.SARLInjectorProvider
import org.junit.Test
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper

/**
 * @author $Author: Sebastian Rodriguez$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class AgentCompilerTest {
	@Inject extension CompilationTestHelper

	@Test
	def basicAgentCompile() {
		'''
			agent A1 {
				
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			}
		''')
		}
}
