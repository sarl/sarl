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
package io.sarl.lang.tests.compilation

import com.google.inject.Inject
import org.eclipse.xtext.junit4.XtextRunner
import org.junit.runner.RunWith
import org.eclipse.xtext.junit4.InjectWith
import io.sarl.lang.SARLInjectorProvider
import org.junit.Test
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper

/**
 * @author $Author: srodriguez$
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
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  @Generated
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			}
		''')
		}
		
}
