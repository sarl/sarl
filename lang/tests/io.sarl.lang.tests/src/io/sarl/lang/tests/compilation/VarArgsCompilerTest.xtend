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
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class VarArgsCompilerTest {
	@Inject extension CompilationTestHelper

	@Test
	def inAgentActionNoPredecessor() {
		'''
			agent A1 {
				def myaction(arg : int...) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final int... arg) {
			    System.out.println(arg);
			  }
			}
		''')
	}
		
	@Test
	def inAgentAction() {
		'''
			agent A1 {
				def myaction(arg1 : char, arg2 : boolean, arg3 : int...) {
					System.out.println(arg3)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final char arg1, final boolean arg2, final int... arg3) {
			    System.out.println(arg3);
			  }
			}
		''')
	}

}
