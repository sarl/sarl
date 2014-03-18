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
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			}
		''')
		}
		
		@Test
		def eventCompile(){
			'''event Factorial {
				var number : Integer
				var value : Integer
			}'''.assertCompilesTo(
				'''
import io.sarl.lang.core.Event;

@SuppressWarnings("all")
public class Factorial extends Event {
  private Integer number;
  
  public Integer getNumber() {
    return this.number;
  }
  
  public void setNumber(final Integer number) {
    this.number = number;
  }
  
  private Integer value;
  
  public Integer getValue() {
    return this.value;
  }
  
  public void setValue(final Integer value) {
    this.value = value;
  }
  
  /**
   * Returns a String representation of the Event Factorial
   */
  public String toString() {
    StringBuilder result = new StringBuilder();
    result.append("Factorial[");
    result.append("number  = ").append(this.number);
    result.append("value  = ").append(this.value);
    result.append("]");
    return result.toString();
  }
}
'''
			)
		}
}
