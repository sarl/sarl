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
import org.junit.Assert

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
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   */
			  @Generated
			  public A1(final UUID parentID) {
			    super(parentID, null);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
			   */
			  @Generated
			  public A1(final UUID parentID, final UUID agentID) {
			    super(parentID, agentID);
			  }
			}
		''')
	}
		
	@Test
	def trueGuardBehaviorUnit() {
		val expectedE1 = '''
		import io.sarl.lang.annotation.Generated;
		import io.sarl.lang.core.Address;
		import io.sarl.lang.core.Event;
		
		@SuppressWarnings("all")
		public class E1 extends Event {
		  /**
		   * Construct an event. The source of the event is unknown.
		   */
		  @Generated
		  public E1() {
		    super();
		  }
		  
		  /**
		   * Construct an event.
		   * @param source - address of the agent that is emitting this event.
		   */
		  @Generated
		  public E1(final Address source) {
		    super(source);
		  }
		  
		  @Generated
		  private final static long serialVersionUID = 588368462L;
		}
		'''
		val expectedA1 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import io.sarl.lang.core.Percept;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   */
			  @Generated
			  public A1(final UUID parentID) {
			    super(parentID, null);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
			   */
			  @Generated
			  public A1(final UUID parentID, final UUID agentID) {
			    super(parentID, agentID);
			  }
			  
			  @Percept
			  public void _handle_E1_1(final E1 occurrence) {
			    System.out.println(occurrence);
			  }
			}
		'''
		
		'''
			event E1
			agent A1 {
				on E1 [ true ] {
					System.out.println(occurrence)
				}
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedE1,r.getGeneratedCode("E1"))
			Assert.assertEquals(expectedA1,r.getGeneratedCode("A1"))
		]);
	}

	@Test
	def falseGuardBehaviorUnit() {
		val expectedE1 = '''
		import io.sarl.lang.annotation.Generated;
		import io.sarl.lang.core.Address;
		import io.sarl.lang.core.Event;
		
		@SuppressWarnings("all")
		public class E1 extends Event {
		  /**
		   * Construct an event. The source of the event is unknown.
		   */
		  @Generated
		  public E1() {
		    super();
		  }
		  
		  /**
		   * Construct an event.
		   * @param source - address of the agent that is emitting this event.
		   */
		  @Generated
		  public E1(final Address source) {
		    super(source);
		  }
		  
		  @Generated
		  private final static long serialVersionUID = 588368462L;
		}
		'''
		val expectedA1 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   */
			  @Generated
			  public A1(final UUID parentID) {
			    super(parentID, null);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
			   */
			  @Generated
			  public A1(final UUID parentID, final UUID agentID) {
			    super(parentID, agentID);
			  }
			}
		'''
		
		'''
			event E1
			agent A1 {
				on E1 [ false ] {
					System.out.println(occurrence)
				}
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedE1,r.getGeneratedCode("E1"))
			Assert.assertEquals(expectedA1,r.getGeneratedCode("A1"))
		]);
	}

}
