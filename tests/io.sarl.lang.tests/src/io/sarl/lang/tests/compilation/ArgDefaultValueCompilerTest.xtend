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
import io.sarl.lang.SARLInjectorProvider
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import org.junit.Assert
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
class ArgDefaultValueCompilerTest extends AbstractSarlTest {
	@Inject extension CompilationTestHelper

	@Test
	def void inAgentAction_1p_int() {
		'''
			agent A1 {
				def myaction(arg : int=4) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4;
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final int arg) {
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("int")
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			  
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
	def void inAgentAction_1p_float() {
		'''
			agent A1 {
				def myaction(arg : float=4.5f) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static float ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4.5f;
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final float arg) {
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("float")
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			  
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
	def void inAgentAction_1p_boolean() {
		'''
			agent A1 {
				def myaction(arg : boolean=true) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static boolean ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = true;
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final boolean arg) {
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("boolean")
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			  
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
	def void inAgentAction_1p_double() {
		'''
			agent A1 {
				def myaction(arg : double=4.5) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static double ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4.5;
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final double arg) {
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("double")
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			  
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
	def void inAgentAction_1p_long() {
		'''
			agent A1 {
				def myaction(arg : long=450) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static long ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 450;
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final long arg) {
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("long")
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			  
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
	def void inAgentAction_1p_String() {
		'''
			agent A1 {
				def myaction(arg : String="abcd") {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = "abcd";
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final String arg) {
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("java.lang.String")
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			  
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
	def void inAgentAction_1p_char() {
		'''
			agent A1 {
				def myaction(arg : char='d') {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static char ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 'd';
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final char arg) {
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("char")
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			  
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
	def void inAgentAction_1p_returnValue() {
		'''
			agent A1 {
				def myaction(arg : int=4) : boolean {
					System.out.println(arg)
					return true
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated("4")
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4;
			  
			  @DefaultValueSource
			  public boolean myaction(@DefaultValue("0_0") final int arg) {
			    System.out.println(arg);
			    return true;
			  }
			  
			  @DefaultValueUse("int")
			  @Generated
			  public final boolean myaction() {
			    return myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			  
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
	def void inAgentAction_5p_0() {
		'''
			agent A1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4;
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public final void myaction(final String arg1, final int arg2, final int arg3, final String arg4) {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, arg1, arg2, arg3, arg4);
			  }
			  
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
	def void inAgentAction_5p_1() {
		'''
			agent A1 {
				def myaction(arg0 : int, arg1 : String="abc", arg2 : int, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = "abc";
			  
			  @DefaultValueSource
			  public void myaction(final int arg0, @DefaultValue("0_1") final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public final void myaction(final int arg0, final int arg2, final int arg3, final String arg4) {
			    myaction(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2, arg3, arg4);
			  }
			  
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
	def void inAgentAction_5p_2() {
		'''
			agent A1 {
				def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg2
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2 = 18;
			  
			  @DefaultValueSource
			  public void myaction(final int arg0, final String arg1, @DefaultValue("0_2") final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public final void myaction(final int arg0, final String arg1, final int arg3, final String arg4) {
			    myaction(arg0, arg1, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2, arg3, arg4);
			  }
			  
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
	def void inAgentAction_5p_3() {
		'''
			agent A1 {
				def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg3
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3 = 34;
			  
			  @DefaultValueSource
			  public void myaction(final int arg0, final String arg1, final int arg2, @DefaultValue("0_3") final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public final void myaction(final int arg0, final String arg1, final int arg2, final String arg4) {
			    myaction(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3, arg4);
			  }
			  
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
	def void inAgentAction_5p_4() {
		'''
			agent A1 {
				def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String="xyz") {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg4
			   */
			  @Generated
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_0_4 = "xyz";
			  
			  @DefaultValueSource
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, @DefaultValue("0_4") final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public final void myaction(final int arg0, final String arg1, final int arg2, final int arg3) {
			    myaction(arg0, arg1, arg2, arg3, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_4);
			  }
			  
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
	def void inAgentAction_5p_0_3() {
		'''
			agent A1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4;
			  
			  /**
			   * Default value for the parameter arg3
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3 = 56;
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final int arg0, final String arg1, final int arg2, @DefaultValue("0_3") final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public final void myaction(final String arg1, final int arg2, final String arg4) {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3, arg4);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public final void myaction(final int arg0, final String arg1, final int arg2, final String arg4) {
			    myaction(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3, arg4);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public final void myaction(final String arg1, final int arg2, final int arg3, final String arg4) {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, arg1, arg2, arg3, arg4);
			  }
			  
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
	def void inAgentAction_5p_returnValue() {
		'''
			agent A1 {
				def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) : boolean {
					System.out.println(arg0)
					return true
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated("4")
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4;
			  
			  @DefaultValueSource
			  public boolean myaction(@DefaultValue("0_0") final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			    return true;
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  @Generated
			  public final boolean myaction(final String arg1, final int arg2, final int arg3, final String arg4) {
			    return myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, arg1, arg2, arg3, arg4);
			  }
			  
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
	def void inAgentAction_3p_vararg_1() {
		'''
			agent A1 {
				def myaction(arg0 : int, arg1 : int=45, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = 45;
			  
			  @DefaultValueSource
			  public void myaction(final int arg0, @DefaultValue("0_1") final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  public final void myaction(final int arg0, final int... arg2) {
			    myaction(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2);
			  }
			  
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
	def void inAgentAction_3p_vararg_0() {
		'''
			agent A1 {
				def myaction(arg0 : int=45, arg1 : int, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 45;
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  public final void myaction(final int arg1, final int... arg2) {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, arg1, arg2);
			  }
			  
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
	def void inAgentAction_3p_vararg_0_1() {
		'''
			agent A1 {
				def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 45;
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = 56;
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final int arg0, @DefaultValue("0_1") final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  public final void myaction(final int... arg2) {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2);
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  public final void myaction(final int arg0, final int... arg2) {
			    myaction(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2);
			  }
			  
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
	def void inAgentAction_3p_vararg_returnValue() {
		'''
			agent A1 {
				def myaction(arg0 : int, arg1 : int=45, arg2 : int*) : boolean {
					System.out.println(arg0)
					return true
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated("45")
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = 45;
			  
			  @DefaultValueSource
			  public boolean myaction(final int arg0, @DefaultValue("0_1") final int arg1, final int... arg2) {
			    System.out.println(arg0);
			    return true;
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  @Generated
			  public final boolean myaction(final int arg0, final int... arg2) {
			    return myaction(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2);
			  }
			  
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
	def void inAgentAction_4p_0_1_2_3() {
		'''
			agent A1 {
				def myaction(arg0 : int=45, arg1 : int=56, arg2 : int=78, arg3 : int=14) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 45;
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = 56;
			  
			  /**
			   * Default value for the parameter arg2
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2 = 78;
			  
			  /**
			   * Default value for the parameter arg3
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3 = 14;
			  
			  @DefaultValueSource
			  public void myaction(@DefaultValue("0_0") final int arg0, @DefaultValue("0_1") final int arg1, @DefaultValue("0_2") final int arg2, @DefaultValue("0_3") final int arg3) {
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,int,int,int")
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3);
			  }
			  
			  @DefaultValueUse("int,int,int,int")
			  public final void myaction(final int arg0) {
			    myaction(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3);
			  }
			  
			  @DefaultValueUse("int,int,int,int")
			  public final void myaction(final int arg0, final int arg1) {
			    myaction(arg0, arg1, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3);
			  }
			  
			  @DefaultValueUse("int,int,int,int")
			  public final void myaction(final int arg0, final int arg1, final int arg2) {
			    myaction(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3);
			  }
			  
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
	def void inBehaviorConstructor_1p_int() {
		'''
			behavior B1 {
				new(arg : int=4) {
				  	super(null) // must be never null in real code
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4;
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final int arg) {
			    super(null);
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("int")
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_float() {
		'''
			behavior B1 {
				new(arg : float=4.5f) {
				  	super(null) // must be never null in real code
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static float ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4.5f;
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final float arg) {
			    super(null);
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("float")
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_boolean() {
		'''
			behavior B1 {
				new(arg : boolean=true) {
				  	super(null) // must be never null in real code
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static boolean ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = true;
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final boolean arg) {
			    super(null);
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("boolean")
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_double() {
		'''
			behavior B1 {
				new(arg : double=4.5) {
				  	super(null) // must be never null in real code
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static double ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4.5;
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final double arg) {
			    super(null);
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("double")
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_long() {
		'''
			behavior B1 {
				new(arg : long=450) {
				  	super(null) // must be never null in real code
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static long ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 450;
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final long arg) {
			    super(null);
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("long")
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_String() {
		'''
			behavior B1 {
				new(arg : String="abcd") {
				  	super(null) // must be never null in real code
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = "abcd";
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final String arg) {
			    super(null);
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("java.lang.String")
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_char() {
		'''
			behavior B1 {
				new(arg : char='d') {
				  	super(null) // must be never null in real code
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  @Generated
			  private final static char ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 'd';
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final char arg) {
			    super(null);
			    System.out.println(arg);
			  }
			  
			  @DefaultValueUse("char")
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_0() {
		'''
			behavior B1 {
				new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {
				  	super(null) // must be never null in real code
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4;
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    super(null);
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public B1(final String arg1, final int arg2, final int arg3, final String arg4) {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, arg1, arg2, arg3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_1() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : String="abc", arg2 : int, arg3 : int, arg4 : String) {
				  	super(null) // must be never null in real code
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = "abc";
			  
			  @DefaultValueSource
			  public B1(final int arg0, @DefaultValue("0_1") final String arg1, final int arg2, final int arg3, final String arg4) {
			    super(null);
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public B1(final int arg0, final int arg2, final int arg3, final String arg4) {
			    this(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2, arg3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_2() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {
				  	super(null) // must be never null in real code
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg2
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2 = 18;
			  
			  @DefaultValueSource
			  public B1(final int arg0, final String arg1, @DefaultValue("0_2") final int arg2, final int arg3, final String arg4) {
			    super(null);
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public B1(final int arg0, final String arg1, final int arg3, final String arg4) {
			    this(arg0, arg1, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2, arg3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_3() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {
				  	super(null) // must be never null in real code
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg3
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3 = 34;
			  
			  @DefaultValueSource
			  public B1(final int arg0, final String arg1, final int arg2, @DefaultValue("0_3") final int arg3, final String arg4) {
			    super(null);
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public B1(final int arg0, final String arg1, final int arg2, final String arg4) {
			    this(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_4() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String="xyz") {
				  	super(null) // must be never null in real code
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg4
			   */
			  @Generated
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_0_4 = "xyz";
			  
			  @DefaultValueSource
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, @DefaultValue("0_4") final String arg4) {
			    super(null);
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3) {
			    this(arg0, arg1, arg2, arg3, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_0_3() {
		'''
			behavior B1 {
				new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {
				  	super(null) // must be never null in real code
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4;
			  
			  /**
			   * Default value for the parameter arg3
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3 = 56;
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final int arg0, final String arg1, final int arg2, @DefaultValue("0_3") final int arg3, final String arg4) {
			    super(null);
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public B1(final String arg1, final int arg2, final String arg4) {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3, arg4);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public B1(final int arg0, final String arg1, final int arg2, final String arg4) {
			    this(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3, arg4);
			  }
			  
			  @DefaultValueUse("int,java.lang.String,int,int,java.lang.String")
			  public B1(final String arg1, final int arg2, final int arg3, final String arg4) {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, arg1, arg2, arg3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_1() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : int=45, arg2 : int*) {
				  	super(null) // must be never null in real code
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = 45;
			  
			  @DefaultValueSource
			  public B1(final int arg0, @DefaultValue("0_1") final int arg1, final int... arg2) {
			    super(null);
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  public B1(final int arg0, final int... arg2) {
			    this(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_0() {
		'''
			behavior B1 {
				new(arg0 : int=45, arg1 : int, arg2 : int*) {
				  	super(null) // must be never null in real code
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 45;
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final int arg0, final int arg1, final int... arg2) {
			    super(null);
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  public B1(final int arg1, final int... arg2) {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, arg1, arg2);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_0_1() {
		'''
			behavior B1 {
				new(arg0 : int=45, arg1 : int=56, arg2 : int*) {
				  	super(null) // must be never null in real code
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 45;
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = 56;
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final int arg0, @DefaultValue("0_1") final int arg1, final int... arg2) {
			    super(null);
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  public B1(final int... arg2) {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2);
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  public B1(final int arg0, final int... arg2) {
			    this(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_4p_0_1_2_3() {
		'''
			behavior B1 {
				new(arg0 : int=45, arg1 : int=56, arg2 : int=78, arg3 : int=14) {
				  	super(null) // must be never null in real code
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 45;
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = 56;
			  
			  /**
			   * Default value for the parameter arg2
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2 = 78;
			  
			  /**
			   * Default value for the parameter arg3
			   */
			  @Generated
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3 = 14;
			  
			  @DefaultValueSource
			  public B1(@DefaultValue("0_0") final int arg0, @DefaultValue("0_1") final int arg1, @DefaultValue("0_2") final int arg2, @DefaultValue("0_3") final int arg3) {
			    super(null);
			    System.out.println(arg0);
			  }
			  
			  @DefaultValueUse("int,int,int,int")
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3);
			  }
			  
			  @DefaultValueUse("int,int,int,int")
			  public B1(final int arg0) {
			    this(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3);
			  }
			  
			  @DefaultValueUse("int,int,int,int")
			  public B1(final int arg0, final int arg1) {
			    this(arg0, arg1, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3);
			  }
			  
			  @DefaultValueUse("int,int,int,int")
			  public B1(final int arg0, final int arg1, final int arg2) {
			    this(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3);
			  }
			}
		''')
	}

	@Test
	def void inCapacity() {
		'''
			capacity C1 {
				def myaction(arg0 : int=45, arg1 : int=56, arg2 : int, arg3 : int=14)
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C1 extends Capacity {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  public final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 45;
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated
			  public final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = 56;
			  
			  /**
			   * Default value for the parameter arg3
			   */
			  @Generated
			  public final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_3 = 14;
			  
			  @DefaultValueSource
			  public abstract void myaction(@DefaultValue("0_0") final int arg0, @DefaultValue("0_1") final int arg1, final int arg2, @DefaultValue("0_3") final int arg3);
			  
			  @DefaultValueUse("int,int,int,int")
			  public abstract void myaction(final int arg2);
			  
			  @DefaultValueUse("int,int,int,int")
			  public abstract void myaction(final int arg0, final int arg2);
			  
			  @DefaultValueUse("int,int,int,int")
			  public abstract void myaction(final int arg0, final int arg1, final int arg2);
			}
		''')
	}

	@Test
	def void overridingCapacitySkill() {
		val expectedC1 = '''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C1 extends Capacity {
			  /**
			   * Default value for the parameter arg0
			   */
			  @Generated
			  public final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 45;
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  @Generated
			  public final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_1 = 56;
			  
			  @DefaultValueSource
			  public abstract void myaction(@DefaultValue("0_0") final int arg0, @DefaultValue("0_1") final int arg1, final int... arg2);
			  
			  @DefaultValueUse("int,int,int*")
			  public abstract void myaction(final int... arg2);
			  
			  @DefaultValueUse("int,int,int*")
			  public abstract void myaction(final int arg0, final int... arg2);
			}
		'''
		val expectedS1 = '''
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import io.sarl.lang.core.Skill;
			
			@SuppressWarnings("all")
			public class S1 extends Skill implements C1 {
			  public void capAction() {
			  }
			  
			  public void myaction(final int arg0, final int arg1, final int... arg2) {
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  public final void myaction(final int... arg2) {
			    myaction(C1.___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, C1.___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2);
			  }
			  
			  @DefaultValueUse("int,int,int*")
			  public final void myaction(final int arg0, final int... arg2) {
			    myaction(arg0, C1.___FORMAL_PARAMETER_DEFAULT_VALUE_0_1, arg2);
			  }
			  
			  /**
			   * Construct a skill.
			   * @param owner - agent that is owning this skill.
			   */
			  @Generated
			  public S1(final Agent owner) {
			    super(owner);
			  }
			  
			  /**
			   * Construct a skill. The owning agent is unknown.
			   */
			  @Generated
			  public S1() {
			    super();
			  }
			}
		'''
		
		'''
			capacity C1 {
				def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*)
			}
			skill S1 implements C1 {
				def capAction {}
				def myaction(arg0 : int, arg1 : int, arg2 : int*) {}
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedC1,r.getGeneratedCode("C1"))
			Assert.assertEquals(expectedS1,r.getGeneratedCode("S1"))
		])
	}
	
	@Test
	def void missedActionImplementation() {
		val expectedC1 = '''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C1 extends Capacity {
			  /**
			   * Default value for the parameter a
			   */
			  @Generated
			  public final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 4;
			  
			  @DefaultValueSource
			  public abstract void myaction1(@DefaultValue("0_0") final int a);
			  
			  @DefaultValueUse("int")
			  public abstract void myaction1();
			}
		'''
		val expectedC2 = '''
			import io.sarl.lang.annotation.DefaultValue;
			import io.sarl.lang.annotation.DefaultValueSource;
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C2 extends Capacity {
			  /**
			   * Default value for the parameter b
			   */
			  @Generated
			  public final static float ___FORMAL_PARAMETER_DEFAULT_VALUE_0_0 = 6;
			  
			  @DefaultValueSource
			  public abstract void myaction2(@DefaultValue("0_0") final float b, final boolean c);
			  
			  @DefaultValueUse("float,boolean")
			  public abstract void myaction2(final boolean c);
			}
		'''
		val expectedS1 = '''
			import io.sarl.lang.annotation.DefaultValueUse;
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import io.sarl.lang.core.Skill;
			
			@SuppressWarnings("all")
			public class S1 extends Skill implements C1, C2 {
			  public void myaction1(final int x) {
			    System.out.println(x);
			  }
			  
			  public void myaction2(final float y, final boolean z) {
			    System.err.println(y);
			    System.err.println(z);
			  }
			  
			  @DefaultValueUse("int")
			  public final void myaction1() {
			    myaction1(C1.___FORMAL_PARAMETER_DEFAULT_VALUE_0_0);
			  }
			  
			  @DefaultValueUse("float,boolean")
			  public final void myaction2(final boolean c) {
			    myaction2(C2.___FORMAL_PARAMETER_DEFAULT_VALUE_0_0, c);
			  }
			  
			  /**
			   * Construct a skill.
			   * @param owner - agent that is owning this skill.
			   */
			  @Generated
			  public S1(final Agent owner) {
			    super(owner);
			  }
			  
			  /**
			   * Construct a skill. The owning agent is unknown.
			   */
			  @Generated
			  public S1() {
			    super();
			  }
			}
		'''
		
		'''
			capacity C1 {
				def myaction1(a : int=4)
			}
			capacity C2 {
				def myaction2(b : float=6, c : boolean)
			}
			skill S1 implements C1, C2 {
				def myaction1(x : int) {
					System.out.println(x);
				}
				def myaction2(y : float, z : boolean) {
					System.err.println(y);
					System.err.println(z);
				}
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedC1,r.getGeneratedCode("C1"))
			Assert.assertEquals(expectedC2,r.getGeneratedCode("C2"))
			Assert.assertEquals(expectedS1,r.getGeneratedCode("S1"))
		])
	}

}