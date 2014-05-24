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
class ArgDefaultValueCompilerTest {
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 4;
			  
			  public void myaction(final int arg) {
			    System.out.println(arg);
			  }
			  
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg
			   */
			  private final static float ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 4.5f;
			  
			  public void myaction(final float arg) {
			    System.out.println(arg);
			  }
			  
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg
			   */
			  private final static boolean ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = true;
			  
			  public void myaction(final boolean arg) {
			    System.out.println(arg);
			  }
			  
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg
			   */
			  private final static double ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 4.5;
			  
			  public void myaction(final double arg) {
			    System.out.println(arg);
			  }
			  
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg
			   */
			  private final static long ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 450;
			  
			  public void myaction(final long arg) {
			    System.out.println(arg);
			  }
			  
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg
			   */
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = "abcd";
			  
			  public void myaction(final String arg) {
			    System.out.println(arg);
			  }
			  
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg
			   */
			  private final static char ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 'd';
			  
			  public void myaction(final char arg) {
			    System.out.println(arg);
			  }
			  
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg0
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 4;
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public final void myaction(final String arg1, final int arg2, final int arg3, final String arg4) {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, arg1, arg2, arg3, arg4);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1 = "abc";
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public final void myaction(final int arg0, final int arg2, final int arg3, final String arg4) {
			    myaction(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, arg2, arg3, arg4);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg2
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2 = 18;
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public final void myaction(final int arg0, final String arg1, final int arg3, final String arg4) {
			    myaction(arg0, arg1, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2, arg3, arg4);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg3
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3 = 34;
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public final void myaction(final int arg0, final String arg1, final int arg2, final String arg4) {
			    myaction(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3, arg4);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg4
			   */
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_1_4 = "xyz";
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public final void myaction(final int arg0, final String arg1, final int arg2, final int arg3) {
			    myaction(arg0, arg1, arg2, arg3, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_4);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg0
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 4;
			  
			  /**
			   * Default value for the parameter arg3
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3 = 56;
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public final void myaction(final String arg1, final int arg2, final String arg4) {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3, arg4);
			  }
			  
			  public final void myaction(final int arg0, final String arg1, final int arg2, final String arg4) {
			    myaction(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3, arg4);
			  }
			  
			  public final void myaction(final String arg1, final int arg2, final int arg3, final String arg4) {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, arg1, arg2, arg3, arg4);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1 = 45;
			  
			  public void myaction(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public final void myaction(final int arg0, final int... arg2) {
			    myaction(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, arg2);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg0
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 45;
			  
			  public void myaction(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public final void myaction(final int arg1, final int... arg2) {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, arg1, arg2);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg0
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 45;
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1 = 56;
			  
			  public void myaction(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public final void myaction(final int... arg2) {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, arg2);
			  }
			  
			  public final void myaction(final int arg0, final int... arg2) {
			    myaction(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, arg2);
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
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifer
			   * of the parent agent and the enclosing contect, at the same time.
			   * 
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  /**
			   * Default value for the parameter arg0
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 45;
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1 = 56;
			  
			  /**
			   * Default value for the parameter arg2
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2 = 78;
			  
			  /**
			   * Default value for the parameter arg3
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3 = 14;
			  
			  public void myaction(final int arg0, final int arg1, final int arg2, final int arg3) {
			    System.out.println(arg0);
			  }
			  
			  public final void myaction() {
			    myaction(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3);
			  }
			  
			  public final void myaction(final int arg0) {
			    myaction(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3);
			  }
			  
			  public final void myaction(final int arg0, final int arg1) {
			    myaction(arg0, arg1, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3);
			  }
			  
			  public final void myaction(final int arg0, final int arg1, final int arg2) {
			    myaction(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_int() {
		'''
			behavior B1 {
				new(arg : int=4) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 4;
			  
			  public B1(final int arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_float() {
		'''
			behavior B1 {
				new(arg : float=4.5f) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  private final static float ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 4.5f;
			  
			  public B1(final float arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_boolean() {
		'''
			behavior B1 {
				new(arg : boolean=true) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  private final static boolean ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = true;
			  
			  public B1(final boolean arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_double() {
		'''
			behavior B1 {
				new(arg : double=4.5) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  private final static double ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 4.5;
			  
			  public B1(final double arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_long() {
		'''
			behavior B1 {
				new(arg : long=450) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  private final static long ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 450;
			  
			  public B1(final long arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_String() {
		'''
			behavior B1 {
				new(arg : String="abcd") {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = "abcd";
			  
			  public B1(final String arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_char() {
		'''
			behavior B1 {
				new(arg : char='d') {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg
			   */
			  private final static char ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 'd';
			  
			  public B1(final char arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_0() {
		'''
			behavior B1 {
				new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg0
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 4;
			  
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final String arg1, final int arg2, final int arg3, final String arg4) {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, arg1, arg2, arg3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_1() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : String="abc", arg2 : int, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg1
			   */
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1 = "abc";
			  
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg0, final int arg2, final int arg3, final String arg4) {
			    this(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, arg2, arg3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_2() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg2
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2 = 18;
			  
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg0, final String arg1, final int arg3, final String arg4) {
			    this(arg0, arg1, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2, arg3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_3() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg3
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3 = 34;
			  
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg0, final String arg1, final int arg2, final String arg4) {
			    this(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_4() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String="xyz") {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg4
			   */
			  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_1_4 = "xyz";
			  
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3) {
			    this(arg0, arg1, arg2, arg3, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_5p_0_3() {
		'''
			behavior B1 {
				new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg0
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 4;
			  
			  /**
			   * Default value for the parameter arg3
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3 = 56;
			  
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final String arg1, final int arg2, final String arg4) {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3, arg4);
			  }
			  
			  public B1(final int arg0, final String arg1, final int arg2, final String arg4) {
			    this(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3, arg4);
			  }
			  
			  public B1(final String arg1, final int arg2, final int arg3, final String arg4) {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, arg1, arg2, arg3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_1() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : int=45, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg1
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1 = 45;
			  
			  public B1(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg0, final int... arg2) {
			    this(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, arg2);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_0() {
		'''
			behavior B1 {
				new(arg0 : int=45, arg1 : int, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg0
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 45;
			  
			  public B1(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg1, final int... arg2) {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, arg1, arg2);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_0_1() {
		'''
			behavior B1 {
				new(arg0 : int=45, arg1 : int=56, arg2 : int*) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg0
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 45;
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1 = 56;
			  
			  public B1(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int... arg2) {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, arg2);
			  }
			  
			  public B1(final int arg0, final int... arg2) {
			    this(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, arg2);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_4p_0_1_2_3() {
		'''
			behavior B1 {
				new(arg0 : int=45, arg1 : int=56, arg2 : int=78, arg3 : int=14) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  /**
			   * Default value for the parameter arg0
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_0 = 45;
			  
			  /**
			   * Default value for the parameter arg1
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1 = 56;
			  
			  /**
			   * Default value for the parameter arg2
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2 = 78;
			  
			  /**
			   * Default value for the parameter arg3
			   */
			  private final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3 = 14;
			  
			  public B1(final int arg0, final int arg1, final int arg2, final int arg3) {
			    System.out.println(arg0);
			  }
			  
			  public B1() {
			    this(___FORMAL_PARAMETER_DEFAULT_VALUE_1_0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3);
			  }
			  
			  public B1(final int arg0) {
			    this(arg0, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_1, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3);
			  }
			  
			  public B1(final int arg0, final int arg1) {
			    this(arg0, arg1, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3);
			  }
			  
			  public B1(final int arg0, final int arg1, final int arg2) {
			    this(arg0, arg1, arg2, ___FORMAL_PARAMETER_DEFAULT_VALUE_1_3);
			  }
			}
		''')
	}

}
