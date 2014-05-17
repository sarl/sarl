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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final int arg) {
			    System.out.println(arg);
			  }
			  
			  public void myaction() {
			    myaction(4);
			  }
			}
		''')
	}

	@Test
	def void inAgentAction_1p_float() {
		'''
			agent A1 {
				def myaction(arg : float=4.5) {
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
			  
			  public void myaction(final float arg) {
			    System.out.println(arg);
			  }
			  
			  public void myaction() {
			    myaction(4.5);
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final boolean arg) {
			    System.out.println(arg);
			  }
			  
			  public void myaction() {
			    myaction(true);
			  }
			}
		''')
	}

	@Test
	def void inAgentAction_1p_byte() {
		'''
			agent A1 {
				def myaction(arg : byte=54) {
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
			  
			  public void myaction(final byte arg) {
			    System.out.println(arg);
			  }
			  
			  public void myaction() {
			    myaction(54);
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final double arg) {
			    System.out.println(arg);
			  }
			  
			  public void myaction() {
			    myaction(4.5);
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final long arg) {
			    System.out.println(arg);
			  }
			  
			  public void myaction() {
			    myaction(450);
			  }
			}
		''')
	}

	@Test
	def void inAgentAction_1p_short() {
		'''
			agent A1 {
				def myaction(arg : short=450) {
					System.out.println(arg as int)
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
			  
			  public void myaction(final short arg) {
			    System.out.println(((int) arg));
			  }
			  
			  public void myaction() {
			    myaction(450);
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final String arg) {
			    System.out.println(arg);
			  }
			  
			  public void myaction() {
			    myaction("abcd");
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final char arg) {
			    System.out.println(arg);
			  }
			  
			  public void myaction() {
			    myaction('d');
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public void myaction(final String arg1, final int arg2, final int arg3, final String arg4) {
			    myaction(4, arg1, arg2, arg3, arg4);
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public void myaction(final int arg0, final int arg2, final int arg3, final String arg4) {
			    myaction(arg0, "abc", arg2, arg3, arg4);
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public void myaction(final int arg0, final String arg1, final int arg3, final String arg4) {
			    myaction(arg0, arg1, 18, arg3, arg4);
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final String arg4) {
			    myaction(arg0, arg1, arg2, 34, arg4);
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3) {
			    myaction(arg0, arg1, arg2, arg3, "xyz");
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public void myaction(final String arg1, final int arg2, final String arg4) {
			    myaction(4, arg1, arg2, 56, arg4);
			  }
			  
			  public void myaction(final int arg0, final String arg1, final int arg2, final String arg4) {
			    myaction(arg0, arg1, arg2, 56, arg4);
			  }
			  
			  public void myaction(final String arg1, final int arg2, final int arg3, final String arg4) {
			    myaction(4, arg1, arg2, arg3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inAgentAction_3p_vararg_1() {
		'''
			agent A1 {
				def myaction(arg0 : int, arg1 : int=45, arg2 : int...) {
					System.out.println(arg0)
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
			  
			  public void myaction(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public void myaction(final int arg0, final int... arg2) {
			    myaction(arg0, 45, arg2);
			  }
			}
		''')
	}

	@Test
	def void inAgentAction_3p_vararg_0() {
		'''
			agent A1 {
				def myaction(arg0 : int=45, arg1 : int, arg2 : int...) {
					System.out.println(arg0)
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
			  
			  public void myaction(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public void myaction(final int arg1, final int... arg2) {
			    myaction(45, arg1, arg2);
			  }
			}
		''')
	}

	@Test
	def void inAgentAction_3p_vararg_0_1() {
		'''
			agent A1 {
				def myaction(arg0 : int=45, arg1 : int=56, arg2 : int...) {
					System.out.println(arg0)
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
			  
			  public void myaction(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public void myaction(final int... arg2) {
			    myaction(45, 56, arg2);
			  }
			  
			  public void myaction(final int arg0, final int... arg2) {
			    myaction(arg0, 56, arg2);
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
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction(final int arg0, final int arg1, final int arg2, final int arg3) {
			    System.out.println(arg0);
			  }
			  
			  public void myaction() {
			    myaction(45, 56, 78, 14);
			  }
			  
			  public void myaction(final int arg0) {
			    myaction(arg0, 56, 78, 14);
			  }
			  
			  public void myaction(final int arg0, final int arg1) {
			    myaction(arg0, arg1, 78, 14);
			  }
			  
			  public void myaction(final int arg0, final int arg1, final int arg2) {
			    myaction(arg0, arg1, arg2, 14);
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
			  public B1(final int arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_float() {
		'''
			behavior B1 {
				new(arg : float=4.5) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  public B1(final float arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(4.5);
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
			  public B1(final boolean arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(true);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_byte() {
		'''
			behavior B1 {
				new(arg : byte=54) {
					System.out.println(arg)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  public B1(final byte arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(54);
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
			  public B1(final double arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(4.5);
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
			  public B1(final long arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this(450);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_1p_short() {
		'''
			behavior B1 {
				new(arg : short=450) {
					System.out.println(arg as int)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  public B1(final short arg) {
			    System.out.println(((int) arg));
			  }
			  
			  public B1() {
			    this(450);
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
			  public B1(final String arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this("abcd");
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
			  public B1(final char arg) {
			    System.out.println(arg);
			  }
			  
			  public B1() {
			    this('d');
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
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final String arg1, final int arg2, final int arg3, final String arg4) {
			    this(4, arg1, arg2, arg3, arg4);
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
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg0, final int arg2, final int arg3, final String arg4) {
			    this(arg0, "abc", arg2, arg3, arg4);
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
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg0, final String arg1, final int arg3, final String arg4) {
			    this(arg0, arg1, 18, arg3, arg4);
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
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg0, final String arg1, final int arg2, final String arg4) {
			    this(arg0, arg1, arg2, 34, arg4);
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
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3) {
			    this(arg0, arg1, arg2, arg3, "xyz");
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
			  public B1(final int arg0, final String arg1, final int arg2, final int arg3, final String arg4) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final String arg1, final int arg2, final String arg4) {
			    this(4, arg1, arg2, 56, arg4);
			  }
			  
			  public B1(final int arg0, final String arg1, final int arg2, final String arg4) {
			    this(arg0, arg1, arg2, 56, arg4);
			  }
			  
			  public B1(final String arg1, final int arg2, final int arg3, final String arg4) {
			    this(4, arg1, arg2, arg3, arg4);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_1() {
		'''
			behavior B1 {
				new(arg0 : int, arg1 : int=45, arg2 : int...) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  public B1(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg0, final int... arg2) {
			    this(arg0, 45, arg2);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_0() {
		'''
			behavior B1 {
				new(arg0 : int=45, arg1 : int, arg2 : int...) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  public B1(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int arg1, final int... arg2) {
			    this(45, arg1, arg2);
			  }
			}
		''')
	}

	@Test
	def void inBehaviorConstructor_3p_vararg_0_1() {
		'''
			behavior B1 {
				new(arg0 : int=45, arg1 : int=56, arg2 : int...) {
					System.out.println(arg0)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Behavior;
			
			@SuppressWarnings("all")
			public class B1 extends Behavior {
			  public B1(final int arg0, final int arg1, final int... arg2) {
			    System.out.println(arg0);
			  }
			  
			  public B1(final int... arg2) {
			    this(45, 56, arg2);
			  }
			  
			  public B1(final int arg0, final int... arg2) {
			    this(arg0, 56, arg2);
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
			  public B1(final int arg0, final int arg1, final int arg2, final int arg3) {
			    System.out.println(arg0);
			  }
			  
			  public B1() {
			    this(45, 56, 78, 14);
			  }
			  
			  public B1(final int arg0) {
			    this(arg0, 56, 78, 14);
			  }
			  
			  public B1(final int arg0, final int arg1) {
			    this(arg0, arg1, 78, 14);
			  }
			  
			  public B1(final int arg0, final int arg1, final int arg2) {
			    this(arg0, arg1, arg2, 14);
			  }
			}
		''')
	}

}
