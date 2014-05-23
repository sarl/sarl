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
class VarDeclarationCompilerTest {
	@Inject extension CompilationTestHelper

	@Test
	def void variableDeclaration_attributeScope() {
		'''
			import java.util.List
			agent A1 {
				var list : List<Integer>
				var i = 45
				var j : double = 45
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			import java.util.List;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  protected List<Integer> list;
			  
			  protected int i = 45;
			  
			  protected double j = 45;
			}
		''')
	}

	@Test
	def void variableDeclaration_localScope() {
		'''
			import java.util.List
			agent A1 {
				def myaction {
					var i : List<Integer>
					var j = 45
					var k : double = 45
					System.out.println(i)
					System.out.println(j)
					System.out.println(k)
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			import java.util.List;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction() {
			    List<Integer> i = null;
			    int j = 45;
			    double k = 45;
			    System.out.println(i);
			    System.out.println(j);
			    System.out.println(k);
			  }
			}
		''')
	}

	@Test
	def void valueDeclaration_attributeScope() {
		'''
			import java.util.List
			agent A1 {
				val list : List<Integer> = null
				val i = 45
				val j : double = 45
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			import java.util.List;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  protected final static List<Integer> list = null;
			  
			  protected final static int i = 45;
			  
			  protected final static double j = 45;
			}
		''')
	}

	@Test
	def void valueDeclaration_localScope() {
		'''
			agent A1 {
				def myaction {
					val j = 45
					val k : double = 45
					System.out.println(j)
					System.out.println(k)
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
			  
			  public void myaction() {
			    final int j = 45;
			    final double k = 45;
			    System.out.println(j);
			    System.out.println(k);
			  }
			}
		''')
	}

	@Test
	def void forLoop_inferredType() {
		'''
			import java.util.List
			agent A1 {
				var list : List<Integer>
				def myaction {
					for( i : list) {
						System.out.println(i)
					}
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			import java.util.List;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  protected List<Integer> list;
			  
			  public void myaction() {
			    for (final Integer i : this.list) {
			      System.out.println(i);
			    }
			  }
			}
		''')
	}

	@Test
	def void forLoop_explicitType() {
		'''
			import java.util.List
			agent A1 {
				var list : List<Integer>
				def myaction {
					for( i as Number : list) {
						System.out.println(i)
					}
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			import java.util.List;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  protected List<Integer> list;
			  
			  public void myaction() {
			    for (final Number i : this.list) {
			      System.out.println(i);
			    }
			  }
			}
		''')
	}

	@Test
	def void catch_oneType() {
		'''
			agent A1 {
				def myaction {
					try {
						System.out.println("G")
					}
					catch(e : Throwable) {
						System.out.println(e)
					}
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			import org.eclipse.xtext.xbase.lib.Exceptions;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction() {
			    try {
			      System.out.println("G");
			    } catch (final Throwable _t) {
			      if (_t instanceof Throwable) {
			        final Throwable e = (Throwable)_t;
			        System.out.println(e);
			      } else {
			        throw Exceptions.sneakyThrow(_t);
			      }
			    }
			  }
			}
		''')
	}

	@Test
	def void multicatch_oneType() {
		'''
			agent A1 {
				def myaction {
					try {
						System.out.println("G")
					}
					catch(e : Exception) {
						System.out.println(e)
					}
					catch(e : Throwable) {
						System.out.println(e)
					}
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			import org.eclipse.xtext.xbase.lib.Exceptions;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public void myaction() {
			    try {
			      System.out.println("G");
			    } catch (final Throwable _t) {
			      if (_t instanceof Exception) {
			        final Exception e = (Exception)_t;
			        System.out.println(e);
			      } else if (_t instanceof Throwable) {
			        final Throwable e_1 = (Throwable)_t;
			        System.out.println(e_1);
			      } else {
			        throw Exceptions.sneakyThrow(_t);
			      }
			    }
			  }
			}
		''')
	}

	@Test
	def void closure_twoParams() {
		'''
			agent A1 {
				def mycall(a : int, f : (Float,Integer) => float) : float {
					return a + f.apply(5.45f, 6)
				}
				def myaction : void {
					mycall(4) [ a : Float, b : Integer |
						2f * a.floatValue + b.intValue
					]
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Agent;
			import org.eclipse.xtext.xbase.lib.Functions.Function2;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  /**
			   * Creates a new Agent of type A1
			   */
			  public A1(final java.util.UUID parentID) {
			    super(parentID);
			  }
			  
			  public float mycall(final int a, final Function2<? super Float, ? super Integer, ? extends Float> f) {
			    Float _apply = f.apply(Float.valueOf(5.45f), Integer.valueOf(6));
			    return (a + (_apply).floatValue());
			  }
			  
			  public void myaction() {
			    final Function2<Float, Integer, Float> _function = new Function2<Float, Integer, Float>() {
			      public Float apply(final Float a, final Integer b) {
			        float _floatValue = a.floatValue();
			        float _multiply = (2f * _floatValue);
			        int _intValue = b.intValue();
			        return Float.valueOf((_multiply + _intValue));
			      }
			    };
			    this.mycall(4, _function);
			  }
			}
		''')
	}

}
