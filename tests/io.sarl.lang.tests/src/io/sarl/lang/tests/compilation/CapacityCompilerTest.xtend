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
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Assert

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class CapacityCompilerTest {
	@Inject extension CompilationTestHelper

	@Test
	def void completeFinalFieldInitialization() {
		val expectedC1 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C1 extends Capacity {
			}
		'''
		val expectedS1 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import io.sarl.lang.core.Skill;
			
			@SuppressWarnings("all")
			public class S1 extends Skill implements C1 {
			  protected final int field1 = 5;
			  
			  protected final String field2 = "";
			  
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
			capacity C1 { }
			skill S1 implements C1 {
				val field1 : int = 5
				val field2 : String = ""
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedC1,r.getGeneratedCode("C1"))
			Assert.assertEquals(expectedS1,r.getGeneratedCode("S1"))
		])
	}

	@Test
	def void missedActionImplementation_0() {
		val expectedC1 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C1 extends Capacity {
			  public abstract void myaction1(final int a);
			}
		'''
		val expectedC2 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C2 extends Capacity {
			  public abstract void myaction2(final float b, final boolean c);
			}
		'''
		val expectedS1 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import io.sarl.lang.core.Skill;
			
			@SuppressWarnings("all")
			public class S1 extends Skill implements C1, C2 {
			  public void myaction1(final int x) {
			  }
			  
			  public void myaction2(final float y, final boolean z) {
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
				def myaction1(a : int)
			}
			capacity C2 {
				def myaction2(b : float, c : boolean)
			}
			skill S1 implements C1, C2 {
				def myaction1(x : int) { }
				def myaction2(y : float, z : boolean) { }
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedC1,r.getGeneratedCode("C1"))
			Assert.assertEquals(expectedC2,r.getGeneratedCode("C2"))
			Assert.assertEquals(expectedS1,r.getGeneratedCode("S1"))
		])
	}

	@Test
	def void compatibleReturnType_0() {
		val expectedC1 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C1 extends Capacity {
			}
		'''
		val expectedC2 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C2 extends Capacity {
			}
		'''
		val expectedS1 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import io.sarl.lang.core.Skill;
			
			@SuppressWarnings("all")
			public class S1 extends Skill implements C1 {
			  public Number myaction(final int a) {
			    return Double.valueOf(0.0);
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
		val expectedS2 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class S2 extends S1 implements C2 {
			  public Double myaction(final int a) {
			    return Double.valueOf(0.0);
			  }
			  
			  /**
			   * Construct a skill.
			   * @param owner - agent that is owning this skill.
			   */
			  @Generated
			  public S2(final Agent owner) {
			    super(owner);
			  }
			  
			  /**
			   * Construct a skill. The owning agent is unknown.
			   */
			  @Generated
			  public S2() {
			    super();
			  }
			}
		'''
		
		'''
			capacity C1 { }
			capacity C2 { }
			skill S1 implements C1 {
				def myaction(a : int) : Number {
					return 0.0
				}
			}
			skill S2 extends S1 implements C2 {
				def myaction(a : int) : Double {
					return 0.0
				}
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedC1,r.getGeneratedCode("C1"))
			Assert.assertEquals(expectedC2,r.getGeneratedCode("C2"))
			Assert.assertEquals(expectedS1,r.getGeneratedCode("S1"))
			Assert.assertEquals(expectedS2,r.getGeneratedCode("S2"))
		])
	}

	@Test
	def void compatibleReturnType_1() {
		val expectedC1 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C1 extends Capacity {
			}
		'''
		val expectedC2 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C2 extends Capacity {
			}
		'''
		val expectedS1 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import io.sarl.lang.core.Skill;
			
			@SuppressWarnings("all")
			public class S1 extends Skill implements C1 {
			  public float myaction(final int a) {
			    return 0f;
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
		val expectedS2 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			
			@SuppressWarnings("all")
			public class S2 extends S1 implements C2 {
			  public float myaction(final int a) {
			    return 0f;
			  }
			  
			  /**
			   * Construct a skill.
			   * @param owner - agent that is owning this skill.
			   */
			  @Generated
			  public S2(final Agent owner) {
			    super(owner);
			  }
			  
			  /**
			   * Construct a skill. The owning agent is unknown.
			   */
			  @Generated
			  public S2() {
			    super();
			  }
			}
		'''
		
		'''
			capacity C1 { }
			capacity C2 { }
			skill S1 implements C1 {
				def myaction(a : int) : float {
					return 0f
				}
			}
			skill S2 extends S1 implements C2 {
				def myaction(a : int) : float {
					return 0f
				}
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedC1,r.getGeneratedCode("C1"))
			Assert.assertEquals(expectedC2,r.getGeneratedCode("C2"))
			Assert.assertEquals(expectedS1,r.getGeneratedCode("S1"))
			Assert.assertEquals(expectedS2,r.getGeneratedCode("S2"))
		])
	}

	@Test
	def void compatibleReturnType_2() {
		val expectedC1 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C1 extends Capacity {
			}
		'''
		val expectedS1 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import io.sarl.lang.core.Skill;
			
			@SuppressWarnings("all")
			public class S1 extends Skill implements C1 {
			  public float myaction(final int a) {
			    return 0f;
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
			capacity C1 { }
			skill S1 implements C1 {
				def myaction(a : int) : float {
					return 0f
				}
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedC1,r.getGeneratedCode("C1"))
			Assert.assertEquals(expectedS1,r.getGeneratedCode("S1"))
		])
	}

	@Test
	def void compatibleReturnType_3() {
		val expectedC1 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C1 extends Capacity {
			  public abstract float myaction(final int a);
			}
		'''
		val expectedS1 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import io.sarl.lang.core.Skill;
			
			@SuppressWarnings("all")
			public class S1 extends Skill implements C1 {
			  public float myaction(final int a) {
			    return 0f;
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
				def myaction(a : int) : float
			}
			skill S1 implements C1 {
				def myaction(a : int) : float {
					return 0f
				}
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedC1,r.getGeneratedCode("C1"))
			Assert.assertEquals(expectedS1,r.getGeneratedCode("S1"))
		])
	}

	@Test
	def void capacityAccessors_inSkill() {
		val expectedC1 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C1 extends Capacity {
			  public abstract float myaction(final int a);
			  
			  public abstract void myaction2(final boolean a);
			}
		'''
		val expectedC2 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface C2 extends Capacity {
			  public abstract float myaction3(final int a);
			  
			  public abstract void myaction4(final boolean a);
			}
		'''
		val expectedS1 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.annotation.ImportedCapacityFeature;
			import io.sarl.lang.core.Agent;
			import io.sarl.lang.core.Skill;
			
			@SuppressWarnings("all")
			public class S1 extends Skill implements C1 {
			  public float myaction(final int a) {
			    return this.myaction3(a);
			  }
			  
			  public void myaction2(final boolean a) {
			    this.myaction4(a);
			  }
			  
			  /**
			   * See the capacity {@link C2#myaction3(int)}.
			   * 
			   * @see C2#myaction3(int).
			   */
			  @Generated
			  @ImportedCapacityFeature(C2.class)
			  protected float myaction3(final int a) {
			    return getSkill(C2.class).myaction3(a);
			  }
			  
			  /**
			   * See the capacity {@link C2#myaction4(boolean)}.
			   * 
			   * @see C2#myaction4(boolean).
			   */
			  @Generated
			  @ImportedCapacityFeature(C2.class)
			  protected void myaction4(final boolean a) {
			    getSkill(C2.class).myaction4(a);
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
				def myaction(a : int) : float
				def myaction2(a : boolean)
			}
			capacity C2 {
				def myaction3(a : int) : float
				def myaction4(a : boolean)
			}
			skill S1 implements C1 {
				uses C2
				def myaction(a : int) : float {
					return myaction3(a)
				}
				def myaction2(a : boolean) {
					myaction4(a)
				}
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedC1,r.getGeneratedCode("C1"))
			Assert.assertEquals(expectedC2,r.getGeneratedCode("C2"))
			Assert.assertEquals(expectedS1,r.getGeneratedCode("S1"))
		])
	}

	@Test
	def void inheritance() {
		val expectedC1 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface CapTest1 extends Capacity {
			  public abstract int func1();
			}
		'''
		val expectedC2 = '''
			@SuppressWarnings("all")
			public interface CapTest2 extends CapTest1 {
			  public abstract void func2(final int a);
			}
		'''
		
		'''
			capacity CapTest1 {
				def func1 : int
			}
			capacity CapTest2 extends CapTest1 {
				def func2(a : int)
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedC1,r.getGeneratedCode("CapTest1"))
			Assert.assertEquals(expectedC2,r.getGeneratedCode("CapTest2"))
		])
	}

}
