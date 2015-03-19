/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http:"",www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.compilation;

import static org.junit.Assert.assertEquals;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	CapacityCompilerTest.FieldTest.class,
	CapacityCompilerTest.ActionTest.class,
	CapacityCompilerTest.ReturnTypeTest.class,
	CapacityCompilerTest.SkillTest.class,
})
@SuppressWarnings("all")
public class CapacityCompilerTest {

	public static class FieldTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected final int field1 = 5;",
					"  ",
					"  protected final String field2 = \"\";",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated",
					"  public S1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  val field1 : int = 5",
					"  val field2 : String = \"\"",
					"}"
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

	}

	public static class ActionTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;


		@Test
		public void missedActionImplementation_0() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  public abstract void myaction1(final int a);",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"  public abstract void myaction2(final float b, final boolean c);",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1, C2 {",
					"  public void myaction1(final int x) {",
					"  }",
					"  ",
					"  public void myaction2(final float y, final boolean z) {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated",
					"  public S1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			String source = multilineString(
					"capacity C1 {",
					"  def myaction1(a : int)",
					"}",
					"capacity C2 {",
					"  def myaction2(b : float, c : boolean)",
					"}",
					"skill S1 implements C1, C2 {",
					"  def myaction1(x : int) { }",
					"  def myaction2(y : float, z : boolean) { }",
					"}"
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedC2,r.getGeneratedCode("C2"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

	}

	public static class ReturnTypeTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void compatibleReturnType_0() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public Number myaction(final int a) {",
					"    return Double.valueOf(0.0);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated",
					"  public S1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			final String expectedS2 = multilineString(
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S2 extends S1 implements C2 {",
					"  public Double myaction(final int a) {",
					"    return Double.valueOf(0.0);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated",
					"  public S2(final Agent owner) {",
					"    super(owner);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated",
					"  public S2() {",
					"    super();",
					"  }",
					"}",
					""
					);
			String source = multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"  def myaction(a : int) : Number {",
					"    return 0.0",
					"  }",
					"}",
					"skill S2 extends S1 implements C2 {",
					"  def myaction(a : int) : Double {",
					"    return 0.0",
					"  }",
					"}"
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedC2,r.getGeneratedCode("C2"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
					assertEquals(expectedS2,r.getGeneratedCode("S2"));
				}
			});
		}

		@Test
		public void compatibleReturnType_1() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated",
					"  public S1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			final String expectedS2 = multilineString(
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S2 extends S1 implements C2 {",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated",
					"  public S2(final Agent owner) {",
					"    super(owner);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated",
					"  public S2() {",
					"    super();",
					"  }",
					"}",
					""
					);
			String source = multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"  def myaction(a : int) : float {",
					"    return 0f",
					"  }",
					"}",
					"skill S2 extends S1 implements C2 {",
					"  def myaction(a : int) : float {",
					"    return 0f",
					"  }",
					"}"
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedC2,r.getGeneratedCode("C2"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
					assertEquals(expectedS2,r.getGeneratedCode("S2"));
				}
			});
		}

		@Test
		public void compatibleReturnType_2() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated",
					"  public S1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  def myaction(a : int) : float {",
					"    return 0f",
					"  }",
					"}"
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void compatibleReturnType_3() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  public abstract float myaction(final int a);",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated",
					"  public S1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			String source = multilineString(
					"capacity C1 {",
					"  def myaction(a : int) : float",
					"}",
					"skill S1 implements C1 {",
					"  def myaction(a : int) : float {",
					"    return 0f",
					"  }",
					"}"
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

	}

	public static class SkillTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void capacityAccessors_inSkill() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  public abstract float myaction(final int a);",
					"  ",
					"  public abstract void myaction2(final boolean a);",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"  public abstract float myaction3(final int a);",
					"  ",
					"  public abstract void myaction4(final boolean a);",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public float myaction(final int a) {",
					"    return this.myaction3(a);",
					"  }",
					"  ",
					"  public void myaction2(final boolean a) {",
					"    this.myaction4(a);",
					"  }",
					"  ",
					"  /**",
					"   * See the capacity {@link C2#myaction3(int)}.",
					"   * ",
					"   * @see C2#myaction3(int)",
					"   */",
					"  @Generated",
					"  @ImportedCapacityFeature(C2.class)",
					"  protected float myaction3(final int a) {",
					"    return getSkill(C2.class).myaction3(a);",
					"  }",
					"  ",
					"  /**",
					"   * See the capacity {@link C2#myaction4(boolean)}.",
					"   * ",
					"   * @see C2#myaction4(boolean)",
					"   */",
					"  @Generated",
					"  @ImportedCapacityFeature(C2.class)",
					"  protected void myaction4(final boolean a) {",
					"    getSkill(C2.class).myaction4(a);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated",
					"  public S1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			String source = multilineString(
					"capacity C1 {",
					"  def myaction(a : int) : float",
					"  def myaction2(a : boolean)",
					"}",
					"capacity C2 {",
					"  def myaction3(a : int) : float",
					"  def myaction4(a : boolean)",
					"}",
					"skill S1 implements C1 {",
					"  uses C2",
					"  def myaction(a : int) : float {",
					"    return myaction3(a)",
					"  }",
					"  def myaction2(a : boolean) {",
					"    myaction4(a)",
					"  }",
					"}"
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedC2,r.getGeneratedCode("C2"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void inheritance() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface CapTest1 extends Capacity {",
					"  public abstract int func1();",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"@SuppressWarnings(\"all\")",
					"public interface CapTest2 extends CapTest1 {",
					"  public abstract void func2(final int a);",
					"}",
					""
					);
			String source = multilineString(
					"capacity CapTest1 {",
					"  def func1 : int",
					"}",
					"capacity CapTest2 extends CapTest1 {",
					"  def func2(a : int)",
					"}"
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("CapTest1"));
					assertEquals(expectedC2,r.getGeneratedCode("CapTest2"));
				}
			});
		}

	}

}