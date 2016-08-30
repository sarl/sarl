/*
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.lang.tests.compilation.aop;

import static org.junit.Assert.assertEquals;

import com.google.inject.Inject;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	SkillCompilerTest.FieldTest.class,
	SkillCompilerTest.ActionTest.class,
	SkillCompilerTest.ReturnTypeTest.class,
	SkillCompilerTest.TopElementTest.class,
})
@SuppressWarnings("all")
public class SkillCompilerTest {

	public static class FieldTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void fieldmodifier_none() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  var field : int",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected int field;",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, (r) -> assertEquals(expectedS1,r.getGeneratedCode("S1")));
		}

		@Test
		public void fieldmodifier_package() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  package var field : int",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  int field;",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void fieldmodifier_protected() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  protected var field : int",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected int field;",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void fieldmodifier_private() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  private var field : int",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  private int field;",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void fieldmodifier_public() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  public var field : int",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public int field;",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void fieldmodifier_final() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  val field : int = 5",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected final int field = 5;",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, (r) -> assertEquals(expectedS1,r.getGeneratedCode("S1")));
		}

		@Test
		public void fieldmodifier_static() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  static var field : int",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected static int field;",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void fieldmodifier_transient() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  transient var field : int",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected transient int field;",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void fieldmodifier_volatile() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  volatile var field : int",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected volatile int field;",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected final int field1 = 5;",
					"  ",
					"  protected final String field2 = \"\";",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
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
			this.compiler.compile(source, (r) -> {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				});
		}

	}

	public static class ActionTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void actionmodifier_override() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def name",
					"}",
					"skill S2 extends S1 {",
					"	override name {}",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public abstract class S1 extends Skill implements C1 {",
					"  public abstract void name();",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			final String expectedS2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S2 extends S1 implements Capacity {",
					"  @Override",
					"  public void name() {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S2() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S2(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, (r) -> {
					assertEquals(expectedS1, r.getGeneratedCode("S1"));
					assertEquals(expectedS2, r.getGeneratedCode("S2"));
				});
		}

		@Test
		public void actionmodifier_none() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  def name {}",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public void name() {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, (r) -> assertEquals(expectedS1,r.getGeneratedCode("S1")));
		}

		@Test
		public void actionmodifier_public() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  public def name {}",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public void name() {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void actionmodifier_protected() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  protected def name {}",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected void name() {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void actionmodifier_package() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  package def name {}",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  void name() {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void actionmodifier_private() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  private def name {}",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  private void name() {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void actionmodifier_abstract_explicit() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  abstract def name",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public abstract class S1 extends Skill implements C1 {",
					"  public abstract void name();",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, (r) -> assertEquals(expectedS1,r.getGeneratedCode("S1")));
		}

		@Test
		public void actionmodifier_abstract_implicit() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  def name",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public abstract class S1 extends Skill implements C1 {",
					"  public abstract void name();",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void actionmodifier_dispatch() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  dispatch def name(a : Integer) { }",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public void _name(final Integer a) {",
					"  }",
					"  ",
					"  public void name(final Integer a) {",
					"    _name(a);",
					"    return;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void actionmodifier_dispatch_final() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  dispatch final def name(a : Integer) { }",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public final void _name(final Integer a) {",
					"  }",
					"  ",
					"  public void name(final Integer a) {",
					"    _name(a);",
					"    return;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void actionmodifier_final() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  final def name { }",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public final void name() {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void actionmodifier_static() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  static def name { }",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public static void name() {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void actionmodifier_synchronized() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  synchronized def name { }",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public synchronized void name() {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void missedActionImplementation_0() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  public abstract void myaction1(final int a);",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"  public abstract void myaction2(final float b, final boolean c);",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1, C2 {",
					"  public void myaction1(final int x) {",
					"  }",
					"  ",
					"  public void myaction2(final float y, final boolean z) {",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
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
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  @Inline(value = \"0.0\", constantExpression = true)",
					"  public Number myaction(final int a) {",
					"    return Double.valueOf(0.0);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			final String expectedS2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S2 extends S1 implements C2 {",
					"  @Inline(value = \"0.0\", constantExpression = true)",
					"  public Double myaction(final int a) {",
					"    return Double.valueOf(0.0);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S2() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S2(final Agent owner) {",
					"    super(owner);",
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
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  @Inline(value = \"0.0\", constantExpression = true)",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			final String expectedS2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S2 extends S1 implements C2 {",
					"  @Inline(value = \"0.0\", constantExpression = true)",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S2() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S2(final Agent owner) {",
					"    super(owner);",
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
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  @Inline(value = \"0.0\", constantExpression = true)",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
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
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  public abstract float myaction(final int a);",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  @Inline(value = \"0.0\", constantExpression = true)",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
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

	public static class TopElementTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void skillmodifier_none() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 { }"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void skillmodifier_public() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"public skill S1 implements C1 { }"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void skillmodifier_package() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"package skill S1 implements C1 { }"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"class S1 extends Skill implements C1 {",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void skillmodifier_abstract() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"abstract skill S1 implements C1 { }"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public abstract class S1 extends Skill implements C1 {",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void skillmodifier_abstract_member() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  def name()",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public abstract class S1 extends Skill implements C1 {",
					"  public abstract void name();",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void skillmodifier_final() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"final skill S1 implements C1 { }"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public final class S1 extends Skill implements C1 {",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void capacityAccessors_inSkill() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  public abstract float myaction(final int a);",
					"  ",
					"  public abstract void myaction2(final boolean a);",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"  public abstract float myaction3(final int a);",
					"  ",
					"  public abstract void myaction4(final boolean a);",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Capacity;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public float myaction(final int a) {",
					"    C2 _$CAPACITY_USE$C2$CALLER = this.$CAPACITY_USE$C2 == null ? (this.$CAPACITY_USE$C2 = getSkill(C2.class)) : this.$CAPACITY_USE$C2;",
					"    return _$CAPACITY_USE$C2$CALLER.myaction3(a);",
					"  }",
					"  ",
					"  public void myaction2(final boolean a) {",
					"    C2 _$CAPACITY_USE$C2$CALLER = this.$CAPACITY_USE$C2 == null ? (this.$CAPACITY_USE$C2 = getSkill(C2.class)) : this.$CAPACITY_USE$C2;",
					"    _$CAPACITY_USE$C2$CALLER.myaction4(a);",
					"  }",
					"  ",
					"  @Extension",
					"  @ImportedCapacityFeature(C2.class)",
					"  @SyntheticMember",
					"  private transient C2 $CAPACITY_USE$C2;",
					"  ",
					"  @Inline(value = \"$CAPACITY_USE$C2 == null ? (this.$CAPACITY_USE$C2 = getSkill(C2.class)) : this.$CAPACITY_USE$C2\")",
					"  @SyntheticMember",
					"  private C2 $CAPACITY_USE$C2$CALLER() {",
					"    if (this.$CAPACITY_USE$C2 == null) {",
					"      this.$CAPACITY_USE$C2 = getSkill(C2.class);",
					"    }",
					"    return this.$CAPACITY_USE$C2;",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @SyntheticMember",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  @Override",
					"  protected <S extends Skill> S $setSkill(final S skill, final Class<? extends Capacity>... capacities) {",
					"    this.$CAPACITY_USE$C2 = null;",
					"    return super.$setSkill(skill, capacities);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  @Override",
					"  protected <S extends Capacity> S clearSkill(final Class<S> capacity) {",
					"    this.$CAPACITY_USE$C2 = null;",
					"    return super.clearSkill(capacity);",
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
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface CapTest1 extends Capacity {",
					"  public abstract int func1();",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
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