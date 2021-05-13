/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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
package io.sarl.lang.tests.general.compilation.aop;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper.Result;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: Skill")
@Tag("core")
@Tag("compileToJava")
public class SkillCompilerTest {

	@Nested
	@DisplayName("Field")
	public class FieldTest extends AbstractSarlTest {

		@Test
		public void fieldmodifier_none() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"  var field : int",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  private int field;",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public boolean equals(final Object obj) {",
					"    if (this == obj)",
					"      return true;",
					"    if (obj == null)",
					"      return false;",
					"    if (getClass() != obj.getClass())",
					"      return false;",
					"    S1 other = (S1) obj;",
					"    if (other.field != this.field)",
					"      return false;",
					"    return super.equals(obj);",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public int hashCode() {",
					"    int result = super.hashCode();",
					"    final int prime = 31;",
					"    result = prime * result + Integer.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, (r) -> assertEquals(expectedS1,r.getGeneratedCode("S1")));
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  int field;",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public boolean equals(final Object obj) {",
					"    if (this == obj)",
					"      return true;",
					"    if (obj == null)",
					"      return false;",
					"    if (getClass() != obj.getClass())",
					"      return false;",
					"    S1 other = (S1) obj;",
					"    if (other.field != this.field)",
					"      return false;",
					"    return super.equals(obj);",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public int hashCode() {",
					"    int result = super.hashCode();",
					"    final int prime = 31;",
					"    result = prime * result + Integer.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected int field;",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public boolean equals(final Object obj) {",
					"    if (this == obj)",
					"      return true;",
					"    if (obj == null)",
					"      return false;",
					"    if (getClass() != obj.getClass())",
					"      return false;",
					"    S1 other = (S1) obj;",
					"    if (other.field != this.field)",
					"      return false;",
					"    return super.equals(obj);",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public int hashCode() {",
					"    int result = super.hashCode();",
					"    final int prime = 31;",
					"    result = prime * result + Integer.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  private int field;",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public boolean equals(final Object obj) {",
					"    if (this == obj)",
					"      return true;",
					"    if (obj == null)",
					"      return false;",
					"    if (getClass() != obj.getClass())",
					"      return false;",
					"    S1 other = (S1) obj;",
					"    if (other.field != this.field)",
					"      return false;",
					"    return super.equals(obj);",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public int hashCode() {",
					"    int result = super.hashCode();",
					"    final int prime = 31;",
					"    result = prime * result + Integer.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public int field;",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public boolean equals(final Object obj) {",
					"    if (this == obj)",
					"      return true;",
					"    if (obj == null)",
					"      return false;",
					"    if (getClass() != obj.getClass())",
					"      return false;",
					"    S1 other = (S1) obj;",
					"    if (other.field != this.field)",
					"      return false;",
					"    return super.equals(obj);",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public int hashCode() {",
					"    int result = super.hashCode();",
					"    final int prime = 31;",
					"    result = prime * result + Integer.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  private final int field = 5;",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public boolean equals(final Object obj) {",
					"    if (this == obj)",
					"      return true;",
					"    if (obj == null)",
					"      return false;",
					"    if (getClass() != obj.getClass())",
					"      return false;",
					"    S1 other = (S1) obj;",
					"    if (other.field != this.field)",
					"      return false;",
					"    return super.equals(obj);",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public int hashCode() {",
					"    int result = super.hashCode();",
					"    final int prime = 31;",
					"    result = prime * result + Integer.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, (r) -> assertEquals(expectedS1,r.getGeneratedCode("S1")));
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  private static int field;",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  private transient int field;",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public boolean equals(final Object obj) {",
					"    if (this == obj)",
					"      return true;",
					"    if (obj == null)",
					"      return false;",
					"    if (getClass() != obj.getClass())",
					"      return false;",
					"    S1 other = (S1) obj;",
					"    if (other.field != this.field)",
					"      return false;",
					"    return super.equals(obj);",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public int hashCode() {",
					"    int result = super.hashCode();",
					"    final int prime = 31;",
					"    result = prime * result + Integer.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  private volatile int field;",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public boolean equals(final Object obj) {",
					"    if (this == obj)",
					"      return true;",
					"    if (obj == null)",
					"      return false;",
					"    if (getClass() != obj.getClass())",
					"      return false;",
					"    S1 other = (S1) obj;",
					"    if (other.field != this.field)",
					"      return false;",
					"    return super.equals(obj);",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public int hashCode() {",
					"    int result = super.hashCode();",
					"    final int prime = 31;",
					"    result = prime * result + Integer.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import java.util.Objects;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  private final int field1 = 5;",
					"  ",
					"  private final String field2 = \"\";",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public boolean equals(final Object obj) {",
					"    if (this == obj)",
					"      return true;",
					"    if (obj == null)",
					"      return false;",
					"    if (getClass() != obj.getClass())",
					"      return false;",
					"    S1 other = (S1) obj;",
					"    if (other.field1 != this.field1)",
					"      return false;",
					"    if (!Objects.equals(this.field2, other.field2))",
					"      return false;",
					"    return super.equals(obj);",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public int hashCode() {",
					"    int result = super.hashCode();",
					"    final int prime = 31;",
					"    result = prime * result + Integer.hashCode(this.field1);",
					"    result = prime * result + Objects.hashCode(this.field2);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
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
			getCompileHelper().compile(source, (r) -> {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				});
		}

	}

	@Nested
	@DisplayName("Action")
	public class ActionTest extends AbstractSarlTest {

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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class S1 extends Skill implements C1 {",
					"  public abstract void name();",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			final String expectedS2 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S2 extends S1 implements Capacity {",
					"  @Override",
					"  public void name() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S2() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S2(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, (r) -> {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public void name() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, (r) -> assertEquals(expectedS1,r.getGeneratedCode("S1")));
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public void name() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  protected void name() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  void name() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  private void name() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class S1 extends Skill implements C1 {",
					"  public abstract void name();",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, (r) -> assertEquals(expectedS1,r.getGeneratedCode("S1")));
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class S1 extends Skill implements C1 {",
					"  public abstract void name();",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
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
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
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
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public final void name() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public static void name() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public synchronized void name() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void missedActionImplementation_0() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  void myaction1(final int a);",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public void myaction1(final int a) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.myaction1(a);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"  void myaction2(final float b, final boolean c);",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C2> extends Capacity.ContextAwareCapacityWrapper<C> implements C2 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public void myaction2(final float b, final boolean c) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.myaction2(b, c);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1, C2 {",
					"  public void myaction1(final int x) {",
					"  }",
					"  ",
					"  public void myaction2(final float y, final boolean z) {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
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
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedC2,r.getGeneratedCode("C2"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

	}

	@Nested
	@DisplayName("Return type")
	public class ReturnTypeTest extends AbstractSarlTest {

		@Test
		public void compatibleReturnType_0() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C2> extends Capacity.ContextAwareCapacityWrapper<C> implements C2 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  @Pure",
					"  public Number myaction(final int a) {",
					"    return Double.valueOf(0.0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			final String expectedS2 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S2 extends S1 implements C2 {",
					"  @Pure",
					"  public Double myaction(final int a) {",
					"    return Double.valueOf(0.0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S2() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S2(final Agent arg0) {",
					"    super(arg0);",
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
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C2> extends Capacity.ContextAwareCapacityWrapper<C> implements C2 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  @Pure",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			final String expectedS2 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S2 extends S1 implements C2 {",
					"  @Pure",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S2() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S2(final Agent arg0) {",
					"    super(arg0);",
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
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  @Pure",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
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
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  float myaction(final int a);",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public float myaction(final int a) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.myaction(a);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public float myaction(final int a) {",
					"    return 0f;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
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
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

	}

	@Nested
	@DisplayName("As top element")
	public class TopElementTest extends AbstractSarlTest {

		@Test
		public void skillmodifier_none() throws Exception {
			String source = multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 { }"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"class S1 extends Skill implements C1 {",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class S1 extends Skill implements C1 {",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class S1 extends Skill implements C1 {",
					"  public abstract void name();",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public final class S1 extends Skill implements C1 {",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void capacityAccessors_inSkill() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  float myaction(final int a);",
					"  ",
					"  void myaction2(final boolean a);",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public float myaction(final int a) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.myaction(a);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void myaction2(final boolean a) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.myaction2(a);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C2 extends Capacity {",
					"  float myaction3(final int a);",
					"  ",
					"  void myaction4(final boolean a);",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C2> extends Capacity.ContextAwareCapacityWrapper<C> implements C2 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public float myaction3(final int a) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.myaction3(a);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void myaction4(final boolean a) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.myaction4(a);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.AtomicSkillReference;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public float myaction(final int a) {",
					"    C2 _$CAPACITY_USE$C2$CALLER = this.$CAPACITY_USE$C2$CALLER();",
					"    return _$CAPACITY_USE$C2$CALLER.myaction3(a);",
					"  }",
					"  ",
					"  public void myaction2(final boolean a) {",
					"    C2 _$CAPACITY_USE$C2$CALLER = this.$CAPACITY_USE$C2$CALLER();",
					"    _$CAPACITY_USE$C2$CALLER.myaction4(a);",
					"  }",
					"  ",
					"  @Extension",
					"  @ImportedCapacityFeature(C2.class)",
					"  @SyntheticMember",
					"  private transient AtomicSkillReference $CAPACITY_USE$C2;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  private C2 $CAPACITY_USE$C2$CALLER() {",
					"    if (this.$CAPACITY_USE$C2 == null || this.$CAPACITY_USE$C2.get() == null) {",
					"      this.$CAPACITY_USE$C2 = $getSkill(C2.class);",
					"    }",
					"    return $castSkill(C2.class, this.$CAPACITY_USE$C2);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public S1(final Agent arg0) {",
					"    super(arg0);",
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
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedC2,r.getGeneratedCode("C2"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void inheritance_00() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface CapTest1 extends Capacity {",
					"  int func1();",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends CapTest1> extends Capacity.ContextAwareCapacityWrapper<C> implements CapTest1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public int func1() {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.func1();",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import CapTest1;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface CapTest2 extends CapTest1 {",
					"  void func2(final int a);",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends CapTest2> extends CapTest1.ContextAwareCapacityWrapper<C> implements CapTest2 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public void func2(final int a) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.func2(a);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
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
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("CapTest1"));
					assertEquals(expectedC2,r.getGeneratedCode("CapTest2"));
				}
			});
		}

		@Test
		public void inheritance_01() throws Exception {
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface CapTest1 extends Capacity {",
					"  @DefaultValueSource",
					"  int func1(@DefaultValue(\"CapTest1#FUNC1_0\") final int a);",
					"  ",
					"  /**",
					"   * Default value for the parameter a",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"5\")",
					"  default int $DEFAULT_VALUE$FUNC1_0() {",
					"    return 5;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @SyntheticMember",
					"  default int func1() {",
					"    return func1($DEFAULT_VALUE$FUNC1_0());",
					"  }",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends CapTest1> extends Capacity.ContextAwareCapacityWrapper<C> implements CapTest1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public int func1(final int a) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.func1(a);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public int func1() {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.func1();",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import CapTest1;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface CapTest2 extends CapTest1 {",
					"  void func2(final int a);",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends CapTest2> extends CapTest1.ContextAwareCapacityWrapper<C> implements CapTest2 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public void func2(final int a) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.func2(a);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class SkillTest extends Skill implements CapTest2 {",
					"  public void func2(final int a) {",
					"  }",
					"  ",
					"  @DefaultValueSource",
					"  public int func1(@DefaultValue(\"CapTest1#FUNC1_0\") final int a) {",
					"    return 6;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public SkillTest() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public SkillTest(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			String source = multilineString(
					"capacity CapTest1 {",
					"  def func1(a : int = 5) : int",
					"}",
					"capacity CapTest2 extends CapTest1 {",
					"  def func2(a : int)",
					"}",
					"skill SkillTest implements CapTest2 {",
					"  def func2(a : int) { }",
					"  def func1(a : int) : int { 6 }",
					"}"
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1,r.getGeneratedCode("CapTest1"));
					assertEquals(expectedC2,r.getGeneratedCode("CapTest2"));
					assertEquals(expectedS1,r.getGeneratedCode("SkillTest"));
				}
			});
		}

	}

}
