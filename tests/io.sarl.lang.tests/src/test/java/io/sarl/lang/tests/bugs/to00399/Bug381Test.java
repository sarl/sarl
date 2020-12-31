/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * $Id$
 * 
 * Copyright (C) 2014-2021 the original authors or authors.
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
package io.sarl.lang.tests.bugs.to00399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
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
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #381")
@SuppressWarnings("all")
@Tag("core")
public class Bug381Test {

	protected static String snippetWithSarlSyntaxWithLocalType = multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def getInstance(type : Class<T>, defaultValue : T) : T with T extends MyObject",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	protected static String snippetWithJavaSyntaxWithLocalType = multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def <T extends MyObject> getInstance(type : Class<T>, defaultValue : T) : T",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	protected static String snippetWithSarlSyntaxWithJREType = multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def getInstance(type : Class<T>, defaultValue : T) : T with T extends Cloneable",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	protected static String snippetWithJavaSyntaxWithJREType = multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def <T extends Cloneable> getInstance(type : Class<T>, defaultValue : T) : T",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	protected static String snippetWithSarlSyntaxWithoutType = multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def getInstance(type : Class<T>, defaultValue : T) : T with T",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	protected static String snippetWithJavaSyntaxWithoutType = multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def <T> getInstance(type : Class<T>, defaultValue : T) : T",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	@Nested
	@Tag("sarlValidation")
	public class ParserTest extends AbstractSarlTest {

		@Test
		public void withSarlSyntaxWithLocalType() throws Exception {
			SarlScript mas = file(getParseHelper(), snippetWithSarlSyntaxWithLocalType);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void withJavaSyntaxWithLocalType() throws Exception {
			SarlScript mas = file(getParseHelper(), snippetWithJavaSyntaxWithLocalType);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void withSarlSyntaxWithJREType() throws Exception {
			SarlScript mas = file(getParseHelper(), snippetWithSarlSyntaxWithJREType);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void withJavaSyntaxWithJREType() throws Exception {
			SarlScript mas = file(getParseHelper(), snippetWithJavaSyntaxWithJREType);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void withSarlSyntaxWithoutType() throws Exception {
			SarlScript mas = file(getParseHelper(), snippetWithSarlSyntaxWithoutType);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void withJavaSyntaxWithoutType() throws Exception {
			SarlScript mas = file(getParseHelper(), snippetWithJavaSyntaxWithoutType);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

	}

	@Nested
	@Tag("compileToJava")
	public class CompilerTest extends AbstractSarlTest {
		
		@Test
		public void withSarlSyntaxWithLocalType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.AtomicSkillReference;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient AtomicSkillReference $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
					"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
					"    }",
					"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A1(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					"");

			getCompileHelper().compile(snippetWithSarlSyntaxWithLocalType,
					(r) -> assertEquals(expected, r.getGeneratedCode("A1")));
		}

		@Test
		public void withJavaSyntaxWithLocalType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.AtomicSkillReference;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient AtomicSkillReference $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
					"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
					"    }",
					"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A1(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					"");

			getCompileHelper().compile(snippetWithJavaSyntaxWithLocalType,
					(r) -> assertEquals(expected, r.getGeneratedCode("A1")));
		}

		@Test
		public void withSarlSyntaxWithJREType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.AtomicSkillReference;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient AtomicSkillReference $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
					"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
					"    }",
					"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A1(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					"");

			getCompileHelper().compile(snippetWithSarlSyntaxWithJREType, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expected, r.getGeneratedCode("A1"));
				}
			});
		}

		@Test
		public void withJavaSyntaxWithJREType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.AtomicSkillReference;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient AtomicSkillReference $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
					"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
					"    }",
					"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A1(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					"");

			getCompileHelper().compile(snippetWithJavaSyntaxWithJREType,
					(r) -> assertEquals(expected, r.getGeneratedCode("A1")));
		}

		@Test
		public void withSarlSyntaxWithoutType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.AtomicSkillReference;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient AtomicSkillReference $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
					"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
					"    }",
					"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A1(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					"");

			getCompileHelper().compile(snippetWithSarlSyntaxWithoutType, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expected, r.getGeneratedCode("A1"));
				}
			});
		}

		@Test
		public void withJavaSyntaxWithoutType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.AtomicSkillReference;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient AtomicSkillReference $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
					"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
					"    }",
					"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A1(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					"");

			getCompileHelper().compile(snippetWithJavaSyntaxWithoutType, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expected, r.getGeneratedCode("A1"));
				}
			});
		}

	}

}
