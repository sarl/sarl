/*
 * $Id$
 * 
 * Copyright (C) 2014-2018 the original authors or authors.
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

import com.google.inject.Inject;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper.Result;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

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
@RunWith(Suite.class)
@SuiteClasses({
	Bug381.ParserTest.class,
	Bug381.CompilerTest.class,
})
@SuppressWarnings("all")
public class Bug381 {

	protected static String snippetWithSarlSyntaxWithLocalType = AbstractSarlTest.multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def getInstance(type : Class<T>, defaultValue : T) : T with T extends MyObject",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	protected static String snippetWithJavaSyntaxWithLocalType = AbstractSarlTest.multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def <T extends MyObject> getInstance(type : Class<T>, defaultValue : T) : T",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	protected static String snippetWithSarlSyntaxWithJREType = AbstractSarlTest.multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def getInstance(type : Class<T>, defaultValue : T) : T with T extends Cloneable",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	protected static String snippetWithJavaSyntaxWithJREType = AbstractSarlTest.multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def <T extends Cloneable> getInstance(type : Class<T>, defaultValue : T) : T",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	protected static String snippetWithSarlSyntaxWithoutType = AbstractSarlTest.multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def getInstance(type : Class<T>, defaultValue : T) : T with T",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	protected static String snippetWithJavaSyntaxWithoutType = AbstractSarlTest.multilineString(
			"class MyObject { }",
			"capacity C1 {",
			"  def <T> getInstance(type : Class<T>, defaultValue : T) : T",
			"}",
			"agent A1 {",
			"  uses C1",
			"}");

	public static class ParserTest extends AbstractSarlTest {

		@Test
		public void withSarlSyntaxWithLocalType() throws Exception {
			SarlScript mas = file(snippetWithSarlSyntaxWithLocalType);
			validate(mas).assertNoErrors();
		}

		@Test
		public void withJavaSyntaxWithLocalType() throws Exception {
			SarlScript mas = file(snippetWithJavaSyntaxWithLocalType);
			validate(mas).assertNoErrors();
		}

		@Test
		public void withSarlSyntaxWithJREType() throws Exception {
			SarlScript mas = file(snippetWithSarlSyntaxWithJREType);
			validate(mas).assertNoErrors();
		}

		@Test
		public void withJavaSyntaxWithJREType() throws Exception {
			SarlScript mas = file(snippetWithJavaSyntaxWithJREType);
			validate(mas).assertNoErrors();
		}

		@Test
		public void withSarlSyntaxWithoutType() throws Exception {
			SarlScript mas = file(snippetWithSarlSyntaxWithoutType);
			validate(mas).assertNoErrors();
		}

		@Test
		public void withJavaSyntaxWithoutType() throws Exception {
			SarlScript mas = file(snippetWithJavaSyntaxWithoutType);
			validate(mas).assertNoErrors();
		}

	}

	public static class CompilerTest extends AbstractSarlTest {
		
		@Test
		public void withSarlSyntaxWithLocalType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import io.sarl.lang.core.Skill;",
					"import io.sarl.lang.util.ClearableReference;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  @Inline(value = \"$castSkill(C1.class, ($0$CAPACITY_USE$C1 == null || $0$CAPACITY_USE$C1.get() == null) ? ($0$CAPACITY_USE$C1 = $0$getSkill(C1.class)) : $0$CAPACITY_USE$C1)\", imported = C1.class)",
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
					"  @Deprecated",
					"  @Inject",
					"  public A1(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
					"    super(arg0, arg1, arg2);",
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
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import io.sarl.lang.core.Skill;",
					"import io.sarl.lang.util.ClearableReference;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  @Inline(value = \"$castSkill(C1.class, ($0$CAPACITY_USE$C1 == null || $0$CAPACITY_USE$C1.get() == null) ? ($0$CAPACITY_USE$C1 = $0$getSkill(C1.class)) : $0$CAPACITY_USE$C1)\", imported = C1.class)",
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
					"  @Deprecated",
					"  @Inject",
					"  public A1(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
					"    super(arg0, arg1, arg2);",
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
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import io.sarl.lang.core.Skill;",
					"import io.sarl.lang.util.ClearableReference;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  @Inline(value = \"$castSkill(C1.class, ($0$CAPACITY_USE$C1 == null || $0$CAPACITY_USE$C1.get() == null) ? ($0$CAPACITY_USE$C1 = $0$getSkill(C1.class)) : $0$CAPACITY_USE$C1)\", imported = C1.class)",
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
					"  @Deprecated",
					"  @Inject",
					"  public A1(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
					"    super(arg0, arg1, arg2);",
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
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import io.sarl.lang.core.Skill;",
					"import io.sarl.lang.util.ClearableReference;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  @Inline(value = \"$castSkill(C1.class, ($0$CAPACITY_USE$C1 == null || $0$CAPACITY_USE$C1.get() == null) ? ($0$CAPACITY_USE$C1 = $0$getSkill(C1.class)) : $0$CAPACITY_USE$C1)\", imported = C1.class)",
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
					"  @Deprecated",
					"  @Inject",
					"  public A1(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
					"    super(arg0, arg1, arg2);",
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
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import io.sarl.lang.core.Skill;",
					"import io.sarl.lang.util.ClearableReference;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  @Inline(value = \"$castSkill(C1.class, ($0$CAPACITY_USE$C1 == null || $0$CAPACITY_USE$C1.get() == null) ? ($0$CAPACITY_USE$C1 = $0$getSkill(C1.class)) : $0$CAPACITY_USE$C1)\", imported = C1.class)",
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
					"  @Deprecated",
					"  @Inject",
					"  public A1(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
					"    super(arg0, arg1, arg2);",
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
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import io.sarl.lang.core.Skill;",
					"import io.sarl.lang.util.ClearableReference;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @SyntheticMember",
					"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
					"  ",
					"  @SyntheticMember",
					"  @Pure",
					"  @Inline(value = \"$castSkill(C1.class, ($0$CAPACITY_USE$C1 == null || $0$CAPACITY_USE$C1.get() == null) ? ($0$CAPACITY_USE$C1 = $0$getSkill(C1.class)) : $0$CAPACITY_USE$C1)\", imported = C1.class)",
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
					"  @Deprecated",
					"  @Inject",
					"  public A1(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
					"    super(arg0, arg1, arg2);",
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
