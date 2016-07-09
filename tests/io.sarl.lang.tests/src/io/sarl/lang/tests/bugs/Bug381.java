/*
 * $Id$
 * 
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.lang.tests.bugs;

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

		@Inject
		private CompilationTestHelper compiler;
		
		@Test
		public void withSarlSyntaxWithLocalType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.Capacity;",
					"import io.sarl.lang.core.Skill;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private transient C1 $CAPACITY_USE$C1;",
					"  ",
					"  @Inline(value = \"$CAPACITY_USE$C1 == null ? (this.$CAPACITY_USE$C1 = getSkill(C1.class)) : this.$CAPACITY_USE$C1\")",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null) {",
					"      this.$CAPACITY_USE$C1 = getSkill(C1.class);",
					"    }",
					"    return this.$CAPACITY_USE$C1;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Skill> S $setSkill(final S skill, final Class<? extends Capacity>[] capacities) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.$setSkill(skill, capacities);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Capacity> S clearSkill(final Class<S> capacity) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.clearSkill(capacity);",
					"  }",
					"}",
					"");

			this.compiler.compile(snippetWithSarlSyntaxWithLocalType,
					(r) -> assertEquals(expected, r.getGeneratedCode("A1")));
		}

		@Test
		public void withJavaSyntaxWithLocalType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.Capacity;",
					"import io.sarl.lang.core.Skill;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private transient C1 $CAPACITY_USE$C1;",
					"  ",
					"  @Inline(value = \"$CAPACITY_USE$C1 == null ? (this.$CAPACITY_USE$C1 = getSkill(C1.class)) : this.$CAPACITY_USE$C1\")",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null) {",
					"      this.$CAPACITY_USE$C1 = getSkill(C1.class);",
					"    }",
					"    return this.$CAPACITY_USE$C1;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Skill> S $setSkill(final S skill, final Class<? extends Capacity>[] capacities) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.$setSkill(skill, capacities);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Capacity> S clearSkill(final Class<S> capacity) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.clearSkill(capacity);",
					"  }",
					"}",
					"");

			this.compiler.compile(snippetWithJavaSyntaxWithLocalType,
					(r) -> assertEquals(expected, r.getGeneratedCode("A1")));
		}

		@Test
		public void withSarlSyntaxWithJREType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.Capacity;",
					"import io.sarl.lang.core.Skill;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private transient C1 $CAPACITY_USE$C1;",
					"  ",
					"  @Inline(value = \"$CAPACITY_USE$C1 == null ? (this.$CAPACITY_USE$C1 = getSkill(C1.class)) : this.$CAPACITY_USE$C1\")",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null) {",
					"      this.$CAPACITY_USE$C1 = getSkill(C1.class);",
					"    }",
					"    return this.$CAPACITY_USE$C1;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Skill> S $setSkill(final S skill, final Class<? extends Capacity>[] capacities) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.$setSkill(skill, capacities);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Capacity> S clearSkill(final Class<S> capacity) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.clearSkill(capacity);",
					"  }",
					"}",
					"");

			this.compiler.compile(snippetWithSarlSyntaxWithJREType, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.Capacity;",
					"import io.sarl.lang.core.Skill;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private transient C1 $CAPACITY_USE$C1;",
					"  ",
					"  @Inline(value = \"$CAPACITY_USE$C1 == null ? (this.$CAPACITY_USE$C1 = getSkill(C1.class)) : this.$CAPACITY_USE$C1\")",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null) {",
					"      this.$CAPACITY_USE$C1 = getSkill(C1.class);",
					"    }",
					"    return this.$CAPACITY_USE$C1;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Skill> S $setSkill(final S skill, final Class<? extends Capacity>[] capacities) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.$setSkill(skill, capacities);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Capacity> S clearSkill(final Class<S> capacity) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.clearSkill(capacity);",
					"  }",
					"}",
					"");

			this.compiler.compile(snippetWithJavaSyntaxWithJREType,
					(r) -> assertEquals(expected, r.getGeneratedCode("A1")));
		}

		@Test
		public void withSarlSyntaxWithoutType() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.ImportedCapacityFeature;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.Capacity;",
					"import io.sarl.lang.core.Skill;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private transient C1 $CAPACITY_USE$C1;",
					"  ",
					"  @Inline(value = \"$CAPACITY_USE$C1 == null ? (this.$CAPACITY_USE$C1 = getSkill(C1.class)) : this.$CAPACITY_USE$C1\")",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null) {",
					"      this.$CAPACITY_USE$C1 = getSkill(C1.class);",
					"    }",
					"    return this.$CAPACITY_USE$C1;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Skill> S $setSkill(final S skill, final Class<? extends Capacity>[] capacities) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.$setSkill(skill, capacities);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Capacity> S clearSkill(final Class<S> capacity) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.clearSkill(capacity);",
					"  }",
					"}",
					"");

			this.compiler.compile(snippetWithSarlSyntaxWithoutType, new IAcceptor<CompilationTestHelper.Result>() {
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
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import io.sarl.lang.core.Capacity;",
					"import io.sarl.lang.core.Skill;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Extension;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  @Extension",
					"  @ImportedCapacityFeature(C1.class)",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private transient C1 $CAPACITY_USE$C1;",
					"  ",
					"  @Inline(value = \"$CAPACITY_USE$C1 == null ? (this.$CAPACITY_USE$C1 = getSkill(C1.class)) : this.$CAPACITY_USE$C1\")",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private C1 $CAPACITY_USE$C1$CALLER() {",
					"    if (this.$CAPACITY_USE$C1 == null) {",
					"      this.$CAPACITY_USE$C1 = getSkill(C1.class);",
					"    }",
					"    return this.$CAPACITY_USE$C1;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Skill> S $setSkill(final S skill, final Class<? extends Capacity>[] capacities) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.$setSkill(skill, capacities);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Override",
					"  protected <S extends Capacity> S clearSkill(final Class<S> capacity) {",
					"    this.$CAPACITY_USE$C1 = null;",
					"    return super.clearSkill(capacity);",
					"  }",
					"}",
					"");

			this.compiler.compile(snippetWithJavaSyntaxWithoutType, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expected, r.getGeneratedCode("A1"));
				}
			});
		}

	}

}
