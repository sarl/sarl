/*
 * Copyright (C) 2014-2015 the original authors or authors.
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
package io.sarl.lang.tests.compilation.general;

import com.google.inject.Inject;

import org.eclipse.xtext.junit4.XtextRunner;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.eclipse.xtext.junit4.InjectWith;

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.SARLVersion;

import org.junit.Test;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;

import io.sarl.tests.api.AbstractSarlTest;
import static org.junit.Assert.*;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	VarArgsCompilerTest.ActionTest.class,
	VarArgsCompilerTest.BehaviorTest.class,
	VarArgsCompilerTest.CapacityTest.class,
	VarArgsCompilerTest.EventTest.class,
	VarArgsCompilerTest.SkillTest.class,
})
@SuppressWarnings("all")
public class VarArgsCompilerTest {

	public static class ActionTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void action_singleParam() throws Exception {
			String source = multilineString(
					"agent A1 {",
					"  def myaction(arg : int*) {",
					"    System.out.println(arg)",
					"  }",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  protected void myaction(final int... arg) {",
					"    System.out.println(arg);",
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
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void action() throws Exception {
			String source = multilineString(
					"agent A1 {",
					"  def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"    System.out.println(arg3)",
					"  }",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  protected void myaction(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
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
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

	public static class BehaviorTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void action_singleParam() throws Exception {
			String source = multilineString(
					"behavior B1 {",
					"  def myaction(arg : int*) {",
					"    System.out.println(arg)",
					"  }",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import javax.annotation.Generated;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class B1 extends Behavior {",
					"  protected void myaction(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a behavior.",
					"   * @param owner - reference to the agent that is owning this behavior.",
					"   */",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public B1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void action() throws Exception {
			String source = multilineString(
					"behavior B1 {",
					"  def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"    System.out.println(arg3)",
					"  }",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import javax.annotation.Generated;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class B1 extends Behavior {",
					"  protected void myaction(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a behavior.",
					"   * @param owner - reference to the agent that is owning this behavior.",
					"   */",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public B1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void constructor_singleParam() throws Exception {
			String source = multilineString(
					"behavior B1 {",
					"  new(arg : int*) {",
					"    super(null) // must be never null in real code",
					"    System.out.println(arg)",
					"  }",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.core.Behavior;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class B1 extends Behavior {",
					"  public B1(final int... arg) {",
					"    super(null);",
					"    System.out.println(arg);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void constructor() throws Exception {
			String source = multilineString(
					"behavior B1 {",
					"  new(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"    super(null) // must be never null in real code",
					"    System.out.println(arg3)",
					"  }",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.core.Behavior;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class B1 extends Behavior {",
					"  public B1(final char arg1, final boolean arg2, final int... arg3) {",
					"    super(null);",
					"    System.out.println(arg3);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

	public static class CapacityTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void action_singleParam() throws Exception {
			String source = multilineString(
					"capacity C1 {",
					"  def myaction(arg : int*)",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  public abstract void myaction(final int... arg);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void action() throws Exception {
			String source = multilineString(
					"capacity C1 {",
					"  def myaction(arg1 : char, arg2 : boolean, arg3 : int*)",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  public abstract void myaction(final char arg1, final boolean arg2, final int... arg3);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

	public static class EventTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void action_singleParam() throws Exception {
			String source = multilineString(
					"event E1 {",
					"  new(arg : int*) {",
					"    System.out.println(arg)",
					"  }",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Event;",
					"import javax.annotation.Generated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class E1 extends Event {",
					"  public E1(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private final static long serialVersionUID = -595401426L;",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void constructor() throws Exception {
			String source = multilineString(
					"event E1 {",
					"  new(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"    System.out.println(arg3)",
					"  }",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Event;",
					"import javax.annotation.Generated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class E1 extends Event {",
					"  public E1(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"  ",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  private final static long serialVersionUID = 2202902528L;",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

	public static class SkillTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void action_singleParam() throws Exception {
			String source = multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"  def myaction(arg : int*) {",
					"    System.out.println(arg)",
					"  }",
					"}"
					);
			final String expectedC1 = multilineString(
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					""
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import javax.annotation.Generated;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public void myaction(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, (r) -> {
					assertEquals(expectedC1,r.getGeneratedCode("C1"));
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				});
		}

		@Test
		public void action() throws Exception {
			String source = multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"  def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"    System.out.println(arg3)",
					"  }",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import javax.annotation.Generated;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public void myaction(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill. The owning agent is unknown.",
					"   */",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public S1() {",
					"    super();",
					"  }",
					"  ",
					"  /**",
					"   * Construct a skill.",
					"   * @param owner - agent that is owning this skill.",
					"   */",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public S1(final Agent owner) {",
					"    super(owner);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void inSkillConstructor_singleParam() throws Exception {
			String source = multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"  new(arg : int*) {",
					"    System.out.println(arg)",
					"  }",
					"}"
					);
			final String expectedS1 = multilineString(
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public S1(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedS1,r.getGeneratedCode("S1"));
				}
			});
		}

		@Test
		public void inSkillConstructor() throws Exception {
			String source = multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"  new(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"    System.out.println(arg3)",
					"  }",
					"}"
					);
			final String expected = multilineString(
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public S1(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expected,r.getGeneratedCode("S1"));
				}
			});
		}

	}

}
