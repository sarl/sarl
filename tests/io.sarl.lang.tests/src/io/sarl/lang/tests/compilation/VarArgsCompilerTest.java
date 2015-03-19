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
package io.sarl.lang.tests.compilation;

import com.google.inject.Inject;

import org.eclipse.xtext.junit4.XtextRunner;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.eclipse.xtext.junit4.InjectWith;

import io.sarl.lang.SARLInjectorProvider;

import org.junit.Test;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;

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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import java.util.UUID;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  public void myaction(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   */",
					"  @Generated",
					"  public A1(final UUID parentID) {",
					"    super(parentID, null);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Generated",
					"  public A1(final UUID parentID, final UUID agentID) {",
					"    super(parentID, agentID);",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import java.util.UUID;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  public void myaction(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   */",
					"  @Generated",
					"  public A1(final UUID parentID) {",
					"    super(parentID, null);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Generated",
					"  public A1(final UUID parentID, final UUID agentID) {",
					"    super(parentID, agentID);",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class B1 extends Behavior {",
					"  public void myaction(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a behavior.",
					"   * @param owner - reference to the agent that is owning this behavior.",
					"   */",
					"  @Generated",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class B1 extends Behavior {",
					"  public void myaction(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"  ",
					"  /**",
					"   * Construct a behavior.",
					"   * @param owner - reference to the agent that is owning this behavior.",
					"   */",
					"  @Generated",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Event;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class E1 extends Event {",
					"  public E1(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"  ",
					"  @Generated",
					"  private final static long serialVersionUID = 588370681L;",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Event;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class E1 extends Event {",
					"  public E1(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"  ",
					"  @Generated",
					"  private final static long serialVersionUID = 588370681L;",
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
			String expected = multilineString(
					"MULTIPLE FILES WERE GENERATED",
					"",
					"File 1 : /myProject/src-gen/C1.java",
					"",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					"",
					"File 2 : /myProject/src-gen/S1.java",
					"",
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public void myaction(final int... arg) {",
					"    System.out.println(arg);",
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
					"",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
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
			String expected = multilineString(
					"MULTIPLE FILES WERE GENERATED",
					"",
					"File 1 : /myProject/src-gen/C1.java",
					"",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					"",
					"File 2 : /myProject/src-gen/S1.java",
					"",
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public void myaction(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
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
					"",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
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
			String expected = multilineString(
					"MULTIPLE FILES WERE GENERATED",
					"",
					"File 1 : /myProject/src-gen/C1.java",
					"",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					"",
					"File 2 : /myProject/src-gen/S1.java",
					"",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public S1(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"}",
					"",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
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
			String expected = multilineString(
					"MULTIPLE FILES WERE GENERATED",
					"",
					"File 1 : /myProject/src-gen/C1.java",
					"",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"}",
					"",
					"File 2 : /myProject/src-gen/S1.java",
					"",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public S1(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"}",
					"",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

}
