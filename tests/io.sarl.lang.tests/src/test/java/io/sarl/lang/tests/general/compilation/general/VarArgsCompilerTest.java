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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.general.compilation.general;

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
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: variadic parameters")
@Tag("core")
@Tag("compileToJava")
public class VarArgsCompilerTest {

	@Nested
	public class ActionTest extends AbstractSarlTest {

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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  protected void myaction(final int... arg) {",
					"    System.out.println(arg);",
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
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  protected void myaction(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
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
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class BehaviorTest extends AbstractSarlTest {

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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@SuppressWarnings(\"all\")",
					"public class B1 extends Behavior {",
					"  public void myaction(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public B1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@SuppressWarnings(\"all\")",
					"public class B1 extends Behavior {",
					"  public void myaction(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public B1(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Behavior;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@SuppressWarnings(\"all\")",
					"public class B1 extends Behavior {",
					"  public B1(final int... arg) {",
					"    super(null);",
					"    System.out.println(arg);",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Behavior;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@SuppressWarnings(\"all\")",
					"public class B1 extends Behavior {",
					"  public B1(final char arg1, final boolean arg2, final int... arg3) {",
					"    super(null);",
					"    System.out.println(arg3);",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class CapacityTest extends AbstractSarlTest {

		@Test
		public void action_singleParam() throws Exception {
			String source = multilineString(
					"capacity C1 {",
					"  def myaction(arg : int*)",
					"}"
					);
			String expected = multilineString(
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
					"  void myaction(final int... arg);",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public void myaction(final int... arg) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.myaction(arg);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void action() throws Exception {
			String source = multilineString(
					"capacity C1 {",
					"  def myaction(arg1 : char, arg2 : boolean, arg3 : int*)",
					"}"
					);
			String expected = multilineString(
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
					"  void myaction(final char arg1, final boolean arg2, final int... arg3);",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public void myaction(final char arg1, final boolean arg2, final int... arg3) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.myaction(arg1, arg2, arg3);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class EventTest extends AbstractSarlTest {

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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Event;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class E1 extends Event {",
					"  public E1(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = -595401426L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Event;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class E1 extends Event {",
					"  public E1(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 2202902528L;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class SkillTest extends AbstractSarlTest {

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
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public void myaction(final int... arg) {",
					"    System.out.println(arg);",
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
			getCompileHelper().compile(source, (r) -> {
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
					"  public void myaction(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
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
			getCompileHelper().compile(source, new IAcceptor<Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public S1(final int... arg) {",
					"    System.out.println(arg);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<Result>() {
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class S1 extends Skill implements C1 {",
					"  public S1(final char arg1, final boolean arg2, final int... arg3) {",
					"    System.out.println(arg3);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expected,r.getGeneratedCode("S1"));
				}
			});
		}

	}

}
