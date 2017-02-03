/*
 * Copyright (C) 2014-2017 the original authors or authors.
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
package io.sarl.lang.tests.general.compilation.oop;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	InterfaceCompilerTest.TopLevelTest.class,
	InterfaceCompilerTest.InClassTest.class,
	InterfaceCompilerTest.InAgentTest.class,
	InterfaceCompilerTest.GenericTest.class,
})
@SuppressWarnings("all")
public class InterfaceCompilerTest {

	public static class TopLevelTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void basic() throws Exception {
			String source = "interface I1 { }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "interface I1 { var v = 45 }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public static int v = 45;",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "interface I1 { val v = 45 }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public final static int v = 45;",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "interface I1 { def fct }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract void fct();",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "interface I1 { def fct(a : int) }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract void fct(final int a);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "interface I1 { def fct(a : int*) }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract void fct(final int... a);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "interface I1 { def fct(a : int = 6) }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  @DefaultValueSource",
					"  public abstract void fct(@DefaultValue(\"I1#FCT_0\") final int a);",
					"  ",
					"  /**",
					"   * Default value for the parameter a",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"6\")",
					"  public final static int $DEFAULT_VALUE$FCT_0 = 6;",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @SyntheticMember",
					"  public abstract void fct();",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

	public static class InClassTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void basic() throws Exception {
			String source = "class Container { interface I1 { } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  public interface I1 {",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "class Container { interface I1 { var v = 45 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  public interface I1 {",
					"    public static int v = 45;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "class Container { interface I1 { val v = 45 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  public interface I1 {",
					"    public final static int v = 45;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "class Container { interface I1 { def fct } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  public interface I1 {",
					"    public abstract void fct();",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "class Container { interface I1 { def fct(a : int) } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  public interface I1 {",
					"    public abstract void fct(final int a);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "class Container { interface I1 { def fct(a : int*) } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  public interface I1 {",
					"    public abstract void fct(final int... a);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "class Container { interface I1 { def fct(a : int = 6) } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  public interface I1 {",
					"    @DefaultValueSource",
					"    public abstract void fct(@DefaultValue(\"Container$I1#FCT_0\") final int a);",
					"    ",
					"    /**",
					"     * Default value for the parameter a",
					"     */",
					"    @SyntheticMember",
					"    @SarlSourceCode(\"6\")",
					"    public final static int $DEFAULT_VALUE$FCT_0 = 6;",
					"    ",
					"    @DefaultValueUse(\"int\")",
					"    @SyntheticMember",
					"    public abstract void fct();",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

	public static class InAgentTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void basic() throws Exception {
			String source = "agent Container { interface I1 { } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  protected interface I1 {",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @SyntheticMember",
					"  public Container(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "agent Container { interface I1 { var v = 45 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  protected interface I1 {",
					"    public static int v = 45;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @SyntheticMember",
					"  public Container(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "agent Container { interface I1 { val v = 45 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  protected interface I1 {",
					"    public final static int v = 45;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @SyntheticMember",
					"  public Container(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "agent Container { interface I1 { def fct } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  protected interface I1 {",
					"    public abstract void fct();",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @SyntheticMember",
					"  public Container(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "agent Container { interface I1 { def fct(a : int) } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  protected interface I1 {",
					"    public abstract void fct(final int a);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @SyntheticMember",
					"  public Container(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "agent Container { interface I1 { def fct(a : int*) } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  protected interface I1 {",
					"    public abstract void fct(final int... a);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @SyntheticMember",
					"  public Container(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "agent Container { interface I1 { def fct(a : int = 6) } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  protected interface I1 {",
					"    @DefaultValueSource",
					"    public abstract void fct(@DefaultValue(\"Container$I1#FCT_0\") final int a);",
					"    ",
					"    /**",
					"     * Default value for the parameter a",
					"     */",
					"    @SyntheticMember",
					"    @SarlSourceCode(\"6\")",
					"    public final static int $DEFAULT_VALUE$FCT_0 = 6;",
					"    ",
					"    @DefaultValueUse(\"int\")",
					"    @SyntheticMember",
					"    public abstract void fct();",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @SyntheticMember",
					"  public Container(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

	public static class GenericTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void interfaceGeneric_X() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1<X> {",
					"	def setX(param : X)",
					"	def getX : X",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1<X extends Object> {",
					"  public abstract void setX(final X param);",
					"  ",
					"  public abstract X getX();",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void interfaceGeneric_XextendsNumber() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1<X extends Number> {",
					"	def setX(param : X)",
					"	def getX : X",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1<X extends Number> {",
					"  public abstract void setX(final X param);",
					"  ",
					"  public abstract X getX();",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void interfaceGeneric_XY() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1<X,Y> {",
					"	def getY : Y",
					"	def setX(param : X)",
					"	def getX : X",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1<X extends Object, Y extends Object> {",
					"  public abstract Y getY();",
					"  ",
					"  public abstract void setX(final X param);",
					"  ",
					"  public abstract X getX();",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void interfaceGeneric_XYextendsX() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1<X,Y extends X> {",
					"	def getY : Y",
					"	def setX(param : X)",
					"	def getX : X",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1<X extends Object, Y extends X> {",
					"  public abstract Y getY();",
					"  ",
					"  public abstract void setX(final X param);",
					"  ",
					"  public abstract X getX();",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_X_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def setX(param : X) : void with X",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract <X extends Object> void setX(final X param);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_X_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def <X> setX(param : X) : void",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract <X extends Object> void setX(final X param);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XextendsNumber_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def setX(param : X) : void with X extends Number",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract <X extends Number> void setX(final X param);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XextendsNumber_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def <X extends Number> setX(param : X) : void",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract <X extends Number> void setX(final X param);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XY_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def setX(param : X) : void with X, Y",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract <X extends Object, Y extends Object> void setX(final X param);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XY_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def <X, Y> setX(param : X) : void",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract <X extends Object, Y extends Object> void setX(final X param);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XYextendsX_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def setX(param : X) : void with X, Y extends X",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract <X extends Object, Y extends X> void setX(final X param);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XYextendsX_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def <X, Y extends X> setX(param : X) : void",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  public abstract <X extends Object, Y extends X> void setX(final X param);",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

}
