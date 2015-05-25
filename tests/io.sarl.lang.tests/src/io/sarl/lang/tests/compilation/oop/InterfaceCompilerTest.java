/*
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.compilation.oop;

import static org.junit.Assert.assertEquals;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.tests.parsing.aop.BehaviorParsingTest;
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
					"import io.sarl.lang.annotation.Generated;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  @DefaultValueSource",
					"  public abstract void fct(@DefaultValue(\"I1#FCT_0\") final int a);",
					"  ",
					"  /**",
					"   * Default value for the parameter a",
					"   */",
					"  @Generated(\" 6\")",
					"  public final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_FCT_0 = 6;",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @Generated",
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
					"@SuppressWarnings(\"all\")",
					"public class Container {",
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
					"@SuppressWarnings(\"all\")",
					"public class Container {",
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
					"@SuppressWarnings(\"all\")",
					"public class Container {",
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
					"@SuppressWarnings(\"all\")",
					"public class Container {",
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
					"@SuppressWarnings(\"all\")",
					"public class Container {",
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
					"@SuppressWarnings(\"all\")",
					"public class Container {",
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
					"import io.sarl.lang.annotation.Generated;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  public interface I1 {",
					"    @DefaultValueSource",
					"    public abstract void fct(@DefaultValue(\"Container$I1#FCT_0\") final int a);",
					"    ",
					"    /**",
					"     * Default value for the parameter a",
					"     */",
					"    @Generated(\" 6\")",
					"    public final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_FCT_0 = 6;",
					"    ",
					"    @DefaultValueUse(\"int\")",
					"    @Generated",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import java.util.UUID;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  protected interface I1 {",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID) {",
					"    super(parentID, null);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID, final UUID agentID) {",
					"    super(parentID, agentID);",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import java.util.UUID;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  protected interface I1 {",
					"    public static int v = 45;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID) {",
					"    super(parentID, null);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID, final UUID agentID) {",
					"    super(parentID, agentID);",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import java.util.UUID;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  protected interface I1 {",
					"    public final static int v = 45;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID) {",
					"    super(parentID, null);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID, final UUID agentID) {",
					"    super(parentID, agentID);",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import java.util.UUID;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  protected interface I1 {",
					"    public abstract void fct();",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID) {",
					"    super(parentID, null);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID, final UUID agentID) {",
					"    super(parentID, agentID);",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import java.util.UUID;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  protected interface I1 {",
					"    public abstract void fct(final int a);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID) {",
					"    super(parentID, null);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID, final UUID agentID) {",
					"    super(parentID, agentID);",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import java.util.UUID;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  protected interface I1 {",
					"    public abstract void fct(final int... a);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID) {",
					"    super(parentID, null);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID, final UUID agentID) {",
					"    super(parentID, agentID);",
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
					"import io.sarl.lang.annotation.Generated;",
					"import io.sarl.lang.core.Agent;",
					"import java.util.UUID;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  protected interface I1 {",
					"    @DefaultValueSource",
					"    public abstract void fct(@DefaultValue(\"Container$I1#FCT_0\") final int a);",
					"    ",
					"    /**",
					"     * Default value for the parameter a",
					"     */",
					"    @Generated(\" 6\")",
					"    public final static int ___FORMAL_PARAMETER_DEFAULT_VALUE_FCT_0 = 6;",
					"    ",
					"    @DefaultValueUse(\"int\")",
					"    @Generated",
					"    public abstract void fct();",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID) {",
					"    super(parentID, null);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Generated",
					"  public Container(final UUID parentID, final UUID agentID) {",
					"    super(parentID, agentID);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

}
