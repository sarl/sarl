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
package io.sarl.lang.tests.compilation.oop;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

import java.util.Map;

import org.eclipse.xtext.common.types.JvmTypeConstraint;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.TypesPackage;

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
	ClassCompilerTest.TopLevelTest.class,
	ClassCompilerTest.InClassTest.class,
	ClassCompilerTest.InAgentTest.class,
	ClassCompilerTest.GenericTest.class,
})
@SuppressWarnings("all")
public class ClassCompilerTest {

	public static class TopLevelTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void basic() throws Exception {
			String source = "class C1 { }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "class C1 { var v = 45 }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  private int v = 45;",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "class C1 { val v = 45 }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  private final int v = 45;",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "class C1 { def fct { 4 } }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public int fct() {",
					"    return 4;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "class C1 { def fct(a : int) { a } }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public int fct(final int a) {",
					"    return a;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "class C1 { def fct(a : int*) { 5 } }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public int fct(final int... a) {",
					"    return 5;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "class C1 { def fct(a : int = 6) { 5 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import javax.annotation.Generated;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @DefaultValueSource",
					"  public int fct(@DefaultValue(\"C1#FCT_0\") final int a) {",
					"    return 5;",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter a",
					"   */",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @SarlSourceCode(\" 6\")",
					"  private final static int $DEFAULT_VALUE$FCT_0 = 6;",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public final int fct() {",
					"    return fct($DEFAULT_VALUE$FCT_0);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void methodOverriding_explicitReturnType() throws Exception {
			String source = multilineString(
					"package io.sarl.docs.reference.oop",
					"class Person {",
					"	var firstName : String",
					"	var lastName : String",
					"	def getFullName : String {",
					"		this.firstName + \" \" + this.lastName",
					"	}",
					"}",
					"class PersonEx extends Person {",
					"	var title : String",
					"	override getFullName : String {",
					"		return title + \" \" + super.fullName",
					"	}",
					"}");
			final String expectedPerson = multilineString(
					"package io.sarl.docs.reference.oop;",
					"",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Person {",
					"  private String firstName;",
					"  ",
					"  private String lastName;",
					"  ",
					"  @Pure",
					"  public String getFullName() {",
					"    return ((this.firstName + \" \") + this.lastName);",
					"  }",
					"}",
					"");
			final String expectedPersonEx = multilineString(
					"package io.sarl.docs.reference.oop;",
					"",
					"import io.sarl.docs.reference.oop.Person;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class PersonEx extends Person {",
					"  private String title;",
					"  ",
					"  @Override",
					"  @Pure",
					"  public String getFullName() {",
					"    String _fullName = super.getFullName();",
					"    return ((this.title + \" \") + _fullName);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedPerson, r.getGeneratedCode("io.sarl.docs.reference.oop.Person"));
					assertEquals(expectedPersonEx, r.getGeneratedCode("io.sarl.docs.reference.oop.PersonEx"));
				}
			});
		}
		
		public void methodOverriding_inferredReturnType() throws Exception {
			String source = multilineString(
					"package io.sarl.docs.reference.oop",
					"class Person {",
					"	var firstName : String",
					"	var lastName : String",
					"	def getFullName : String {",
					"		this.firstName + \" \" + this.lastName",
					"	}",
					"}",
					"class PersonEx extends Person {",
					"	var title : String",
					"	override getFullName {",
					"		return title + \" \" + super.fullName",
					"	}",
					"}");
			final String expectedPerson = multilineString(
					"package io.sarl.docs.reference.oop;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Person {",
					"  private String firstName;",
					"  ",
					"  private String lastName;",
					"  ",
					"  public String getFullName() {",
					"    return ((this.firstName + \" \") + this.lastName);",
					"  }",
					"}",
					"");
			final String expectedPersonEx = multilineString(
					"package io.sarl.docs.reference.oop;",
					"",
					"import io.sarl.docs.reference.oop.Person;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class PersonEx extends Person {",
					"  private String title;",
					"  ",
					"  @Override",
					"  public String getFullName() {",
					"    String _fullName = super.getFullName();",
					"    return ((this.title + \" \") + _fullName);",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedPerson, r.getGeneratedCode("io.sarl.docs.reference.oop.Person"));
					assertEquals(expectedPersonEx, r.getGeneratedCode("io.sarl.docs.reference.oop.PersonEx"));
				}
			});
		}
		
	}

	public static class InClassTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void basic() throws Exception {
			String source = "class Container { class C1 { } }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  public class C1 {",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "class Container { class C1 { var v = 45 } }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  public class C1 {",
					"    private int v = 45;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "class Container { class C1 { val v = 45 } }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  public class C1 {",
					"    private final int v = 45;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "class Container { class C1 { def fct { 4 } } }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  public class C1 {",
					"    public int fct() {",
					"      return 4;",
					"    }",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "class Container { class C1 { def fct(a : int) { a } } }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  public class C1 {",
					"    public int fct(final int a) {",
					"      return a;",
					"    }",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "class Container { class C1 { def fct(a : int*) { 5 } } }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  public class C1 {",
					"    public int fct(final int... a) {",
					"      return 5;",
					"    }",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "class Container { class C1 { def fct(a : int = 6) { 5 } } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import javax.annotation.Generated;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  public class C1 {",
					"    @DefaultValueSource",
					"    public int fct(@DefaultValue(\"Container$C1#FCT_0\") final int a) {",
					"      return 5;",
					"    }",
					"    ",
					"    /**",
					"     * Default value for the parameter a",
					"     */",
					"    @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"    @SarlSourceCode(\" 6\")",
					"    private final static int $DEFAULT_VALUE$FCT_0 = 6;",
					"    ",
					"    @DefaultValueUse(\"int\")",
					"    @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"    public final int fct() {",
					"      return fct($DEFAULT_VALUE$FCT_0);",
					"    }",
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
			String source = "agent Container { class C1 { } }";
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
					"public class Container extends Agent {",
					"  protected class C1 {",
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
			String source = "agent Container { class C1 { var v = 45 } }";
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
					"public class Container extends Agent {",
					"  protected class C1 {",
					"    private int v = 45;",
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
			String source = "agent Container { class C1 { val v = 45 } }";
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
					"public class Container extends Agent {",
					"  protected class C1 {",
					"    private final int v = 45;",
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
			String source = "agent Container { class C1 { def fct { 4 } } }";
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
					"public class Container extends Agent {",
					"  protected class C1 {",
					"    public int fct() {",
					"      return 4;",
					"    }",
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
			String source = "agent Container { class C1 { def fct(a : int) { a } } }";
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
					"public class Container extends Agent {",
					"  protected class C1 {",
					"    public int fct(final int a) {",
					"      return a;",
					"    }",
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
			String source = "agent Container { class C1 { def fct(a : int*) { 5 } } }";
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
					"public class Container extends Agent {",
					"  protected class C1 {",
					"    public int fct(final int... a) {",
					"      return 5;",
					"    }",
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
			String source = "agent Container { class C1 { def fct(a : int = 6) { 5 } } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  protected class C1 {",
					"    @DefaultValueSource",
					"    public int fct(@DefaultValue(\"Container$C1#FCT_0\") final int a) {",
					"      return 5;",
					"    }",
					"    ",
					"    /**",
					"     * Default value for the parameter a",
					"     */",
					"    @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"    @SarlSourceCode(\" 6\")",
					"    private final static int $DEFAULT_VALUE$FCT_0 = 6;",
					"    ",
					"    @DefaultValueUse(\"int\")",
					"    @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"    public final int fct() {",
					"      return fct($DEFAULT_VALUE$FCT_0);",
					"    }",
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
		public void classGeneric_X() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X> {",
					"	var x : X",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1<X extends Object> {",
					"  private X x;",
					"  ",
					"  public X setX(final X param) {",
					"    return this.x = param;",
					"  }",
					"  ",
					"  @Pure",
					"  public X getX() {",
					"    return this.x;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void classGeneric_XextendsNumber() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X extends Number> {",
					"	var x : X",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1<X extends Number> {",
					"  private X x;",
					"  ",
					"  public X setX(final X param) {",
					"    return this.x = param;",
					"  }",
					"  ",
					"  @Pure",
					"  public X getX() {",
					"    return this.x;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void classGeneric_XY() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X,Y> {",
					"	var x : X",
					"	def getY : Y { null }",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1<X extends Object, Y extends Object> {",
					"  private X x;",
					"  ",
					"  @Pure",
					"  public Y getY() {",
					"    return null;",
					"  }",
					"  ",
					"  public X setX(final X param) {",
					"    return this.x = param;",
					"  }",
					"  ",
					"  @Pure",
					"  public X getX() {",
					"    return this.x;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void classGeneric_XYextendsX() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X,Y extends X> {",
					"	var x : X",
					"	def getY : Y { null }",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1<X extends Object, Y extends X> {",
					"  private X x;",
					"  ",
					"  @Pure",
					"  public Y getY() {",
					"    return null;",
					"  }",
					"  ",
					"  public X setX(final X param) {",
					"    return this.x = param;",
					"  }",
					"  ",
					"  @Pure",
					"  public X getX() {",
					"    return this.x;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_X_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X { var xxx : X }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object> void setX(final X param) {",
					"    X xxx = null;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_X_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X> setX(param : X) : void { var xxx : X }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object> void setX(final X param) {",
					"    X xxx = null;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XextendsNumber_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X extends Number { var xxx : X }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Number> void setX(final X param) {",
					"    X xxx = null;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XextendsNumber_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X extends Number> setX(param : X) : void { var xxx : X }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Number> void setX(final X param) {",
					"    X xxx = null;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XY_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X, Y { var xxx : X; var yyy : Y }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object, Y extends Object> void setX(final X param) {",
					"    X xxx = null;",
					"    Y yyy = null;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XY_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X, Y> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object, Y extends Object> void setX(final X param) {",
					"    X xxx = null;",
					"    Y yyy = null;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XYextendsX_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X, Y extends X { var xxx : X; var yyy : Y }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object, Y extends X> void setX(final X param) {",
					"    X xxx = null;",
					"    Y yyy = null;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XYextendsX_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X, Y extends X> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object, Y extends X> void setX(final X param) {",
					"    X xxx = null;",
					"    Y yyy = null;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

}
