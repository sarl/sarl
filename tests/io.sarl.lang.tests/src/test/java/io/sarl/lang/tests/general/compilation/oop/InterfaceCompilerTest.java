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
package io.sarl.lang.tests.general.compilation.oop;

import static io.sarl.tests.api.tools.TestUtils.multilineString;

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
@DisplayName("Compilation: interface")
@Tag("core")
@Tag("compileToJava")
public class InterfaceCompilerTest {

	@Nested
	public class TopLevelTest extends AbstractSarlTest {

		@Test
		public void basic() throws Exception {
			String source = "interface I1 { }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "interface I1 { var v = 45 }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  static int v = 45;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "interface I1 { val v = 45 }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  static final int v = 45;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "interface I1 { def fct }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  void fct();",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "interface I1 { def fct(a : int) }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  void fct(final int a);",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "interface I1 { def fct(a : int*) }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  void fct(final int... a);",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "interface I1 { def fct(a : int = 6) }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  @DefaultValueSource",
					"  void fct(@DefaultValue(\"I1#FCT_0\") final int a);",
					"  ",
					"  /**",
					"   * Default value for the parameter a",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"6\")",
					"  default int $DEFAULT_VALUE$FCT_0() {",
					"    return 6;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @SyntheticMember",
					"  default void fct() {",
					"    fct($DEFAULT_VALUE$FCT_0());",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_4() throws Exception {
			String source = "interface I1 { def fct(a : int) : float { a + 1f } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  @Pure",
					"  default float fct(final int a) {",
					"    return (a + 1f);",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_5() throws Exception {
			String source = "interface I1 { def fct(a : int = 6) : float { a + 1f } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  @DefaultValueSource",
					"  @Pure",
					"  default float fct(@DefaultValue(\"I1#FCT_0\") final int a) {",
					"    return (a + 1f);",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter a",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"6\")",
					"  default int $DEFAULT_VALUE$FCT_0() {",
					"    return 6;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @SyntheticMember",
					"  @Pure",
					"  default float fct() {",
					"    return fct($DEFAULT_VALUE$FCT_0());",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class InClassTest extends AbstractSarlTest {

		@Test
		public void basic() throws Exception {
			String source = "class Container { interface I1 { } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  public interface I1 {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "class Container { interface I1 { var v = 45 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  public interface I1 {",
					"    static int v = 45;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "class Container { interface I1 { val v = 45 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  public interface I1 {",
					"    static final int v = 45;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "class Container { interface I1 { def fct } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  public interface I1 {",
					"    void fct();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "class Container { interface I1 { def fct(a : int) } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  public interface I1 {",
					"    void fct(final int a);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "class Container { interface I1 { def fct(a : int*) } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  public interface I1 {",
					"    void fct(final int... a);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "class Container { interface I1 { def fct(a : int = 6) } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  public interface I1 {",
					"    @DefaultValueSource",
					"    void fct(@DefaultValue(\"Container$I1#FCT_0\") final int a);",
					"    ",
					"    /**",
					"     * Default value for the parameter a",
					"     */",
					"    @Pure",
					"    @SyntheticMember",
					"    @SarlSourceCode(\"6\")",
					"    default int $DEFAULT_VALUE$FCT_0() {",
					"      return 6;",
					"    }",
					"    ",
					"    @DefaultValueUse(\"int\")",
					"    @SyntheticMember",
					"    default void fct() {",
					"      fct($DEFAULT_VALUE$FCT_0());",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class InAgentTest extends AbstractSarlTest {

		@Test
		public void basic() throws Exception {
			String source = "agent Container { interface I1 { } }";
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
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  protected interface I1 {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "agent Container { interface I1 { var v = 45 } }";
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
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  protected interface I1 {",
					"    static int v = 45;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "agent Container { interface I1 { val v = 45 } }";
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
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  protected interface I1 {",
					"    static final int v = 45;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "agent Container { interface I1 { def fct } }";
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
					"public class Container extends Agent {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  protected interface I1 {",
					"    void fct();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "agent Container { interface I1 { def fct(a : int) } }";
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
					"public class Container extends Agent {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  protected interface I1 {",
					"    void fct(final int a);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "agent Container { interface I1 { def fct(a : int*) } }";
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
					"public class Container extends Agent {",
					"  @FunctionalInterface",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  protected interface I1 {",
					"    void fct(final int... a);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "agent Container { interface I1 { def fct(a : int = 6) } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"  protected interface I1 {",
					"    @DefaultValueSource",
					"    void fct(@DefaultValue(\"Container$I1#FCT_0\") final int a);",
					"    ",
					"    /**",
					"     * Default value for the parameter a",
					"     */",
					"    @Pure",
					"    @SyntheticMember",
					"    @SarlSourceCode(\"6\")",
					"    default int $DEFAULT_VALUE$FCT_0() {",
					"      return 6;",
					"    }",
					"    ",
					"    @DefaultValueUse(\"int\")",
					"    @SyntheticMember",
					"    default void fct() {",
					"      fct($DEFAULT_VALUE$FCT_0());",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class GenericTest extends AbstractSarlTest {

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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1<X extends Object> {",
					"  void setX(final X param);",
					"  ",
					"  @Pure",
					"  X getX();",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1<X extends Number> {",
					"  void setX(final X param);",
					"  ",
					"  @Pure",
					"  X getX();",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1<X extends Object, Y extends Object> {",
					"  @Pure",
					"  Y getY();",
					"  ",
					"  void setX(final X param);",
					"  ",
					"  @Pure",
					"  X getX();",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1<X extends Object, Y extends X> {",
					"  @Pure",
					"  Y getY();",
					"  ",
					"  void setX(final X param);",
					"  ",
					"  @Pure",
					"  X getX();",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  <X extends Object> void setX(final X param);",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  <X extends Object> void setX(final X param);",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  <X extends Number> void setX(final X param);",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  <X extends Number> void setX(final X param);",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  <X extends Object, Y extends Object> void setX(final X param);",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  <X extends Object, Y extends Object> void setX(final X param);",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  <X extends Object, Y extends X> void setX(final X param);",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
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
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface I1 {",
					"  <X extends Object, Y extends X> void setX(final X param);",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

}
