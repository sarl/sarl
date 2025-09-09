/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.api.core.tests.bugs;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestUtils.multilineString2;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Guards are not evaluated when the event is fired across multiple kernels.
 *
 * <p>https://github.com/sarl/sarl/issues/885
 *
 * @author $Author: sgalland$
 * @author $Author: alombard$
 * @version api.core 0.15.0 20250909-115748
 * @mavengroupid io.sarl.sdk
 * @mavenartifactid api.core
 * @see "https://github.com/sarl/sarl/issues/885"
 */
@DisplayName("Bug #885")
@SuppressWarnings("all")
@Tag("core")
public class Bug885Test extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

	private static final String SNIPSET_LOCAL_PARAMETER = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>, p : Address) {",
			"    f [ it.UUID == p.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_LOCAL_PARAMETER = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s, final Address p) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = p.getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, p.getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: local parameter")
	@Tag("sarlValidation")
	public void parsingLocalParameter() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_LOCAL_PARAMETER);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}


	@Test
	@DisplayName("Compile: local parameter")
	@Tag("compileToJava")
	public void compilingLocalParameter() throws Exception {
		this.compiler.compile(SNIPSET_LOCAL_PARAMETER, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_LOCAL_PARAMETER, actual);
		});
	}

	private static final String SNIPSET_LOCAL_VARIABLE = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    var p : Address = null",
			"    f [ it.UUID == p.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_LOCAL_VARIABLE = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    Address p = null;",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = p.getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, p.getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: local variable")
	@Tag("sarlValidation")
	public void parseLocalVariable() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_LOCAL_VARIABLE);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: local variable")
	@Tag("compileToJava")
	public void compilingLocalVariable() throws Exception {
		this.compiler.compile(SNIPSET_LOCAL_VARIABLE, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_LOCAL_VARIABLE, actual);
		});
	}

	private static final String SNIPSET_FIELD = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  var p : Address = null",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == p.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_FIELD = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  private Address p = null;",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = Boot.this.p.getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.p.getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: field")
	@Tag("sarlValidation")
	public void parseField() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_FIELD);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: field")
	@Tag("compileToJava")
	public void compilingField() throws Exception {
		this.compiler.compile(SNIPSET_FIELD, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_FIELD, actual);
		});
	}

	private static final String SNIPSET_SUPER_FIELD = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"agent SuperBoot {",
			"  protected var p : Address = null",
			"}",
			"agent Boot extends SuperBoot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == p.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_SUPER_FIELD = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends SuperBoot {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = Boot.this.p.getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.p.getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: super's field")
	@Tag("sarlValidation")
	public void parseSuperField() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_SUPER_FIELD);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: super's field")
	@Tag("compileToJava")
	public void compilingSuperField() throws Exception {
		this.compiler.compile(SNIPSET_SUPER_FIELD, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_SUPER_FIELD, actual);
		});
	}

	private static final String SNIPSET_LOCAL_FUNCTION_01 = multilineString2(false,
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == callF.UUID ]",
			"  }",
			"  def callF : Address { null }",
			"}");

	private static final String EXPECTED_LOCAL_FUNCTION_01 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = Boot.this.callF().getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.callF().getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Pure",
			"  protected Address callF() {",
			"    return null;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: local function with Address")
	@Tag("sarlValidation")
	public void parseLocalFunction_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_LOCAL_FUNCTION_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: local function with Address")
	@Tag("compileToJava")
	public void compilingLocalFunction_01() throws Exception {
		this.compiler.compile(SNIPSET_LOCAL_FUNCTION_01, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_LOCAL_FUNCTION_01, actual);
		});
	}

	private static final String SNIPSET_LOCAL_FUNCTION_02 = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == callF ]",
			"  }",
			"  def callF : UUID { null }",
			"}");

	private static final String EXPECTED_LOCAL_FUNCTION_02 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_callF;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_callF) {",
			"        this.$_callF = $_callF;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_callF);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _callF = Boot.this.callF();",
			"        return Objects.equals(_uUID, _callF);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.callF());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Pure",
			"  protected UUID callF() {",
			"    return null;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: local function with UUID")
	@Tag("sarlValidation")
	public void parseLocalFunction_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_LOCAL_FUNCTION_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: local function with UUID")
	@Tag("compileToJava")
	public void compilingLocalFunction_02() throws Exception {
		this.compiler.compile(SNIPSET_LOCAL_FUNCTION_02, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_LOCAL_FUNCTION_02, actual);
		});
	}

	private static final String SNIPSET_SUPER_FUNCTION_01 = multilineString2(false,
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"agent SuperBoot {",
			"  def callF : Address { null }",
			"}",
			"agent Boot extends SuperBoot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == callF.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_SUPER_FUNCTION_01 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends SuperBoot {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = Boot.this.callF().getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.callF().getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: inherited function with Address")
	@Tag("sarlValidation")
	public void parseSuperFunction_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_SUPER_FUNCTION_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: inherited function with Address")
	@Tag("compileToJava")
	public void compilingSuperFunction_01() throws Exception {
		this.compiler.compile(SNIPSET_SUPER_FUNCTION_01, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_SUPER_FUNCTION_01, actual);
		});
	}

	private static final String SNIPSET_SUPER_FUNCTION_02 = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"",
			"agent SuperBoot {",
			"  def callF : UUID { null }",
			"}",
			"agent Boot extends SuperBoot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == callF ]",
			"  }",
			"}");

	private static final String EXPECTED_SUPER_FUNCTION_02 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends SuperBoot {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_callF;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_callF) {",
			"        this.$_callF = $_callF;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_callF);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _callF = Boot.this.callF();",
			"        return Objects.equals(_uUID, _callF);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.callF());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: inherited function with UUID")
	@Tag("sarlValidation")
	public void parseSuperFunction_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_SUPER_FUNCTION_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: inherited function with UUID")
	@Tag("compileToJava")
	public void compilingSuperFunction_02() throws Exception {
		this.compiler.compile(SNIPSET_SUPER_FUNCTION_02, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_SUPER_FUNCTION_02, actual);
		});
	}

	private static final String SNIPSET_EXTENSION_FIELD = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"class Provider {",
			"  def callF : Address { null }",
			"}",
			"agent Boot {",
			"  extension var p : Provider",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == callF.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_EXTENSION_FIELD = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  @Extension",
			"  private Provider p;",
			"",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = Boot.this.p.callF().getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.p.callF().getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse:: extension field")
	@Tag("sarlValidation")
	public void parseExtensionField() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_EXTENSION_FIELD);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile:: extension field")
	@Tag("compileToJava")
	public void compilingExtensionField() throws Exception {
		this.compiler.compile(SNIPSET_EXTENSION_FIELD, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_EXTENSION_FIELD, actual);
		});
	}

	private static final String SNIPSET_EXTENSION_VARIABLE = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"class Provider {",
			"  def callF : Address { null }",
			"}",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    extension var p : Provider",
			"    f [ it.UUID == callF.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_EXTENSION_VARIABLE = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    @Extension",
			"    Provider p = null;",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = p.callF().getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, p.callF().getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: extension variable")
	@Tag("sarlValidation")
	public void parseExtensionVariable() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_EXTENSION_VARIABLE);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: extension variable")
	@Tag("compileToJava")
	public void compilingExtensionVariable() throws Exception {
		this.compiler.compile(SNIPSET_EXTENSION_VARIABLE, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_EXTENSION_VARIABLE, actual);
		});
	}

	private static final String SNIPSET_EXTENSION_PARAMETER = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"class Provider {",
			"  def callF : Address { null }",
			"}",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>, extension p : Provider) {",
			"    f [ it.UUID == callF.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_EXTENSION_PARAMETER = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s, @Extension final Provider p) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = p.callF().getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, p.callF().getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: extension parameter")
	@Tag("sarlValidation")
	public void parseExtensionParameter() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_EXTENSION_PARAMETER);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Parse: extension parameter")
	@Tag("compileToJava")
	public void compilingExtensionParameter() throws Exception {
		this.compiler.compile(SNIPSET_EXTENSION_PARAMETER, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_EXTENSION_PARAMETER, actual);
		});
	}

	private static final String SNIPSET_STATIC_IMPORT = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"import static foo.StaticTools.*",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == getAdr.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_STATIC_IMPORT = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import foo.StaticTools;",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = StaticTools.getAdr().getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, StaticTools.getAdr().getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: static import")
	@Tag("sarlValidation")
	public void parseStaticImport() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_STATIC_IMPORT);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: static import")
	@Tag("compileToJava")
	public void compilingStaticImport() throws Exception {
		this.compiler.compile(SNIPSET_STATIC_IMPORT, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_STATIC_IMPORT, actual);
		});
	}

	private static final String SNIPSET_STATIC_EXTENSION_IMPORT = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"import static extension foo.StaticTools.*",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == new Object().getAdr2.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_STATIC_EXTENSION_IMPORT = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import foo.StaticTools;",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = StaticTools.getAdr2(new Object()).getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, StaticTools.getAdr2(new Object()).getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: extension static import")
	@Tag("sarlValidation")
	public void parseStaticExtensionImport() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_STATIC_EXTENSION_IMPORT);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: extension static import")
	@Tag("compileToJava")
	public void compilingStaticExtensionImport() throws Exception {
		this.compiler.compile(SNIPSET_STATIC_EXTENSION_IMPORT, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_STATIC_EXTENSION_IMPORT, actual);
		});
	}

	private static final String SNIPSET_LOCAL_INSTANCE_CREATION_01 = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == callF(new Object()) ]",
			"  }",
			"  def callF(p : Object) : UUID { null }",
			"}");

	private static final String EXPECTED_LOCAL_INSTANCE_CREATION_01 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_callF;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_callF) {",
			"        this.$_callF = $_callF;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_callF);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        Object _object = new Object();",
			"        UUID _callF = Boot.this.callF(_object);",
			"        return Objects.equals(_uUID, _callF);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.callF(new Object()));",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Pure",
			"  protected UUID callF(final Object p) {",
			"    return null;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: object creation in argument")
	@Tag("sarlValidation")
	public void parseLocalInstanceCreation01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_LOCAL_INSTANCE_CREATION_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: object creation in argument")
	@Tag("compileToJava")
	public void compilingLocalInstanceCreation01() throws Exception {
		this.compiler.compile(SNIPSET_LOCAL_INSTANCE_CREATION_01, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_LOCAL_INSTANCE_CREATION_01, actual);
		});
	}

	private static final String SNIPSET_LOCAL_INSTANCE_CREATION_02 = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == new Object ]",
			"  }",
			"}");

	private static final String EXPECTED_LOCAL_INSTANCE_CREATION_02 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      public $SerializableClosureProxy() {",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        Object _object = new Object();",
			"        return Objects.equals(_uUID, _object);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        Object _object = new Object();",
			"        return Objects.equals(_uUID, _object);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class);",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: object creation")
	@Tag("sarlValidation")
	public void parseLocalInstanceCreation02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_LOCAL_INSTANCE_CREATION_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: object creation")
	@Tag("compileToJava")
	public void compilingLocalInstanceCreation02() throws Exception {
		this.compiler.compile(SNIPSET_LOCAL_INSTANCE_CREATION_02, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_LOCAL_INSTANCE_CREATION_02, actual);
		});
	}

	private static final String SNIPSET_ARITHMETIC_PARAMETER = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == callF(1+2*3) ]",
			"  }",
			"  def callF(p : Object) : UUID { null }",
			"}");

	private static final String EXPECTED_ARITHMETIC_PARAMETER = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_callF;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_callF) {",
			"        this.$_callF = $_callF;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_callF);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _callF = Boot.this.callF(Integer.valueOf((1 + (2 * 3))));",
			"        return Objects.equals(_uUID, _callF);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.callF(Integer.valueOf((1 + (2 * 3)))));",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Pure",
			"  protected UUID callF(final Object p) {",
			"    return null;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: airthmetic expression as argument")
	@Tag("sarlValidation")
	public void parseArithmetiParameter() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_ARITHMETIC_PARAMETER);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: airthmetic expression as argument")
	@Tag("compileToJava")
	public void compilingArithmetiParameter() throws Exception {
		this.compiler.compile(SNIPSET_ARITHMETIC_PARAMETER, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_ARITHMETIC_PARAMETER, actual);
		});
	}

	private static final String SNIPSET_IF_THEN_ON_IT = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ if (it !== null) { it.UUID == callF(1+2*3) } else { false } ]",
			"  }",
			"  def callF(p : Object) : UUID { null }",
			"}");

	private static final String EXPECTED_IF_THEN_ON_IT = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_callF;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_callF) {",
			"        this.$_callF = $_callF;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xifexpression = false;",
			"        if ((it != null)) {",
			"          UUID _uUID = it.getID();",
			"          _xifexpression = Objects.equals(_uUID, $_callF);",
			"        } else {",
			"          _xifexpression = false;",
			"        }",
			"        return _xifexpression;",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xifexpression = false;",
			"        if ((it != null)) {",
			"          UUID _uUID = it.getID();",
			"          UUID _callF = Boot.this.callF(Integer.valueOf((1 + (2 * 3))));",
			"          _xifexpression = Objects.equals(_uUID, _callF);",
			"        } else {",
			"          _xifexpression = false;",
			"        }",
			"        return _xifexpression;",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.callF(Integer.valueOf((1 + (2 * 3)))));",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Pure",
			"  protected UUID callF(final Object p) {",
			"    return null;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: branch on it")
	@Tag("sarlValidation")
	public void parseIfThenOnIt() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_IF_THEN_ON_IT);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: branch on it")
	@Tag("compileToJava")
	public void compilingIfThenOnIt() throws Exception {
		this.compiler.compile(SNIPSET_IF_THEN_ON_IT, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_IF_THEN_ON_IT, actual);
		});
	}

	private static final String SNIPSET_IF_THEN_ON_LOCAL_PARAMETER = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ if (s !== null) { it.UUID == callF(1+2*3) } else { false } ]",
			"  }",
			"  def callF(p : Object) : UUID { null }",
			"}");

	private static final String EXPECTED_IF_THEN_ON_LOCAL_PARAMETER = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final Scope<Address> s;",
			"      ",
			"      private final UUID $_callF;",
			"      ",
			"      public $SerializableClosureProxy(final Scope<Address> s, final UUID $_callF) {",
			"        this.s = s;",
			"        this.$_callF = $_callF;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xifexpression = false;",
			"        if ((s != null)) {",
			"          UUID _uUID = it.getID();",
			"          _xifexpression = Objects.equals(_uUID, $_callF);",
			"        } else {",
			"          _xifexpression = false;",
			"        }",
			"        return _xifexpression;",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xifexpression = false;",
			"        if ((s != null)) {",
			"          UUID _uUID = it.getID();",
			"          UUID _callF = Boot.this.callF(Integer.valueOf((1 + (2 * 3))));",
			"          _xifexpression = Objects.equals(_uUID, _callF);",
			"        } else {",
			"          _xifexpression = false;",
			"        }",
			"        return _xifexpression;",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, s, Boot.this.callF(Integer.valueOf((1 + (2 * 3)))));",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Pure",
			"  protected UUID callF(final Object p) {",
			"    return null;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: branch in local parameter")
	@Tag("sarlValidation")
	public void parseIfThenOnLocalParameter() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_IF_THEN_ON_LOCAL_PARAMETER);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: branch in local parameter")
	@Tag("compileToJava")
	public void compilingIfThenOnLocalParameter() throws Exception {
		this.compiler.compile(SNIPSET_IF_THEN_ON_LOCAL_PARAMETER, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_IF_THEN_ON_LOCAL_PARAMETER, actual);
		});
	}

	private static final String SNIPSET_MULTIPLE_LAMBDA = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == callF(1+2*3) ]",
			"    f [ it.UUID != callF(1+2*3) ]",
			"  }",
			"  def callF(p : Object) : UUID { null }",
			"}");

	private static final String EXPECTED_MULTIPLE_LAMBDA = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_callF;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_callF) {",
			"        this.$_callF = $_callF;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_callF);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _callF = Boot.this.callF(Integer.valueOf((1 + (2 * 3))));",
			"        return Objects.equals(_uUID, _callF);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.callF(Integer.valueOf((1 + (2 * 3)))));",
			"      }",
			"    };",
			"    this.f(_function);",
			"    class $SerializableClosureProxy_1 implements Scope<Address> {",
			"      ",
			"      private final UUID $_callF;",
			"      ",
			"      public $SerializableClosureProxy_1(final UUID $_callF) {",
			"        this.$_callF = $_callF;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return (!Objects.equals(_uUID, $_callF));",
			"      }",
			"    }",
			"    final Scope<Address> _function_1 = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _callF = Boot.this.callF(Integer.valueOf((1 + (2 * 3))));",
			"        return (!Objects.equals(_uUID, _callF));",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy_1.class, Boot.this.callF(Integer.valueOf((1 + (2 * 3)))));",
			"      }",
			"    };",
			"    this.f(_function_1);",
			"  }",
			"",
			"  @Pure",
			"  protected UUID callF(final Object p) {",
			"    return null;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: multiple lambdas")
	@Tag("sarlValidation")
	public void parseMultipleLambda() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_MULTIPLE_LAMBDA);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: multiple lambdas")
	@Tag("compileToJava")
	public void compilingMultipleLambda() throws Exception {
		this.compiler.compile(SNIPSET_MULTIPLE_LAMBDA, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_MULTIPLE_LAMBDA, actual);
		});
	}

	private static final String SNIPSET_LOCAL_STATIC_FUNCTION_01 = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == callF.UUID ]",
			"  }",
			"  static def callF : Address { null }",
			"}");

	private static final String EXPECTED_LOCAL_STATIC_FUNCTION_01 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_uUID) {",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_uUID);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _uUID_1 = Boot.callF().getID();",
			"        return Objects.equals(_uUID, _uUID_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.callF().getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Pure",
			"  protected static Address callF() {",
			"    return null;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: local static function with Address")
	@Tag("sarlValidation")
	public void parseLocalStaticFunction_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_LOCAL_STATIC_FUNCTION_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: local static function with Address")
	@Tag("compileToJava")
	public void compilingLocalStaticFunction_01() throws Exception {
		this.compiler.compile(SNIPSET_LOCAL_STATIC_FUNCTION_01, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_LOCAL_STATIC_FUNCTION_01, actual);
		});
	}

	private static final String SNIPSET_LOCAL_STATIC_FUNCTION_02 = multilineString2(false,
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == callF ]",
			"  }",
			"  static def callF : UUID { null }",
			"}");

	private static final String EXPECTED_LOCAL_STATIC_FUNCTION_02 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_callF;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_callF) {",
			"        this.$_callF = $_callF;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equals(_uUID, $_callF);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        UUID _callF = Boot.callF();",
			"        return Objects.equals(_uUID, _callF);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.callF());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Pure",
			"  protected static UUID callF() {",
			"    return null;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: local static function with UUID")
	@Tag("sarlValidation")
	public void parseLocalStaticFunction_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_LOCAL_STATIC_FUNCTION_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: local static function with UUID")
	@Tag("compileToJava")
	public void compilingLocalStaticFunction_02() throws Exception {
		this.compiler.compile(SNIPSET_LOCAL_STATIC_FUNCTION_02, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_LOCAL_STATIC_FUNCTION_02, actual);
		});
	}

	private static final String SNIPSET_NO_EXTERNAL_REFERENCE = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ it.UUID == null ]",
			"  }",
			"}");

	private static final String EXPECTED_NO_EXTERNAL_REFERENCE = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      public $SerializableClosureProxy() {",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return (((_uUID) == null && (null) == null) || ((_uUID) != null && (_uUID).equals((null).getID())));",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return (((_uUID) == null && (null) == null) || ((_uUID) != null && (_uUID).equals((null).getID())));",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class);",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: without external reference")
	@Tag("sarlValidation")
	public void parseNotExternalReference() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_NO_EXTERNAL_REFERENCE);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: without external reference")
	@Tag("compileToJava")
	public void compilingNotExternalReference() throws Exception {
		this.compiler.compile(SNIPSET_NO_EXTERNAL_REFERENCE, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_NO_EXTERNAL_REFERENCE, actual);
		});
	}

	private static final String SNIPSET_CLOSURE_VARIABLE = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ var p : Address = null",
			"        it.UUID == p.UUID ]",
			"  }",
			"}");

	private static final String EXPECTED_CLOSURE_VARIABLE = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      public $SerializableClosureProxy() {",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xblockexpression = false;",
			"        {",
			"          Address p = null;",
			"          UUID _uUID = it.getID();",
			"          UUID _uUID_1 = p.getID();",
			"          _xblockexpression = Objects.equals(_uUID, _uUID_1);",
			"        }",
			"        return _xblockexpression;",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xblockexpression = false;",
			"        {",
			"          Address p = null;",
			"          UUID _uUID = it.getID();",
			"          UUID _uUID_1 = p.getID();",
			"          _xblockexpression = Objects.equals(_uUID, _uUID_1);",
			"        }",
			"        return _xblockexpression;",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class);",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: variable in lambda")
	@Tag("sarlValidation")
	public void parsingClosureVariable() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_CLOSURE_VARIABLE);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: variable in lambda")
	@Tag("compileToJava")
	public void compilingClosureVariable() throws Exception {
		this.compiler.compile(SNIPSET_CLOSURE_VARIABLE, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_CLOSURE_VARIABLE, actual);
		});
	}

	private static final String SNIPSET_LOCAL_VARIABLE_AS_PARAMETER = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ var p : Address = null",
			"        s.matches(p)",
			"      ]",
			"  }",
			"}");

	private static final String EXPECTED_LOCAL_VARIABLE_AS_PARAMETER = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final Scope<Address> s;",
			"      ",
			"      public $SerializableClosureProxy(final Scope<Address> s) {",
			"        this.s = s;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xblockexpression = false;",
			"        {",
			"          Address p = null;",
			"          _xblockexpression = s.matches(p);",
			"        }",
			"        return _xblockexpression;",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xblockexpression = false;",
			"        {",
			"          Address p = null;",
			"          _xblockexpression = s.matches(p);",
			"        }",
			"        return _xblockexpression;",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, s);",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: variable in closure as argument")
	@Tag("sarlValidation")
	public void parsingLocalVariavleAsParameter() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_LOCAL_VARIABLE_AS_PARAMETER);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}


	@Test
	@DisplayName("Compile: variable in closure as argument")
	@Tag("compileToJava")
	public void compilingLocalVariavleAsParameter() throws Exception {
		this.compiler.compile(SNIPSET_LOCAL_VARIABLE_AS_PARAMETER, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_LOCAL_VARIABLE_AS_PARAMETER, actual);
		});
	}

	private static final String SNIPSET_MULTIPLE_CALLS_TO_SAME_FUNCTION = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2 {",
			"    f [ callF(1+2) == callF(5) ]",
			"  }",
			"  def callF(a : int) : int { 0 }",
			"}");

	private static final String EXPECTED_MULTIPLE_CALLS_TO_SAME_FUNCTION = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2() {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final int $_callF;",
			"      ",
			"      private final int $_callF_2;",
			"      ",
			"      public $SerializableClosureProxy(final int $_callF, final int $_callF_2) {",
			"        this.$_callF = $_callF;",
			"        this.$_callF_2 = $_callF_2;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        return ($_callF == $_callF_2);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        int _callF = Boot.this.callF((1 + 2));",
			"        int _callF_1 = Boot.this.callF(5);",
			"        return (_callF == _callF_1);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, Boot.this.callF((1 + 2)), Boot.this.callF(5));",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Pure",
			"  protected int callF(final int a) {",
			"    return 0;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: multiple function calls")
	@Tag("sarlValidation")
	public void parsingMultipleCallsToSameFunction() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_MULTIPLE_CALLS_TO_SAME_FUNCTION);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: multiple function calls")
	@Tag("compileToJava")
	public void compilingMultipleCallsToSameFunction() throws Exception {
		this.compiler.compile(SNIPSET_MULTIPLE_CALLS_TO_SAME_FUNCTION, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_MULTIPLE_CALLS_TO_SAME_FUNCTION, actual);
		});
	}

	private static final String SNIPSET_MULTIPLE_CALLS_TO_SAME_PARAMETER = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(p : int) {",
			"    f [ p == callF(p) ]",
			"  }",
			"  def callF(a : int) : int { a }",
			"}");

	private static final String EXPECTED_MULTIPLE_CALLS_TO_SAME_PARAMETER = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final int p) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final int p;",
			"      ",
			"      private final int $_callF;",
			"      ",
			"      public $SerializableClosureProxy(final int p, final int $_callF) {",
			"        this.p = p;",
			"        this.$_callF = $_callF;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        return (p == $_callF);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        int _callF = Boot.this.callF(p);",
			"        return (p == _callF);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, p, Boot.this.callF(p));",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Pure",
			"  protected int callF(final int a) {",
			"    return a;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: multiple calls to same parameter")
	@Tag("sarlValidation")
	public void parsingMultipleCallsToSameParameter() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_MULTIPLE_CALLS_TO_SAME_PARAMETER);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: multiple calls to same parameter")
	@Tag("compileToJava")
	public void compilingMultipleCallsToSameParameter() throws Exception {
		this.compiler.compile(SNIPSET_MULTIPLE_CALLS_TO_SAME_PARAMETER, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_MULTIPLE_CALLS_TO_SAME_PARAMETER, actual);
		});
	}

	private static final String SNIPSET_COMPLEX_LAMBDA_01 = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import java.util.UUID",
			"",
			"agent Boot {",
			"  var field : int",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ var p : Address = null",
			"        var r = UUID::randomUUID !== null",
			"        for (var i = 0; i < field; i++) {",
			"          if (p !== null && s !== null) {",
			"            r = r && !s.matches(p)",
			"          }",
			"        }",
			"        return r",
			"      ]",
			"  }",
			"}");

	private static final String EXPECTED_COMPLEX_LAMBDA_01 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  private int field;",
			"",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID $_randomUUID;",
			"      ",
			"      private final int $_field;",
			"      ",
			"      private final Scope<Address> s;",
			"      ",
			"      public $SerializableClosureProxy(final UUID $_randomUUID, final int $_field, final Scope<Address> s) {",
			"        this.$_randomUUID = $_randomUUID;",
			"        this.$_field = $_field;",
			"        this.s = s;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        Address p = null;",
			"        boolean r = ($_randomUUID != null);",
			"        for (int i = 0; (i < $_field); i++) {",
			"          if (((p != null) && (s != null))) {",
			"            r = (r && (!s.matches(p)));",
			"          }",
			"        }",
			"        return r;",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        Address p = null;",
			"        UUID _randomUUID = UUID.randomUUID();",
			"        boolean r = (_randomUUID != null);",
			"        for (int i = 0; (i < Boot.this.field); i++) {",
			"          if (((p != null) && (s != null))) {",
			"            r = (r && (!s.matches(p)));",
			"          }",
			"        }",
			"        return r;",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, UUID.randomUUID(), Boot.this.field, s);",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    if (this == obj)",
			"      return true;",
			"    if (obj == null)",
			"      return false;",
			"    if (getClass() != obj.getClass())",
			"      return false;",
			"    Boot other = (Boot) obj;",
			"    if (other.field != this.field)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.field);",
			"    return result;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: lambda with loop and branch")
	@Tag("sarlValidation")
	public void parsingComplexLambda01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_COMPLEX_LAMBDA_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: lambda with loop and branch")
	@Tag("compileToJava")
	public void compilingComplexLambda01() throws Exception {
		this.compiler.compile(SNIPSET_COMPLEX_LAMBDA_01, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_COMPLEX_LAMBDA_01, actual);
		});
	}

	private static final String SNIPSET_COMPLEX_LAMBDA_02 = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Address",
			"import io.sarl.lang.core.Scope",
			"import java.util.UUID",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    f [ val p = it",
			"        var sss : (Address) => UUID = [a|if(a !== null) a.UUID else p.UUID]",
			"        it.UUID == sss.apply(p) ]",
			"  }",
			"}");

	private static final String EXPECTED_COMPLEX_LAMBDA_02 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Functions.Function1;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final Address $_a;",
			"      ",
			"      private final UUID $_uUID;",
			"      ",
			"      public $SerializableClosureProxy(final Address $_a, final UUID $_uUID) {",
			"        this.$_a = $_a;",
			"        this.$_uUID = $_uUID;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xblockexpression = false;",
			"        {",
			"          final Address p = it;",
			"          final Function1<Address, UUID> _function = (Address a) -> {",
			"            UUID _xifexpression = null;",
			"            if (($_a != null)) {",
			"              _xifexpression = $_uUID;",
			"            } else {",
			"              _xifexpression = p.getID();",
			"            }",
			"            return _xifexpression;",
			"          };",
			"          Function1<? super Address, ? extends UUID> sss = _function;",
			"          UUID _uUID = it.getID();",
			"          UUID _apply = sss.apply(p);",
			"          _xblockexpression = Objects.equals(_uUID, _apply);",
			"        }",
			"        return _xblockexpression;",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xblockexpression = false;",
			"        {",
			"          final Address p = it;",
			"          final Function1<Address, UUID> _function = (Address a) -> {",
			"            UUID _xifexpression = null;",
			"            if ((a != null)) {",
			"              _xifexpression = a.getID();",
			"            } else {",
			"              _xifexpression = p.getID();",
			"            }",
			"            return _xifexpression;",
			"          };",
			"          Function1<? super Address, ? extends UUID> sss = _function;",
			"          UUID _uUID = it.getID();",
			"          UUID _apply = sss.apply(p);",
			"          _xblockexpression = Objects.equals(_uUID, _apply);",
			"        }",
			"        return _xblockexpression;",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, a, a.getID());",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: lambda inside lambda")
	@Tag("sarlValidation")
	public void parsingComplexLambda02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_COMPLEX_LAMBDA_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: lambda inside lambda")
	@Tag("compileToJava")
	public void compilingComplexLambda02() throws Exception {
		this.compiler.compile(SNIPSET_COMPLEX_LAMBDA_02, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_COMPLEX_LAMBDA_02, actual);
		});
	}

	private static final String SNIPSET_COMPLEX_LAMBDA_03 = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Address",
			"import io.sarl.lang.core.Scope",
			"import java.util.UUID",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    for (UUID : <UUID>newArrayList) {",
			"      f [it.UUID == UUID]",
			"    }",
			"  }",
			"}");

	private static final String EXPECTED_COMPLEX_LAMBDA_03 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.ArrayList;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.CollectionLiterals;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    ArrayList<UUID> _newArrayList = CollectionLiterals.<UUID>newArrayList();",
			"    for (final UUID UUID : _newArrayList) {",
			"      class $SerializableClosureProxy implements Scope<Address> {",
			"        ",
			"        private final java.util.UUID UUID;",
			"        ",
			"        public $SerializableClosureProxy(final java.util.UUID UUID) {",
			"          this.UUID = UUID;",
			"        }",
			"        ",
			"        @Override",
			"        public boolean matches(final Address it) {",
			"          java.util.UUID _uUID = it.getID();",
			"          return Objects.equals(_uUID, UUID);",
			"        }",
			"      }",
			"      final Scope<Address> _function = new Scope<Address>() {",
			"        @Override",
			"        public boolean matches(final Address it) {",
			"          java.util.UUID _uUID = it.getID();",
			"          return Objects.equals(_uUID, UUID);",
			"        }",
			"        private Object writeReplace() throws ObjectStreamException {",
			"          return new SerializableProxy($SerializableClosureProxy.class, UUID);",
			"        }",
			"      };",
			"      this.f(_function);",
			"    }",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: lambda with loop")
	@Tag("sarlValidation")
	public void parseComplexLambda03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_COMPLEX_LAMBDA_03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: lambda with loop")
	@Tag("compileToJava")
	public void compilingComplexLambda03() throws Exception {
		this.compiler.compile(SNIPSET_COMPLEX_LAMBDA_03, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_COMPLEX_LAMBDA_03, actual);
		});
	}

	private static final String SNIPSET_COMPLEX_LAMBDA_04 = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Address",
			"import io.sarl.lang.core.Scope",
			"import java.util.UUID",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    try {",
			"      throw new UUIDException(UUID.randomUUID);",
			"    } catch (e: UUIDException) {",
			"      f [ it.UUID == e.uuid]",
			"    }",
			"  }",
			"  static class UUIDException extends Exception {",
			"    @Accessors(PUBLIC_GETTER)",
			"    val uuid: UUID",
			"    new(uuid: UUID) {",
			"      this.uuid = uuid",
			"    }",
			"  }",
			"}");

	private static final String EXPECTED_COMPLEX_LAMBDA_04 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtend.lib.annotations.AccessorType;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Exceptions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"  @XbaseGenerated",
			"  protected static class UUIDException extends Exception {",
			"    @Accessors(AccessorType.PUBLIC_GETTER)",
			"    private final UUID uuid;",
			"",
			"    public UUIDException(final UUID uuid) {",
			"      this.uuid = uuid;",
			"    }",
			"",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public boolean equals(final Object obj) {",
			"      if (this == obj)",
			"        return true;",
			"      if (obj == null)",
			"        return false;",
			"      if (getClass() != obj.getClass())",
			"        return false;",
			"      UUIDException other = (UUIDException) obj;",
			"      if (!Objects.equals(this.uuid, other.uuid))",
			"        return false;",
			"      return super.equals(obj);",
			"    }",
			"",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public int hashCode() {",
			"      int result = super.hashCode();",
			"      final int prime = 31;",
			"      result = prime * result + Objects.hashCode(this.uuid);",
			"      return result;",
			"    }",
			"",
			"    @SyntheticMember",
			"    private static final long serialVersionUID = 2117806963L;",
			"",
			"    @Pure",
			"    public UUID getUuid() {",
			"      return this.uuid;",
			"    }",
			"  }",
			"",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    try {",
			"      UUID _randomUUID = UUID.randomUUID();",
			"      throw new Boot.UUIDException(_randomUUID);",
			"    } catch (final Throwable _t) {",
			"      if (_t instanceof Boot.UUIDException) {",
			"        final Boot.UUIDException e = (Boot.UUIDException)_t;",
			"        class $SerializableClosureProxy implements Scope<Address> {",
			"          ",
			"          private final UUID $_uuid;",
			"          ",
			"          public $SerializableClosureProxy(final UUID $_uuid) {",
			"            this.$_uuid = $_uuid;",
			"          }",
			"          ",
			"          @Override",
			"          public boolean matches(final Address it) {",
			"            UUID _uUID = it.getID();",
			"            return Objects.equals(_uUID, $_uuid);",
			"          }",
			"        }",
			"        final Scope<Address> _function = new Scope<Address>() {",
			"          @Override",
			"          public boolean matches(final Address it) {",
			"            UUID _uUID = it.getID();",
			"            return Objects.equals(_uUID, e.uuid);",
			"          }",
			"          private Object writeReplace() throws ObjectStreamException {",
			"            return new SerializableProxy($SerializableClosureProxy.class, e.uuid);",
			"          }",
			"        };",
			"        this.f(_function);",
			"      } else {",
			"        throw Exceptions.sneakyThrow(_t);",
			"      }",
			"    }",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: in catch statement")
	@Tag("sarlValidation")
	public void parseComplexLambda04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_COMPLEX_LAMBDA_04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: in catch statement")
	@Tag("compileToJava")
	public void compilingComplexLambda04() throws Exception {
		this.compiler.compile(SNIPSET_COMPLEX_LAMBDA_04, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_COMPLEX_LAMBDA_04, actual);
		});
	}

	private static final String SNIPSET_COMPLEX_LAMBDA_05 = multilineString(
			"package io.sarl.lang.tests.bug885",
			"import io.sarl.lang.core.Address",
			"import io.sarl.lang.core.Scope",
			"import java.util.UUID",
			"agent Boot {",
			"  def f(s : Scope<Address>) {",
			"  }",
			"  def f2(s : Scope<Address>) {",
			"    val random = UUID.randomUUID",
			"    f [ val uuid = it.UUID",
			"        val it = random",
			"        uuid == it",
			"    ]",
			"  }",
			"}");

	private static final String EXPECTED_COMPLEX_LAMBDA_05 = multilineString2(false,
			"package io.sarl.lang.tests.bug885;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Objects;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  protected void f(final Scope<Address> s) {",
			"  }",
			"",
			"  protected void f2(final Scope<Address> s) {",
			"    final UUID random = UUID.randomUUID();",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID random;",
			"      ",
			"      public $SerializableClosureProxy(final UUID random) {",
			"        this.random = random;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xblockexpression = false;",
			"        {",
			"          final UUID uuid = it.getID();",
			"          final UUID it_1 = random;",
			"          _xblockexpression = Objects.equals(uuid, it_1);",
			"        }",
			"        return _xblockexpression;",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        boolean _xblockexpression = false;",
			"        {",
			"          final UUID uuid = it.getID();",
			"          final UUID it_1 = random;",
			"          _xblockexpression = Objects.equals(uuid, it_1);",
			"        }",
			"        return _xblockexpression;",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, random);",
			"      }",
			"    };",
			"    this.f(_function);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: it redefinition")
	@Tag("sarlValidation")
	public void parseComplexLambda05() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET_COMPLEX_LAMBDA_05);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: it redefinition")
	@Tag("compileToJava")
	public void compilingComplexLambda05() throws Exception {
		this.compiler.compile(SNIPSET_COMPLEX_LAMBDA_05, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug885.Boot");
			assertEquals(EXPECTED_COMPLEX_LAMBDA_05, actual);
		});
	}

}
