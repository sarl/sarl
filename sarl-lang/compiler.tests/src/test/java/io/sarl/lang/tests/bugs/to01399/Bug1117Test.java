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

package io.sarl.lang.tests.bugs.to01399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Stack overflow in Eclipse IDE.
 *
 * <p>https://github.com/sarl/sarl/issues/1117
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 * @see "https://github.com/sarl/sarl/issues/1117"
 */
@DisplayName("Bug #1117")
@SuppressWarnings("all")
@Tag("core")
public class Bug1117Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1117",
			"class Bug1117Case {",
			"  def fct(obj : String) : int {",
			"    if (obj === null || obj.length <= 1) {",
			"      return 1",
			"    } else {",
			"      return fct(obj.substring(1))",
			"    }",
			"  }",
			"}");

	private static final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1117;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Bug1117Case {",
			"  @Pure",
			"  public int fct(final String obj) {",
			"    if (((obj == null) || (obj.length() <= 1))) {",
			"      return 1;",
			"    } else {",
			"      return this.fct(obj.substring(1));",
			"    }",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Bug1117Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: Single direct recursive call")
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compile: Single direct recursive call")
	@Tag("compileToJava")
	public void compiling01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1117.Bug1117Case");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

	private static final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1117",
			"class Bug1117Case {",
			"  def fct0(obj : String) {",
			"    return fct(obj)",
			"  }",
			"  def fct(obj : String) : int {",
			"    if (obj === null || obj.length <= 1) {",
			"      return 1",
			"    } else {",
			"      return fct0(obj.substring(1))",
			"    }",
			"  }",
			"}");

	private static final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1117;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Bug1117Case {",
			"  @Pure",
			"  public int fct0(final String obj) {",
			"    return this.fct(obj);",
			"  }",
			"",
			"  @Pure",
			"  public int fct(final String obj) {",
			"    if (((obj == null) || (obj.length() <= 1))) {",
			"      return 1;",
			"    } else {",
			"      return this.fct0(obj.substring(1));",
			"    }",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Bug1117Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: Single indirect recursive call")
	@Tag("sarlParsing")
	public void parsing02() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compile: Single indirect recursive call")
	@Tag("compileToJava")
	public void compiling02() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1117.Bug1117Case");
			assertEquals(JAVA_CODE_02, actual);
		});
	}

	private static final String SARL_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1117",
			"class Bug1117Case {",
			"  def fct(obj : String) : int {",
			"    if (obj === null || obj.length <= 1) {",
			"      return 1",
			"    } else {",
			"      return fct(obj.substring(1))",
			"    }",
			"  }",
			"  def fct(obj : int) : int {",
			"    return 0",
			"  }",
			"}");

	private static final String JAVA_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1117;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Bug1117Case {",
			"  @Pure",
			"  public int fct(final String obj) {",
			"    if (((obj == null) || (obj.length() <= 1))) {",
			"      return 1;",
			"    } else {",
			"      return this.fct(obj.substring(1));",
			"    }",
			"  }",
			"",
			"  @Pure",
			"  public int fct(final int obj) {",
			"    return 0;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Bug1117Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: Multi direct recursive call")
	@Tag("sarlParsing")
	public void parsing03() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compile: Multi direct recursive call")
	@Tag("compileToJava")
	public void compiling03() throws Exception {
		getCompileHelper().compile(SARL_CODE_03, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1117.Bug1117Case");
			assertEquals(JAVA_CODE_03, actual);
		});
	}

	private static final String SARL_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug1117",
			"class Bug1117Case {",
			"  def fct(obj : String = \"abc\") : int {",
			"    if (obj === null || obj.length <= 1) {",
			"      return 1",
			"    } else {",
			"      return fct(obj.substring(1))",
			"    }",
			"  }",
			"  def fct(obj : int) : int {",
			"    return 0",
			"  }",
			"}");

	private static final String JAVA_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug1117;",
			"",
			"import io.sarl.lang.core.annotation.DefaultValue;",
			"import io.sarl.lang.core.annotation.DefaultValueSource;",
			"import io.sarl.lang.core.annotation.DefaultValueUse;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSourceCode;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Bug1117Case {",
			"  @DefaultValueSource",
			"  @Pure",
			"  public int fct(@DefaultValue(\"io.sarl.lang.tests.bug1117.Bug1117Case#FCT_0\") final String obj) {",
			"    if (((obj == null) || (obj.length() <= 1))) {",
			"      return 1;",
			"    } else {",
			"      return this.fct(obj.substring(1));",
			"    }",
			"  }",
			"",
			"  @Pure",
			"  public int fct(final int obj) {",
			"    return 0;",
			"  }",
			"",
			"  /**",
			"   * Default value for the parameter obj",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"\\\"abc\\\"\")",
			"  private final String $DEFAULT_VALUE$FCT_0() {",
			"    return \"abc\";",
			"  }",
			"",
			"  @DefaultValueUse(\"java.lang.String\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final int fct() {",
			"    return fct($DEFAULT_VALUE$FCT_0());",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Bug1117Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: Option direct recursive call")
	@Tag("sarlParsing")
	public void parsing04() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compile: Option direct recursive call")
	@Tag("compileToJava")
	public void compiling04() throws Exception {
		getCompileHelper().compile(SARL_CODE_04, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1117.Bug1117Case");
			assertEquals(JAVA_CODE_04, actual);
		});
	}

}
