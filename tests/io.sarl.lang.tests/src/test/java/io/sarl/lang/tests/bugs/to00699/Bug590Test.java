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

package io.sarl.lang.tests.bugs.to00699;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtend.core.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Automatic generation of an event constructor.
 *
 * <p>https://github.com/sarl/sarl/issues/590
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #590")
@SuppressWarnings("all")
@Tag("core")
public class Bug590Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug590",
			"import java.text.MessageFormat",
			"class LocaleMessageFormat extends MessageFormat {",
			"  override applyPattern(pattern : String) {",
			"    super.applyPattern(pattern)",
			"  }",
			"}");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug590",
			"import java.text.MessageFormat",
			"class LocaleMessageFormat extends MessageFormat {",
			"  override applyPattern(pattern : String) {",
			"    super.applyPattern(pattern)",
			"  }",
			"}",
			"class LocaleMessageFormat2 extends LocaleMessageFormat {",
			"}");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug590",
			"import java.text.MessageFormat",
			"class LocaleMessageFormat2 extends LocaleMessageFormat {",
			"}",
			"class LocaleMessageFormat extends MessageFormat {",
			"  override applyPattern(pattern : String) {",
			"    super.applyPattern(pattern)",
			"  }",
			"}");

	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug590",
			"import java.text.MessageFormat",
			"class LocaleMessageFormat extends MessageFormat {",
			"  override applyPattern(pattern : String) {",
			"    super.applyPattern(pattern)",
			"  }",
			"  new {",
			"  }",
			"}");

	private static final String SNIPSET5 = multilineString(
			"package io.sarl.lang.tests.bug590",
			"import java.text.MessageFormat",
			"class LocaleMessageFormat extends MessageFormat {",
			"  override applyPattern(pattern : String) {",
			"    super.applyPattern(pattern)",
			"  }",
			"  new {",
			"    super(null)",
			"  }",
			"}");

	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().assertCompilesTo(SNIPSET1, multilineString(
				"package io.sarl.lang.tests.bug590;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import java.text.MessageFormat;",
				"import java.util.Locale;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class LocaleMessageFormat extends MessageFormat {",
				"  @Override",
				"  public void applyPattern(final String pattern) {",
				"    super.applyPattern(pattern);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public LocaleMessageFormat clone() {",
				"    try {",
				"      return (LocaleMessageFormat) super.clone();",
				"    } catch (Throwable exception) {",
				"      throw new Error(exception);",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public LocaleMessageFormat(final String arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public LocaleMessageFormat(final String arg0, final Locale arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = -211302830L;",
				"}",
				""));
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET2);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET2, (it) -> {
			assertEquals(multilineString(
				"package io.sarl.lang.tests.bug590;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import java.text.MessageFormat;",
				"import java.util.Locale;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class LocaleMessageFormat extends MessageFormat {",
				"  @Override",
				"  public void applyPattern(final String pattern) {",
				"    super.applyPattern(pattern);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public LocaleMessageFormat clone() {",
				"    try {",
				"      return (LocaleMessageFormat) super.clone();",
				"    } catch (Throwable exception) {",
				"      throw new Error(exception);",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public LocaleMessageFormat(final String arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public LocaleMessageFormat(final String arg0, final Locale arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = -211302830L;",
				"}",
				""),
				it.getGeneratedCode("io.sarl.lang.tests.bug590.LocaleMessageFormat"));
			assertEquals(multilineString(
					"package io.sarl.lang.tests.bug590;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.Locale;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class LocaleMessageFormat2 extends LocaleMessageFormat {",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public LocaleMessageFormat2 clone() {",
					"    try {",
					"      return (LocaleMessageFormat2) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public LocaleMessageFormat2(final String arg0) {",
					"    super(arg0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public LocaleMessageFormat2(final String arg0, final Locale arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1L;",
					"}",
					""),
					it.getGeneratedCode("io.sarl.lang.tests.bug590.LocaleMessageFormat2"));
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET3);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_03() throws Exception {
		getCompileHelper().compile(SNIPSET2, (it) -> {
			assertEquals(multilineString(
				"package io.sarl.lang.tests.bug590;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import java.text.MessageFormat;",
				"import java.util.Locale;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class LocaleMessageFormat extends MessageFormat {",
				"  @Override",
				"  public void applyPattern(final String pattern) {",
				"    super.applyPattern(pattern);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public LocaleMessageFormat clone() {",
				"    try {",
				"      return (LocaleMessageFormat) super.clone();",
				"    } catch (Throwable exception) {",
				"      throw new Error(exception);",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public LocaleMessageFormat(final String arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public LocaleMessageFormat(final String arg0, final Locale arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = -211302830L;",
				"}",
				""),
				it.getGeneratedCode("io.sarl.lang.tests.bug590.LocaleMessageFormat"));
			assertEquals(multilineString(
					"package io.sarl.lang.tests.bug590;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.Locale;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class LocaleMessageFormat2 extends LocaleMessageFormat {",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public LocaleMessageFormat2 clone() {",
					"    try {",
					"      return (LocaleMessageFormat2) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public LocaleMessageFormat2(final String arg0) {",
					"    super(arg0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public LocaleMessageFormat2(final String arg0, final Locale arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1L;",
					"}",
					""),
					it.getGeneratedCode("io.sarl.lang.tests.bug590.LocaleMessageFormat2"));
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET4);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlConstructor(),
				IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR,
				"No default constructor in super type MessageFormat");
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_05() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET5);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_05() throws Exception {
		getCompileHelper().assertCompilesTo(SNIPSET5, multilineString(
				"package io.sarl.lang.tests.bug590;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import java.text.MessageFormat;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class LocaleMessageFormat extends MessageFormat {",
				"  @Override",
				"  public void applyPattern(final String pattern) {",
				"    super.applyPattern(pattern);",
				"  }",
				"  ",
				"  public LocaleMessageFormat() {",
				"    super(null);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public LocaleMessageFormat clone() {",
				"    try {",
				"      return (LocaleMessageFormat) super.clone();",
				"    } catch (Throwable exception) {",
				"      throw new Error(exception);",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = -211300601L;",
				"}",
				""));
	}

}
