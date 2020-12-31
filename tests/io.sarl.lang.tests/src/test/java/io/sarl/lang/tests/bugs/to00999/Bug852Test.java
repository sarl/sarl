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

package io.sarl.lang.tests.bugs.to00999;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Wrong Ambiguous binary operation error at CLI compile time (!= and .equals()).
 *
 * <p>https://github.com/sarl/sarl/issues/852
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/852"
 */
@DisplayName("Bug #852")
@SuppressWarnings("all")
@Tag("core")
public class Bug852Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug852",
			"agent SomeAgent{",
			"   def mytest(value : Integer) : void{",
			"       if (value == null) {}",
			"   }",
			"}");

	private static final String EXPECTED01 = multilineString(
			"package io.sarl.lang.tests.bug852;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  protected void mytest(final Integer value) {",
			"    if ((value == null ? (null == null) : (null != null && value.intValue() == null.doubleValue()))) {",
			"    }",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET02 = multilineString(
			"package io.sarl.lang.tests.bug852",
			"agent SomeAgent{",
			"   def mytest(value : Integer) : void{",
			"       if (value == 2.0l) {}",
			"   }",
			"}");

	private static final String EXPECTED02 = multilineString(
			"package io.sarl.lang.tests.bug852;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  protected void mytest(final Integer value) {",
			"    if ((value != null && (value.intValue() == 2.0l))) {",
			"    }",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET03 = multilineString(
			"package io.sarl.lang.tests.bug852",
			"agent SomeAgent{",
			"   def mytest(value : Integer, value2 : Integer) : void{",
			"       if (value == value2) {}",
			"   }",
			"}");

	private static final String EXPECTED03 = multilineString(
			"package io.sarl.lang.tests.bug852;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  protected void mytest(final Integer value, final Integer value2) {",
			"    if ((value == null ? (value2 == null) : (value2 != null && value.intValue() == value2.doubleValue()))) {",
			"    }",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET04 = multilineString(
			"package io.sarl.lang.tests.bug852",
			"agent SomeAgent{",
			"   def mytest(value : Integer, value2 : Integer) : void{",
			"       if (value <= value2) {}",
			"   }",
			"}");

	private static final String EXPECTED04 = multilineString(
			"package io.sarl.lang.tests.bug852;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  protected void mytest(final Integer value, final Integer value2) {",
			"    if ((value.intValue() <= value2.doubleValue())) {",
			"    }",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET05 = multilineString(
			"package io.sarl.lang.tests.bug852",
			"agent SomeAgent{",
			"   def mytest(value : Integer, value2 : Integer) : int {",
			"       value + value2",
			"   }",
			"}");

	private static final String EXPECTED05 = multilineString(
			"package io.sarl.lang.tests.bug852;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  @Pure",
			"  protected int mytest(final Integer value, final Integer value2) {",
			"    return (((value) == null ? 0 : (value).intValue()) + ((value2) == null ? 0 : (value2).intValue()));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET06 = multilineString(
			"package io.sarl.lang.tests.bug852",
			"agent SomeAgent{",
			"   def mytest(value : Integer, value2 : long) : long {",
			"       value + value2",
			"   }",
			"}");

	private static final String EXPECTED06 = multilineString(
			"package io.sarl.lang.tests.bug852;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  @Pure",
			"  protected long mytest(final Integer value, final long value2) {",
			"    return (((value) == null ? 0 : (value).intValue()) + value2);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET07 = multilineString(
			"package io.sarl.lang.tests.bug852",
			"agent SomeAgent{",
			"   def mytest(value : Integer, value2 : Number) : double {",
			"       value + value2",
			"   }",
			"}");

	private static final String EXPECTED07 = multilineString(
			"package io.sarl.lang.tests.bug852;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  @Pure",
			"  protected double mytest(final Integer value, final Number value2) {",
			"    return ((value).intValue() + (value2).doubleValue());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET08 = multilineString(
			"package io.sarl.lang.tests.bug852",
			"agent SomeAgent{",
			"   def mytest(value : Byte) : float {",
			"       5.0f + value",
			"   }",
			"}");

	private static final String EXPECTED08 = multilineString(
			"package io.sarl.lang.tests.bug852;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  @Pure",
			"  protected float mytest(final Byte value) {",
			"    return (5.0f + ((value) == null ? 0 : (value).byteValue()));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET09 = multilineString(
			"package io.sarl.lang.tests.bug852",
			"agent SomeAgent{",
			"   def mytest(value : Number) : double {",
			"       5.0f + value",
			"   }",
			"}");

	private static final String EXPECTED09 = multilineString(
			"package io.sarl.lang.tests.bug852;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  @Pure",
			"  protected double mytest(final Number value) {",
			"    return (5.0f + (value).doubleValue());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET10 = multilineString(
			"package io.sarl.lang.tests.bug852",
			"agent SomeAgent{",
			"   def mytest(value : Integer) : void{",
			"       if (null == value) {}",
			"   }",
			"}");

	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug852.SomeAgent");
			assertEquals(EXPECTED01, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug852.SomeAgent");
			assertEquals(EXPECTED02, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_03() throws Exception {
		getCompileHelper().compile(SNIPSET03, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug852.SomeAgent");
			assertEquals(EXPECTED03, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_04() throws Exception {
		getCompileHelper().compile(SNIPSET04, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug852.SomeAgent");
			assertEquals(EXPECTED04, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_05() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET05);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_05() throws Exception {
		getCompileHelper().compile(SNIPSET05, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug852.SomeAgent");
			assertEquals(EXPECTED05, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_06() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET06);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_06() throws Exception {
		getCompileHelper().compile(SNIPSET06, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug852.SomeAgent");
			assertEquals(EXPECTED06, actual);
		});
	}

	// TODO: Enable when the issue on implicitly imported functions is fixed.
	@Disabled
	@Test
	@Tag("sarlValidation")
	public void parsing_07() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET07);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	// TODO: Enable when the issue on implicitly imported functions is fixed.
	@Disabled
	@Test
	@Tag("compileToJava")
	public void compiling_07() throws Exception {
		getCompileHelper().compile(SNIPSET07, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug852.SomeAgent");
			assertEquals(EXPECTED07, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_08() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET08);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_08() throws Exception {
		getCompileHelper().compile(SNIPSET08, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug852.SomeAgent");
			assertEquals(EXPECTED08, actual);
		});
	}

	// TODO: Enable when the issue on implicitly imported functions is fixed.
	@Disabled
	@Test
	@Tag("sarlValidation")
	public void parsing_09() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET09);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	// TODO: Enable when the issue on implicitly imported functions is fixed.
	@Disabled
	@Test
	@Tag("compileToJava")
	public void compiling_09() throws Exception {
		getCompileHelper().compile(SNIPSET09, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug852.SomeAgent");
			assertEquals(EXPECTED09, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_10() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET10);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
				org.eclipse.xtext.xbase.validation.IssueCodes.AMBIGUOUS_FEATURE_CALL,
				"operator_equals");
	}

}

