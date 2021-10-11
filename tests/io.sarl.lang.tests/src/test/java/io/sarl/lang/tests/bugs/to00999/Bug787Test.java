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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Error with the Maven compiler on CLI: type references are not associated to the right resource
 *
 * <p>https://github.com/sarl/sarl/issues/787
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/787"
 */
@DisplayName("Bug #787")
@SuppressWarnings("all")
@Tag("core")
public class Bug787Test extends AbstractSarlTest {


	private static final String SNIPSET01 = multilineString(
			"package foo.bug787",
			"class C1 extends SuperReturnInt {",
			"  def fct {",
			"    1",
			"  }",
			"}");
	
	private static final String EXPECTED01 = multilineString(
			"package foo.bug787;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class C1 extends SuperReturnInt {",
			"  public int fct() {",
			"    return 1;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public C1() {",
			"    super();",
			"  }",
			"}",
			"");

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
			final String actual = it.getGeneratedCode("foo.bug787.C1");
			assertEquals(EXPECTED01, actual);
		});
	}

	private static final String SNIPSET02 = multilineString(
			"package foo.bug787",
			"class C2 extends SuperReturnVoid {",
			"  def fct {",
			"  }",
			"}");
	
	private static final String EXPECTED02 = multilineString(
			"package foo.bug787;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class C2 extends SuperReturnVoid {",
			"  public void fct() {",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public C2() {",
			"    super();",
			"  }",
			"}",
			"");

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
			final String actual = it.getGeneratedCode("foo.bug787.C2");
			assertEquals(EXPECTED02, actual);
		});
	}

	private static final String SNIPSET03 = multilineString(
			"package foo.bug787",
			"class C3 extends SuperReturnObject {",
			"  def fct {",
			"  }",
			"}");
	
	private static final String EXPECTED03 = multilineString(
			"package foo.bug787;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.SpaceID;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class C3 extends SuperReturnObject {",
			"  public SpaceID fct() {",
			"    return null;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public C3() {",
			"    super();",
			"  }",
			"}",
			"");

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
			final String actual = it.getGeneratedCode("foo.bug787.C3");
			assertEquals(EXPECTED03, actual);
		});
	}

	private static final String SNIPSET04 = multilineString(
			"package foo.bug787",
			"class C4 extends SuperParamInt {",
			"}");
	
	private static final String EXPECTED04 = multilineString(
			"package foo.bug787;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class C4 extends SuperParamInt {",
			"  @SyntheticMember",
			"  public C4(final int arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

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
			final String actual = it.getGeneratedCode("foo.bug787.C4");
			assertEquals(EXPECTED04, actual);
		});
	}


	private static final String SNIPSET05 = multilineString(
			"package foo.bug787",
			"class C5 extends SuperParamObject {",
			"}");
	
	private static final String EXPECTED05 = multilineString(
			"package foo.bug787;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.SpaceID;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class C5 extends SuperParamObject {",
			"  @SyntheticMember",
			"  public C5(final SpaceID arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_05() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET05);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_05() throws Exception {
		getCompileHelper().compile(SNIPSET05, (it) -> {
			final String actual = it.getGeneratedCode("foo.bug787.C5");
			assertEquals(EXPECTED05, actual);
		});
	}

	private static final String SNIPSET06 = multilineString(
			"package foo.bug787",
			"interface X<T> {",
			"  def a(p1 : T, p2 : U) with U",
			"}",
			"interface Y<T> {",
			"}",
			"class Z<TT> implements X<TT>, Y<TT> {",
			"  def a(p1 : TT, p2 : W) with W { }",
			"}");
	
	private static final String EXPECTED06 = multilineString(
			"package foo.bug787;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Z<TT extends Object> implements X<TT>, Y<TT> {",
			"  public <W extends Object> void a(final TT p1, final W p2) {",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Z() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_06() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET06);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_06() throws Exception {
		getCompileHelper().compile(SNIPSET06, (it) -> {
			final String actual = it.getGeneratedCode("foo.bug787.Z");
			assertEquals(EXPECTED06, actual);
		});
	}

	private static final String SNIPSET07 = multilineString(
			"package foo.bug787",
			"class X<T> {",
			"  new(p1 : T) { }",
			"}",
			"class Y<TT> extends X<TT> {",
			"}");
	
	private static final String EXPECTED07 = multilineString(
			"package foo.bug787;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Y<TT extends Object> extends X<TT> {",
			"  @SyntheticMember",
			"  public Y(final TT p1) {",
			"    super(p1);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_07() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET07);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_07() throws Exception {
		getCompileHelper().compile(SNIPSET07, (it) -> {
			final String actual = it.getGeneratedCode("foo.bug787.Y");
			assertEquals(EXPECTED07, actual);
		});
	}

	private static final String SNIPSET08 = multilineString(
			"package foo.bug787",
			"class Type2 extends Type1 {",
			"}");
	
	private static final String EXPECTED08 = multilineString(
			"package foo.bug787;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.SpaceID;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Type2 extends Type1 {",
			"  @SyntheticMember",
			"  public Type2(final SpaceID arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_08() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET08);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_08() throws Exception {
		getCompileHelper().compile(SNIPSET08, (it) -> {
			final String actual = it.getGeneratedCode("foo.bug787.Type2");
			assertEquals(EXPECTED08, actual);
		});
	}

}

