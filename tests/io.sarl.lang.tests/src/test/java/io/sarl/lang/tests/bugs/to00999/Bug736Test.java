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
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Add static initialization blocks.
 *
 * <p>https://github.com/sarl/sarl/issues/736
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #736")
@SuppressWarnings("all")
@Tag("core")
public class Bug736Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug736",
			"class Test {",
			"  static var X : int",
			"  static new {",
			"    X = 1",
			"  }",
			"  new {",
			"  }",
			"}");

	private final String EXPECTED1 = multilineString(
			"package io.sarl.lang.tests.bug736;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Test {",
			"  private static int X;",
			"  ",
			"  static {",
			"    Test.X = 1;",
			"  }",
			"  ",
			"  public Test() {",
			"  }",
			"}", 
			"");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug736",
			"class Test {",
			"  static val X : int",
			"  static new {",
			"    X = 1",
			"  }",
			"  new {",
			"  }",
			"}");


	private final String EXPECTED2 = multilineString(
			"package io.sarl.lang.tests.bug736;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Test {",
			"  private static final int X;",
			"  ",
			"  static {",
			"    Test.X = 1;",
			"  }",
			"  ",
			"  public Test() {",
			"  }",
			"}", 
			"");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug736",
			"class Test {",
			"  static var X : int",
			"  static new(a : int) {",
			"    X = 1",
			"  }",
			"  new {",
			"  }",
			"}");

	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug736",
			"class Test {",
			"  static var X : int",
			"  static new with T {",
			"    X = 1",
			"  }",
			"  new {",
			"  }",
			"}");

	private static final String SNIPSET5 = multilineString(
			"package io.sarl.lang.tests.bug736",
			"class Test {",
			"  static var X : int",
			"  static new throws Exception {",
			"    X = 1",
			"  }",
			"  new {",
			"  }",
			"}");

	private static final String SNIPSET6 = multilineString(
			"package io.sarl.lang.tests.bug736",
			"event MyEvent",
			"class Test {",
			"  static var X : int",
			"  public static new {",
			"    X = 1",
			"  }",
			"  new {",
			"  }",
			"}");

	private static final String SNIPSET7 = multilineString(
			"package io.sarl.lang.tests.bug736",
			"event MyEvent",
			"class Test {",
			"  static var X : int",
			"  private static new {",
			"    X = 1",
			"  }",
			"  new {",
			"  }",
			"}");

	private static final String SNIPSET8 = multilineString(
			"package io.sarl.lang.tests.bug736",
			"event MyEvent",
			"class Test {",
			"  var X : int",
			"  static new {",
			"    X = 1",
			"  }",
			"  new {",
			"  }",
			"}");

	private static final String SNIPSET9 = multilineString(
			"package io.sarl.lang.tests.bug736",
			"event MyEvent",
			"class Test {",
			"  static var X : int",
			"  static new {",
			"    X = 1",
			"  }",
			"  new {",
			"  }",
			"  static new {",
			"    X = 1",
			"  }",
			"}");

	private static final String SNIPSET10 = multilineString(
			"package io.sarl.lang.tests.bug736",
			"final class Messages {",
			"  static val BUNDLE_NAME = typeof(Messages).getPackage.name + \".messages\"",
			"  static new {",
			"    System.out.println(BUNDLE_NAME)",
			"  }",
			"  private new {",
			"  }",
			"}");


	private final String EXPECTED10 = multilineString(
			"package io.sarl.lang.tests.bug736;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public final class Messages {",
			"  private static final String BUNDLE_NAME = (Messages.class.getPackage().getName() + \".messages\");",
			"  ",
			"  static {",
			"    System.out.println(Messages.BUNDLE_NAME);",
			"  }",
			"  ",
			"  private Messages() {",
			"  }",
			"}", 
			"");
	
	@Test
	@Tag("sarlValidation")
	public void validating_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}
	
	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET1, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug736.Test");
			assertEquals(EXPECTED1, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void validating_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET2);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET2, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug736.Test");
			assertEquals(EXPECTED2, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void validating_03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET3);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlConstructor(),
				IssueCodes.UNEXPECTED_FORMAL_PARAMETER,
				"Unexpected formal parameter to a static constructor");
	}

	@Test
	@Tag("sarlValidation")
	public void validating_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET4);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlConstructor(),
				org.eclipse.xtend.core.validation.IssueCodes.CONSTRUCTOR_TYPE_PARAMS_NOT_SUPPORTED,
				"Unexpected type parameter to a static constructor");
	}

	@Test
	@Tag("sarlValidation")
	public void validating_05() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET5);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlConstructor(),
				IssueCodes.UNEXPECTED_EXCEPTION_THROW,
				"Unexpected exception to a static constructor");
	}

	@Test
	@Tag("sarlValidation")
	public void validating_06() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET6);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlConstructor(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the definition");
	}

	@Test
	@Tag("sarlValidation")
	public void validating_07() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET7);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlConstructor(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the definition");
	}

	@Test
	@Tag("sarlValidation")
	public void validating_08() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET8);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXAssignment(),
				org.eclipse.xtext.xbase.validation.IssueCodes.STATIC_ACCESS_TO_INSTANCE_MEMBER,
				"Cannot make a static reference to the non-static field X");
	}

	@Test
	@Tag("sarlValidation")
	public void validating_09() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET9);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlConstructor(),
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_METHOD,
				"Duplicate method " + Utils.getStaticConstructorName());
	}

	@Test
	@Tag("sarlValidation")
	public void validating_10() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET10);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_10() throws Exception {
		getCompileHelper().compile(SNIPSET10, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug736.Messages");
			assertEquals(EXPECTED10, actual);
		});
	}

}
