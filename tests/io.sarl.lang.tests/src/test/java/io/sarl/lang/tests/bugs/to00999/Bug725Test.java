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

/** Testing class for issue: Invalid functional interface with static function.
 *
 * <p>https://github.com/sarl/sarl/issues/725
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #725")
@SuppressWarnings("all")
@Tag("core")
public class Bug725Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug725",
			"interface Boot", 
			"{", 
			"  static def main(args : String*) {",
			"  }",
			"}");

	private final String EXPECTED1 = multilineString(
			"package io.sarl.lang.tests.bug725;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
			"@SuppressWarnings(\"all\")",
			"public interface Boot {",
			"  static void main(final String... args) {",
			"  }",
			"}",
			"");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug725",
			"interface Boot", 
			"{", 
			"  def main(args : String*) {",
			"  }",
			"}");

	private final String EXPECTED2 = multilineString(
			"package io.sarl.lang.tests.bug725;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
			"@SuppressWarnings(\"all\")",
			"public interface Boot {",
			"  default void main(final String... args) {",
			"  }",
			"}",
			"");
	
	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug725",
			"interface Boot", 
			"{", 
			"  def main(args : String*)",
			"}");

	private final String EXPECTED3 = multilineString(
			"package io.sarl.lang.tests.bug725;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"",
			"@FunctionalInterface",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
			"@SuppressWarnings(\"all\")",
			"public interface Boot {",
			"  void main(final String... args);",
			"}",
			"");
	
	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug725",
			"interface Boot", 
			"{", 
			"  def main(args : String*)",
			"  static def fct {}",
			"}");

	private final String EXPECTED4 = multilineString(
			"package io.sarl.lang.tests.bug725;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
			"@SuppressWarnings(\"all\")",
			"public interface Boot {",
			"  void main(final String... args);",
			"  ",
			"  static void fct() {",
			"  }",
			"}",
			"");

	private static final String SNIPSET5 = multilineString(
			"package io.sarl.lang.tests.bug725",
			"interface Boot", 
			"{", 
			"  def main(args : String*)",
			"  def fct {}",
			"}");

	private final String EXPECTED5 = multilineString(
			"package io.sarl.lang.tests.bug725;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
			"@SuppressWarnings(\"all\")",
			"public interface Boot {",
			"  void main(final String... args);",
			"  ",
			"  default void fct() {",
			"  }",
			"}",
			"");

	private static final String SNIPSET6 = multilineString(
			"package io.sarl.lang.tests.bug725",
			"interface Boot", 
			"{", 
			"  def main(args : String*)",
			"  def fct",
			"}");

	private final String EXPECTED6 = multilineString(
			"package io.sarl.lang.tests.bug725;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
			"@SuppressWarnings(\"all\")",
			"public interface Boot {",
			"  void main(final String... args);",
			"  ",
			"  void fct();",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET1, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug725.Boot");
			assertEquals(EXPECTED1, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET2);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET2, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug725.Boot");
			assertEquals(EXPECTED2, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET3);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_03() throws Exception {
		getCompileHelper().compile(SNIPSET3, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug725.Boot");
			assertEquals(EXPECTED3, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET4);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_04() throws Exception {
		getCompileHelper().compile(SNIPSET4, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug725.Boot");
			assertEquals(EXPECTED4, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_05() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET5);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_05() throws Exception {
		getCompileHelper().compile(SNIPSET5, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug725.Boot");
			assertEquals(EXPECTED5, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_06() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET6);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_06() throws Exception {
		getCompileHelper().compile(SNIPSET6, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug725.Boot");
			assertEquals(EXPECTED6, actual);
		});
	}
}
