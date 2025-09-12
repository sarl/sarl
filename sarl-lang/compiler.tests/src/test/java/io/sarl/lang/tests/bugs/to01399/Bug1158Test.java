/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;


/** Testing class for issue: General notation for the "main" function.
 *
 * <p>https://github.com/sarl/sarl/issues/1158
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1158"
 */
@DisplayName("Bug #1158")
@SuppressWarnings("all")
@Tag("core")
public class Bug1158Test {

	@Nested
	@DisplayName("In Class")
	public class InClass {

		@Nested
		@DisplayName("Return type: void")
		public class WithVoidReturnType extends AbstractSarlTest {

			private static final String SARL_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  def main : void{ println(\"x\")}",
					"}");

			private static final String JAVA_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  public void main() {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main")
			public void parsing_1() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_01);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: main")
			public void compiling_1() throws Exception {
				getCompileHelper().compile(SARL_CODE_01, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_01, actual);
				});
			}

			private static final String SARL_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String*) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  public static void main(final String... args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String*)")
			public void parsing_2() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_02);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String*)")
			public void compiling_2() throws Exception {
				getCompileHelper().compile(SARL_CODE_02, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_02, actual);
				});
			}

			private static final String SARL_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  public static void main(final String[] args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[])")
			public void parsing_3() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_03);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String[])")
			public void compiling_3() throws Exception {
				getCompileHelper().compile(SARL_CODE_03, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_03, actual);
				});
			}

			private static final String SARL_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(it : String, b : String) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    $main_0(it[0], it[1]);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static void $main_0(final String it, final String b) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String)")
			public void parsing_4() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_04);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String)")
			public void compiling_4() throws Exception {
				getCompileHelper().compile(SARL_CODE_04, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_04, actual);
				});
			}

			private static final String SARL_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : void { println(\"x\") }",
					"  static def main(args : String, b : String) : void { println(\"y\") }",
					"}");

			private static final String JAVA_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  public static void main(final String[] args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  public static void main(final String args, final String b) {",
					"    InputOutput.<String>println(\"y\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String) / main(String[])")
			public void parsing_5() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_05);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String) / main(String[])")
			public void compiling_5() throws Exception {
				getCompileHelper().compile(SARL_CODE_05, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_05, actual);
				});
			}

			private static final String SARL_CODE_06 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : Object[]) : void { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main(Object[])")
			public void parsing_6() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_06);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						IssueCodes.INVALID_TYPE,
						"Invalid type for the parameter 'args'. Only the type String is allowed in the declaration of the main function of the program")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_07 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  dispatch def main(a : String) : void { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: dispatch main")
			public void parsing_7() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_07);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_08 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static dispatch def main(a : String) : void { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static dispatch main")
			public void parsing_8() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_08);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_09 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  protected static def main(args : String[]) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_09 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  protected static void main(final String[] args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: protected static main(String[])")
			public void parsing_9() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_09);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: protected static main(String[])")
			public void compiling_9() throws Exception {
				getCompileHelper().compile(SARL_CODE_09, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_09, actual);
				});
			}

			private static final String SARL_CODE_10 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[] = null) : void { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]=null)")
			public void parsing_10() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_10);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_DEFAULT_VALUE,
						"Unexpected definition of a default value for the formal parameter 'args'")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_11 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def <T> main(args : String[]) : void { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static <T> main(String[])")
			public void parsing_11() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_11);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_12 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : void with T { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]) with T")
			public void parsing_12() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_12);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    $main_0();",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static void $main_0() {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main()")
			public void parsing_13() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_13);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main()")
			public void compiling_13() throws Exception {
				getCompileHelper().compile(SARL_CODE_13, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_13, actual);
				});
			}

		}

		@Nested
		@DisplayName("Return type: int")
		public class WithIntReturnType extends AbstractSarlTest {

			private static final String SARL_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  def main : int { println(\"x\"); 1 }",
					"}");

			private static final String JAVA_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  public int main() {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main")
			public void parsing_1() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_01);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: main")
			public void compiling_1() throws Exception {
				getCompileHelper().compile(SARL_CODE_01, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_01, actual);
				});
			}

			private static final String SARL_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String*) : int { println(\"x\"); 1 }",
					"}");

			private static final String JAVA_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final int returnCode = $main_0(args);",
					"    System.exit(returnCode);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static int $main_0(final String... args) {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String*)")
			public void parsing_2() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_02);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String*)")
			public void compiling_2() throws Exception {
				getCompileHelper().compile(SARL_CODE_02, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_02, actual);
				});
			}

			private static final String SARL_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : int { println(\"x\"); 1 }",
					"}");

			private static final String JAVA_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final int returnCode = $main_0(args);",
					"    System.exit(returnCode);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static int $main_0(final String[] args) {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[])")
			public void parsing_3() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_03);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String[])")
			public void compiling_3() throws Exception {
				getCompileHelper().compile(SARL_CODE_03, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_03, actual);
				});
			}

			private static final String SARL_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(it : String, b : String) : int { println(\"x\"); 1 }",
					"}");

			private static final String JAVA_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    final int returnCode = $main_0(it[0], it[1]);",
					"    System.exit(returnCode);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static int $main_0(final String it, final String b) {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String)")
			public void parsing_4() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_04);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String)")
			public void compiling_4() throws Exception {
				getCompileHelper().compile(SARL_CODE_04, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_04, actual);
				});
			}

			private static final String SARL_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : int { println(\"x\"); 1 }",
					"  static def main(args : String, b : String) : int { println(\"y\"); 2 }",
					"}");

			private static final String JAVA_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final int returnCode = $main_0(args);",
					"    System.exit(returnCode);",
					"  }",
					"",
					"  public static int main(final String args, final String b) {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"y\");",
					"      _xblockexpression = 2;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static int $main_0(final String[] args) {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String) / main(String[])")
			public void parsing_5() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_05);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String) / main(String[])")
			public void compiling_5() throws Exception {
				getCompileHelper().compile(SARL_CODE_05, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_05, actual);
				});
			}

			private static final String SARL_CODE_06 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : Object[]) : int { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main(Object[])")
			public void parsing_6() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_06);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						IssueCodes.INVALID_TYPE,
						"Invalid type for the parameter 'args'. Only the type String is allowed in the declaration of the main function of the program")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_07 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  dispatch def main(a : String) : int { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: dispatch main")
			public void parsing_7() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_07);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_08 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static dispatch def main(a : String) : int { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static dispatch main")
			public void parsing_8() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_08);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_09 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  protected static def main(args : String[]) : int { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: protected static main(String[])")
			public void parsing_9() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_09);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_10 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[] = null) : int { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]=null)")
			public void parsing_10() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_10);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_DEFAULT_VALUE,
						"Unexpected definition of a default value for the formal parameter 'args'")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_11 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def <T> main(args : String[]) : int { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static <T> main(String[])")
			public void parsing_11() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_11);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_12 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : int with T { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]) with T")
			public void parsing_12() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_12);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main : int { println(\"x\"); 1 }",
					"}");

			private static final String JAVA_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    final int returnCode = $main_0();",
					"    System.exit(returnCode);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static int $main_0() {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main()")
			public void parsing_13() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_13);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main()")
			public void compiling_13() throws Exception {
				getCompileHelper().compile(SARL_CODE_13, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_13, actual);
				});
			}

		}

		@Nested
		@DisplayName("Return type: String")
		public class WithStringReturnType extends AbstractSarlTest {

			private static final String SARL_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  def main : String { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  public String main() {",
					"    return InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main")
			public void parsing_1() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_01);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: main")
			public void compiling_1() throws Exception {
				getCompileHelper().compile(SARL_CODE_01, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_01, actual);
				});
			}

			private static final String SARL_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String*) : String { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final String returnCode = $main_0(args);",
					"    System.exit(returnCode == null ? 255 : 0);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static String $main_0(final String... args) {",
					"    return InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String*)")
			public void parsing_2() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_02);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String*)")
			public void compiling_2() throws Exception {
				getCompileHelper().compile(SARL_CODE_02, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_02, actual);
				});
			}

			private static final String SARL_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : String { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final String returnCode = $main_0(args);",
					"    System.exit(returnCode == null ? 255 : 0);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static String $main_0(final String[] args) {",
					"    return InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[])")
			public void parsing_3() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_03);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String[])")
			public void compiling_3() throws Exception {
				getCompileHelper().compile(SARL_CODE_03, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_03, actual);
				});
			}

			private static final String SARL_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(it : String, b : String) : String { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    final String returnCode = $main_0(it[0], it[1]);",
					"    System.exit(returnCode == null ? 255 : 0);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static String $main_0(final String it, final String b) {",
					"    return InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String)")
			public void parsing_4() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_04);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String)")
			public void compiling_4() throws Exception {
				getCompileHelper().compile(SARL_CODE_04, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_04, actual);
				});
			}

			private static final String SARL_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : String { println(\"x\") }",
					"  static def main(args : String, b : String) : String { println(\"y\") }",
					"}");

			private static final String JAVA_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final String returnCode = $main_0(args);",
					"    System.exit(returnCode == null ? 255 : 0);",
					"  }",
					"",
					"  public static String main(final String args, final String b) {",
					"    return InputOutput.<String>println(\"y\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static String $main_0(final String[] args) {",
					"    return InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String) / main(String[])")
			public void parsing_5() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_05);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String) / main(String[])")
			public void compiling_5() throws Exception {
				getCompileHelper().compile(SARL_CODE_05, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_05, actual);
				});
			}

			private static final String SARL_CODE_06 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : Object[]) : String { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main(Object[])")
			public void parsing_6() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_06);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						IssueCodes.INVALID_TYPE,
						"Invalid type for the parameter 'args'. Only the type String is allowed in the declaration of the main function of the program")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_07 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  dispatch def main(a : String) : String { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: dispatch main")
			public void parsing_7() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_07);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_08 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static dispatch def main(a : String) : String { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static dispatch main")
			public void parsing_8() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_08);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_09 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  protected static def main(args : String[]) : String { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: protected static main(String[])")
			public void parsing_9() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_09);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_10 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[] = null) : String { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]=null)")
			public void parsing_10() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_10);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_DEFAULT_VALUE,
						"Unexpected definition of a default value for the formal parameter 'args'")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_11 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def <T> main(args : String[]) : String { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static <T> main(String[])")
			public void parsing_11() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_11);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_12 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : String with T { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]) with T")
			public void parsing_12() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_12);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : int, b : float) : String { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(int,float)")
			public void parsing_13() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_13);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						IssueCodes.INVALID_TYPE,
						"Invalid type for the parameter 'args'. Only the type String is allowed in the declaration of the main function of the program")
				.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						IssueCodes.INVALID_TYPE,
						"Invalid type for the parameter 'b'. Only the type String is allowed in the declaration of the main function of the program")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}


			private static final String SARL_CODE_14 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main : String { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_14 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    final String returnCode = $main_0();",
					"    System.exit(returnCode == null ? 255 : 0);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static String $main_0() {",
					"    return InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main()")
			public void parsing_14() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_14);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main()")
			public void compiling_14() throws Exception {
				getCompileHelper().compile(SARL_CODE_14, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_14, actual);
				});
			}

		}

		@Nested
		@DisplayName("Return type: Integer")
		public class WithIntegerReturnType extends AbstractSarlTest {

			private static final String SARL_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  def main : Integer { println(\"x\"); 1 }",
					"}");

			private static final String JAVA_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  public Integer main() {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return Integer.valueOf(_xblockexpression);",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main")
			public void parsing_1() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_01);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: main")
			public void compiling_1() throws Exception {
				getCompileHelper().compile(SARL_CODE_01, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_01, actual);
				});
			}

			private static final String SARL_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String*) : Integer { println(\"x\"); 1 }",
					"}");

			private static final String JAVA_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final Integer returnCode = $main_0(args);",
					"    System.exit(returnCode == null ? 255 : returnCode.intValue());",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static Integer $main_0(final String... args) {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return Integer.valueOf(_xblockexpression);",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String*)")
			public void parsing_2() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_02);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String*)")
			public void compiling_2() throws Exception {
				getCompileHelper().compile(SARL_CODE_02, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_02, actual);
				});
			}

			private static final String SARL_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : Integer { println(\"x\"); 1 }",
					"}");

			private static final String JAVA_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final Integer returnCode = $main_0(args);",
					"    System.exit(returnCode == null ? 255 : returnCode.intValue());",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static Integer $main_0(final String[] args) {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return Integer.valueOf(_xblockexpression);",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[])")
			public void parsing_3() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_03);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String[])")
			public void compiling_3() throws Exception {
				getCompileHelper().compile(SARL_CODE_03, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_03, actual);
				});
			}

			private static final String SARL_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(it : String, b : String) : Integer { println(\"x\"); 1 }",
					"}");

			private static final String JAVA_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    final Integer returnCode = $main_0(it[0], it[1]);",
					"    System.exit(returnCode == null ? 255 : returnCode.intValue());",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static Integer $main_0(final String it, final String b) {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return Integer.valueOf(_xblockexpression);",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String)")
			public void parsing_4() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_04);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String)")
			public void compiling_4() throws Exception {
				getCompileHelper().compile(SARL_CODE_04, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_04, actual);
				});
			}

			private static final String SARL_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : Integer { println(\"x\"); 1 }",
					"  static def main(args : String, b : String) : Integer { println(\"y\"); 2 }",
					"}");

			private static final String JAVA_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final Integer returnCode = $main_0(args);",
					"    System.exit(returnCode == null ? 255 : returnCode.intValue());",
					"  }",
					"",
					"  public static Integer main(final String args, final String b) {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"y\");",
					"      _xblockexpression = 2;",
					"    }",
					"    return Integer.valueOf(_xblockexpression);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static Integer $main_0(final String[] args) {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return Integer.valueOf(_xblockexpression);",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String) / main(String[])")
			public void parsing_5() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_05);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String) / main(String[])")
			public void compiling_5() throws Exception {
				getCompileHelper().compile(SARL_CODE_05, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_05, actual);
				});
			}

			private static final String SARL_CODE_06 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : Object[]) : Integer { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main(Object[])")
			public void parsing_6() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_06);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						IssueCodes.INVALID_TYPE,
						"Invalid type for the parameter 'args'. Only the type String is allowed in the declaration of the main function of the program")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_07 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  dispatch def main(a : String) : Integer { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: dispatch main")
			public void parsing_7() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_07);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_08 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static dispatch def main(a : String) : Integer { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static dispatch main")
			public void parsing_8() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_08);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_09 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  protected static def main(args : String[]) : Integer { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: protected static main(String[])")
			public void parsing_9() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_09);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_10 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[] = null) : Integer { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]=null)")
			public void parsing_10() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_10);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_DEFAULT_VALUE,
						"Unexpected definition of a default value for the formal parameter 'args'")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_11 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def <T> main(args : String[]) : Integer { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static <T> main(String[])")
			public void parsing_11() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_11);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_12 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : Integer with T { println(\"x\"); 1 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]) with T")
			public void parsing_12() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_12);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main : Integer { println(\"x\"); 1 }",
					"}");

			private static final String JAVA_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    final Integer returnCode = $main_0();",
					"    System.exit(returnCode == null ? 255 : returnCode.intValue());",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static Integer $main_0() {",
					"    int _xblockexpression = (int) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1;",
					"    }",
					"    return Integer.valueOf(_xblockexpression);",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main()")
			public void parsing_13() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_13);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main()")
			public void compiling_13() throws Exception {
				getCompileHelper().compile(SARL_CODE_13, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_13, actual);
				});
			}

		}

		@Nested
		@DisplayName("Return type: Object")
		public class WithObjectReturnType extends AbstractSarlTest {

			private static final String SARL_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  def main : Object { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  public Object main() {",
					"    return InputOutput.<Object>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main")
			public void parsing_1() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_01);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: main")
			public void compiling_1() throws Exception {
				getCompileHelper().compile(SARL_CODE_01, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_01, actual);
				});
			}

			private static final String SARL_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String*) : Object { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final Object returnCode = $main_0(args);",
					"    System.exit(returnCode == null ? 255 : 0);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static Object $main_0(final String... args) {",
					"    return InputOutput.<Object>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String*)")
			public void parsing_2() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_02);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String*)")
			public void compiling_2() throws Exception {
				getCompileHelper().compile(SARL_CODE_02, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_02, actual);
				});
			}

			private static final String SARL_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : Object { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final Object returnCode = $main_0(args);",
					"    System.exit(returnCode == null ? 255 : 0);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static Object $main_0(final String[] args) {",
					"    return InputOutput.<Object>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[])")
			public void parsing_3() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_03);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String[])")
			public void compiling_3() throws Exception {
				getCompileHelper().compile(SARL_CODE_03, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_03, actual);
				});
			}

			private static final String SARL_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(it : String, b : String) : Object { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    final Object returnCode = $main_0(it[0], it[1]);",
					"    System.exit(returnCode == null ? 255 : 0);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static Object $main_0(final String it, final String b) {",
					"    return InputOutput.<Object>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String)")
			public void parsing_4() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_04);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String)")
			public void compiling_4() throws Exception {
				getCompileHelper().compile(SARL_CODE_04, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_04, actual);
				});
			}

			private static final String SARL_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : Object { println(\"x\") }",
					"  static def main(args : String, b : String) : Object { println(\"y\") }",
					"}");

			private static final String JAVA_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final Object returnCode = $main_0(args);",
					"    System.exit(returnCode == null ? 255 : 0);",
					"  }",
					"",
					"  public static Object main(final String args, final String b) {",
					"    return InputOutput.<Object>println(\"y\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static Object $main_0(final String[] args) {",
					"    return InputOutput.<Object>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String) / main(String[])")
			public void parsing_5() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_05);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String) / main(String[])")
			public void compiling_5() throws Exception {
				getCompileHelper().compile(SARL_CODE_05, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_05, actual);
				});
			}

			private static final String SARL_CODE_06 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : Object[]) : Object { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main(Object[])")
			public void parsing_6() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_06);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						IssueCodes.INVALID_TYPE,
						"Invalid type for the parameter 'args'. Only the type String is allowed in the declaration of the main function of the program")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_07 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  dispatch def main(a : String) : Object { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: dispatch main")
			public void parsing_7() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_07);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_08 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static dispatch def main(a : String) : Object { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static dispatch main")
			public void parsing_8() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_08);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_09 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  protected static def main(args : String[]) : Object { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: protected static main(String[])")
			public void parsing_9() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_09);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_10 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[] = null) : Object { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]=null)")
			public void parsing_10() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_10);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_DEFAULT_VALUE,
						"Unexpected definition of a default value for the formal parameter 'args'")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_11 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def <T> main(args : String[]) : Object { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static <T> main(String[])")
			public void parsing_11() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_11);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_12 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : Object with T { println(\"x\") }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]) with T")
			public void parsing_12() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_12);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main : Object { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    final Object returnCode = $main_0();",
					"    System.exit(returnCode == null ? 255 : 0);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static Object $main_0() {",
					"    return InputOutput.<Object>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main()")
			public void parsing_13() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_13);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main()")
			public void compiling_13() throws Exception {
				getCompileHelper().compile(SARL_CODE_13, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_13, actual);
				});
			}

		}

		@Nested
		@DisplayName("Return type: double")
		public class WithDoubleReturnType extends AbstractSarlTest {

			private static final String SARL_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  def main : double { println(\"x\"); 1.0 }",
					"}");

			private static final String JAVA_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  public double main() {",
					"    double _xblockexpression = (double) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1.0;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main")
			public void parsing_1() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_01);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: main")
			public void compiling_1() throws Exception {
				getCompileHelper().compile(SARL_CODE_01, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_01, actual);
				});
			}

			private static final String SARL_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String*) : double { println(\"x\"); 1.0 }",
					"}");

			private static final String JAVA_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final double returnCode = $main_0(args);",
					"    System.exit((int) returnCode);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static double $main_0(final String... args) {",
					"    double _xblockexpression = (double) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1.0;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String*)")
			public void parsing_2() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_02);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String*)")
			public void compiling_2() throws Exception {
				getCompileHelper().compile(SARL_CODE_02, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_02, actual);
				});
			}

			private static final String SARL_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : double { println(\"x\"); 1.0 }",
					"}");

			private static final String JAVA_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final double returnCode = $main_0(args);",
					"    System.exit((int) returnCode);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static double $main_0(final String[] args) {",
					"    double _xblockexpression = (double) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1.0;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[])")
			public void parsing_3() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_03);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String[])")
			public void compiling_3() throws Exception {
				getCompileHelper().compile(SARL_CODE_03, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_03, actual);
				});
			}

			private static final String SARL_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(it : String, b : String) : double { println(\"x\"); 1.0 }",
					"}");

			private static final String JAVA_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    final double returnCode = $main_0(it[0], it[1]);",
					"    System.exit((int) returnCode);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static double $main_0(final String it, final String b) {",
					"    double _xblockexpression = (double) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1.0;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String)")
			public void parsing_4() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_04);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String)")
			public void compiling_4() throws Exception {
				getCompileHelper().compile(SARL_CODE_04, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_04, actual);
				});
			}

			private static final String SARL_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : double { println(\"x\"); 1.0 }",
					"  static def main(args : String, b : String) : double { println(\"y\"); 2.0 }",
					"}");

			private static final String JAVA_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] args) {",
					"    final double returnCode = $main_0(args);",
					"    System.exit((int) returnCode);",
					"  }",
					"",
					"  public static double main(final String args, final String b) {",
					"    double _xblockexpression = (double) 0;",
					"    {",
					"      InputOutput.<String>println(\"y\");",
					"      _xblockexpression = 2.0;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static double $main_0(final String[] args) {",
					"    double _xblockexpression = (double) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1.0;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String) / main(String[])")
			public void parsing_5() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_05);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String) / main(String[])")
			public void compiling_5() throws Exception {
				getCompileHelper().compile(SARL_CODE_05, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_05, actual);
				});
			}

			private static final String SARL_CODE_06 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : Object[]) : double { println(\"x\"); 1.0 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main(Object[])")
			public void parsing_6() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_06);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						IssueCodes.INVALID_TYPE,
						"Invalid type for the parameter 'args'. Only the type String is allowed in the declaration of the main function of the program")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_07 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  dispatch def main(a : String) : double { println(\"x\"); 1.0 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: dispatch main")
			public void parsing_7() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_07);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
						"Potential missing of the static modifier. This function has the same name as the main function of the program and therefore should be declared with this modifier")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_08 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static dispatch def main(a : String) : double { println(\"x\"); 1.0 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static dispatch main")
			public void parsing_8() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_08);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertWarning(SarlPackage.eINSTANCE.getSarlAction(), org.eclipse.xtend.core.validation.IssueCodes.SINGLE_DISPATCH_FUNCTION)
				.assertNoIssues();
			}

			private static final String SARL_CODE_09 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  protected static def main(args : String[]) : double { println(\"x\"); 1.0 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: protected static main(String[])")
			public void parsing_9() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_09);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
						"Illegal modifier for the method main; only public, static and def are permitted")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_10 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[] = null) : double { println(\"x\"); 1.0 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]=null)")
			public void parsing_10() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_10);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_DEFAULT_VALUE,
						"Unexpected definition of a default value for the formal parameter 'args'")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_11 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def <T> main(args : String[]) : double { println(\"x\"); 1.0 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static <T> main(String[])")
			public void parsing_11() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_11);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_12 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main(args : String[]) : double with T { println(\"x\"); 1.0 }",
					"}");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]) with T")
			public void parsing_12() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_12);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						SarlPackage.eINSTANCE.getSarlAction(),
						io.sarl.lang.validation.IssueCodes.UNEXPECTED_TYPE_PARAMETER,
						"Unexpected definition of a type parameter (generic type) for the main function")
				.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			private static final String SARL_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"class TheProgram {",
					"  static def main : double { println(\"x\"); 1.0 }",
					"}");

			private static final String JAVA_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram {",
					"  @SyntheticMember",
					"  public static void main(final String[] it) {",
					"    final double returnCode = $main_0();",
					"    System.exit((int) returnCode);",
					"  }",
					"",
					"  @SyntheticMember",
					"  private static double $main_0() {",
					"    double _xblockexpression = (double) 0;",
					"    {",
					"      InputOutput.<String>println(\"x\");",
					"      _xblockexpression = 1.0;",
					"    }",
					"    return _xblockexpression;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram() {",
					"    super();",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main()")
			public void parsing_13() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_13);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main()")
			public void compiling_13() throws Exception {
				getCompileHelper().compile(SARL_CODE_13, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_13, actual);
				});
			}

		}

	}

	@Nested
	@DisplayName("In Behavior")
	public class InBehavior {

		@Nested
		@DisplayName("Return type: void")
		public class WithVoidReturnType extends AbstractSarlTest {

			private static final String SARL_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  def main : void{ println(\"x\")}",
					"}");

			private static final String JAVA_CODE_01 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public void main() {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main")
			public void parsing_1() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_01);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: main")
			public void compiling_1() throws Exception {
				getCompileHelper().compile(SARL_CODE_01, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_01, actual);
				});
			}

			private static final String SARL_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  static def main(args : String*) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_02 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public static void main(final String... args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String*)")
			public void parsing_2() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_02);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String*)")
			public void compiling_2() throws Exception {
				getCompileHelper().compile(SARL_CODE_02, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_02, actual);
				});
			}

			private static final String SARL_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  static def main(args : String[]) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_03 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public static void main(final String[] args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[])")
			public void parsing_3() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_03);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String[])")
			public void compiling_3() throws Exception {
				getCompileHelper().compile(SARL_CODE_03, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_03, actual);
				});
			}

			private static final String SARL_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  static def main(it : String, b : String) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_04 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public static void main(final String it, final String b) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String)")
			public void parsing_4() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_04);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String)")
			public void compiling_4() throws Exception {
				getCompileHelper().compile(SARL_CODE_04, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_04, actual);
				});
			}

			private static final String SARL_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  static def main(args : String[]) : void { println(\"x\") }",
					"  static def main(args : String, b : String) : void { println(\"y\") }",
					"}");

			private static final String JAVA_CODE_05 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public static void main(final String[] args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  public static void main(final String args, final String b) {",
					"    InputOutput.<String>println(\"y\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String,String) / main(String[])")
			public void parsing_5() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_05);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String,String) / main(String[])")
			public void compiling_5() throws Exception {
				getCompileHelper().compile(SARL_CODE_05, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_05, actual);
				});
			}

			private static final String SARL_CODE_06 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  static def main(args : Object[]) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_06 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public static void main(final Object[] args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: main(Object[])")
			public void parsing_6() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_06);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: main(Object[])")
			public void compiling_6() throws Exception {
				getCompileHelper().compile(SARL_CODE_06, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_06, actual);
				});
			}

			private static final String SARL_CODE_07 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  dispatch def main(a : String) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_07 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public void _main(final String a) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @XbaseGenerated",
					"  public void main(final String a) {",
					"    _main(a);",
					"    return;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: dispatch main")
			public void parsing_7() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_07);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: dispatch main")
			public void compiling_7() throws Exception {
				getCompileHelper().compile(SARL_CODE_07, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_07, actual);
				});
			}

			private static final String SARL_CODE_08 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  static dispatch def main(a : String) : void { println(\"x\") }",
					"}");


			private static final String JAVA_CODE_08 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public void _main(final String a) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @XbaseGenerated",
					"  public void main(final String a) {",
					"    _main(a);",
					"    return;",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static dispatch main")
			public void parsing_8() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_08);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static dispatch main")
			public void compiling_8() throws Exception {
				getCompileHelper().compile(SARL_CODE_07, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_07, actual);
				});
			}

			private static final String SARL_CODE_09 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  protected static def main(args : String[]) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_09 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  protected static void main(final String[] args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: protected static main(String[])")
			public void parsing_9() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_09);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: protected static main(String[])")
			public void compiling_9() throws Exception {
				getCompileHelper().compile(SARL_CODE_09, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_09, actual);
				});
			}

			private static final String SARL_CODE_10 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  static def main(args : String[] = null) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_10 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.DefaultValue;",
					"import io.sarl.lang.core.annotation.DefaultValueSource;",
					"import io.sarl.lang.core.annotation.DefaultValueUse;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSourceCode;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  @DefaultValueSource",
					"  public static void main(@DefaultValue(\"io.sarl.lang.tests.bug1158.TheProgram#MAIN_0\") final String[] args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  /**",
					"   * Default value for the parameter args",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"null\")",
					"  private static String[] $DEFAULT_VALUE$MAIN_0() {",
					"    return null;",
					"  }",
					"",
					"  @DefaultValueUse(\"java.lang.String[]\")",
					"  @SyntheticMember",
					"  public static void main() {",
					"    main($DEFAULT_VALUE$MAIN_0());",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]=null)")
			public void parsing_10() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_10);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String[]=null)")
			public void compiling_10() throws Exception {
				getCompileHelper().compile(SARL_CODE_10, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_10, actual);
				});
			}

			private static final String SARL_CODE_11 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  static def <T> main(args : String[]) : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_11 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public static <T extends Object> void main(final String[] args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static <T> main(String[])")
			public void parsing_11() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_11);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static <T> main(String[])")
			public void compiling_11() throws Exception {
				getCompileHelper().compile(SARL_CODE_11, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_11, actual);
				});
			}

			private static final String SARL_CODE_12 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  static def main(args : String[]) : void with T { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_12 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public static <T extends Object> void main(final String[] args) {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main(String[]) with T")
			public void parsing_12() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_12);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main(String[]) with T")
			public void compiling_12() throws Exception {
				getCompileHelper().compile(SARL_CODE_12, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_12, actual);
				});
			}

			private static final String SARL_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158",
					"behavior TheProgram {",
					"  static def main : void { println(\"x\") }",
					"}");

			private static final String JAVA_CODE_13 = multilineString(
					"package io.sarl.lang.tests.bug1158;",
					"",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.annotation.SarlElementType;",
					"import io.sarl.lang.core.annotation.SarlSpecification;",
					"import io.sarl.lang.core.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.InputOutput;",
					"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
					"@XbaseGenerated",
					"@SuppressWarnings(\"all\")",
					"public class TheProgram extends Behavior {",
					"  public static void main() {",
					"    InputOutput.<String>println(\"x\");",
					"  }",
					"",
					"  @SyntheticMember",
					"  public TheProgram(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			@Test
			@Tag("sarlValidation")
			@DisplayName("parse: static main()")
			public void parsing_13() throws Exception {
				SarlScript mas = file(getParseHelper(), SARL_CODE_13);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertWarning(XbasePackage.eINSTANCE.getXFeatureCall(), IssueCodes.DISCOURAGED_REFERENCE)
				.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("compile: static main()")
			public void compiling_13() throws Exception {
				getCompileHelper().compile(SARL_CODE_13, (it) -> {
					String actual;
					actual = it.getGeneratedCode("io.sarl.lang.tests.bug1158.TheProgram");
					assertEquals(JAVA_CODE_13, actual);
				});
			}

		}

	}
	
}
