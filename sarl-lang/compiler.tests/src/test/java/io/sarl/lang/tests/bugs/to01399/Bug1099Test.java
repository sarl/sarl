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
import static org.eclipse.xtext.xbase.validation.IssueCodes.STATIC_ACCESS_TO_INSTANCE_MEMBER;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid priority of minus operator with extension method.
 *
 * <p>https://github.com/sarl/sarl/issues/1099
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 * @see "https://github.com/sarl/sarl/issues/1099"
 */
@DisplayName("Bug #1099")
@SuppressWarnings("all")
@Tag("core")
public class Bug1099Test extends AbstractSarlTest {

	private final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     -125.abs",
			"   }",
			"}");

	private final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1099;",
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
			"public class Test {",
			"  @Pure",
			"  public static double prog() {",
			"    int _abs = Math.abs(125);",
			"    return (-_abs);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Test() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing: -125.abs")
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
			XbasePackage.eINSTANCE.getXUnaryOperation(),
			io.sarl.lang.validation.IssueCodes.AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER,
			"Potential ambiguous notation for -125.abs");
	}

	@Test
	@DisplayName("Compiling: -125.abs")
	@Tag("compileToJava")
	public void compiling01() throws Exception {
		getCompileHelper().assertCompilesTo(SARL_CODE_01, JAVA_CODE_01);
	}

	private final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     abs(-125)",
			"   }",
			"}");

	private final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1099;",
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
			"public class Test {",
			"  @Pure",
			"  public static double prog() {",
			"    return Math.abs((-125));",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Test() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing: abs(-125)")
	@Tag("sarlParsing")
	public void parsing02() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling: abs(-125)")
	@Tag("compileToJava")
	public void compiling02() throws Exception {
		getCompileHelper().assertCompilesTo(SARL_CODE_02, JAVA_CODE_02);
	}

	private final String SARL_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     (-125).abs",
			"   }",
			"}");

	private final String JAVA_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1099;",
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
			"public class Test {",
			"  @Pure",
			"  public static double prog() {",
			"    return Math.abs((-125));",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Test() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing: (-125).abs")
	@Tag("sarlParsing")
	public void parsing03() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling: (-125).abs")
	@Tag("compileToJava")
	public void compiling03() throws Exception {
		getCompileHelper().assertCompilesTo(SARL_CODE_03, JAVA_CODE_03);
	}

	private final String SARL_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     ---125.abs",
			"   }",
			"}");

	@Test
	@DisplayName("Parsing: ---125.abs")
	@Tag("sarlParsing")
	public void parsing04() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXBlockExpression(),
				Diagnostic.SYNTAX_DIAGNOSTIC,
				"mismatched input '--'");
	}

	private final String SARL_CODE_05 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     --125.abs",
			"   }",
			"}");

	@Test
	@DisplayName("Parsing: --125.abs")
	@Tag("sarlParsing")
	public void parsing05() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_05);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXBlockExpression(),
				Diagnostic.SYNTAX_DIAGNOSTIC,
				"mismatched input '--'");
	}

	private final String SARL_CODE_06 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     +-125.abs",
			"   }",
			"}");

	@Test
	@DisplayName("Parsing: +-125.abs")
	@Tag("sarlParsing")
	public void parsing06() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_06);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.INVALID_NUMBER_OF_ARGUMENTS,
				"Invalid number of arguments", "operator_plus(int, double)");
	}

	private final String SARL_CODE_07 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     -+125.abs",
			"   }",
			"}");

	@Test
	@DisplayName("Parsing: -+125.abs")
	@Tag("sarlParsing")
	public void parsing07() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_07);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.INVALID_NUMBER_OF_ARGUMENTS,
				"Invalid number of arguments", "operator_plus(int, double)");
	}

	private final String SARL_CODE_08 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     7--125.abs",
			"   }",
			"}");

	@Test
	@DisplayName("Parsing: 7--125.abs")
	@Tag("sarlParsing")
	public void parsing08() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_08);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXNumberLiteral(),
				IssueCodes.ASSIGNMENT_TO_NO_VARIABLE,
				"The left-hand side of an assignment must be a variable");
	}

	private final String SARL_CODE_09 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     7+-125.abs",
			"   }",
			"}");

	private final String JAVA_CODE_09 = multilineString(
			"package io.sarl.lang.tests.bug1099;",
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
			"public class Test {",
			"  @Pure",
			"  public static double prog() {",
			"    int _abs = Math.abs(125);",
			"    return (7 + (-_abs));",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Test() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing: 7+-125.abs")
	@Tag("sarlParsing")
	public void parsing09() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_09);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				io.sarl.lang.validation.IssueCodes.AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER,
				"Potential ambiguous notation for -125.abs");
	}

	@Test
	@DisplayName("Compiling: 7+-125.abs")
	@Tag("compileToJava")
	public void compiling09() throws Exception {
		getCompileHelper().assertCompilesTo(SARL_CODE_09, JAVA_CODE_09);
	}

	private final String SARL_CODE_10 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     7-+125.abs",
			"   }",
			"}");

	@Test
	@DisplayName("Parsing: 7-+125.abs")
	@Tag("sarlParsing")
	public void parsing10() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_10);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.INVALID_NUMBER_OF_ARGUMENTS,
				"Invalid number of arguments", "operator_plus(int, double)");
	}
	
	private final String SARL_CODE_11 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     - - -125.abs",
			"   }",
			"}");

	private final String JAVA_CODE_11 = multilineString(
			"package io.sarl.lang.tests.bug1099;",
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
			"public class Test {",
			"  @Pure",
			"  public static double prog() {",
			"    int _abs = Math.abs(125);",
			"    return (-(-(-_abs)));",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Test() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing: - - -125.abs")
	@Tag("sarlParsing")
	public void parsing11() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_11);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				io.sarl.lang.validation.IssueCodes.AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER,
				"Potential ambiguous notation for -125.abs");
	}

	@Test
	@DisplayName("Compiling: - - -125.abs")
	@Tag("compileToJava")
	public void compiling11() throws Exception {
		getCompileHelper().assertCompilesTo(SARL_CODE_11, JAVA_CODE_11);
	}

	private final String SARL_CODE_12 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     - -125.abs",
			"   }",
			"}");

	private final String JAVA_CODE_12 = multilineString(
			"package io.sarl.lang.tests.bug1099;",
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
			"public class Test {",
			"  @Pure",
			"  public static double prog() {",
			"    int _abs = Math.abs(125);",
			"    return (-(-_abs));",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Test() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing: - -125.abs")
	@Tag("sarlParsing")
	public void parsing12() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_12);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				io.sarl.lang.validation.IssueCodes.AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER,
				"Potential ambiguous notation for -125.abs");
	}

	@Test
	@DisplayName("Compiling: - -125.abs")
	@Tag("compileToJava")
	public void compiling12() throws Exception {
		getCompileHelper().assertCompilesTo(SARL_CODE_12, JAVA_CODE_12);
	}

	private final String SARL_CODE_13 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     + -125.abs",
			"   }",
			"}");

	@Test
	@DisplayName("Parsing: + -125.abs")
	@Tag("sarlParsing")
	public void parsing13() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_13);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.INVALID_NUMBER_OF_ARGUMENTS,
				"Invalid number of arguments", "operator_plus(int, double)");
	}

	private final String SARL_CODE_14 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     - +125.abs",
			"   }",
			"}");

	@Test
	@DisplayName("Parsing: - +125.abs")
	@Tag("sarlParsing")
	public void parsing14() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_14);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.INVALID_NUMBER_OF_ARGUMENTS,
				"Invalid number of arguments", "operator_plus(int, double)");
	}

	private final String SARL_CODE_15 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     7 - -125.abs",
			"   }",
			"}");

	private final String JAVA_CODE_15 = multilineString(
			"package io.sarl.lang.tests.bug1099;",
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
			"public class Test {",
			"  @Pure",
			"  public static double prog() {",
			"    int _abs = Math.abs(125);",
			"    return (7 - (-_abs));",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Test() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing: 7 - -125.abs")
	@Tag("sarlParsing")
	public void parsing15() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_15);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				io.sarl.lang.validation.IssueCodes.AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER,
				"Potential ambiguous notation for -125.abs");
	}

	@Test
	@DisplayName("Compiling: 7 - -125.abs")
	@Tag("compileToJava")
	public void compiling15() throws Exception {
		getCompileHelper().assertCompilesTo(SARL_CODE_15, JAVA_CODE_15);
	}

	private final String SARL_CODE_16 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     7 + -125.abs",
			"   }",
			"}");

	private final String JAVA_CODE_16 = multilineString(
			"package io.sarl.lang.tests.bug1099;",
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
			"public class Test {",
			"  @Pure",
			"  public static double prog() {",
			"    int _abs = Math.abs(125);",
			"    return (7 + (-_abs));",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Test() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing: 7 + -125.abs")
	@Tag("sarlParsing")
	public void parsing16() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_16);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				io.sarl.lang.validation.IssueCodes.AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER,
				"Potential ambiguous notation for -125.abs");
	}

	@Test
	@DisplayName("Compiling: 7 + -125.abs")
	@Tag("compileToJava")
	public void compiling16() throws Exception {
		getCompileHelper().assertCompilesTo(SARL_CODE_16, JAVA_CODE_16);
	}

	private final String SARL_CODE_17 = multilineString(
			"package io.sarl.lang.tests.bug1099",
			"import static extension java.lang.Math.abs",
			"class Test {",
			"   static def prog : double {",
			"     7 - +125.abs",
			"   }",
			"}");

	private final String JAVA_CODE_17 = multilineString(
			"package io.sarl.lang.tests.bug1099;",
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
			"public class Test {",
			"  @Pure",
			"  public static double prog() {",
			"    return 7 - Math.abs((125));",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Test() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing: 7 - +125.abs")
	@Tag("sarlParsing")
	public void parsing17() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_17);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.INVALID_NUMBER_OF_ARGUMENTS,
				"Invalid number of arguments", "operator_plus(int, double)");
	}

}
