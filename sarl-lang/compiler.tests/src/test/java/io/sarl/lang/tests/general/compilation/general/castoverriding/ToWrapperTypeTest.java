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
package io.sarl.lang.tests.general.compilation.general.castoverriding;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.common.types.TypesPackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.tests.api.globalcompilation.GlobalCompilationSuite;
import io.sarl.lang.tests.api.globalcompilation.GlobalCompilationTestContribution;
import io.sarl.lang.tests.api.globalcompilation.ResourceSetGlobalCompilationContext;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.tools.TestValidator.Validator;


/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 * @since 0.9
 */
@GlobalCompilationSuite
@SuppressWarnings("all")
@DisplayName("Compilation: as Object")
@Tag("core")
@Tag("compileToJava")
public class ToWrapperTypeTest extends AbstractSarlTest {

	private static final String NO_OPERATOR_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : Double {",
			"    var y = fct(null)",
			"    return x as Double",
			"  }",
			"}"
			);

	@Test
	public void parseNoOperator() throws Exception {
		SarlScript mas = file(getParseHelper(), NO_OPERATOR_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	private static final String TYPE_SARL_00 = multilineString(
			"class A1 {",
			"  def doubleValue : double { 0.0 }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : Double {",
			"    var y = fct(null)",
			"    return x as Double",
			"  }",
			"}"
			);

	private static final String TYPE_JAVA_00 = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public Double fct(final A1 x) {",
			"    Double y = this.fct(null);",
			"    return (x == null ? null : PrimitiveCastExtensions.toDouble(x));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseLocal00() throws Exception {
		SarlScript mas = file(getParseHelper(), TYPE_SARL_00);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toDouble'");
	}

	@GlobalCompilationTestContribution
	public void compileLocal00(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(TYPE_SARL_00, TYPE_JAVA_00);
	}

	private static final String TYPE_SARL_01 = multilineString(
			"class A1 {",
			"  def doubleValue : Double { 0.0 }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : Double {",
			"    var y = fct(null)",
			"    return x as Double",
			"  }",
			"}"
			);

	@Test
	public void parseLocal01() throws Exception {
		SarlScript mas = file(getParseHelper(), TYPE_SARL_01);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	private static final String TYPE_SARL_02 = multilineString(
			"class A1 {",
			"  def toDouble : Double { 0.0 }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : Double {",
			"    var y = fct(null)",
			"    return x as Double",
			"  }",
			"}"
			);

	private static final String TYPE_JAVA_02 = multilineString(
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
			"public class A2 {",
			"  @Pure",
			"  public Double fct(final A1 x) {",
			"    Double y = this.fct(null);",
			"    return (x == null ? null : x.toDouble());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseLocal02() throws Exception {
		SarlScript mas = file(getParseHelper(), TYPE_SARL_02);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toDouble'");
	}

	@GlobalCompilationTestContribution
	public void compileLocal02(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(TYPE_SARL_02, TYPE_JAVA_02);
	}

	private static final String TYPE_SARL_03 = multilineString(
			"class A1 {",
			"  def toDouble : double { 0.0 }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : Double {",
			"    var y = fct(null)",
			"    return x as Double",
			"  }",
			"}"
			);

	@Test
	public void parseLocal03() throws Exception {
		SarlScript mas = file(getParseHelper(), TYPE_SARL_03);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

}
