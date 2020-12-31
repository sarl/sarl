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
package io.sarl.lang.tests.general.compilation.general.castoverriding;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.common.types.TypesPackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.globalcompilation.GlobalCompilationSuite;
import io.sarl.tests.api.globalcompilation.GlobalCompilationTestContribution;
import io.sarl.tests.api.globalcompilation.ResourceSetGlobalCompilationContext;
import io.sarl.tests.api.tools.TestValidator.Validator;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
@GlobalCompilationSuite
@SuppressWarnings("all")
@DisplayName("Compilation: Inheritance of casting operators")
@Tag("core")
@Tag("compileToJava")
public class InheritanceTest extends AbstractSarlTest {

	private static final String TO_A3_FROM_A1_SARL = multilineString(
			"class A0 {",
			"}",
			"class A3 extends A0 {",
			"}",
			"class A1 {",
			" def toA3 : A3 { null }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			"}"
			);

	private static final String TO_A3_FROM_A1_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileToA3FromA1.A0;",
			"import io.sarl.lang.core.tests.compileToA3FromA1.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : x.toA3());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseToA3FromA1() throws Exception {
		SarlScript mas = file(getParseHelper(), TO_A3_FROM_A1_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toA3'", "'A0'");
	}

	@GlobalCompilationTestContribution
	public static void compileToA3FromA1(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(TO_A3_FROM_A1_SARL, TO_A3_FROM_A1_JAVA);
	}

	private static final String TO_A3_FROM_A4_SARL = multilineString(
			"import foo.FooUtils2",
			"class A0 {",
			"}",
			"class A3 extends A0 {",
			"}",
			"class A4 {",
			" def toA3 : A3 { null }",
			"}",
			"class A1 extends A4 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			"}"
			);

	private static final String TO_A3_FROM_A4_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileToA3FromA4.A0;",
			"import io.sarl.lang.core.tests.compileToA3FromA4.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : x.toA3());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseToA3FromA4() throws Exception {
		SarlScript mas = file(getParseHelper(), TO_A3_FROM_A4_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toA3'", "'A0'");
	}

	@GlobalCompilationTestContribution
	public static void compileToA3FromA4(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(TO_A3_FROM_A4_SARL, TO_A3_FROM_A4_JAVA);
	}

	private static final String STATIC_TO_A3_FROM_A1_SARL = multilineString(
			"import foo.FooUtils2",
			"class A0 {",
			"}",
			"class A3 extends A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			" static def toA3(x : A1) : A3 { null }",
			"}"
			);

	private static final String STATIC_TO_A3_FROM_A1_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileStaticToA3FromA1.A0;",
			"import io.sarl.lang.core.tests.compileStaticToA3FromA1.A1;",
			"import io.sarl.lang.core.tests.compileStaticToA3FromA1.A3;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : A2.toA3(x));",
			"  }",
			"  ",
			"  @Pure",
			"  public static A3 toA3(final A1 x) {",
			"    return null;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseStaticToA3FromA1() throws Exception {
		SarlScript mas = file(getParseHelper(), STATIC_TO_A3_FROM_A1_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toA3'", "'A0'");
	}

	@GlobalCompilationTestContribution
	public static void compileStaticToA3FromA1(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STATIC_TO_A3_FROM_A1_SARL, STATIC_TO_A3_FROM_A1_JAVA);
	}

	private static final String STATIC_TO_A3_FROM_A4_SARL = multilineString(
			"import foo.FooUtils2",
			"class A0 {",
			"}",
			"class A3 extends A0 {",
			"}",
			"class A4 {",
			"}",
			"class A1 extends A4 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			" static def toA3(x : A4) : A3 { null }",
			"}"
			);

	private static final String STATIC_TO_A3_FROM_A4_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileStaticToA3FromA4.A0;",
			"import io.sarl.lang.core.tests.compileStaticToA3FromA4.A1;",
			"import io.sarl.lang.core.tests.compileStaticToA3FromA4.A3;",
			"import io.sarl.lang.core.tests.compileStaticToA3FromA4.A4;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : A2.toA3(x));",
			"  }",
			"  ",
			"  @Pure",
			"  public static A3 toA3(final A4 x) {",
			"    return null;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseStaticToA3FromA4() throws Exception {
		SarlScript mas = file(getParseHelper(), STATIC_TO_A3_FROM_A4_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toA3'", "'A0'");
	}

	@GlobalCompilationTestContribution
	public static void compileStaticToA3FromA4(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STATIC_TO_A3_FROM_A4_SARL, STATIC_TO_A3_FROM_A4_JAVA);
	}

}
