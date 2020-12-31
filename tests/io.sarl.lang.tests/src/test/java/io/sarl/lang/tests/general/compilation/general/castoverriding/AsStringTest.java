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
@DisplayName("Compilation: as String")
@Tag("core")
@Tag("compileToJava")
public class AsStringTest extends AbstractSarlTest {

	private static final String OBJECT_TOSTRING_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    return x as String",
			"  }",
			"}"
			);

	private static final String OBJECT_TOSTRING_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileObjectToString.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return (x == null ? null : x.toString());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseObjectToString() throws Exception {
		SarlScript mas = file(getParseHelper(), OBJECT_TOSTRING_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileObjectToString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(OBJECT_TOSTRING_SARL, OBJECT_TOSTRING_JAVA);
	}

	private static final String STRING_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    return \"hello\" as String",
			"  }",
			"}"
			);

	private static final String STRING_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileString.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return \"hello\";",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseString() throws Exception {
		SarlScript mas = file(getParseHelper(), STRING_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST,
				"Unnecessary cast from String to String")
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public static void compileString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_SARL, STRING_JAVA);
	}

	private static final String IFTHEN_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1, y : A1) : String {",
			"    return (if (x !== null) x else y) as String",
			"  }",
			"}"
			);

	private static final String IFTHEN_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileIfThen.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x, final A1 y) {",
			"    A1 _xifexpression = null;",
			"    if ((x != null)) {",
			"      _xifexpression = x;",
			"    } else {",
			"      _xifexpression = y;",
			"    }",
			"    return (_xifexpression == null ? null : _xifexpression.toString());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseIfThen() throws Exception {
		SarlScript mas = file(getParseHelper(), IFTHEN_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileIfThen(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(IFTHEN_SARL, IFTHEN_JAVA);
	}

	private static final String CAST_SARL = multilineString(
			"class A2 {",
			"  def fct(x : double) : String {",
			"    return x as String as String",
			"  }",
			"}"
			);

	private static final String CAST_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final double x) {",
			"    return Double.toString(x);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseCast() throws Exception {
		SarlScript mas = file(getParseHelper(), CAST_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST,
				"String to String")
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileCast(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(CAST_SARL, CAST_JAVA);
	}

	private static final String FUNCTION_CALL_SARL = multilineString(
			"class A0 {",
			"  def toString() : String { null }",
			"}",
			"class A1 {",
			"  def getX : A0 { null }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    return x.x as String",
			"  }",
			"}"
			);

	private static final String FUNCTION_CALL_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileFunctionCall.A0;",
			"import io.sarl.lang.core.tests.compileFunctionCall.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    A0 _x = x.getX();",
			"    return (_x == null ? null : _x.toString());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseFunctionCall() throws Exception {
		SarlScript mas = file(getParseHelper(), FUNCTION_CALL_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileFunctionCall(ResourceSetGlobalCompilationContext ctx) throws Exception {
		//XXX: ctx.compileTo(FUNCTION_CALL_SARL, FUNCTION_CALL_JAVA);
	}

	private static final String LOCAL_TOSTRING_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    return x as String",
			"  }",
			"  def toString : String {",
			"    null",
			"  }",
			"  def toString(x : A1) : String {",
			"    null",
			"  }",
			"  def toString(x : A1, y : float) : String {",
			"    null",
			"  }",
			"  def toString(x : String) : String {",
			"    null",
			"  }",
			"}"
			);

	private static final String LOCAL_TOSTRING_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalToString.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return (x == null ? null : this.toString(x));",
			"  }",
			"  ",
			"  @Pure",
			"  public String toString() {",
			"    return null;",
			"  }",
			"  ",
			"  @Pure",
			"  public String toString(final A1 x) {",
			"    return null;",
			"  }",
			"  ",
			"  @Pure",
			"  public String toString(final A1 x, final float y) {",
			"    return null;",
			"  }",
			"  ",
			"  @Pure",
			"  public String toString(final String x) {",
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
	public void parseLocalToString() throws Exception {
		SarlScript mas = file(getParseHelper(), LOCAL_TOSTRING_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileLocalToString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(LOCAL_TOSTRING_SARL, LOCAL_TOSTRING_JAVA);
	}

	private static final String A1_TOSTRING_SARL = multilineString(
			"class A1 {",
			"  def toString : String {",
			"  }",
			"  def toString(x : int) : String {",
			"  }",
			"  def toString(x : int, y : float) : String {",
			"  }",
			"  def toString(x : String) : String {",
			"  }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    return x as String",
			"  }",
			"}"
			);

	private static final String A1_TOSTRING_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileA1ToString.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return (x == null ? null : x.toString());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseA1ToString() throws Exception {
		SarlScript mas = file(getParseHelper(), A1_TOSTRING_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileA1ToString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(A1_TOSTRING_SARL, A1_TOSTRING_JAVA);
	}

	private static final String LOCAL_STATIC_TOSTRING_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    return x as String",
			"  }",
			"  static def toString(x : A1) : String {",
			"  }",
			"  static def toString(x : A1, y : float) : String {",
			"  }",
			"  static def toString(x : String) : String {",
			"  }",
			"}"
			);

	private static final String LOCAL_STATIC_TOSTRING_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalStaticToString.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return (x == null ? null : A2.toString(x));",
			"  }",
			"  ",
			"  @Pure",
			"  public static String toString(final A1 x) {",
			"    return null;",
			"  }",
			"  ",
			"  @Pure",
			"  public static String toString(final A1 x, final float y) {",
			"    return null;",
			"  }",
			"  ",
			"  @Pure",
			"  public static String toString(final String x) {",
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
	public void parseLocalStaticToString() throws Exception {
		SarlScript mas = file(getParseHelper(), LOCAL_STATIC_TOSTRING_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileLocalStaticToString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(LOCAL_STATIC_TOSTRING_SARL, LOCAL_STATIC_TOSTRING_JAVA);
	}

	private static final String SUPER_STATIC_TOSTRING_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  static def toString(x : A1) : String {",
			"  }",
			"  static def toString(x : A1, y : float) : String {",
			"  }",
			"  static def toString(x : String) : String {",
			"  }",
			"}",
			"class A3 extends A2 {",
			"  def fct(x : A1) : String {",
			"    return x as String",
			"  }",
			"}"
			);

	private static final String SUPER_STATIC_TOSTRING_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileSuperStaticToString.A1;",
			"import io.sarl.lang.core.tests.compileSuperStaticToString.A2;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A3 extends A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return (x == null ? null : A2.toString(x));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A3() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseSuperStaticToString() throws Exception {
		SarlScript mas = file(getParseHelper(), SUPER_STATIC_TOSTRING_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileSuperStaticToString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(SUPER_STATIC_TOSTRING_SARL, SUPER_STATIC_TOSTRING_JAVA);
	}

	private static final String SUPER_TOSTRING_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  def toString(x : A1) : String {",
			"  }",
			"  def toString(x : A1, y : float) : String {",
			"  }",
			"  def toString(x : String) : String {",
			"  }",
			"}",
			"class A3 extends A2 {",
			"  def fct(x : A1) : String {",
			"    return x as String",
			"  }",
			"}"
			);

	private static final String SUPER_TOSTRING_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileSuperToString.A1;",
			"import io.sarl.lang.core.tests.compileSuperToString.A2;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A3 extends A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return (x == null ? null : this.toString(x));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A3() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseSuperToString() throws Exception {
		SarlScript mas = file(getParseHelper(), SUPER_TOSTRING_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileSuperToString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(SUPER_TOSTRING_SARL, SUPER_TOSTRING_JAVA);
	}

	private static final String STATIC_IMPORT_TOSTRING_SARL = multilineString(
			"import static foo.FooUtils.*",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    return x as String",
			"  }",
			"}"
			);

	private static final String STATIC_IMPORT_TOSTRING_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileStaticImportToString.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return (x == null ? null : x.toString());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseStaticImportToString() throws Exception {
		SarlScript mas = file(getParseHelper(), STATIC_IMPORT_TOSTRING_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileStaticImportToString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STATIC_IMPORT_TOSTRING_SARL, STATIC_IMPORT_TOSTRING_JAVA);
	}

	private static final String EXTENSION_IMPORT_TOSTRING_SARL = multilineString(
			"import static extension foo.FooUtils.*",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    return x as String",
			"  }",
			"}"
			);

	private static final String EXTENSION_IMPORT_TOSTRING_JAVA = multilineString(
			"import foo.FooUtils;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileExtensionImportToString.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return (x == null ? null : FooUtils.toString(x));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseExtensionImportToString() throws Exception {
		SarlScript mas = file(getParseHelper(), EXTENSION_IMPORT_TOSTRING_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileExtensionImportToString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(EXTENSION_IMPORT_TOSTRING_SARL, EXTENSION_IMPORT_TOSTRING_JAVA);
	}

	private static final String EXTENSION_TOSTRING_SARL = multilineString(
			"import foo.FooUtils2",
			"class A1 {",
			"}",
			"class A2 {",
			"  extension var foo : FooUtils2",
			"  def fct(x : A1) : String {",
			"    return x as String",
			"  }",
			"}"
			);

	private static final String EXTENSION_TOSTRING_JAVA = multilineString(
			"import foo.FooUtils2;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileExtensionToString.A1;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Extension",
			"  private FooUtils2 foo;",
			"  ",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return (x == null ? null : this.foo.toString(x));",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseExtensionToString() throws Exception {
		SarlScript mas = file(getParseHelper(), EXTENSION_TOSTRING_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileExtensionToString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(EXTENSION_TOSTRING_SARL, EXTENSION_TOSTRING_JAVA);
	}

	private static final String BOOLEAN_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    return false as String",
			"  }",
			"}"
			);

	private static final String BOOLEAN_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileBoolean.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return Boolean.toString(false);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseBoolean() throws Exception {
		SarlScript mas = file(getParseHelper(), BOOLEAN_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileBoolean(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(BOOLEAN_SARL, BOOLEAN_JAVA);
	}

	private static final String INTEGER_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    return 3 as String",
			"  }",
			"}"
			);

	private static final String INTEGER_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileIntegerToString.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    return Integer.toString(3);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseIntegerToString() throws Exception {
		SarlScript mas = file(getParseHelper(), INTEGER_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileIntegerToString(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(INTEGER_SARL, INTEGER_JAVA);
	}

	private static final String ARITH_SARL = multilineString(
			"class A2 {",
			"  def fct(x : double) : String {",
			"    return (x + 3) as String",
			"  }",
			"}"
			);

	private static final String ARITH_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final double x) {",
			"    return Double.toString((x + 3));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseArith() throws Exception {
		SarlScript mas = file(getParseHelper(), ARITH_SARL);
		Validator val = validate(getValidationHelper(), getInjector(), mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"toString");
	}

	@GlobalCompilationTestContribution
	public static void compileArith(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(ARITH_SARL, ARITH_JAVA);
	}

}
