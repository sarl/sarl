/*
 * Copyright (C) 2014-2018 the original authors or authors.
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

import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.common.types.TypesPackage;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.MassiveCompilationSuite;
import io.sarl.tests.api.MassiveCompilationSuite.CompilationTest;
import io.sarl.tests.api.MassiveCompilationSuite.Context;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
@RunWith(MassiveCompilationSuite.class)
@SuppressWarnings("all")
public class A0NoIngerithanceTest extends AbstractSarlTest {

	private static final String NO_FUNCTION_SARL = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			"}"
			);

	@Test
	public void parseNoFunction() throws Exception {
		SarlScript mas = file(NO_FUNCTION_SARL);
		Validator val = validate(mas);
		val
			.assertError(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST,
				"Cannot cast from A1 to A0")
			.assertNoWarnings(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertNoWarnings(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	private static final String INTEGER_SARL = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return 3 as A0",
			"  }",
			"}"
			);

	@Test
	public void parseInteger() throws Exception {
		SarlScript mas = file(INTEGER_SARL);
		Validator val = validate(mas);
		val
			.assertError(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST,
				"Cannot cast from int or Integer to A0")
			.assertNoWarnings(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertNoWarnings(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	private static final String BOOLEAN_SARL = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return false as A0",
			"  }",
			"}"
			);

	@Test
	public void parseBoolean() throws Exception {
		SarlScript mas = file(BOOLEAN_SARL);
		Validator val = validate(mas);
		val
			.assertError(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST,
				"Cannot cast from boolean or Boolean to A0")
			.assertNoWarnings(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertNoWarnings(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	private static final String STRING_SARL = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return \"hello\" as A0",
			"  }",
			"}"
			);

	@Test
	public void parseString() throws Exception {
		SarlScript mas = file(STRING_SARL);
		Validator val = validate(mas);
		val
			.assertError(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST,
				"Cannot cast from String to A0")
			.assertNoWarnings(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertNoWarnings(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	private static final String LOCAL_INTEGER_SARL = multilineString(
			"class A0 {",
			"}",
			"class A2 {",
			"  def fct : A0 {",
			"    var y = fct",
			"    return 3 as A0",
			"  }",
			"  def toA0(x : int) : A0 {",
			"    null",
			"  }",
			"}"
			);

	private static final String LOCAL_INTEGER_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalInteger.A0;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct() {",
			"    A0 y = this.fct();",
			"    return this.toA0(3);",
			"  }",
			"  ",
			"  @Pure",
			"  public A0 toA0(final int x) {",
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
	public void parseLocalInteger() throws Exception {
		SarlScript mas = file(LOCAL_INTEGER_SARL);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileLocalInteger(Context ctx) throws Exception {
		ctx.compileTo(LOCAL_INTEGER_SARL, LOCAL_INTEGER_JAVA);
	}

	private static final String LOCAL_BOOLEAN_SARL = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return false as A0",
			"  }",
			"  def toA0(x : boolean) : A0 {",
			"    null",
			"  }",
			"}"
			);

	private static final String LOCAL_BOOLEAN_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalBoolean.A0;",
			"import io.sarl.lang.core.tests.compileLocalBoolean.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return this.toA0(false);",
			"  }",
			"  ",
			"  @Pure",
			"  public A0 toA0(final boolean x) {",
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
	public void parseLocalBoolean() throws Exception {
		SarlScript mas = file(LOCAL_BOOLEAN_SARL);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileLocalBoolean(Context ctx) throws Exception {
		ctx.compileTo(LOCAL_BOOLEAN_SARL, LOCAL_BOOLEAN_JAVA);
	}

	private static final String LOCAL_STRING_SARL_01 = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return \"hello\" as A0",
			"  }",
			"  def toA0(x : String) : A0 {",
			"    null",
			"  }",
			"}"
			);

	private static final String LOCAL_STRING_JAVA_01 = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalString01.A0;",
			"import io.sarl.lang.core.tests.compileLocalString01.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return this.toA0(\"hello\");",
			"  }",
			"  ",
			"  @Pure",
			"  public A0 toA0(final String x) {",
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
	public void parseLocalString01() throws Exception {
		SarlScript mas = file(LOCAL_STRING_SARL_01);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileLocalString01(Context ctx) throws Exception {
		ctx.compileTo(LOCAL_STRING_SARL_01, LOCAL_STRING_JAVA_01);
	}

	private static final String LOCAL_STRING_SARL_02 = multilineString(
			"class A0 {",
			"}",
			"class A2 {",
			"  def fct(x : String) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			"  def toA0(x : String) : A0 {",
			"    null",
			"  }",
			"}"
			);

	private static final String LOCAL_STRING_JAVA_02 = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalString02.A0;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final String x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : this.toA0(x));",
			"  }",
			"  ",
			"  @Pure",
			"  public A0 toA0(final String x) {",
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
	public void parseLocalString02() throws Exception {
		SarlScript mas = file(LOCAL_STRING_SARL_02);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileLocalString02(Context ctx) throws Exception {
		ctx.compileTo(LOCAL_STRING_SARL_02, LOCAL_STRING_JAVA_02);
	}

	private static final String LOCAL_IFTHEN_SARL = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1, y : A1) : A0 {",
			"    var z = fct(null, null)",
			"    return (if (x !== null) x else y) as A0",
			"  }",
			"  def toA0(x : A1) : A0 {",
			"    null",
			"  }",
			"}"
			);

	private static final String LOCAL_IFTHEN_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalIfThen.A0;",
			"import io.sarl.lang.core.tests.compileLocalIfThen.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x, final A1 y) {",
			"    A0 z = this.fct(null, null);",
			"    A1 _xifexpression = null;",
			"    if ((x != null)) {",
			"      _xifexpression = x;",
			"    } else {",
			"      _xifexpression = y;",
			"    }",
			"    return (_xifexpression == null ? null : this.toA0(_xifexpression));",
			"  }",
			"  ",
			"  @Pure",
			"  public A0 toA0(final A1 x) {",
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
	public void parseLocalIfThen() throws Exception {
		SarlScript mas = file(LOCAL_IFTHEN_SARL);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileLocalIfThen(Context ctx) throws Exception {
		ctx.compileTo(LOCAL_IFTHEN_SARL, LOCAL_IFTHEN_JAVA);
	}

	private static final String LOCAL_ARITH_SARL = multilineString(
			"class A0 {",
			"}",
			"class A2 {",
			"  def fct(x : double) : A0 {",
			"    var y = fct(0)",
			"    return (x + 3) as A0",
			"  }",
			"  def toA0(x : double) : A0 {",
			"    null",
			"  }",
			"}"
			);

	private static final String LOCAL_ARITH_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalArith.A0;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final double x) {",
			"    A0 y = this.fct(0);",
			"    return this.toA0((x + 3));",
			"  }",
			"  ",
			"  @Pure",
			"  public A0 toA0(final double x) {",
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
	public void parseLocalArith() throws Exception {
		SarlScript mas = file(LOCAL_ARITH_SARL);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileLocalArith(Context ctx) throws Exception {
		ctx.compileTo(LOCAL_ARITH_SARL, LOCAL_ARITH_JAVA);
	}

	private static final String LOCAL_CAST_SARL = multilineString(
			"class A0 {",
			"}",
			"class A2 {",
			"  def fct(x : double) : A0 {",
			"    var y = fct(0)",
			"    return x as long as A0",
			"  }",
			"  def toA0(x : long) : A0 {",
			"    null",
			"  }",
			"}"
			);

	private static final String LOCAL_CAST_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalCast.A0;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final double x) {",
			"    A0 y = this.fct(0);",
			"    return this.toA0(((long) x));",
			"  }",
			"  ",
			"  @Pure",
			"  public A0 toA0(final long x) {",
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
	public void parseLocalCast() throws Exception {
		SarlScript mas = file(LOCAL_CAST_SARL);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileLocalCast(Context ctx) throws Exception {
		ctx.compileTo(LOCAL_CAST_SARL, LOCAL_CAST_JAVA);
	}

	private static final String LOCAL_FUNCTION_CALL_SARL_00 = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"  def getX : A1 { null }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x.x as A0",
			"  }",
			"}"
			);

	@Test
	public void parseLocalFunctionCall00() throws Exception {
		SarlScript mas = file(LOCAL_FUNCTION_CALL_SARL_00);
		Validator val = validate(mas);
		val
			.assertError(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST,
				"Cannot cast from A1 to A0")
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	private static final String LOCAL_FUNCTION_CALL_SARL_01 = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"  def getX : A1 { null }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x.x as A0",
			"  }",
			"  def toA0(x : A1) : A0 { null }",
			"}"
			);

	private static final String LOCAL_FUNCTION_CALL_JAVA_01 = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalFunctionCall01.A0;",
			"import io.sarl.lang.core.tests.compileLocalFunctionCall01.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    A1 _x = x.getX();",
			"    return (_x == null ? null : this.toA0(_x));",
			"  }",
			"  ",
			"  @Pure",
			"  public A0 toA0(final A1 x) {",
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
	public void parseLocalFunctionCall01() throws Exception {
		SarlScript mas = file(LOCAL_FUNCTION_CALL_SARL_01);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileLocalFunctionCall01(Context ctx) throws Exception {
		ctx.compileTo(LOCAL_FUNCTION_CALL_SARL_01, LOCAL_FUNCTION_CALL_JAVA_01);
	}

	private static final String OBJECT_SARL = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"  def toA0 : A0 { null }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			"}"
			);

	private static final String OBJECT_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileObject.A0;",
			"import io.sarl.lang.core.tests.compileObject.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : x.toA0());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseObject() throws Exception {
		SarlScript mas = file(OBJECT_SARL);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileObject(Context ctx) throws Exception {
		ctx.compileTo(OBJECT_SARL, OBJECT_JAVA);
	}

	private static final String LOCAL_STATIC_SARL = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			"  static def toA0(x : A1) : A0 {",
			"    null",
			"  }",
			"  static def toString(x : A1, y : float) : String {",
			"    null",
			"  }",
			"  static def toA0(x : String) : A0 {",
			"    null",
			"  }",
			"}"
			);

	private static final String LOCAL_STATIC_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocalStatic.A0;",
			"import io.sarl.lang.core.tests.compileLocalStatic.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : A2.toA0(x));",
			"  }",
			"  ",
			"  @Pure",
			"  public static A0 toA0(final A1 x) {",
			"    return null;",
			"  }",
			"  ",
			"  @Pure",
			"  public static String toString(final A1 x, final float y) {",
			"    return null;",
			"  }",
			"  ",
			"  @Pure",
			"  public static A0 toA0(final String x) {",
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
	public void parseLocalStatic() throws Exception {
		SarlScript mas = file(LOCAL_STATIC_SARL);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileLocalStatic(Context ctx) throws Exception {
		ctx.compileTo(LOCAL_STATIC_SARL, LOCAL_STATIC_JAVA);
	}

	private static final String SUPER_STATIC_SARL = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  static def toA0(x : A1) : A0 {",
			"  }",
			"  static def toA0(x : A1, y : float) : String {",
			"  }",
			"  static def toA0(x : String) : A0 {",
			"  }",
			"}",
			"class A3 extends A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			"}"
			);

	private static final String SUPER_STATIC_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileSuperStatic.A0;",
			"import io.sarl.lang.core.tests.compileSuperStatic.A1;",
			"import io.sarl.lang.core.tests.compileSuperStatic.A2;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A3 extends A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : A2.toA0(x));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A3() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseSuperStatic() throws Exception {
		SarlScript mas = file(SUPER_STATIC_SARL);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileSuperStatic(Context ctx) throws Exception {
		ctx.compileTo(SUPER_STATIC_SARL, SUPER_STATIC_JAVA);
	}

	private static final String SUPER_SARL = multilineString(
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class A2 {",
			"  def toA0(x : A1) : A0 {",
			"  }",
			"  def toA0(x : A1, y : float) : String {",
			"  }",
			"  def toA0(x : String) : A0 {",
			"  }",
			"}",
			"class A3 extends A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			"}"
			);

	private static final String SUPER_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileSuper.A0;",
			"import io.sarl.lang.core.tests.compileSuper.A1;",
			"import io.sarl.lang.core.tests.compileSuper.A2;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A3 extends A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : this.toA0(x));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A3() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseSuper() throws Exception {
		SarlScript mas = file(SUPER_SARL);
		Validator val = validate(mas);
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
				"toA0");
	}

	@CompilationTest
	public static void compileSuper(Context ctx) throws Exception {
		ctx.compileTo(SUPER_SARL, SUPER_JAVA);
	}

	private static final String STATIC_IMPORT_SARL = multilineString(
			"import static foo.FooUtils.*",
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : String {",
			"    var y = fct(null)",
			"    return x as String",
			"  }",
			"}"
			);

	private static final String STATIC_IMPORT_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileStaticImport.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public String fct(final A1 x) {",
			"    String y = this.fct(null);",
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
	public void parseStaticImport() throws Exception {
		SarlScript mas = file(STATIC_IMPORT_SARL);
		Validator val = validate(mas);
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
				"toString");
	}

	@CompilationTest
	public static void compileStaticImport(Context ctx) throws Exception {
		ctx.compileTo(STATIC_IMPORT_SARL, STATIC_IMPORT_JAVA);
	}

	private static final String EXTENSION_IMPORT_A_SARL = multilineString(
			"package io.sarl.lang.core.tests.compileExtensionImport",
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class FooUtils {",
			"  static def toA0(x : A1) : A0 { null }",
			"}"
			);

	private static final String EXTENSION_IMPORT_B_SARL = multilineString(
			"package io.sarl.lang.core.tests.compileExtensionImport",
			"import static extension io.sarl.lang.core.tests.compileExtensionImport.FooUtils.*",
			"class A2 {",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			"}"
			);

	private static final String EXTENSION_IMPORT_JAVA = multilineString(
			"package io.sarl.lang.core.tests.compileExtensionImport;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileExtensionImport.A0;",
			"import io.sarl.lang.core.tests.compileExtensionImport.A1;",
			"import io.sarl.lang.core.tests.compileExtensionImport.FooUtils;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : FooUtils.toA0(x));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A2() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parseExtensionImport() throws Exception {
		SarlScript mas0 = file(EXTENSION_IMPORT_A_SARL);
		validate(mas0).assertNoErrors();
		SarlScript mas1 = file(EXTENSION_IMPORT_B_SARL, mas0.eResource().getResourceSet(), false);
		Validator val = validate(mas1);
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
				"toA0");
	}

	@CompilationTest
	public static void compileExtensionImport(Context ctx) throws Exception {
		ctx.compileTo(
				new String[] { EXTENSION_IMPORT_A_SARL, EXTENSION_IMPORT_B_SARL },
				"io.sarl.lang.core.tests.compileExtensionImport.A2",
				EXTENSION_IMPORT_JAVA);
	}

	private static final String EXTENSION_A_SARL = multilineString(
			"package io.sarl.lang.core.tests.compileExtension",
			"class A0 {",
			"}",
			"class A1 {",
			"}",
			"class FooUtils {",
			"  def toA0(x : A1) : A0 { null }",
			"}"
			);

	private static final String EXTENSION_B_SARL = multilineString(
			"package io.sarl.lang.core.tests.compileExtension",
			"import io.sarl.lang.tests.castoperatoroverriding.FooUtils",
			"class A2 {",
			"  extension var foo : FooUtils",
			"  def fct(x : A1) : A0 {",
			"    var y = fct(null)",
			"    return x as A0",
			"  }",
			"}"
			);

	private static final String EXTENSION_JAVA = multilineString(
			"package io.sarl.lang.core.tests.compileExtension;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileExtension.A0;",
			"import io.sarl.lang.core.tests.compileExtension.A1;",
			"import io.sarl.lang.core.tests.compileExtension.FooUtils;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Extension",
			"  private FooUtils foo;",
			"  ",
			"  @Pure",
			"  public A0 fct(final A1 x) {",
			"    A0 y = this.fct(null);",
			"    return (x == null ? null : this.foo.toA0(x));",
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
	public void parseExtension() throws Exception {
		SarlScript mas0 = file(EXTENSION_A_SARL);
		validate(mas0).assertNoErrors();
		ResourceSet rs = mas0.eResource().getResourceSet();
		SarlScript mas1 = file(EXTENSION_B_SARL, rs, false);
		Validator val = validate(mas1);
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
				"toA0");
	}

	@CompilationTest
	public static void compileExtension(Context ctx) throws Exception {
		ctx.compileTo(
				new String[] { EXTENSION_A_SARL, EXTENSION_B_SARL },
				"io.sarl.lang.core.tests.compileExtension.A2",
				EXTENSION_JAVA);
	}

}
