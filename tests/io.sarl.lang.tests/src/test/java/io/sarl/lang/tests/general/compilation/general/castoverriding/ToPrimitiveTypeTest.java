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

import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.XbasePackage;
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
public class ToPrimitiveTypeTest extends AbstractSarlTest {

	private static final String NO_OPERATOR_SARL = multilineString(
			"class A1 {",
			"}",
			"class A2 {",
			"  def fct(x : A1) : double {",
			"    var y = fct(null)",
			"    return x as double",
			"  }",
			"}"
			);

	@Test
	public void parseNoOperator() throws Exception {
		SarlScript mas = file(NO_OPERATOR_SARL);
		Validator val = validate(mas);
		val.assertError(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST,
				"Cannot cast from A1 to double");
	}

	private static final String TYPE_SARL_00 = multilineString(
			"class A1 {",
			"  def doubleValue : double { 0.0 }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : double {",
			"    var y = fct(null)",
			"    return x as double",
			"  }",
			"}"
			);

	private static final String TYPE_JAVA_00 = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocal00.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public double fct(final A1 x) {",
			"    double y = this.fct(null);",
			"    return (x == null ? 0 : x.doubleValue());",
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
		SarlScript mas = file(TYPE_SARL_00);
		Validator val = validate(mas);
		val
			.assertNoErrors()
			.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'doubleValue'");
	}

	@CompilationTest
	public static void compileLocal00(Context ctx) throws Exception {
		ctx.compileTo(TYPE_SARL_00, TYPE_JAVA_00);
	}

	private static final String TYPE_SARL_01 = multilineString(
			"class A1 {",
			"  def doubleValue : Double { 0.0 }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : double {",
			"    var y = fct(null)",
			"    return x as double",
			"  }",
			"}"
			);

	@Test
	public void parseLocal01() throws Exception {
		SarlScript mas = file(TYPE_SARL_01);
		Validator val = validate(mas);
		val.assertError(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST,
				"Cannot cast from A1 to double");
	}

	private static final String TYPE_SARL_02 = multilineString(
			"class A1 {",
			"  def toDouble : Double { 0.0 }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : double {",
			"    var y = fct(null)",
			"    return x as double",
			"  }",
			"}"
			);

	private static final String TYPE_JAVA_02 = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.tests.compileLocal02.A1;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A2 {",
			"  @Pure",
			"  public double fct(final A1 x) {",
			"    double y = this.fct(null);",
			"    return (x == null ? 0 : ((x.toDouble()) == null ? 0 : (x.toDouble()).doubleValue()));",
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
		SarlScript mas = file(TYPE_SARL_02);
		Validator val = validate(mas);
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

	@CompilationTest
	public static void compileLocal02(Context ctx) throws Exception {
		ctx.compileTo(TYPE_SARL_02, TYPE_JAVA_02);
	}

	private static final String TYPE_SARL_03 = multilineString(
			"class A1 {",
			"  def toDouble : double { 0.0 }",
			"}",
			"class A2 {",
			"  def fct(x : A1) : double {",
			"    var y = fct(null)",
			"    return x as double",
			"  }",
			"}"
			);

	@Test
	public void parseLocal03() throws Exception {
		SarlScript mas = file(TYPE_SARL_03);
		Validator val = validate(mas);
		val.assertError(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST,
				"Cannot cast from A1 to double");
	}

}
