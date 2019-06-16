/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import com.google.inject.Inject;
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class for issue: Check if multi-parameter lambda could be implemented.
 *
 * <p>https://github.com/sarl/sarl/issues/919
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/919"
 */
@SuppressWarnings("all")
public class Bug919 extends AbstractSarlTest {

	private static final String SARL_CODE = multilineString(
			"package io.sarl.lang.tests.bug919",
			"interface X {",
			"    def fct(a : double, b : double, c : Object, d : String)",
			"}",
			"",
			"class Test {",
			"   def called(a : int, b : X) { }",
			"   def test {",
			"       called(1) [",
			"       ]",
			"   }",
			"}");

	private static final String JAVA_CODE = multilineString(
			"package io.sarl.lang.tests.bug919;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.tests.bug919.X;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Test {",
			"  @Pure",
			"  public void called(final int a, final X b) {",
			"  }",
			"  ",
			"  @Pure",
			"  public void test() {",
			"    final X _function = (double $0, double $1, Object $2, String $3) -> {",
			"    };",
			"    this.called(1, _function);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Test() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parsing() throws Exception {
		SarlScript mas = file(SARL_CODE);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void compiling() throws Exception {
		getCompileHelper().compile(SARL_CODE, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug919.Test");
			assertEquals(JAVA_CODE, actual);
		});
	}

	private static final String PARAMETER_DECLARATION_SARL_CODE = multilineString(
			"package io.sarl.lang.tests.bug919",
			"class Test2 {",
			"   def test($1 : int) {",
			"   }",
			"}");

	@Test
	public void parsingParameterDeclaration() throws Exception {
		SarlScript mas = file(PARAMETER_DECLARATION_SARL_CODE);
		final Validator validator = validate(mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlFormalParameter(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid parameter name '$1'");
	}

	private static final String LAMBDA_PARAMETER_DECLARATION_SARL_CODE = multilineString(
			"package io.sarl.lang.tests.bug919",
			"interface X2 {",
			"    def fct(a : double, b : double, c : Object, d : String)",
			"}",
			"",
			"class Test3 {",
			"   def called(a : int, b : X2) { }",
			"   def test {",
			"       called(1) [x, $3, z |",
			"       ]",
			"   }",
			"}");

	@Test
	public void parsingLambdaParameterDeclaration() throws Exception {
		SarlScript mas = file(LAMBDA_PARAMETER_DECLARATION_SARL_CODE);
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXClosure(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid parameter name '$3'");
	}

	private static final String LOCAL_VARIABLE_DECLARATION_SARL_CODE = multilineString(
			"package io.sarl.lang.tests.bug919",
			"class Test4 {",
			"   def test : int {",
			"       var $2 : int = 1",
			"       return $2 + 1",
			"   }",
			"}");

	@Test
	public void parsingLocalVariableDeclaration() throws Exception {
		SarlScript mas = file(LOCAL_VARIABLE_DECLARATION_SARL_CODE);
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXVariableDeclaration(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid variable name '$2'");
	}

	private static final String FIELD_DECLARATION_SARL_CODE = multilineString(
			"package io.sarl.lang.tests.bug919",
			"class Test4 {",
			"   var $2 : int",
			"   def test : int {",
			"       return $2 + 1",
			"   }",
			"}");

	@Test
	public void parsingFieldDeclaration() throws Exception {
		SarlScript mas = file(FIELD_DECLARATION_SARL_CODE);
		final Validator validator = validate(mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid attribute name '$2'");
	}

	private static final String ACTION_DECLARATION_SARL_CODE = multilineString(
			"package io.sarl.lang.tests.bug919",
			"class Test6 {",
			"   def $2 : int {",
			"   }",
			"}");

	@Test
	public void parsingActionDeclaration() throws Exception {
		SarlScript mas = file(ACTION_DECLARATION_SARL_CODE);
		final Validator validator = validate(mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME,
				"Invalid action name '$2'");
	}

	private static final String DOLLAR_VARIABLE_REFERENCE_SARL_CODE = multilineString(
			"package io.sarl.lang.tests.bug919",
			"interface X3 {",
			"    def fct(a : double, b : double, c : Object, d : String) : double",
			"}",
			"",
			"class Test5 {",
			"   def called(a : int, b : X3) { }",
			"   def test {",
			"       called(1) [",
			"          $1",
			"       ]",
			"   }",
			"}");

	private static final String DOLLAR_VARIABLE_REFERENCE_JAVA_CODE = multilineString(
			"package io.sarl.lang.tests.bug919;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.tests.bug919.X3;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Test5 {",
			"  @Pure",
			"  public void called(final int a, final X3 b) {",
			"  }",
			"  ",
			"  @Pure",
			"  public void test() {",
			"    final X3 _function = (double $0, double $1, Object $2, String $3) -> {",
			"      return $1;",
			"    };",
			"    this.called(1, _function);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Test5() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parsingDollarVariableReference() throws Exception {
		SarlScript mas = file(DOLLAR_VARIABLE_REFERENCE_SARL_CODE);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compilingDollarVariableReference() throws Exception {
		getCompileHelper().compile(DOLLAR_VARIABLE_REFERENCE_SARL_CODE, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug919.Test5");
			assertEquals(DOLLAR_VARIABLE_REFERENCE_JAVA_CODE, actual);
		});
	}

	private static final String INVALID_DOLLAR_VARIABLE_REFERENCE_SARL_CODE = multilineString(
			"package io.sarl.lang.tests.bug919",
			"class Test6 {",
			"   def test : Object {",
			"      $1",
			"   }",
			"}");

	@Test
	public void parsingInvalidDollarVariableReference() throws Exception {
		SarlScript mas = file(INVALID_DOLLAR_VARIABLE_REFERENCE_SARL_CODE);
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				org.eclipse.xtext.diagnostics.Diagnostic.LINKING_DIAGNOSTIC,
				"The method or field $1 is undefined");
	}

}
