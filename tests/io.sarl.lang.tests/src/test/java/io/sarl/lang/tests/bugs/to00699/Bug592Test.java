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

package io.sarl.lang.tests.bugs.to00699;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Cannot put class constant in parameter default value.
 *
 * <p>https://github.com/sarl/sarl/issues/592
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/592"
 * @see "https://github.com/sarl/sarl/issues/612"
 */
@DisplayName("Bug #592")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug592Test extends AbstractSarlTest {

	@Test
	public void numberLiteral() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : int = 1) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	public void stringLiteral() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : String = \"\") { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	public void arrayLiteral() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : int[] = #{1, 2}) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	public void lambdaLiteral() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : (int) => int = [1]) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	public void constantExpression() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : int = 1 + 3 / 2) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	public void enumLiteral01() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"enum MyEnum { TOK1, TOK2 }",
				"class XXX {",
				"  def fct(param : MyEnum = MyEnum::TOK2) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	public void enumLiteral02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"enum MyEnum { TOK1, TOK2 }",
				"class XXX {",
				"  def fct(param : MyEnum = MyEnum.TOK2) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	public void localStaticVariable() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  static var myField = 1",
				"  def fct(param : int = myField) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		// Test has been changed due to the evolution of the default value support (Issue #612)
		validator.assertNoIssues();
	}

	@Test
	public void localStaticValue() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  static val myField = 1",
				"  def fct(param : int = myField) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	public void localVariable() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  var myField = 1",
				"  def fct(param : int = myField) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		// Test has been changed due to the evolution of the default value support (Issue #612)
		validator.assertNoIssues();
	}

	@Test
	public void localValue() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  val myField = 1",
				"  def fct(param : int = myField) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		// Test has been changed due to the evolution of the default value support (Issue #612)
		validator.assertNoIssues();
	}

	@Test
	public void outsideStaticFunction() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class YYY {",
				"  static def otherFct : int { 1 }",
				"}",
				"class XXX {",
				"  def fct(param : int = YYY::otherFct) { }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	public void functionValue() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : int = myField) {",
				"    val myField = 1",
				"  }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				Diagnostic.LINKING_DIAGNOSTIC,
				"The method or field myField is undefined");
	}

}
