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

package io.sarl.lang.tests.bugs.to00699;

import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Cannot put class constant in parameter default value.
 *
 * <p>https://github.com/sarl/sarl/issues/592
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug592 extends AbstractSarlTest {

	@Test
	public void numberLiteral() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : int = 1) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void stringLiteral() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : String = \"\") { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void arrayLiteral() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : int[] = #{1, 2}) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void lambdaLiteral() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : (int) => int = [1]) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void constantExpression() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : int = 1 + 3 / 2) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void enumLiteral01() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"enum MyEnum { TOK1, TOK2 }",
				"class XXX {",
				"  def fct(param : MyEnum = MyEnum::TOK2) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void enumLiteral02() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"enum MyEnum { TOK1, TOK2 }",
				"class XXX {",
				"  def fct(param : MyEnum = MyEnum.TOK2) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void localStaticVariable() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  static var myField = 1",
				"  def fct(param : int = myField) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.FORBIDDEN_REFERENCE,
				"Forbidden reference to not final field myField from a default value");
	}

	@Test
	public void localStaticValue() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  static val myField = 1",
				"  def fct(param : int = myField) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void localVariable() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  var myField = 1",
				"  def fct(param : int = myField) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.STATIC_ACCESS_TO_INSTANCE_MEMBER,
				"Cannot make a static reference to the non-static field myField");
	}

	@Test
	public void localValue() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  val myField = 1",
				"  def fct(param : int = myField) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.STATIC_ACCESS_TO_INSTANCE_MEMBER,
				"Cannot make a static reference to the non-static field myField");
	}

	@Test
	public void outsideStaticFunction() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class YYY {",
				"  static def otherFct : int { 1 }",
				"}",
				"class XXX {",
				"  def fct(param : int = YYY::otherFct) { }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void functionValue() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug592",
				"class XXX {",
				"  def fct(param : int = myField) {",
				"    val myField = 1",
				"  }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				Diagnostic.LINKING_DIAGNOSTIC,
				"The method or field myField is undefined");
	}

}
