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
package io.sarl.lang.tests.general.parsing.general;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: break")
@Tag("core")
public class BreakKeywordTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void insideFunction() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct {",
				"    break",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlBreakExpression(),
				IssueCodes.INVALID_USE_OF_LOOP_BREAKING_KEYWORD,
				"Invalid use of the break keyword");
	}

	@Test
	@Tag("sarlValidation")
	public void insideIfThen() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    if (a == 1) {",
				"      break",
				"    }",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlBreakExpression(),
				IssueCodes.INVALID_USE_OF_LOOP_BREAKING_KEYWORD,
				"Invalid use of the break keyword");
	}

	@Test
	@Tag("sarlValidation")
	public void insideField() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  var field = [",
				"    break",
				"  ]",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlBreakExpression(),
				IssueCodes.INVALID_USE_OF_LOOP_BREAKING_KEYWORD,
				"Invalid use of the break keyword");
	}

	@Test
	@Tag("sarlValidation")
	public void insideWhileWithoutBranch() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    var b = a",
				"    while (b > 0) {",
				"      break",
				"      b--",
				"    }",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.eINSTANCE.getXPostfixOperation(),
				org.eclipse.xtext.xbase.validation.IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

	@Test
	@Tag("sarlValidation")
	public void insideWhileWithBranch() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    var b = a",
				"    while (b > 0) {",
				"      if (a == 5) break",
				"      b--",
				"    }",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("sarlValidation")
	public void insideDoWhileWithoutBranch() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    var b = a",
				"    do {",
				"      break",
				"      b--",
				"    } while (b > 0)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.eINSTANCE.getXPostfixOperation(),
				org.eclipse.xtext.xbase.validation.IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

	@Test
	@Tag("sarlValidation")
	public void insideDoWhileWithBranch() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    var b = a",
				"    do {",
				"      if (a == 5) break",
				"      b--",
				"    } while (b > 0)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("sarlValidation")
	public void insideForWithoutBranch() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(b : 0..a) {",
				"      break",
				"    }",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("sarlValidation")
	public void insideForWithBranch() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(b : 0..a) {",
				"      if (b == 5) break",
				"    }",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("sarlValidation")
	public void insideBasicForWithoutBranch() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(var b = 0; b < a; b++) {",
				"      break",
				"    }",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlBreakExpression(),
				IssueCodes.DISCOURAGED_LOOP_BREAKING_KEYWORD_USE,
				"Discouraged use of the break keyword inside a basic loop");
	}

	@Test
	@Tag("sarlValidation")
	public void insideBasicForWithBranch() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(var b = 0; b < a; b++) {",
				"      if (b == 5) break",
				"    }",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlBreakExpression(),
				IssueCodes.DISCOURAGED_LOOP_BREAKING_KEYWORD_USE,
				"Discouraged use of the break keyword inside a basic loop");
	}

	@Test
	@Tag("sarlValidation")
	public void unreachableCode() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(b : 0..a) {",
				"      if (b == 5) {",
				"        break",
				"        println(b)",
				"      }",
				"    }",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				org.eclipse.xtext.xbase.validation.IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

}
