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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: assert")
@Tag("core")
public class AssertKeywordTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void assertTrue() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert true",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void assertBooleanTrue() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert Boolean::TRUE",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void assertFalse() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert false",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void assertBooleanFalse() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert Boolean::FALSE",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void assertOnParameter() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert x > 0",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void assertComplexBooleanExpression() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    var y = x + 1",
				"    assert x > 0 && y < 100",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void assertTrueWithMessage() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert true, \"Hello world!\"",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void assertComplexBooleanExpressionWithMessage() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    var y = x + 1",
				"    assert x > 0 && y < 100, \"That's all, folks!\"",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

}
