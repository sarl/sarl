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
import org.eclipse.xtext.xbase.validation.IssueCodes;
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
@DisplayName("Syntax: @PrivateAPIAccess")
@Tag("core")
public class PrivateAPIAccessTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void privateFunctionPublicCaller_01() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import foo.PrivateAPIObject",
				"",
				"class Accessor {",
				"	def doSomething(a : PrivateAPIObject) {",
				"		a.function",
				"	}",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.Literals.XMEMBER_FEATURE_CALL,
				IssueCodes.FORBIDDEN_REFERENCE,
				"Forbidden feature call");
	}

	@Test
	@Tag("sarlValidation")
	public void privateFunctionPrivateCaller_01() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject",
				"",
				"class Accessor {",
				"  @PrivateAPI(isCallerOnly=true)",
				"  def doSomething(a : PrivateAPIObject) {",
				"    a.function",
				"  }",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void privateFunctionPrivateCaller_02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject",
				"",
				"@PrivateAPI(isCallerOnly=true)",
				"class Accessor {",
				"  def doSomething(a : PrivateAPIObject) {",
				"    a.function",
				"  }",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void privateFunctionPublicCaller_02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import foo.PrivateAPIObject2",
				"",
				"class Accessor {",
				"	def doSomething(a : PrivateAPIObject2) {",
				"		a.function",
				"	}",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.Literals.XMEMBER_FEATURE_CALL,
				IssueCodes.FORBIDDEN_REFERENCE,
				"Forbidden feature call");
	}

	@Test
	@Tag("sarlValidation")
	public void privateFunctionPrivateCaller_03() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject2",
				"",
				"class Accessor {",
				"  @PrivateAPI(isCallerOnly=true)",
				"  def doSomething(a : PrivateAPIObject2) {",
				"    a.function",
				"  }",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void privateFunctionPrivateCaller_04() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject2",
				"",
				"@PrivateAPI(isCallerOnly=true)",
				"class Accessor {",
				"  def doSomething(a : PrivateAPIObject2) {",
				"    a.function",
				"  }",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void ambigousPrivateAPI_01() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject2",
				"import static extension foo.PrivateAPIObject3.*",
				"",
				"class Accessor {",
				"  def doSomething(a : PrivateAPIObject2) {",
				"    a.function",
				"  }",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.Literals.XMEMBER_FEATURE_CALL,
				IssueCodes.FORBIDDEN_REFERENCE,
				"Forbidden feature call");
	}

	@Test
	@Tag("sarlValidation")
	public void ambigousPrivateAPI_02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject2",
				"import static extension foo.PrivateAPIObject3.*",
				"",
				"@PrivateAPI",
				"class Accessor {",
				"  def doSomething(a : PrivateAPIObject2) {",
				"    a.function",
				"  }",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void ambigousPrivateAPI_03() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject2",
				"import static extension foo.PrivateAPIObject3.*",
				"",
				"@PrivateAPI(isCallerOnly=true)",
				"class Accessor {",
				"  def doSomething(a : PrivateAPIObject2) {",
				"    a.function",
				"  }",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void ambigousPrivateAPI_04() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import io.sarl.core.AgentTask",
				"import io.sarl.core.Schedules",
				"",
				"agent Accessor {",
				"  uses Schedules",
				"  var t : AgentTask",
				"  def action : void {",
				"    this.t.name = \"hello\"",
				"  }",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

}
