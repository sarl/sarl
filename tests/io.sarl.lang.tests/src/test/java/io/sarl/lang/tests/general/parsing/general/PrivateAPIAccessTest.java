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
package io.sarl.lang.tests.general.parsing.general;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class PrivateAPIAccessTest extends AbstractSarlTest {

	@Test
	public void privateFunctionPublicCaller_01() throws Exception {
		SarlScript mas = file(multilineString(
				"import foo.PrivateAPIObject",
				"",
				"class Accessor {",
				"	def doSomething(a : PrivateAPIObject) {",
				"		a.function",
				"	}",
				"}",
				""));
		validate(mas).assertError(
				XbasePackage.Literals.XMEMBER_FEATURE_CALL,
				IssueCodes.FORBIDDEN_REFERENCE,
				"Forbidden feature call");
	}

	@Test
	public void privateFunctionPrivateCaller_01() throws Exception {
		SarlScript mas = file(multilineString(
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
		validate(mas).assertNoErrors();
	}

	@Test
	public void privateFunctionPrivateCaller_02() throws Exception {
		SarlScript mas = file(multilineString(
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
		validate(mas).assertNoErrors();
	}

	@Test
	public void privateFunctionPublicCaller_02() throws Exception {
		SarlScript mas = file(multilineString(
				"import foo.PrivateAPIObject2",
				"",
				"class Accessor {",
				"	def doSomething(a : PrivateAPIObject2) {",
				"		a.function",
				"	}",
				"}",
				""));
		validate(mas).assertError(
				XbasePackage.Literals.XMEMBER_FEATURE_CALL,
				IssueCodes.FORBIDDEN_REFERENCE,
				"Forbidden feature call");
	}

	@Test
	public void privateFunctionPrivateCaller_03() throws Exception {
		SarlScript mas = file(multilineString(
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
		validate(mas).assertNoErrors();
	}

	@Test
	public void privateFunctionPrivateCaller_04() throws Exception {
		SarlScript mas = file(multilineString(
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
		validate(mas).assertNoErrors();
	}

	@Test
	public void ambigousPrivateAPI_01() throws Exception {
		SarlScript mas = file(multilineString(
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
		validate(mas).assertError(
				XbasePackage.Literals.XMEMBER_FEATURE_CALL,
				IssueCodes.FORBIDDEN_REFERENCE,
				"Forbidden feature call");
	}

	@Test
	public void ambigousPrivateAPI_02() throws Exception {
		SarlScript mas = file(multilineString(
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
		validate(mas).assertNoErrors();
	}

	@Test
	public void ambigousPrivateAPI_03() throws Exception {
		SarlScript mas = file(multilineString(
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
		validate(mas).assertNoErrors();
	}

	@Test
	public void ambigousPrivateAPI_04() throws Exception {
		SarlScript mas = file(multilineString(
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
		validate(mas).assertNoErrors();
	}

}
