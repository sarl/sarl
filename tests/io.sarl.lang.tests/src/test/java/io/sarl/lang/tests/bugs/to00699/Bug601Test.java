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

import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Occurrence keyword redefinition outside a event handler.
 *
 * <p>https://github.com/sarl/sarl/issues/601
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #601")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug601Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug601",
			"class XXX {",
			"  def fct(occurrence : int) { }",
			"}");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug601",
			"class XXX {",
			"  var occurrence : int",
			"}");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug601",
			"class XXX {",
			"  val occurrence = 1",
			"}");

	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug601",
			"class XXX {",
			"  def fct {",
			"    var occurrence : int",
			"  }",
			"}");

	private static final String SNIPSET5 = multilineString(
			"package io.sarl.lang.tests.bug601",
			"agent XXX {",
			"  on Event {",
			"    var occurrence = 1",
			"  }",
			"}");

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlFormalParameter(),
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid name 'occurrence'");
	}

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET2);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid name 'occurrence'");
	}

	@Test
	public void parsing_03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET3);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid name 'occurrence'");
	}

	@Test
	public void parsing_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET4);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XtendPackage.eINSTANCE.getXtendVariableDeclaration(),
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid name 'occurrence'");
	}

	@Test
	public void parsing_05() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET5);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XtendPackage.eINSTANCE.getXtendVariableDeclaration(),
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid name 'occurrence'");
	}

}
