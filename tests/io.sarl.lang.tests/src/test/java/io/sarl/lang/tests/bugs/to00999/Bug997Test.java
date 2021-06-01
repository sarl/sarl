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

package io.sarl.lang.tests.bugs.to00999;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.common.types.TypesPackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Duplicate definition of $0, $1, etc.
 *
 * <p>https://github.com/sarl/sarl/issues/997
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/997"
 */
@DisplayName("Bug #997")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug997Test extends AbstractSarlTest {

	/** Expression elements are inside the same resource as the expression.
	 */
	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug997",
			"interface Interface1 {",
			"  def fct1(a : int, b : String, c : double)",
			"}",
			"interface Interface2 {",
			"  def fct2(a : int, b : String, c : double, d : char)",
			"}",
			"class Bug997Case {",
			"  def x(i : Interface1) {}",
			"  def y(i : Interface2) {}",
			"  def fct : void {",
			"    x [",
			"      y [",
			"        System::out.println($1)",
			"        System::out.println($01)",
			"      ]",
			"    ]",
			"  }",
			"}");

	@Test
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertError(
				TypesPackage.eINSTANCE.getJvmFormalParameter(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				"Duplicate", "$0")
			.assertError(
				TypesPackage.eINSTANCE.getJvmFormalParameter(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				"Duplicate", "$1")
			.assertError(
				TypesPackage.eINSTANCE.getJvmFormalParameter(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				"Duplicate", "$2");
	}

}
