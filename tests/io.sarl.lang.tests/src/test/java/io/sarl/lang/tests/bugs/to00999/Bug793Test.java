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

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Impossible d'assigner une valeur negative a une variable short
 *
 * <p>https://github.com/sarl/sarl/issues/793
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/793"
 * @see "https://github.com/eclipse/xtext-lib/issues/68"
 */
@DisplayName("Bug #793")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug793Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"class XXX {",
			"  def fct : short {",
			"    var temp_node_i : short = -1",
			"    temp_node_i",
			"  }",
			"}");
	
	private static final String SNIPSET2 = multilineString(
			"class XXX {",
			"  def fct : short {",
			"    var temp_node_i : short; temp_node_i = -1 as short",
			"    temp_node_i",
			"  }",
			"}");

	private static final String SNIPSET3 = multilineString(
			"class XXX {",
			"  def fct : short {",
			"    var temp_node_i : short = 1",
			"    temp_node_i",
			"  }",
			"}");

	private static final String SNIPSET4 = multilineString(
			"class XXX {",
			"  def fct : short {",
			"    var temp_node_i : short = 1 as short",
			"    temp_node_i",
			"  }",
			"}");

	private static final String SNIPSET5 = multilineString(
			"class XXX {",
			"  var temp_node_i : short = -1",
			"}");
	
	private static final String SNIPSET6 = multilineString(
			"class XXX {",
			"  var temp_node_i : short = -1 as short",
			"}");

	private static final String SNIPSET7 = multilineString(
			"class XXX {",
			"  var temp_node_i : short = 1",
			"}");

	private static final String SNIPSET8 = multilineString(
			"class XXX {",
			"  var temp_node_i : short = 1 as short",
			"}");

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.INCOMPATIBLE_TYPES,
				"cannot convert from int to short");
	}

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET2);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}
	
	@Test
	public void parsing_03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET3);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXNumberLiteral(),
				IssueCodes.INCOMPATIBLE_TYPES,
				"cannot convert from int to short");
	}

	@Test
	public void parsing_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET4);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	public void parsing_05() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET5);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.INCOMPATIBLE_TYPES,
				"cannot convert from int to short");
	}

	@Test
	public void parsing_06() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET6);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}
	
	@Test
	public void parsing_07() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET7);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXNumberLiteral(),
				IssueCodes.INCOMPATIBLE_TYPES,
				"cannot convert from int to short");
	}

	@Test
	public void parsing_08() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET8);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

}

