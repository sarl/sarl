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

import org.eclipse.xtend.core.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid "function not used" warning for functions with default valued parameter.
 *
 * <p>https://github.com/sarl/sarl/issues/895
 *
 * @author $Author: sgalland$
 * @author $Author: alombard$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/895"
 */
@DisplayName("Bug #895")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug895Test extends AbstractSarlTest {

	private static final String CALL_WITH_DEFAULT_VALUE = multilineString(
			"package io.sarl.lang.tests.bug895",
			"class X {",
			"  private def f1(param : int = 0) : void {",
			"  }",
			"  def f {",
			"    f1",
			"  }",
			"}");


	@Test
	public void callWithDefaultValue() throws Exception {
		SarlScript mas = file(getParseHelper(), CALL_WITH_DEFAULT_VALUE);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				IssueCodes.UNUSED_PRIVATE_MEMBER);
	}

	private static final String CALL_WITH_EXPLICIT_VALUE = multilineString(
			"package io.sarl.lang.tests.bug895",
			"class X {",
			"  private def f1(param : int = 0) : void {",
			"  }",
			"  def f {",
			"    f1(4)",
			"  }",
			"}");


	@Test
	public void callWithExplicitValue() throws Exception {
		SarlScript mas = file(getParseHelper(), CALL_WITH_EXPLICIT_VALUE);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				IssueCodes.UNUSED_PRIVATE_MEMBER);
	}

	private static final String CALL_WITH_CLASSIC_ARGUMENT = multilineString(
			"package io.sarl.lang.tests.bug895",
			"class X {",
			"  private def f1(param : int) : void {",
			"  }",
			"  def f {",
			"    f1(4)",
			"  }",
			"}");


	@Test
	public void callWithClassicArgument() throws Exception {
		SarlScript mas = file(getParseHelper(), CALL_WITH_CLASSIC_ARGUMENT);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				IssueCodes.UNUSED_PRIVATE_MEMBER);
	}

	private static final String NO_CALL_WITH_CLASSIC_ARGUMENT = multilineString(
			"package io.sarl.lang.tests.bug895",
			"class X {",
			"  private def f1(param : int) : void {",
			"  }",
			"  private def f2 : void {",
			"  }",
			"  def f {",
			"    f2",
			"  }",
			"}");


	@Test
	public void noCallWithClassicArgument() throws Exception {
		SarlScript mas = file(getParseHelper(), NO_CALL_WITH_CLASSIC_ARGUMENT);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				SarlPackage.eINSTANCE.getSarlAction(),
				IssueCodes.UNUSED_PRIVATE_MEMBER,
				"f1(int)");
	}

}
