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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.bugs.to00699.Bug558Test;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Ignore functions in deprecated types.
 *
 * <p>https://github.com/sarl/sarl/issues/730
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see Bug558Test
 */
@DisplayName("Bug #730")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug730Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug730",
			"event MyEvent",
			"@Deprecated",
			"capacity C1 {",
			"  def myfunction",
			"  def myfunction2",
			"}",
			"capacity C2 {",
			"  def myfunction",
			"  def myfunction3",
			"}",
			"agent TestAgent {",
			"  uses C1, C2",
			"  on MyEvent {",
			"    myfunction",
			"    myfunction2",
			"    myfunction3",
			"  }",
			"}");
	
	@Test
	public void snipset2() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

}
