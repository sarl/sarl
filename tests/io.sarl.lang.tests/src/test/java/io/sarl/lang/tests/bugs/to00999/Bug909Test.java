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
import static io.sarl.tests.api.tools.TestTimeout.startTimeOut;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestTimeout.TimeOutHandler;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: SARL compiler never return.
 *
 * <p>https://github.com/sarl/sarl/issues/909
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/909"
 */
@DisplayName("Bug #909")
@SuppressWarnings("all")
@Tag("core")
public class Bug909Test extends AbstractSarlTest {

	private static final String SARL_CODE = multilineString(
			"package io.sarl.docs.tutorials.masinitialization",
			"import io.sarl.core.Initialize",
			"import io.sarl.core.Lifecycle",
			"import io.sarl.core.AgentSpawned",
			"import io.sarl.core.DefaultContextInteractions",
			"event StartApplication",
			"agent MyAgent {}",
			"agent BootAgent {",
			"  uses Lifecycle, DefaultContextInteractions",
			"  var count = 0",
			"  on Initialize {",
			"    for (i : 1..100) {",
			"      spawn(typeof(MyAgent))",
			"    }",
			"  }",
			"  on AgentSpawned [it.agentID != ID] {",
			"    count++",
			"    if (count == 100) {",
			"      emit(new StartApplication) [it.ID != ID]",
			"      killMe",
			"    }",
			"  }",
			"}");

	@Test
	@Tag("sarlValidation")
	public void parsing() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void compiling() throws Exception {
		final TimeOutHandler handler = startTimeOut();
		try {
			getCompileHelper().compile(SARL_CODE, (it) -> {
			});
		} finally {
			handler.stop();
		}
	}

}
