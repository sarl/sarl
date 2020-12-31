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
package io.sarl.lang.tests.bugs.to00399;

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
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #336")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug336Test extends AbstractSarlTest {
	
	private String snippet = multilineString(
			"event E1",
			"event E2",
			"capacity C1 {",
			"	def debug(txt : String)",
			"}",
			"agent AbstractAgent {",
			"    uses C1",
			"    on E1 {",
			"        debug(\"Hello World in the super agent!\")",
			"    }",
			"}",
			"agent HelloChildAgent extends AbstractAgent {",
			"    uses C1",
			"    on E2 {",
			"        debug(\"Hello World in the child agent!\")",
			"    }",
			"}");

	private String snippet2 = multilineString(
			"event E1",
			"event E2",
			"capacity C1 {",
			"	def debug(txt : String)",
			"}",
			"agent AbstractAgent {",
			"    uses C1",
			"    on E1 {",
			"        debug(\"Hello World in the super agent!\")",
			"    }",
			"}",
			"agent HelloChildAgent extends AbstractAgent {",
			"    uses C1",
			"    on E2 {",
			"        debug(\"Hello World in the child agent!\")",
			"    }",
			"	 def debug(m : String) { }",	
			"}");

	@Test
	public void bug336() throws Exception {
		SarlScript mas = file(getParseHelper(), snippet);
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	public void localOverride() throws Exception {
		SarlScript mas = file(getParseHelper(), snippet2);
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

}
