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

package io.sarl.lang.tests.bugs.to00999;

import com.google.inject.Inject;
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

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
@SuppressWarnings("all")
public class Bug909 extends AbstractSarlTest {

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
			"  on AgentSpawned [!it.agentIdentifiers.contains(ID)] {",
			"    count++",
			"    if (count == 100) {",
			"      emit(new StartApplication) [it.UUID != ID]",
			"      killMe",
			"    }",
			"  }",
			"}");

	@Test
	public void parsing() throws Exception {
		SarlScript mas = file(SARL_CODE);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
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
