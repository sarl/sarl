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
package io.sarl.lang.ui.tests.parsing;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.TestScope;

import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.junit.Assume;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	AgentParsingTest.TopElementUiBaseTest.class,
})
@SuppressWarnings("all")
public class AgentParsingTest {

	public static class TopElementUiBaseTest extends AbstractSarlUiTest {

		@Test
		public void invalidExtend() throws Exception {
			SarlScript script = file(multilineString(
					"import foo.MockFinalAgent",
					"agent InvalidAgentDeclaration extends MockFinalAgent {",
					"}"));
			validate(script).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.OVERRIDDEN_FINAL,
					"Attempt to override final class");
		}

	}

}
