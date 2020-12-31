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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.ui.tests.contentassist;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.tests.api.TestClasspath;
import io.sarl.tests.api.TestScope;

@SuppressWarnings("all")
public class AgentContentAssistTest extends AbstractContentAssistTest {

	@Override
	protected String getPrefix() {
		return "agent TestAgent { ";
	}

	@Override
	protected String getSuffix() {
		return "}";
	}

	@Test
	@TestScope(eclipse = true, tycho = false)
	public void usesLifecycle() throws Exception {
		// Create the fake lifecycle
		helper().createFile(helper().generateFilename(), multilineString(
				"package io.sarl.core",
				"capacity Lifecycle {",
				"  def killMe",
				"}"));
		newBuilder().append("uses Li").assertText("io.sarl.core.Lifecycle");
	}

}
