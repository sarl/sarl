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

@SuppressWarnings("all")
public class RootContentAssistTest extends AbstractContentAssistTest {

	@Test
	public void emptyInput() throws Exception {
		assertTextInsideProposals(newBuilder(),
				"package", "import", "agent", "behavior", "capacity", "skill", "event",
				"class", "interface", "enum", "annotation");
	}
	
	@Test
	public void startWith_c() throws Exception {
		assertTextInsideProposals(newBuilder().append("c"), "capacity", "class");
	}

	@Test
	public void startWith_cl() throws Exception {
		newBuilder().append("cl").assertText("class");
	}

	@Test
	public void startWith_i() throws Exception {
		newBuilder().append("i").assertText("import", "interface");
	}

	@Test
	public void startWith_interface() throws Exception {
		newBuilder().append("interface").assertText("interface");
	}

	@Test
	public void startWith_interfaceSpace() throws Exception {
		newBuilder().append("interface ").assertCount(0);
	}

}
