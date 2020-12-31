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

@SuppressWarnings("all")
public class ClassContentAssistTest extends AbstractContentAssistTest {

	@Override
	protected String getPrefix() {
		return "class TestClass ";
	}

	@Test
	public void afterAgentTypeName() throws Exception {
		newBuilder().assertText("extends", "implements");
	}
	
	@Test
	public void ext() throws Exception {
		newBuilder().append("ext").assertText("extends");
	}

	@Test
	public void im() throws Exception {
		newBuilder().append("im").assertText("implements");
	}

	@Test
	public void afterExtends() throws Exception {
		newBuilder().append("extends").assertText("extends");
	}

	@Test
	public void afterImplements() throws Exception {
		newBuilder().append("implements").assertText("implements");
	}

	@Test
	public void startBlock() throws Exception {
		assertTextInsideProposals(newBuilder().append("{ "),
				// Inner types are expected.
				"class", "interface", "enum", "annotation",
				// Expected members
				"extension",
				"new");
	}

	@Test
	public void continueBlock() throws Exception {
		assertTextInsideProposals(newBuilder().append("{ new() { } "),
				// Inner types are expected.
				"class", "interface", "enum", "annotation",
				// Expected members
				"extension",
				"new");
	}

}
