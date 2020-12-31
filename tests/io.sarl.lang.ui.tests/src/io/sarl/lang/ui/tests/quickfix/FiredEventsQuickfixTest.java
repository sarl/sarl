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
package io.sarl.lang.ui.tests.quickfix;

import org.eclipse.xtext.ui.editor.quickfix.IssueResolution;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import static org.junit.Assert.*;

@SuppressWarnings("all")
public class FiredEventsQuickfixTest extends AbstractSARLQuickfixTest {

	/**
	 */
	@Test
	public void fixInvalidFiringEventType_0() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_FIRING_EVENT_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "event E2", "capacity C1 { }",
						"agent A1 {", "	def myfct fires C1, E1, E2 { }",
						"}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "event E2", "capacity C1 { }",
						"agent A1 {", "	def myfct fires E1, E2 { }", "}"));
	}

	/**
	 */
	@Test
	public void fixInvalidFiringEventType_1() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_FIRING_EVENT_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "event E2", "capacity C1 { }",
						"agent A1 {", "	def myfct fires E1, C1, E2 { }",
						"}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "event E2", "capacity C1 { }",
						"agent A1 {", "	def myfct fires E1, E2 { }", "}"));
	}

	/**
	 */
	@Test
	public void fixInvalidFiringEventType_2() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_FIRING_EVENT_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "event E2", "capacity C1 { }",
						"agent A1 {", "	def myfct fires E1, E2, C1 { }",
						"}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "event E2", "capacity C1 { }",
						"agent A1 {", "	def myfct fires E1, E2 { }", "}"));
	}

}
