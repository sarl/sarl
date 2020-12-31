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

import org.junit.Test;

@SuppressWarnings("all")
public class DuplicatesQuickfixTest extends AbstractSARLQuickfixTest {

	@Test
	public void fixDuplicateTypeName() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_TYPE_NAME,
				//
				// Code to fix:
				//
				multilineString(
						"agent A1 { }",
						"event E1",
						"agent A1 { }"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 { }",
						"event E1"));
	}

	@Test
	public void fixDuplicateField() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
				//
				// Code to fix:
				//
				multilineString(
						"agent A1 {",
						"	var attr1 = true",
						"	var attr1 = \"\"",
						"}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	var attr1 = true",
						"}"),
				multilineString(
						"agent A1 {",
						"	var attr1 = \"\"",
						"}"));
	}

	@Test
	public void fixDuplicateMethod() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_METHOD,
				//
				// Code to fix:
				//
				multilineString(
						"agent A1 {",
						"	def method1(a : int) : boolean {",
						"		true",
						"	}",
						"	def method1(a : int) : float {",
						"		1.0f",
						"	}",
						"}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	def method1(a : int) : boolean {",
						"		true",
						"	}",
						"}"),
				multilineString(
						"agent A1 {",
						"	def method1(a : int) : float {",
						"		1.0f",
						"	}",
						"}"));
	}

}
