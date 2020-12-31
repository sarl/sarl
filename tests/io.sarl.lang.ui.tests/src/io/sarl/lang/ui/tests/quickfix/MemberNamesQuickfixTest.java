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

import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

@SuppressWarnings("all")
public class MemberNamesQuickfixTest extends AbstractSARLQuickfixTest {

	/**
	 */
	@Test
	public void fixVariableNameShadowing() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				//
				// Code to fix:
				//
				multilineString(
						"agent A1 {",
						"	protected var attr1 : boolean",
						"}",
						"agent A2 extends A1 {",
						"	var attr1 : int",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	protected var attr1 : boolean",
						"}",
						"agent A2 extends A1 {",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Rename to attr10",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	protected var attr1 : boolean",
						"}",
						"agent A2 extends A1 {",
						"	var attr10 : int",
						"}"));
	}

	@Test
	public void fixDisallowedFieldName() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				//
				// Code to fix:
				//
				multilineString(
						"agent A1 {",
						"	var $DEFAULT_VALUE$myattr1 : boolean",
						"	var myattr2 : float",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Rename to _DEFAULT_VALUE_myattr1",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	var _DEFAULT_VALUE_myattr1 : boolean",
						"	var myattr2 : float",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	var myattr2 : float",
						"}"));
		asserts.assertNoQuickFix();
	}

	@Test
	public void fixDiscouragedFieldName() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				IssueCodes.VARIABLE_NAME_DISCOURAGED,
				//
				// Code to fix:
				//
				multilineString(
						"agent A1 {",
						"	var self : boolean",
						"	var myattr2 : float",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	var myattr2 : float",
						"}"));
	}

	/**
	 */
	@Test
	public void fixMethodName_0() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME,
				//
				// Code to fix:
				//
				multilineString(
						"agent A1 {",
						"	def $handle_method1(a : int) : boolean {",
						"		true",
						"	}",
						"	def method2(a : int) : float {",
						"		1.0f",
						"	}",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Rename to _handle_method1",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	def _handle_method1(a : int) : boolean {",
						"		true",
						"	}",
						"	def method2(a : int) : float {",
						"		1.0f",
						"	}",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	def method2(a : int) : float {",
						"		1.0f",
						"	}",
						"}"));
		asserts.assertNoQuickFix();
	}

	/**
	 */
	@Test
	public void fixMethodName_1() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME,
				//
				// Code to fix:
				//
				multilineString(
						"agent A1 {",
						"	def $eventhandler_guard_method1(a : int) : boolean {",
						"		true",
						"	}",
						"	def method2(a : int) : float {",
						"		1.0f",
						"	}",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Rename to _eventhandler_guard_method1",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	def _eventhandler_guard_method1(a : int) : boolean {",
						"		true",
						"	}",
						"	def method2(a : int) : float {",
						"		1.0f",
						"	}",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	def method2(a : int) : float {",
						"		1.0f",
						"	}",
						"}"));
		asserts.assertNoQuickFix();
	}

	/**
	 */
	@Test
	public void fixMethodName_2() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME,
				//
				// Code to fix:
				//
				multilineString(
						"agent A1 {",
						"	def $eventhandler_body_method1(a : int) : boolean {",
						"		true",
						"	}",
						"	def method2(a : int) : float {",
						"		1.0f",
						"	}",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Rename to _eventhandler_body_method1",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	def _eventhandler_body_method1(a : int) : boolean {",
						"		true",
						"	}",
						"	def method2(a : int) : float {",
						"		1.0f",
						"	}",
						"}"));
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"agent A1 {",
						"	def method2(a : int) : float {",
						"		1.0f",
						"	}",
						"}"));
		asserts.assertNoQuickFix();
	}

}
