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
public class AddSupressWarningQuickfixTest extends AbstractSARLQuickfixTest {

	@Test
	public void fixAddToFunction() {
		assertQuickFix(
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION,
				//
				// Code to fix:
				//
				multilineString(
						"package io.sarl.lang.tests.test",
						"agent A1 {",
						"	def mytst : void {",
						"     if (1==1) {",
						"     }",
						"   }",
						"}"),
				//
				// Label and description:
				//
				"Add @SuppressWarnings to mytst",
				//
				// Expected fixed code:
				//
				multilineString(
						"package io.sarl.lang.tests.test",
						"agent A1 {",
						"	@SuppressWarnings(\"constant_condition\")",
						"	def mytst : void {",
						"     if (1==1) {",
						"     }",
						"   }",
						"}"));
	}

	@Test
	public void fixAddToType() {
		assertQuickFix(
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION,
				//
				// Code to fix:
				//
				multilineString(
						"package io.sarl.lang.tests.test",
						"agent A1 {",
						"	def mytst : void {",
						"     if (1==1) {",
						"     }",
						"   }",
						"}"),
				//
				// Label and description:
				//
				"Add @SuppressWarnings to A1",
				//
				// Expected fixed code:
				//
				multilineString(
						"package io.sarl.lang.tests.test",
						"@SuppressWarnings(\"constant_condition\")",
						"agent A1 {",
						"	def mytst : void {",
						"     if (1==1) {",
						"     }",
						"   }",
						"}"));
	}

	@Test
	public void fixMulti_01() {
		assertQuickFix(
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION,
				//
				// Code to fix:
				//
				multilineString(
						"package io.sarl.lang.tests.test",
						"agent A1 {",
						"   @SuppressWarnings(\"x.y.z\")",
						"	def mytst : void {",
						"     if (1==1) {",
						"     }",
						"   }",
						"}"),
				//
				// Label and description:
				//
				"Add @SuppressWarnings to mytst",
				//
				// Expected fixed code:
				//
				multilineString(
						"package io.sarl.lang.tests.test",
						"agent A1 {",
						"   @SuppressWarnings(\"constant_condition\", \"x.y.z\")",
						"	def mytst : void {",
						"     if (1==1) {",
						"     }",
						"   }",
						"}"));
	}

	@Test
	public void fixMulti_02() {
		assertQuickFix(
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION,
				//
				// Code to fix:
				//
				multilineString(
						"package io.sarl.lang.tests.test",
						"agent A1 {",
						"   @SuppressWarnings({\"x.y.z\"})",
						"	def mytst : void {",
						"     if (1==1) {",
						"     }",
						"   }",
						"}"),
				//
				// Label and description:
				//
				"Add @SuppressWarnings to mytst",
				//
				// Expected fixed code:
				//
				multilineString(
						"package io.sarl.lang.tests.test",
						"agent A1 {",
						"   @SuppressWarnings({\"constant_condition\", \"x.y.z\"})",
						"	def mytst : void {",
						"     if (1==1) {",
						"     }",
						"   }",
						"}"));
	}


	@Test
	public void fixMulti_03() {
		assertQuickFix(
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION,
				//
				// Code to fix:
				//
				multilineString(
						"package io.sarl.lang.tests.test",
						"agent A1 {",
						"   @SuppressWarnings ( {   \"x.y.z\"})",
						"	def mytst : void {",
						"     if (1==1) {",
						"     }",
						"   }",
						"}"),
				//
				// Label and description:
				//
				"Add @SuppressWarnings to mytst",
				//
				// Expected fixed code:
				//
				multilineString(
						"package io.sarl.lang.tests.test",
						"agent A1 {",
						"   @SuppressWarnings ( {   \"constant_condition\", \"x.y.z\"})",
						"	def mytst : void {",
						"     if (1==1) {",
						"     }",
						"   }",
						"}"));
	}

}
