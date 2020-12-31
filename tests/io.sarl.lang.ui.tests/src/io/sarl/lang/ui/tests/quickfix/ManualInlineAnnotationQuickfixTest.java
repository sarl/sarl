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

import io.sarl.lang.validation.IssueCodes;

@SuppressWarnings("all")
public class ManualInlineAnnotationQuickfixTest extends AbstractSARLQuickfixTest {

	@Test
	public void fixParameter() {
		assertQuickFix(
				IssueCodes.MANUAL_INLINE_DEFINITION,
				//
				// Code to fix:
				//
				multilineString(
						"import org.eclipse.xtext.xbase.lib.Inline",
						"agent A1 {",
						"   @Inline(\"fct2()\")",
						"	def fct() {",
						"	}",
						"   def fct2() {",
						"   }",
						"}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"import org.eclipse.xtext.xbase.lib.Inline",
						"agent A1 {",
						"	def fct() {",
						"	}",
						"   def fct2() {",
						"   }",
						"}"));
	}

}
