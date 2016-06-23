/*
 * Copyright (C) 2014-2016 the original authors or authors.
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
public class OverridesQuickfixTest extends AbstractSARLQuickfixTest {

	/**
	 */
	@Test
	public void fixOverriddenFinaMethod() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.OVERRIDDEN_FINAL,
				//
				// Code to fix:
				//
				multilineString(
						"agent A1 {",
						"	def myfct(a : boolean, a : int = 4) { }",
						"}",
						"agent A2 extends A1 {",
						"	def myfct(b : boolean) { }",
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
						"	def myfct(a : boolean, a : int = 4) { }",
						"}",
						"agent A2 extends A1 {",
						"}"));
	}

	@Test
	public void fixOverriddenFinalType() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.OVERRIDDEN_FINAL,
				//
				// Code to fix:
				//
				multilineString(
						"import foo.MockFinalAgent",
						"agent A1 extends MockFinalAgent { }"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"import foo.MockFinalAgent",
						"agent A1 { }"));
	}

	/**
	 */
	@Test
	public void fixMissedClassicPrototype_0() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 {",
						"	def fct1",
						"	def fct2(a : int)",
						"	def fct3 : boolean",
						"}",
						"skill S1 implements C1 {",
						"}"),
				//
				// Label and description:
				//
				"Add unimplemented methods",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 {",
						"	def fct1",
						"	def fct2(a : int)",
						"	def fct3 : boolean",
						"}",
						"skill S1 implements C1 {",
						"",
						"	override fct1 {",
						"		// TODO Auto-generated action.",
						"	}",
						"	",
						"	override fct2(a : int) {",
						"		// TODO Auto-generated action.",
						"	}",
						"	",
						"	override fct3 : boolean {",
						"		// TODO Auto-generated action.",
						"		true",
						"	}",
						"	",
						"}"));
	}

	/**
	 */
	@Test
	public void fixVariadicParameter_0() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int*)",
						"}",
						"skill S1 implements C1 { }"),
				//
				// Label and description:
				//
				"Add unimplemented methods",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int*)",
						"}",
						"skill S1 implements C1 {",
						"",
						"	override fct(a : int*) {",
						"		// TODO Auto-generated action.",
						"	}",
						"	",
						"}"));
	}

	/**
	 */
	@Test
	public void fixVariadicParameter_1() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 {",
						"	def fct1(a : int*)",
						"	def fct2(b : String, c : double*)",
						"}",
						"skill S1 implements C1 {",
						"}"),
				//
				// Label and description:
				//
				"Add unimplemented methods",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 {",
						"	def fct1(a : int*)",
						"	def fct2(b : String, c : double*)",
						"}",
						"skill S1 implements C1 {",
						"",
						"	override fct1(a : int*) {",
						"		// TODO Auto-generated action.",
						"	}",
						"	",
						"	override fct2(b : String, c : double*) {",
						"		// TODO Auto-generated action.",
						"	}",
						"	",
						"}"));
	}

	/**
	 */
	@Test
	public void fixParameterDefaultValue_0() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int=4)",
						"}",
						"skill S1 implements C1 { }"),
				//
				// Label and description:
				//
				"Add unimplemented methods",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int=4)",
						"}",
						"skill S1 implements C1 {",
						"",
						"	override fct(a : int = 4) {",
						"		// TODO Auto-generated action.",
						"	}",
						"	",
						"}"));
	}

	/**
	 */
	@Test
	public void fixParameterDefaultValue_1() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int=4)",
						"}",
						"skill S1 implements C1 { }"),
				//
				// Label and description:
				//
				"Make class abstract",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int=4)",
						"}",
						"abstract skill S1 implements C1 { }"));
	}

	/**
	 */
	@Test
	public void fixParameterDefaultValue_2() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int = 4, b : String)",
						"}",
						"skill S1 implements C1 { }"),
				//
				// Label and description:
				//
				"Add unimplemented methods",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int = 4, b : String)",
						"}",
						"skill S1 implements C1 {",
						"",
						"	override fct(a : int = 4, b : String) {",
						"		// TODO Auto-generated action.",
						"	}",
						"	",
						"}"));
	}

	/**
	 */
	@Test
	public void fixParameterDefaultValue_3() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int = 4, b : String)",
						"}",
						"skill S1 implements C1 { }"),
				//
				// Label and description:
				//
				"Make class abstract",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int = 4, b : String)",
						"}",
						"abstract skill S1 implements C1 { }"));
	}

	/**
	 */
	@Test
	public void fixParameterDefaultValueAndVariadicParameter() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int = 4, b : String, c : int*)",
						"}",
						"skill S1 implements C1 { }"),
				//
				// Label and description:
				//
				"Add unimplemented methods",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 {",
						"	def fct(a : int = 4, b : String, c : int*)",
						"}",
						"skill S1 implements C1 {",
						"",
						"	override fct(a : int = 4, b : String, c : int*) {",
						"		// TODO Auto-generated action.",
						"	}",
						"	",
						"}"));
	}

}
