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
public class SuperTypesQuickfixTest extends AbstractSARLQuickfixTest {

	/**
	 */
	@Test
	public void fixRedundantInterface_0() {
		assertQuickFix(
				io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 { }",
						"capacity C2 { }",
						"skill S1 implements C1, C2, C1 { }"),
				//
				// Label and description:
				//
				"Remove C1",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 { }",
						"capacity C2 { }",
						"skill S1 implements C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixRedundantInterface_1() {
		assertQuickFix(
				io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 { }",
						"capacity C2 { }",
						"skill S1 implements C1, C1, C2 { }"),
				//
				// Label and description:
				//
				"Remove C1",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 { }",
						"capacity C2 { }",
						"skill S1 implements C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixRedundantInterface_2() {
		assertQuickFix(
				io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 { }",
						"capacity C2 { }",
						"skill S1 implements C2, C1, C1 { }"),
				//
				// Label and description:
				//
				"Remove C1",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 { }",
						"capacity C2 { }",
						"skill S1 implements C2, C1 { }"));
	}

	/**
	 */
	@Test
	public void fixRedundantInterface_3() {
		assertQuickFix(
				io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends C1 { }",
						"skill S1 implements C1, C2, C3 { }"),
				//
				// Label and description:
				//
				"Remove C1",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends C1 { }",
						"skill S1 implements C2, C3 { }"));
	}

	/**
	 */
	@Test
	public void fixInvalidImplementedType_0() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_IMPLEMENTED_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }",
						"skill S2 implements E1, C1, C2 { }"),
				//
				// Label and description:
				//
				"Remove E1",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "skill S2 implements C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixInvalidImplementedType_1() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_IMPLEMENTED_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }",
						"skill S2 implements C1, E1, C2 { }"),
				//
				// Label and description:
				//
				"Remove E1",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "skill S2 implements C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixInvalidImplementedType_2() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_IMPLEMENTED_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }",
						"skill S2 implements C1, C2, E1 { }"),
				//
				// Label and description:
				//
				"Remove E1",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "skill S2 implements C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixInvalidImplementedType_3() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_IMPLEMENTED_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "skill S2 implements E1 { }"),
				//
				// Label and description:
				//
				"Remove E1",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "skill S2 { }"));
	}

	/**
	 */
	@Test
	public void fixInvalidImplementedType_4() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_IMPLEMENTED_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"skill S1 implements C1 { }",
						"skill S2 extends S1 implements E1 { }"),
				//
				// Label and description:
				//
				"Remove E1",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"skill S1 implements C1 { }",
						"skill S2 extends S1 { }"));
	}

	/**
	 */
	@Test
	public void fixInvalidExtendedType_0() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_EXTENDED_TYPE,
				//
				// Code to fix:
				//
				multilineString(
						"interface I1 { }",
						"capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends I1, C1, C2 { }"),
				//
				// Label and description:
				//
				"Remove I1",
				//
				// Expected fixed code:
				//
				multilineString(
						"interface I1 { }",
						"capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixInvalidExtendedType_1() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_EXTENDED_TYPE,
				//
				// Code to fix:
				//
				multilineString(
						"interface I1 { }",
						"capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends C1, I1, C2 { }"),
				//
				// Label and description:
				//
				"Remove I1",
				//
				// Expected fixed code:
				//
				multilineString(
						"interface I1 { }",
						"capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixInvalidExtendedType_2() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_EXTENDED_TYPE,
				//
				// Code to fix:
				//
				multilineString(
						"interface I1 { }",
						"capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends C1, C2, I1 { }"),
				//
				// Label and description:
				//
				"Remove I1",
				//
				// Expected fixed code:
				//
				multilineString(
						"interface I1 { }",
						"capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixExpectingInterface_0() {
		assertQuickFix(org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
				//
				// Code to fix:
				//
				multilineString(
						"event E1",
						"capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends E1, C1, C2 { }"),
				//
				// Label and description:
				//
				"Remove E1",
				//
				// Expected fixed code:
				//
				multilineString(
						"event E1",
						"capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixExpectingInterface_1() {
		assertQuickFix(org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends C1, E1, C2 { }"),
				//
				// Label and description:
				//
				"Remove E1",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "capacity C3 extends C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixExpectingInterface_2() {
		assertQuickFix(org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }",
						"capacity C3 extends C1, C2, E1 { }"),
				//
				// Label and description:
				//
				"Remove E1",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "capacity C3 extends C1, C2 { }"));
	}

	/**
	 */
	@Test
	public void fixExpectingInterface_3() {
		assertQuickFix(org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C3 extends E1 { }"),
				//
				// Label and description:
				//
				"Remove E1",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C3 { }"));
	}

	/**
	 */
	@Test
	public void fixExpectingInterface_4() {
		assertQuickFix(org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C3 extends E1 { }"),
				//
				// Label and description:
				//
				"Remove E1",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C3 { }"));
	}

	/**
	 */
	@Test
	public void fixExpectingClass_0() {
		assertQuickFix(org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED,
				//
				// Code to fix:
				//
				multilineString(
						"capacity C1 { }",
						"event E1 extends C1 { }"),
				//
				// Label and description:
				//
				"Remove C1",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity C1 { }",
						"event E1 { }"));
	}

	/**
	 */
	@Test
	public void fixInconsistentTypeHierarchy_0() {
		assertQuickFix(org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 extends C1 { }"),
				//
				// Label and description:
				//
				"Remove C1",
				//
				// Expected fixed code:
				//
				"capacity C1 { }");
	}

}
