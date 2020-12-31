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
public class CapacityQuickfixTest extends AbstractSARLQuickfixTest {

	/**
	 */
	@Test
	public void fixInvalidCapacityType_0() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "agent A1 {", "	uses E1", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "agent A1 {", "}"));
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_1() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {",
						"	uses E1, C1, C2", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {", "	uses C1, C2",
						"}"));
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_2() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {",
						"	uses C1, E1, C2", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {", "	uses C1, C2",
						"}"));
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_3() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {",
						"	uses C1, C2, E1", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {", "	uses C1, C2",
						"}"));
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_4() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "agent A1 {", "	requires E1",
						"}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "agent A1 {", "}"));
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_5() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {",
						"	requires E1, C1, C2", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {",
						"	requires C1, C2", "}"));
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_6() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {",
						"	requires C1, E1, C2", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {",
						"	requires C1, C2", "}"));
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_7() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {",
						"	requires C1, C2, E1", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("event E1", "capacity C1 { }",
						"capacity C2 { }", "agent A1 {",
						"	requires C1, C2", "}"));
	}

	/**
	 * {@link IssueCodes#DISCOURAGED_CAPACITY_DEFINITION}.
	 */
	@Test
	public void fixDiscouragedCapacityDefinition() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				io.sarl.lang.validation.IssueCodes.DISCOURAGED_CAPACITY_DEFINITION,
				//
				// Code to fix:
				//
				"capacity C1 { }");
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				"");
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Add aFunction",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 {", "	def aFunction", "}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_0() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "agent A1 {",
						"	uses C1, C2, C3",
						"	def testfct { myfct; myfct2 }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "agent A1 {",
						"	uses C1, C3", "	def testfct { myfct; myfct2 }",
						"}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_1() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "agent A1 {",
						"	uses C1, C3, C2",
						"	def testfct { myfct; myfct2 }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "agent A1 {",
						"	uses C1, C3", "	def testfct { myfct; myfct2 }",
						"}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_2() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "agent A1 {",
						"	uses C2, C1, C3",
						"	def testfct { myfct; myfct2 }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "agent A1 {",
						"	uses C1, C3", "	def testfct { myfct; myfct2 }",
						"}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_3() {
		assertQuickFix(
				io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString(
						"capacity Logging { def println(v : String) }",
						"capacity Lifecycle { def killMe }",
						"event Initialize", "agent AgentB {",
						"	uses Logging, Lifecycle", "	on Initialize {",
						"		println(\"Je me presente\")", "	}", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity Logging { def println(v : String) }",
						"capacity Lifecycle { def killMe }",
						"event Initialize", "agent AgentB {",
						"	uses Logging", "	on Initialize {",
						"		println(\"Je me presente\")", "	}", "}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_4() {
		assertQuickFix(
				io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString(
						"capacity Logging { def println(v : String) }",
						"capacity Lifecycle { def killMe }",
						"event Initialize", "agent AgentB {",
						"	uses Lifecycle, Logging", "	on Initialize {",
						"		println(\"Je me presente\")", "	}", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity Logging { def println(v : String) }",
						"capacity Lifecycle { def killMe }",
						"event Initialize", "agent AgentB {",
						"	uses Logging", "	on Initialize {",
						"		println(\"Je me presente\")", "	}", "}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_5() {
		assertQuickFix(
				io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString("capacity Lifecycle { def killMe }",
						"event Initialize", "agent AgentB {",
						"	uses Lifecycle", "	on Initialize {", "	}", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity Lifecycle { def killMe }",
						"event Initialize", "agent AgentB {",
						"	on Initialize {", "	}", "}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_6() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "behavior B1 {",
						"	uses C1, C2, C3",
						"	def testfct { myfct; myfct2 }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "behavior B1 {",
						"	uses C1, C3", "	def testfct { myfct; myfct2 }",
						"}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_7() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "behavior B1 {",
						"	uses C1, C3, C2",
						"	def testfct { myfct; myfct2 }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "behavior B1 {",
						"	uses C1, C3", "	def testfct { myfct; myfct2 }",
						"}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_8() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "behavior B1 {",
						"	uses C2, C1, C3",
						"	def testfct { myfct; myfct2 }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }",
						"capacity C3 { def myfct2 }", "behavior B1 {",
						"	uses C1, C3", "	def testfct { myfct; myfct2 }",
						"}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_9() {
		assertQuickFix(
				io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString(
						"capacity Logging { def println(v : String) }",
						"capacity Lifecycle { def killMe }",
						"event Initialize", "behavior B {",
						"	uses Logging, Lifecycle", "	on Initialize {",
						"		println(\"Je me presente\")", "	}", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity Logging { def println(v : String) }",
						"capacity Lifecycle { def killMe }",
						"event Initialize", "behavior B {",
						"	uses Logging", "	on Initialize {",
						"		println(\"Je me presente\")", "	}", "}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_10() {
		assertQuickFix(
				io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString(
						"capacity Logging { def println(v : String) }",
						"capacity Lifecycle { def killMe }",
						"event Initialize", "behavior B {",
						"	uses Lifecycle, Logging", "	on Initialize {",
						"		println(\"Je me presente\")", "	}", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString(
						"capacity Logging { def println(v : String) }",
						"capacity Lifecycle { def killMe }",
						"event Initialize", "behavior B {",
						"	uses Logging", "	on Initialize {",
						"		println(\"Je me presente\")", "	}", "}"));
	}

	/**
	 */
	@Test
	public void fixUnusedAgentCapacity_11() {
		assertQuickFix(
				io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY,
				//
				// Code to fix:
				//
				multilineString("capacity Lifecycle { def killMe }",
						"event Initialize", "behavior B {",
						"	uses Lifecycle", "	on Initialize {", "	}", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity Lifecycle { def killMe }",
						"event Initialize", "behavior B {",
						"	on Initialize {", "	}", "}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_0() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "agent A1 {",
						"	uses C1, C2, C1",
						"	def testfct { myfct; iddle }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "agent A1 {",
						"	uses C1, C2", "	def testfct { myfct; iddle }",
						"}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_1() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }", "agent A1 {",
						"	uses C1, C1", "	def testfct { myfct }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }", "agent A1 {",
						"	uses C1", "	def testfct { myfct }", "}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_2() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "agent A1 {",
						"	uses C1, C1, C2, C1",
						"	def testfct { myfct; iddle }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "agent A1 {",
						"	uses C1, C2, C1",
						"	def testfct { myfct; iddle }", "}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_3() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "agent A1 {",
						"	uses C1, C2, C1, C1",
						"	def testfct { myfct; iddle }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "agent A1 {",
						"	uses C1, C2, C1",
						"	def testfct { myfct; iddle }", "}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_4() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "agent A1 {",
						"	uses C1, C2", "	def testfct { myfct; iddle }",
						"	uses C1", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "agent A1 {",
						"	uses C1, C2", "	def testfct { myfct; iddle }",
						"}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_5() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "agent A1 {",
						"	uses C1, C2", "	def testfct { myfct; iddle }",
						"	uses C1, C2, C1", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "agent A1 {",
						"	uses C1, C2", "	def testfct { myfct; iddle }",
						"	uses C2, C1", "}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_6() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "behavior B1 {",
						"	uses C1, C2, C1",
						"	def testfct { myfct; iddle }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "behavior B1 {",
						"	uses C1, C2", "	def testfct { myfct; iddle }",
						"}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_7() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"behavior B1 {", "	uses C1, C1",
						"	def testfct { myfct }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"behavior B1 {", "	uses C1",
						"	def testfct { myfct }", "}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_8() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "behavior B1 {",
						"	uses C1, C1, C2, C1",
						"	def testfct { myfct; iddle }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "behavior B1 {",
						"	uses C1, C2, C1",
						"	def testfct { myfct; iddle }", "}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_9() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "behavior B1 {",
						"	uses C1, C2, C1, C1",
						"	def testfct { myfct; iddle }", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "behavior B1 {",
						"	uses C1, C2, C1",
						"	def testfct { myfct; iddle }", "}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_10() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "behavior B1 {",
						"	uses C1, C2", "	def testfct { myfct; iddle }",
						"	uses C1", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "behavior B1 {",
						"	uses C1, C2", "	def testfct { myfct; iddle }",
						"}"));
	}

	/**
	 */
	@Test
	public void fixRedundantCapacityUse_11() {
		assertQuickFix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE,
				//
				// Code to fix:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "behavior B1 {",
						"	uses C1, C2", "	def testfct { myfct; iddle }",
						"	uses C1, C2, C1", "}"),
				//
				// Label and description:
				//
				"Remove",
				//
				// Expected fixed code:
				//
				multilineString("capacity C1 { def myfct }",
						"capacity C2 { def iddle }", "behavior B1 {",
						"	uses C1, C2", "	def testfct { myfct; iddle }",
						"	uses C2, C1", "}"));
	}

}
