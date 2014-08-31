/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.lang.validation.IssueCodes;

import org.junit.Test;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLQuickfixProviderTest extends AbstractSARLQuickfixTest {
	
	/**
	 */
	@Test
	public void fixPackageName() {
		assertQuickFix(
				IssueCodes.WRONG_PACKAGE,
				// Code to fix:
				"package io.sarl.lang.ui.tests.quickfix.invalidPackage", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Change package declaration to 'io.sarl.lang.ui.tests.quickfix.iosarllangvalidationIssueCodeswrong_package'", //$NON-NLS-1$
				"Change package declaration to 'io.sarl.lang.ui.tests.quickfix.iosarllangvalidationIssueCodeswrong_package'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				"package io.sarl.lang.ui.tests.quickfix.iosarllangvalidationIssueCodeswrong_package"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixDuplicateTopElements() {
		assertQuickFix(
				IssueCodes.DUPLICATE_TYPE_NAME,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "agent A1 { }\n" //$NON-NLS-1$
				+ "event E1\n" //$NON-NLS-1$
				+ "agent A1 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the duplicate definition of 'A1'", //$NON-NLS-1$
				"Remove the duplicate definition of 'A1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 { }\n" //$NON-NLS-1$
				+ "event E1\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixDuplicateAttribute() {
		assertQuickFix(
				IssueCodes.DUPLICATE_FIELD,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "var attr1 = true\n" //$NON-NLS-1$
				+ "var attr1 = \"\"\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the duplicate field 'attr1'", //$NON-NLS-1$
				"Remove the duplicate field 'attr1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "var attr1 = true\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixDuplicateMethod() {
		assertQuickFix(
				IssueCodes.DUPLICATE_METHOD,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def method1(a : int) : boolean {\n" //$NON-NLS-1$
				+ "true" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "def method1(a : int) : float {\n" //$NON-NLS-1$
				+ "1.0f\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the duplicate method 'method1(a : int)'", //$NON-NLS-1$
				"Remove the duplicate method 'method1(a : int)'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def method1(a : int) : boolean {\n" //$NON-NLS-1$
				+ "true" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixMemberName_action() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				IssueCodes.INVALID_MEMBER_NAME,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def _handle_method1(a : int) : boolean {\n" //$NON-NLS-1$
				+ "true" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "def method2(a : int) : float {\n" //$NON-NLS-1$
				+ "1.0f\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Rename the action '_handle_method1' to 'handleMethod1'", //$NON-NLS-1$
				"Rename the action '_handle_method1' to 'handleMethod1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def handleMethod1(a : int) : boolean {\n" //$NON-NLS-1$
				+ "true" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "def method2(a : int) : float {\n" //$NON-NLS-1$
				+ "1.0f\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Rename the action '_handle_method1' to 'method1'", //$NON-NLS-1$
				"Rename the action '_handle_method1' to 'method1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def method1(a : int) : boolean {\n" //$NON-NLS-1$
				+ "true" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "def method2(a : int) : float {\n" //$NON-NLS-1$
				+ "1.0f\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove the action '_handle_method1'", //$NON-NLS-1$
				"Remove the action '_handle_method1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def method2(a : int) : float {\n" //$NON-NLS-1$
				+ "1.0f\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertNoQuickFix();
	}

	/**
	 */
	@Test
	public void fixMemberName_attribute() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				IssueCodes.INVALID_MEMBER_NAME,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "var ___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1 : boolean\n" //$NON-NLS-1$
				+ "var myattr2 : float\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Rename the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1' to 'myattr1'", //$NON-NLS-1$
				"Rename the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1' to 'myattr1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "var myattr1 : boolean\n" //$NON-NLS-1$
				+ "var myattr2 : float\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1'", //$NON-NLS-1$
				"Remove the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "var myattr2 : float\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertNoQuickFix();
	}

	/**
	 */
	@Test
	public void fixRedundantInterface_0() {
		assertQuickFix(
				IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C1, C2, C1 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the redundant feature 'C1'", //$NON-NLS-1$
				"Remove the redundant feature 'C1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C1, C2 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixRedundantInterface_1() {
		assertQuickFix(
				IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C1, C1, C2 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the redundant feature 'C1'", //$NON-NLS-1$
				"Remove the redundant feature 'C1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C1, C2 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixRedundantInterface_2() {
		assertQuickFix(
				IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C2, C1, C1 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the redundant feature 'C1'", //$NON-NLS-1$
				"Remove the redundant feature 'C1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C2, C1 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixRedundantInterface_3() {
		assertQuickFix(
				IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "capacity C3 extends C1 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C1, C2, C3 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the redundant feature 'C1'", //$NON-NLS-1$
				"Remove the redundant feature 'C1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "capacity C3 extends C1 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C2, C3 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixVariableNameShadowing() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "var attr1 : boolean\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "agent A2 extends A1 {\n" //$NON-NLS-1$
				+ "var attr1 : int\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove the field 'attr1'", //$NON-NLS-1$
				"Remove the field 'attr1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "var attr1 : boolean\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "agent A2 extends A1 {\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Rename the field 'attr1' to 'attr10'", //$NON-NLS-1$
				"Rename the field 'attr1' to 'attr10'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "var attr1 : boolean\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "agent A2 extends A1 {\n" //$NON-NLS-1$
				+ "var attr10 : int\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertNoQuickFix();
	}

	/**
	 */
	@Test
	public void fixOverriddenFinal() {
		assertQuickFix(
				IssueCodes.OVERRIDDEN_FINAL_OPERATION,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def myfct(a : boolean, a : int = 4) { }\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "agent A2 extends A1 {\n" //$NON-NLS-1$
				+ "def myfct(b : boolean) { }\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the action 'myfct(boolean)'", //$NON-NLS-1$
				"Remove the action 'myfct(boolean)'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def myfct(a : boolean, a : int = 4) { }\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "agent A2 extends A1 {\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixDiscouragedBooleanExpression() {
		assertQuickFix(
				IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "on E1 [true] { [ false ] }\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the guard", //$NON-NLS-1$
				"Remove the guard", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "on E1 { [ false ] }\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixUnreachableBehaviorUnit() {
		assertQuickFix(
				IssueCodes.UNREACHABLE_BEHAVIOR_UNIT,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "on E1 [false] { [ false ] }\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the behavior unit on 'E1'", //$NON-NLS-1$
				"Remove the behavior unit on 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_0() {
		assertQuickFix(
				IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "uses E1\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_1() {
		assertQuickFix(
				IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "uses E1, C1, C2\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "uses C1, C2\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_2() {
		assertQuickFix(
				IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "uses C1, E1, C2\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "uses C1, C2\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_3() {
		assertQuickFix(
				IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "uses C1, C2, E1\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "uses C1, C2\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_4() {
		assertQuickFix(
				IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "requires E1\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_5() {
		assertQuickFix(
				IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "requires E1, C1, C2\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "requires C1, C2\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_6() {
		assertQuickFix(
				IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "requires C1, E1, C2\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "requires C1, C2\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidCapacityType_7() {
		assertQuickFix(
				IssueCodes.INVALID_CAPACITY_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "requires C1, C2, E1\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "requires C1, C2\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidFiringEventType_0() {
		assertQuickFix(
				IssueCodes.INVALID_FIRING_EVENT_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "event E2\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def myfct fires C1, E1, E2 { }\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'C1'", //$NON-NLS-1$
				"Remove the type 'C1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "event E2\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def myfct fires E1, E2 { }\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidFiringEventType_1() {
		assertQuickFix(
				IssueCodes.INVALID_FIRING_EVENT_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "event E2\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def myfct fires E1, C1, E2 { }\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'C1'", //$NON-NLS-1$
				"Remove the type 'C1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "event E2\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def myfct fires E1, E2 { }\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidFiringEventType_2() {
		assertQuickFix(
				IssueCodes.INVALID_FIRING_EVENT_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "event E2\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def myfct fires E1, E2, C1 { }\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'C1'", //$NON-NLS-1$
				"Remove the type 'C1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "event E2\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "def myfct fires E1, E2 { }\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidImplementedType_0() {
		assertQuickFix(
				IssueCodes.INVALID_IMPLEMENTED_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S2 implements E1, C1, C2 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S2 implements C1, C2 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidImplementedType_1() {
		assertQuickFix(
				IssueCodes.INVALID_IMPLEMENTED_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S2 implements C1, E1, C2 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S2 implements C1, C2 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidImplementedType_2() {
		assertQuickFix(
				IssueCodes.INVALID_IMPLEMENTED_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S2 implements C1, C2, E1 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "skill S2 implements C1, C2 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidImplementedType_3() {
		assertQuickFix(
				IssueCodes.INVALID_IMPLEMENTED_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "skill S2 implements E1 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "skill S2 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidImplementedType_4() {
		assertQuickFix(
				IssueCodes.INVALID_IMPLEMENTED_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C1 { }\n" //$NON-NLS-1$
				+ "skill S2 extends S1 implements E1 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C1 { }\n" //$NON-NLS-1$
				+ "skill S2 extends S1 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidExtendedType_0() {
		assertQuickFix(
				IssueCodes.INVALID_EXTENDED_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "capacity C3 extends E1, C1, C2 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "capacity C3 extends C1, C2 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidExtendedType_1() {
		assertQuickFix(
				IssueCodes.INVALID_EXTENDED_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "capacity C3 extends C1, E1, C2 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "capacity C3 extends C1, C2 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidExtendedType_2() {
		assertQuickFix(
				IssueCodes.INVALID_EXTENDED_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "capacity C3 extends C1, C2, E1 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "capacity C2 { }\n" //$NON-NLS-1$
				+ "capacity C3 extends C1, C2 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidExtendedType_3() {
		assertQuickFix(
				IssueCodes.INVALID_EXTENDED_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C3 extends E1 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'E1'", //$NON-NLS-1$
				"Remove the type 'E1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "event E1\n" //$NON-NLS-1$
				+ "capacity C3 { }\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixInvalidExtendedType_4() {
		assertQuickFix(
				IssueCodes.INVALID_EXTENDED_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "event E3 extends C1 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Remove the type 'C1'", //$NON-NLS-1$
				"Remove the type 'C1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "event E3 { }\n"); //$NON-NLS-1$
	}

	/** {@link IssueCodes#DISCOURAGED_CAPACITY_DEFINITION}.
	 */
	@Test
	public void fixDiscouragedCapacityDefinition() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				IssueCodes.DISCOURAGED_CAPACITY_DEFINITION,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n"); //$NON-NLS-1$
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove the capacity 'C1'", //$NON-NLS-1$
				"Remove the capacity 'C1'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT);
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Add the action 'aFunction'", //$NON-NLS-1$
				"Add the action 'aFunction'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 {\n" //$NON-NLS-1$
				+ "\tdef aFunction\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertNoQuickFix();
	}

	/**
	 */
	@Test
	public void fixMissingMethodImplementation_0() {
		assertQuickFix(
				IssueCodes.MISSING_METHOD_IMPLEMENTATION,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 {\n" //$NON-NLS-1$
				+ "\tdef fct1\n" //$NON-NLS-1$
				+ "\tdef fct2(a : int)\n" //$NON-NLS-1$
				+ "\tdef fct3 : boolean\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "skill S1 implements C1 { }\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Add unimplemented actions", //$NON-NLS-1$
				"Add the following unimplemented actions:\n- def fct1\n- def fct2(a : int)\n- def fct3 : boolean\n", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 {\n" //$NON-NLS-1$
				+ "\tdef fct1\n" //$NON-NLS-1$
				+ "\tdef fct2(a : int)\n" //$NON-NLS-1$
				+ "\tdef fct3 : boolean\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "skill S1 implements C1 {\n" //$NON-NLS-1$
				+ "\tdef fct1 {\n" //$NON-NLS-1$
				+ "\t\t// TODO: Auto-generated action.\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "\tdef fct2(a : int) {\n" //$NON-NLS-1$
				+ "\t\t// TODO: Auto-generated action.\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "\tdef fct3 : boolean {\n" //$NON-NLS-1$
				+ "\t\t// TODO: Auto-generated action.\n" //$NON-NLS-1$
				+ "\t\ttrue\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixMissingMethodImplementation_1() {
		assertQuickFix(
				IssueCodes.MISSING_METHOD_IMPLEMENTATION,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 {\n" //$NON-NLS-1$
				+ "\tdef fct1\n" //$NON-NLS-1$
				+ "\tdef fct2(a : int)\n" //$NON-NLS-1$
				+ "\tdef fct3 : boolean\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "skill S1 implements C1 {\n" //$NON-NLS-1$
				+ "\tdef fct2(b : int) { }\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Add unimplemented actions", //$NON-NLS-1$
				"Add the following unimplemented actions:\n- def fct1\n- def fct3 : boolean\n", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "capacity C1 {\n" //$NON-NLS-1$
				+ "\tdef fct1\n" //$NON-NLS-1$
				+ "\tdef fct2(a : int)\n" //$NON-NLS-1$
				+ "\tdef fct3 : boolean\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "skill S1 implements C1 {\n" //$NON-NLS-1$
				+ "\tdef fct2(b : int) { }\n" //$NON-NLS-1$
				+ "\tdef fct1 {\n" //$NON-NLS-1$
				+ "\t\t// TODO: Auto-generated action.\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "\tdef fct3 : boolean {\n" //$NON-NLS-1$
				+ "\t\t// TODO: Auto-generated action.\n" //$NON-NLS-1$
				+ "\t\ttrue\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixIncompatibleReturnType() {
		assertQuickFix(
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "\tdef fct1 : int {\n" //$NON-NLS-1$
				+ "\t\t123\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "agent A2 extends A1 {\n" //$NON-NLS-1$
				+ "\tdef fct1 : boolean {\n" //$NON-NLS-1$
				+ "\t\ttrue\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "}\n", //$NON-NLS-1$
				//
				// Label and description:
				//
				"Replace the type by 'int'", //$NON-NLS-1$
				"Replace the type by 'int'", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "\tdef fct1 : int {\n" //$NON-NLS-1$
				+ "\t\t123\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "agent A2 extends A1 {\n" //$NON-NLS-1$
				+ "\tdef fct1 : int {\n" //$NON-NLS-1$
				+ "\t\ttrue\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void fixNoDefaultValueForVariadicParameter() {
		QuickFixAsserts asserts = getQuickFixAsserts(
				IssueCodes.INVALID_USE_OF_VAR_ARG,
				//
				// Code to fix:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "\tdef fct1(a : int, b : boolean = true *) {\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove the variadic parameter", //$NON-NLS-1$
				"Remove the variadic parameter", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "\tdef fct1(a : int, b : boolean = true) {\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertQuickFix(
				//
				// Label and description:
				//
				"Remove the default value", //$NON-NLS-1$
				"Remove the default value", //$NON-NLS-1$
				//
				// Expected fixed code:
				//
				PACKAGE_STATEMENT
				+ "agent A1 {\n" //$NON-NLS-1$
				+ "\tdef fct1(a : int, b : boolean *) {\n" //$NON-NLS-1$
				+ "\t}\n" //$NON-NLS-1$
				+ "}\n"); //$NON-NLS-1$
		asserts.assertNoQuickFix();
	}

}
