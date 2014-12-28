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

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	SARLQuickfixProviderTest.WrongPackage.class,
	SARLQuickfixProviderTest.DuplicateTypeName.class,
	SARLQuickfixProviderTest.DuplicateField.class,
	SARLQuickfixProviderTest.DuplicateMethod.class,
	SARLQuickfixProviderTest.InvalidMemberName.class,
	SARLQuickfixProviderTest.RedundantInterfaceImplementation.class,
	SARLQuickfixProviderTest.VariableNameShadowing.class,
	SARLQuickfixProviderTest.OverriddenFinalOperation.class,
	SARLQuickfixProviderTest.DiscouragedBooleanExpression.class,
	SARLQuickfixProviderTest.UnreachableBehaviorUnit.class,
	SARLQuickfixProviderTest.InvalidCapacityType.class,
	SARLQuickfixProviderTest.InvalidFiringEventType.class,
	SARLQuickfixProviderTest.InvalidImplementedType.class,
	SARLQuickfixProviderTest.InvalidExtendedType.class,
	SARLQuickfixProviderTest.DiscouragedCapacityDefinition.class,
	SARLQuickfixProviderTest.MissingMethodImplementation.class,
	SARLQuickfixProviderTest.IncompatibleReturnType.class,
	SARLQuickfixProviderTest.InvalidUseOfVarargs.class,
	SARLQuickfixProviderTest.InconsistentTypeHierarchy.class,
	SARLQuickfixProviderTest.OverriddenFinalType.class,
	SARLQuickfixProviderTest.UnusedAgentCapacity.class,
	SARLQuickfixProviderTest.RedundantCapacityUse.class,
})
@SuppressWarnings("all")
public class SARLQuickfixProviderTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class WrongPackage extends AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixPackageName() {
			assertQuickFix(
					IssueCodes.WRONG_PACKAGE,
					// Code to fix:
					"package io.sarl.lang.ui.tests.quickfix.invalidPackage",
					//
					// Label and description:
					//
					"Change package declaration to 'io.sarl.lang.ui.tests.quickfix.iosarllangvalidationIssueCodeswrong_package'",
					"Change package declaration to 'io.sarl.lang.ui.tests.quickfix.iosarllangvalidationIssueCodeswrong_package'",
					//
					// Expected fixed code:
					//
					"package io.sarl.lang.ui.tests.quickfix.iosarllangvalidationIssueCodeswrong_package");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class DuplicateTypeName extends AbstractSARLQuickfixTest {

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
					+ "agent A1 { }\n"
					+ "event E1\n"
					+ "agent A1 { }\n",
					//
					// Label and description:
					//
					"Remove the duplicate definition of 'A1'",
					"Remove the duplicate definition of 'A1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 { }\n"
					+ "event E1\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class DuplicateField extends AbstractSARLQuickfixTest {

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
					+ "agent A1 {\n"
					+ "var attr1 = true\n"
					+ "var attr1 = \"\"\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the duplicate field 'attr1'",
					"Remove the duplicate field 'attr1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "var attr1 = true\n"
					+ "}\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class DuplicateMethod extends AbstractSARLQuickfixTest {

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
					+ "agent A1 {\n"
					+ "def method1(a : int) : boolean {\n"
					+ "true"
					+ "}\n"
					+ "def method1(a : int) : float {\n"
					+ "1.0f\n"
					+ "}\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the duplicate method 'method1(a : int)'",
					"Remove the duplicate method 'method1(a : int)'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "def method1(a : int) : boolean {\n"
					+ "true"
					+ "}\n"
					+ "}\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class InvalidMemberName extends AbstractSARLQuickfixTest {

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
					+ "agent A1 {\n"
					+ "def _handle_method1(a : int) : boolean {\n"
					+ "true"
					+ "}\n"
					+ "def method2(a : int) : float {\n"
					+ "1.0f\n"
					+ "}\n"
					+ "}\n");
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Rename the action '_handle_method1' to 'handleMethod1'",
					"Rename the action '_handle_method1' to 'handleMethod1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "def handleMethod1(a : int) : boolean {\n"
					+ "true"
					+ "}\n"
					+ "def method2(a : int) : float {\n"
					+ "1.0f\n"
					+ "}\n"
					+ "}\n");
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Rename the action '_handle_method1' to 'method1'",
					"Rename the action '_handle_method1' to 'method1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "def method1(a : int) : boolean {\n"
					+ "true"
					+ "}\n"
					+ "def method2(a : int) : float {\n"
					+ "1.0f\n"
					+ "}\n"
					+ "}\n");
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Remove the action '_handle_method1'",
					"Remove the action '_handle_method1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "def method2(a : int) : float {\n"
					+ "1.0f\n"
					+ "}\n"
					+ "}\n");
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
					+ "agent A1 {\n"
					+ "var ___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1 : boolean\n"
					+ "var myattr2 : float\n"
					+ "}\n");
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Rename the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1' to 'myattr1'",
					"Rename the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1' to 'myattr1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "var myattr1 : boolean\n"
					+ "var myattr2 : float\n"
					+ "}\n");
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Remove the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1'",
					"Remove the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "var myattr2 : float\n"
					+ "}\n");
			asserts.assertNoQuickFix();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class RedundantInterfaceImplementation extends AbstractSARLQuickfixTest {

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
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S1 implements C1, C2, C1 { }\n",
					//
					// Label and description:
					//
					"Remove the redundant feature 'C1'",
					"Remove the redundant feature 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S1 implements C1, C2 { }\n");
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
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S1 implements C1, C1, C2 { }\n",
					//
					// Label and description:
					//
					"Remove the redundant feature 'C1'",
					"Remove the redundant feature 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S1 implements C1, C2 { }\n");
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
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S1 implements C2, C1, C1 { }\n",
					//
					// Label and description:
					//
					"Remove the redundant feature 'C1'",
					"Remove the redundant feature 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S1 implements C2, C1 { }\n");
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
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1 { }\n"
					+ "skill S1 implements C1, C2, C3 { }\n",
					//
					// Label and description:
					//
					"Remove the redundant feature 'C1'",
					"Remove the redundant feature 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1 { }\n"
					+ "skill S1 implements C2, C3 { }\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class VariableNameShadowing extends AbstractSARLQuickfixTest {

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
					+ "agent A1 {\n"
					+ "var attr1 : boolean\n"
					+ "}\n"
					+ "agent A2 extends A1 {\n"
					+ "var attr1 : int\n"
					+ "}\n");
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Remove the field 'attr1'",
					"Remove the field 'attr1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "var attr1 : boolean\n"
					+ "}\n"
					+ "agent A2 extends A1 {\n"
					+ "}\n");
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Rename the field 'attr1' to 'attr10'",
					"Rename the field 'attr1' to 'attr10'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "var attr1 : boolean\n"
					+ "}\n"
					+ "agent A2 extends A1 {\n"
					+ "var attr10 : int\n"
					+ "}\n");
			asserts.assertNoQuickFix();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class OverriddenFinalOperation extends AbstractSARLQuickfixTest {

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
					+ "agent A1 {\n"
					+ "def myfct(a : boolean, a : int = 4) { }\n"
					+ "}\n"
					+ "agent A2 extends A1 {\n"
					+ "def myfct(b : boolean) { }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the action 'myfct(boolean)'",
					"Remove the action 'myfct(boolean)'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "def myfct(a : boolean, a : int = 4) { }\n"
					+ "}\n"
					+ "agent A2 extends A1 {\n"
					+ "}\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class DiscouragedBooleanExpression extends AbstractSARLQuickfixTest {

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
					+ "event E1\n"
					+ "agent A1 {\n"
					+ "on E1 [true] { [ false ] }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the guard",
					"Remove the guard",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "agent A1 {\n"
					+ "on E1 { [ false ] }\n"
					+ "}\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class UnreachableBehaviorUnit extends AbstractSARLQuickfixTest {

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
					+ "event E1\n"
					+ "agent A1 {\n"
					+ "on E1 [false] { [ false ] }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the behavior unit on 'E1'",
					"Remove the behavior unit on 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "agent A1 {\n"
					+ "}\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class InvalidCapacityType extends AbstractSARLQuickfixTest {

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
					+ "event E1\n"
					+ "agent A1 {\n"
					+ "uses E1\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "agent A1 {\n"
					+ "}\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "uses E1, C1, C2\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2\n"
					+ "}\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "uses C1, E1, C2\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2\n"
					+ "}\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2, E1\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2\n"
					+ "}\n");
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
					+ "event E1\n"
					+ "agent A1 {\n"
					+ "requires E1\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "agent A1 {\n"
					+ "}\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "requires E1, C1, C2\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "requires C1, C2\n"
					+ "}\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "requires C1, E1, C2\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "requires C1, C2\n"
					+ "}\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "requires C1, C2, E1\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "agent A1 {\n"
					+ "requires C1, C2\n"
					+ "}\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class InvalidFiringEventType extends AbstractSARLQuickfixTest {

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
					+ "event E1\n"
					+ "event E2\n"
					+ "capacity C1 { }\n"
					+ "agent A1 {\n"
					+ "def myfct fires C1, E1, E2 { }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'C1'",
					"Remove the type 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "event E2\n"
					+ "capacity C1 { }\n"
					+ "agent A1 {\n"
					+ "def myfct fires E1, E2 { }\n"
					+ "}\n");
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
					+ "event E1\n"
					+ "event E2\n"
					+ "capacity C1 { }\n"
					+ "agent A1 {\n"
					+ "def myfct fires E1, C1, E2 { }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'C1'",
					"Remove the type 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "event E2\n"
					+ "capacity C1 { }\n"
					+ "agent A1 {\n"
					+ "def myfct fires E1, E2 { }\n"
					+ "}\n");
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
					+ "event E1\n"
					+ "event E2\n"
					+ "capacity C1 { }\n"
					+ "agent A1 {\n"
					+ "def myfct fires E1, E2, C1 { }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the type 'C1'",
					"Remove the type 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "event E2\n"
					+ "capacity C1 { }\n"
					+ "agent A1 {\n"
					+ "def myfct fires E1, E2 { }\n"
					+ "}\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class InvalidImplementedType extends AbstractSARLQuickfixTest {

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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S2 implements E1, C1, C2 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S2 implements C1, C2 { }\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S2 implements C1, E1, C2 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S2 implements C1, C2 { }\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S2 implements C1, C2, E1 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "skill S2 implements C1, C2 { }\n");
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
					+ "event E1\n"
					+ "skill S2 implements E1 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "skill S2 { }\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "skill S1 implements C1 { }\n"
					+ "skill S2 extends S1 implements E1 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "skill S1 implements C1 { }\n"
					+ "skill S2 extends S1 { }\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class InvalidExtendedType extends AbstractSARLQuickfixTest {

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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends E1, C1, C2 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1, C2 { }\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1, E1, C2 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1, C2 { }\n");
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
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1, C2, E1 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1, C2 { }\n");
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
					+ "event E1\n"
					+ "capacity C3 extends E1 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'E1'",
					"Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "event E1\n"
					+ "capacity C3 { }\n");
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
					+ "capacity C1 { }\n"
					+ "event E3 extends C1 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'C1'",
					"Remove the type 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "event E3 { }\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class DiscouragedCapacityDefinition extends AbstractSARLQuickfixTest {

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
					+ "capacity C1 { }\n");
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT);
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Add the action 'aFunction'",
					"Add the action 'aFunction'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 {\n"
					+ "\tdef aFunction\n"
					+ "}\n");
			asserts.assertNoQuickFix();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class MissingMethodImplementation extends AbstractSARLQuickfixTest {

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
					+ "capacity C1 {\n"
					+ "\tdef fct1\n"
					+ "\tdef fct2(a : int)\n"
					+ "\tdef fct3 : boolean\n"
					+ "}\n"
					+ "skill S1 implements C1 { }\n",
					//
					// Label and description:
					//
					"Add unimplemented actions",
					"Add the following unimplemented actions:\n- def fct1\n- def fct2(a : int)\n- def fct3 : boolean\n",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 {\n"
					+ "\tdef fct1\n"
					+ "\tdef fct2(a : int)\n"
					+ "\tdef fct3 : boolean\n"
					+ "}\n"
					+ "skill S1 implements C1 {\n"
					+ "\tdef fct1 {\n"
					+ "\t\t// TODO: Auto-generated action.\n"
					+ "\t}\n"
					+ "\tdef fct2(a : int) {\n"
					+ "\t\t// TODO: Auto-generated action.\n"
					+ "\t}\n"
					+ "\tdef fct3 : boolean {\n"
					+ "\t\t// TODO: Auto-generated action.\n"
					+ "\t\ttrue\n"
					+ "\t}\n"
					+ "}\n");
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
					+ "capacity C1 {\n"
					+ "\tdef fct1\n"
					+ "\tdef fct2(a : int)\n"
					+ "\tdef fct3 : boolean\n"
					+ "}\n"
					+ "skill S1 implements C1 {\n"
					+ "\tdef fct2(b : int) { }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Add unimplemented actions",
					"Add the following unimplemented actions:\n- def fct1\n- def fct3 : boolean\n",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 {\n"
					+ "\tdef fct1\n"
					+ "\tdef fct2(a : int)\n"
					+ "\tdef fct3 : boolean\n"
					+ "}\n"
					+ "skill S1 implements C1 {\n"
					+ "\tdef fct2(b : int) { }\n"
					+ "\tdef fct1 {\n"
					+ "\t\t// TODO: Auto-generated action.\n"
					+ "\t}\n"
					+ "\tdef fct3 : boolean {\n"
					+ "\t\t// TODO: Auto-generated action.\n"
					+ "\t\ttrue\n"
					+ "\t}\n"
					+ "}\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class IncompatibleReturnType extends AbstractSARLQuickfixTest {

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
					+ "agent A1 {\n"
					+ "\tdef fct1 : int {\n"
					+ "\t\t123\n"
					+ "\t}\n"
					+ "}\n"
					+ "agent A2 extends A1 {\n"
					+ "\tdef fct1 : boolean {\n"
					+ "\t\ttrue\n"
					+ "\t}\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Replace the type by 'int'",
					"Replace the type by 'int'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "\tdef fct1 : int {\n"
					+ "\t\t123\n"
					+ "\t}\n"
					+ "}\n"
					+ "agent A2 extends A1 {\n"
					+ "\tdef fct1 : int {\n"
					+ "\t\ttrue\n"
					+ "\t}\n"
					+ "}\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class InvalidUseOfVarargs extends AbstractSARLQuickfixTest {

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
					+ "agent A1 {\n"
					+ "\tdef fct1(a : int, b : boolean = true *) {\n"
					+ "\t}\n"
					+ "}\n");
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Remove the variadic parameter",
					"Remove the variadic parameter",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "\tdef fct1(a : int, b : boolean = true) {\n"
					+ "\t}\n"
					+ "}\n");
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Remove the default value",
					"Remove the default value",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "agent A1 {\n"
					+ "\tdef fct1(a : int, b : boolean *) {\n"
					+ "\t}\n"
					+ "}\n");
			asserts.assertNoQuickFix();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class InconsistentTypeHierarchy extends AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_0() {
			assertQuickFix(
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 extends C1 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'C1'",
					"Remove the type 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n");
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_1() {
			assertQuickFix(
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 extends C2 { }\n"
					+ "capacity C2 extends C1 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'C2'",
					"Remove the type 'C2'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 extends C1 { }\n");
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_3() {
			assertQuickFix(
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 extends C2 { }\n"
					+ "capacity C2 extends C3 { }\n"
					+ "capacity C3 extends C1 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'C2'",
					"Remove the type 'C2'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 extends C3 { }\n"
					+ "capacity C3 extends C1 { }\n");
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_4() {
			assertQuickFix(
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C3 extends C1 { }\n"
					+ "capacity C2 extends C3 { }\n"
					+ "capacity C1 extends C2 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'C1'",
					"Remove the type 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C3 { }\n"
					+ "capacity C2 extends C3 { }\n"
					+ "capacity C1 extends C2 { }\n");
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_5() {
			assertQuickFix(
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1, C2, C3 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'C3'",
					"Remove the type 'C3'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1, C2 { }\n");
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_6() {
			assertQuickFix(
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1, C3, C2 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'C3'",
					"Remove the type 'C3'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1, C2 { }\n");
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_7() {
			assertQuickFix(
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C3, C1, C2 { }\n",
					//
					// Label and description:
					//
					"Remove the type 'C3'",
					"Remove the type 'C3'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { }\n"
					+ "capacity C2 { }\n"
					+ "capacity C3 extends C1, C2 { }\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class OverriddenFinalType extends AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		@Ignore
		public void fixOverriddenFinalType() {
			assertQuickFix(
					IssueCodes.OVERRIDDEN_FINAL_TYPE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "import foo.MockFinalAgent\n"
					+ "agent A1 extends MockFinalAgent { }\n",
					//
					// Label and description:
					//
					"Remove the type 'MockFinalAgent'",
					"Remove the type 'MockFinalAgent'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "import foo.MockFinalAgent\n"
					+ "agent A1 { }\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class UnusedAgentCapacity extends AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_0() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2, C3\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C2'",
					"Remove the capacity 'C2'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "agent A1 {\n"
					+ "uses C1, C3\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_1() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "agent A1 {\n"
					+ "uses C1, C3, C2\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C2'",
					"Remove the capacity 'C2'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "agent A1 {\n"
					+ "uses C1, C3\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_2() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "agent A1 {\n"
					+ "uses C2, C1, C3\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C2'",
					"Remove the capacity 'C2'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "agent A1 {\n"
					+ "uses C1, C3\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_3() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity Logging { def println(v : String) }\n"
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "agent AgentB {\n"
					+ "\tuses Logging, Lifecycle\n"
					+ "\ton Initialize {\n"
					+ "\t\tprintln(\"Je me presente\")\n"
					+ "\t}\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity Logging { def println(v : String) }\n"
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "agent AgentB {\n"
					+ "\tuses Logging\n"
					+ "\ton Initialize {\n"
					+ "\t\tprintln(\"Je me presente\")\n"
					+ "\t}\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_4() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity Logging { def println(v : String) }\n"
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "agent AgentB {\n"
					+ "\tuses Lifecycle, Logging\n"
					+ "\ton Initialize {\n"
					+ "\t\tprintln(\"Je me presente\")\n"
					+ "\t}\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity Logging { def println(v : String) }\n"
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "agent AgentB {\n"
					+ "\tuses Logging\n"
					+ "\ton Initialize {\n"
					+ "\t\tprintln(\"Je me presente\")\n"
					+ "\t}\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_5() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "agent AgentB {\n"
					+ "\tuses Lifecycle\n"
					+ "\ton Initialize {\n"
					+ "\t}\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "agent AgentB {\n"
					+ "\ton Initialize {\n"
					+ "\t}\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_6() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C2, C3\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C2'",
					"Remove the capacity 'C2'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C3\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_7() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C3, C2\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C2'",
					"Remove the capacity 'C2'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C3\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_8() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "behavior B1 {\n"
					+ "uses C2, C1, C3\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C2'",
					"Remove the capacity 'C2'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "capacity C3 { def myfct2 }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C3\n"
					+ "def testfct { myfct; myfct2 }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_9() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity Logging { def println(v : String) }\n"
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "behavior B {\n"
					+ "\tuses Logging, Lifecycle\n"
					+ "\ton Initialize {\n"
					+ "\t\tprintln(\"Je me presente\")\n"
					+ "\t}\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity Logging { def println(v : String) }\n"
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "behavior B {\n"
					+ "\tuses Logging\n"
					+ "\ton Initialize {\n"
					+ "\t\tprintln(\"Je me presente\")\n"
					+ "\t}\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_10() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity Logging { def println(v : String) }\n"
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "behavior B {\n"
					+ "\tuses Lifecycle, Logging\n"
					+ "\ton Initialize {\n"
					+ "\t\tprintln(\"Je me presente\")\n"
					+ "\t}\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity Logging { def println(v : String) }\n"
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "behavior B {\n"
					+ "\tuses Logging\n"
					+ "\ton Initialize {\n"
					+ "\t\tprintln(\"Je me presente\")\n"
					+ "\t}\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixUnusedAgentCapacity_11() {
			assertQuickFix(
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "behavior B {\n"
					+ "\tuses Lifecycle\n"
					+ "\ton Initialize {\n"
					+ "\t}\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity Lifecycle { def killMe }\n"
					+ "event Initialize\n"
					+ "behavior B {\n"
					+ "\ton Initialize {\n"
					+ "\t}\n"
					+ "}\n");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class RedundantCapacityUse extends AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_0() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2, C1\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_1() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "agent A1 {\n"
					+ "uses C1, C1\n"
					+ "def testfct { myfct }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "agent A1 {\n"
					+ "uses C1\n"
					+ "def testfct { myfct }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_2() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "agent A1 {\n"
					+ "uses C1, C1, C2, C1\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2, C1\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_3() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2, C1, C1\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2, C1\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_4() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2\n"
					+ "def testfct { myfct; iddle }\n"
					+ "uses C1\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_5() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2\n"
					+ "def testfct { myfct; iddle }\n"
					+ "uses C1, C2, C1\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "agent A1 {\n"
					+ "uses C1, C2\n"
					+ "def testfct { myfct; iddle }\n"
					+ "uses C2, C1\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_6() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C2, C1\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C2\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_7() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C1\n"
					+ "def testfct { myfct }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "behavior B1 {\n"
					+ "uses C1\n"
					+ "def testfct { myfct }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_8() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C1, C2, C1\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C2, C1\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_9() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C2, C1, C1\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C2, C1\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_10() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C2\n"
					+ "def testfct { myfct; iddle }\n"
					+ "uses C1\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C2\n"
					+ "def testfct { myfct; iddle }\n"
					+ "}\n");
		}

		/**
		 */
		@Test
		public void fixRedundantCapacityUse_11() {
			assertQuickFix(
					IssueCodes.REDUNDANT_CAPACITY_USE,
					//
					// Code to fix:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C2\n"
					+ "def testfct { myfct; iddle }\n"
					+ "uses C1, C2, C1\n"
					+ "}\n",
					//
					// Label and description:
					//
					"Remove the capacity 'C1'",
					"Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					PACKAGE_STATEMENT
					+ "capacity C1 { def myfct }\n"
					+ "capacity C2 { def iddle }\n"
					+ "behavior B1 {\n"
					+ "uses C1, C2\n"
					+ "def testfct { myfct; iddle }\n"
					+ "uses C2, C1\n"
					+ "}\n");
		}

	}

}
