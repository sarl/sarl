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
@SuiteClasses({ SARLQuickfixProviderTest.WrongPackage.class,
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
		SARLQuickfixProviderTest.RedundantCapacityUse.class, })
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
					multilineString("agent A1 { }", "event E1", "agent A1 { }"),
					//
					// Label and description:
					//
					"Remove the duplicate definition of 'A1'",
					"Remove the duplicate definition of 'A1'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 { }", "event E1"));
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
					multilineString("agent A1 {", "	var attr1 = true",
							"	var attr1 = \"\"", "}"),
					//
					// Label and description:
					//
					"Remove the duplicate field 'attr1'",
					"Remove the duplicate field 'attr1'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {", "	var attr1 = true", "}"));
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
					multilineString("agent A1 {",
							"	def method1(a : int) : boolean {", "		true",
							"	}", "	def method1(a : int) : float {", "		1.0f",
							"	}", "}"),
					//
					// Label and description:
					//
					"Remove the duplicate method 'method1(a : int)'",
					"Remove the duplicate method 'method1(a : int)'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {",
							"	def method1(a : int) : boolean {", "		true",
							"	}", "}"));
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
					multilineString("agent A1 {",
							"	def _handle_method1(a : int) : boolean {",
							"		true", "	}", "	def method2(a : int) : float {",
							"		1.0f", "	}", "}"));
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Rename the action '_handle_method1' to 'handleMethod1'",
					"Rename the action '_handle_method1' to 'handleMethod1'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {",
							"	def handleMethod1(a : int) : boolean {",
							"		true", "	}", "	def method2(a : int) : float {",
							"		1.0f", "	}", "}"));
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Rename the action '_handle_method1' to 'method1'",
					"Rename the action '_handle_method1' to 'method1'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {",
							"	def method1(a : int) : boolean {", "		true",
							"	}", "	def method2(a : int) : float {", "		1.0f",
							"	}", "}"));
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Remove the action '_handle_method1'",
					"Remove the action '_handle_method1'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {",
							"	def method2(a : int) : float {", "		1.0f", "	}",
							"}"));
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
					multilineString(
							"agent A1 {",
							"var ___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1 : boolean",
							"var myattr2 : float", "}"));
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Rename the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1' to 'myattr1'",
					"Rename the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1' to 'myattr1'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {", "var myattr1 : boolean",
							"var myattr2 : float", "}"));
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Remove the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1'",
					"Remove the attribute '___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {", "var myattr2 : float", "}"));
			asserts.assertNoQuickFix();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class RedundantInterfaceImplementation extends
			AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixRedundantInterface_0() {
			assertQuickFix(
					IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
					//
					// Code to fix:
					//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"skill S1 implements C1, C2, C1 { }"),
					//
					// Label and description:
					//
					"Remove the redundant feature 'C1'",
					"Remove the redundant feature 'C1'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"skill S1 implements C1, C2 { }"));
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
					multilineString("capacity C1 { }", "capacity C2 { }",
							"skill S1 implements C1, C1, C2 { }"),
					//
					// Label and description:
					//
					"Remove the redundant feature 'C1'",
					"Remove the redundant feature 'C1'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"skill S1 implements C1, C2 { }"));
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
					multilineString("capacity C1 { }", "capacity C2 { }",
							"skill S1 implements C2, C1, C1 { }"),
					//
					// Label and description:
					//
					"Remove the redundant feature 'C1'",
					"Remove the redundant feature 'C1'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"skill S1 implements C2, C1 { }"));
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
					multilineString("capacity C1 { }", "capacity C2 { }",
							"capacity C3 extends C1 { }",
							"skill S1 implements C1, C2, C3 { }"),
					//
					// Label and description:
					//
					"Remove the redundant feature 'C1'",
					"Remove the redundant feature 'C1'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"capacity C3 extends C1 { }",
							"skill S1 implements C2, C3 { }"));
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
					multilineString("agent A1 {", "	var attr1 : boolean", "}",
							"agent A2 extends A1 {", "	var attr1 : int", "}"));
			asserts.assertQuickFix(
			//
			// Label and description:
			//
					"Remove the field 'attr1'", "Remove the field 'attr1'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {", "	var attr1 : boolean", "}",
							"agent A2 extends A1 {", "}"));
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Rename the field 'attr1' to 'attr10'",
					"Rename the field 'attr1' to 'attr10'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {", "	var attr1 : boolean", "}",
							"agent A2 extends A1 {", "	var attr10 : int", "}"));
			asserts.assertNoQuickFix();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class OverriddenFinalOperation extends
			AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixOverriddenFinal() {
			assertQuickFix(
					IssueCodes.OVERRIDDEN_FINAL_OPERATION,
					//
					// Code to fix:
					//
					multilineString("agent A1 {",
							"	def myfct(a : boolean, a : int = 4) { }", "}",
							"agent A2 extends A1 {",
							"	def myfct(b : boolean) { }", "}"),
					//
					// Label and description:
					//
					"Remove the action 'myfct(boolean)'",
					"Remove the action 'myfct(boolean)'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {",
							"	def myfct(a : boolean, a : int = 4) { }", "}",
							"agent A2 extends A1 {", "}"));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class DiscouragedBooleanExpression extends
			AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixDiscouragedBooleanExpression() {
			assertQuickFix(IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION,
			//
			// Code to fix:
			//
					multilineString("event E1", "agent A1 {",
							"	on E1 [true] { [ false ] }", "}"),
					//
					// Label and description:
					//
					"Remove the guard", "Remove the guard",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "agent A1 {",
							"	on E1 { [ false ] }", "}"));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class UnreachableBehaviorUnit extends
			AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixUnreachableBehaviorUnit() {
			assertQuickFix(
					IssueCodes.UNREACHABLE_BEHAVIOR_UNIT,
					//
					// Code to fix:
					//
					multilineString("event E1", "agent A1 {",
							"	on E1 [false] { [ false ] }", "}"),
					//
					// Label and description:
					//
					"Remove the behavior unit on 'E1'",
					"Remove the behavior unit on 'E1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "agent A1 {", "}"));
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
			assertQuickFix(IssueCodes.INVALID_CAPACITY_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "agent A1 {", "	uses E1", "}"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "agent A1 {", "}"));
		}

		/**
		 */
		@Test
		public void fixInvalidCapacityType_1() {
			assertQuickFix(IssueCodes.INVALID_CAPACITY_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }", "agent A1 {",
							"	uses E1, C1, C2", "}"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
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
			assertQuickFix(IssueCodes.INVALID_CAPACITY_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }", "agent A1 {",
							"	uses C1, E1, C2", "}"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
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
			assertQuickFix(IssueCodes.INVALID_CAPACITY_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }", "agent A1 {",
							"	uses C1, C2, E1", "}"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
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
			assertQuickFix(IssueCodes.INVALID_CAPACITY_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "agent A1 {", "	requires E1",
							"}"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "agent A1 {", "}"));
		}

		/**
		 */
		@Test
		public void fixInvalidCapacityType_5() {
			assertQuickFix(IssueCodes.INVALID_CAPACITY_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }", "agent A1 {",
							"	requires E1, C1, C2", "}"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
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
			assertQuickFix(IssueCodes.INVALID_CAPACITY_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }", "agent A1 {",
							"	requires C1, E1, C2", "}"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
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
			assertQuickFix(IssueCodes.INVALID_CAPACITY_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }", "agent A1 {",
							"	requires C1, C2, E1", "}"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }", "agent A1 {",
							"	requires C1, C2", "}"));
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
			assertQuickFix(IssueCodes.INVALID_FIRING_EVENT_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "event E2", "capacity C1 { }",
							"agent A1 {", "	def myfct fires C1, E1, E2 { }",
							"}"),
					//
					// Label and description:
					//
					"Remove the type 'C1'", "Remove the type 'C1'",
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
			assertQuickFix(IssueCodes.INVALID_FIRING_EVENT_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "event E2", "capacity C1 { }",
							"agent A1 {", "	def myfct fires E1, C1, E2 { }",
							"}"),
					//
					// Label and description:
					//
					"Remove the type 'C1'", "Remove the type 'C1'",
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
			assertQuickFix(IssueCodes.INVALID_FIRING_EVENT_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "event E2", "capacity C1 { }",
							"agent A1 {", "	def myfct fires E1, E2, C1 { }",
							"}"),
					//
					// Label and description:
					//
					"Remove the type 'C1'", "Remove the type 'C1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "event E2", "capacity C1 { }",
							"agent A1 {", "	def myfct fires E1, E2 { }", "}"));
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
			assertQuickFix(IssueCodes.INVALID_IMPLEMENTED_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }",
							"skill S2 implements E1, C1, C2 { }"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
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
			assertQuickFix(IssueCodes.INVALID_IMPLEMENTED_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }",
							"skill S2 implements C1, E1, C2 { }"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
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
			assertQuickFix(IssueCodes.INVALID_IMPLEMENTED_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }",
							"skill S2 implements C1, C2, E1 { }"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
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
			assertQuickFix(IssueCodes.INVALID_IMPLEMENTED_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "skill S2 implements E1 { }"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "skill S2 { }"));
		}

		/**
		 */
		@Test
		public void fixInvalidImplementedType_4() {
			assertQuickFix(IssueCodes.INVALID_IMPLEMENTED_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"skill S1 implements C1 { }",
							"skill S2 extends S1 implements E1 { }"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "capacity C1 { }",
							"skill S1 implements C1 { }",
							"skill S2 extends S1 { }"));
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
			assertQuickFix(IssueCodes.INVALID_EXTENDED_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }",
							"capacity C3 extends E1, C1, C2 { }"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }", "capacity C3 extends C1, C2 { }"));
		}

		/**
		 */
		@Test
		public void fixInvalidExtendedType_1() {
			assertQuickFix(IssueCodes.INVALID_EXTENDED_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }",
							"capacity C3 extends C1, E1, C2 { }"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }", "capacity C3 extends C1, C2 { }"));
		}

		/**
		 */
		@Test
		public void fixInvalidExtendedType_2() {
			assertQuickFix(IssueCodes.INVALID_EXTENDED_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }",
							"capacity C3 extends C1, C2, E1 { }"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "capacity C1 { }",
							"capacity C2 { }", "capacity C3 extends C1, C2 { }"));
		}

		/**
		 */
		@Test
		public void fixInvalidExtendedType_3() {
			assertQuickFix(IssueCodes.INVALID_EXTENDED_TYPE,
			//
			// Code to fix:
			//
					multilineString("event E1", "capacity C3 extends E1 { }"),
					//
					// Label and description:
					//
					"Remove the type 'E1'", "Remove the type 'E1'",
					//
					// Expected fixed code:
					//
					multilineString("event E1", "capacity C3 { }"));
		}

		/**
		 */
		@Test
		public void fixInvalidExtendedType_4() {
			assertQuickFix(IssueCodes.INVALID_EXTENDED_TYPE,
			//
			// Code to fix:
			//
					multilineString("capacity C1 { }",
							"event E3 extends C1 { }"),
					//
					// Label and description:
					//
					"Remove the type 'C1'", "Remove the type 'C1'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { }", "event E3 { }"));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class DiscouragedCapacityDefinition extends
			AbstractSARLQuickfixTest {

		/**
		 * {@link IssueCodes#DISCOURAGED_CAPACITY_DEFINITION}.
		 */
		@Test
		public void fixDiscouragedCapacityDefinition() {
			QuickFixAsserts asserts = getQuickFixAsserts(
					IssueCodes.DISCOURAGED_CAPACITY_DEFINITION,
					//
					// Code to fix:
					//
					"capacity C1 { }");
			asserts.assertQuickFix(
			//
			// Label and description:
			//
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					"");
			asserts.assertQuickFix(
			//
			// Label and description:
			//
					"Add the action 'aFunction'", "Add the action 'aFunction'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 {", "	def aFunction", "}"));
			asserts.assertNoQuickFix();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class MissingMethodImplementation extends
			AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixClassicPrototype_0() {
			assertQuickFix(
					IssueCodes.MISSING_METHOD_IMPLEMENTATION,
					//
					// Code to fix:
					//
					multilineString(
							"capacity C1 {",
							"	def fct1",
							"	def fct2(a : int)",
							"	def fct3 : boolean",
							"}",
							"skill S1 implements C1 { }"),
					//
					// Label and description:
					//
					"Add unimplemented actions",
					"Add the following unimplemented actions:\n- def fct1\n- def fct2(a : int)\n- def fct3 : boolean\n",
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
							"	def fct1 {",
							"		// TODO Auto-generated action.",
							"	}",
							"	def fct2(a : int) {",
							"		// TODO Auto-generated action.",
							"	}",
							"	def fct3 : boolean {",
							"		// TODO Auto-generated action.",
							"		true",
							"	}",
							"}"));
		}

		/**
		 */
		@Test
		public void fixClassicPrototype_1() {
			assertQuickFix(
					IssueCodes.MISSING_METHOD_IMPLEMENTATION,
					//
					// Code to fix:
					//
					multilineString("capacity C1 {", "	def fct1",
							"	def fct2(a : int)", "	def fct3 : boolean", "}",
							"skill S1 implements C1 {",
							"	def fct2(b : int) { }", "}"),
					//
					// Label and description:
					//
					"Add unimplemented actions",
					"Add the following unimplemented actions:\n- def fct1\n- def fct3 : boolean\n",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 {", "	def fct1",
							"	def fct2(a : int)", "	def fct3 : boolean", "}",
							"skill S1 implements C1 {",
							"	def fct2(b : int) { }", "	def fct1 {",
							"		// TODO Auto-generated action.", "	}",
							"	def fct3 : boolean {",
							"		// TODO Auto-generated action.", "		true", "	}",
							"}"));
		}

		/**
		 */
		@Test
		public void fixVariadicParameter_0() {
			assertQuickFix(
					IssueCodes.MISSING_METHOD_IMPLEMENTATION,
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
					"Add unimplemented actions",
					"Add the following unimplemented actions:\n- def fct(a : int*)\n",
					//
					// Expected fixed code:
					//
					multilineString(
							"capacity C1 {",
							"	def fct(a : int*)",
							"}",
							"skill S1 implements C1 {",
							"	def fct(a : int*) {",
							"		// TODO Auto-generated action.",
							"	}",
							"}"));
		}

		/**
		 */
		@Test
		public void fixVariadicParameter_1() {
			assertQuickFix(
					IssueCodes.MISSING_METHOD_IMPLEMENTATION,
					//
					// Code to fix:
					//
					multilineString(
							"capacity C1 {",
							"	def fct1(a : int*)",
							"	def fct2(b : String, c : double*)",
							"}",
							"skill S1 implements C1 { }"),
					//
					// Label and description:
					//
					"Add unimplemented actions",
					"Add the following unimplemented actions:\n- def fct1(a : int*)\n- def fct2(b : String, c : double*)\n",
					//
					// Expected fixed code:
					//
					multilineString(
							"capacity C1 {",
							"	def fct1(a : int*)",
							"	def fct2(b : String, c : double*)",
							"}",
							"skill S1 implements C1 {",
							"	def fct1(a : int*) {",
							"		// TODO Auto-generated action.",
							"	}",
							"	def fct2(b : String, c : double*) {",
							"		// TODO Auto-generated action.",
							"	}",
							"}"));
		}

		/**
		 */
		@Test
		public void fixParameterDefaultValue_0() {
			assertQuickFix(
					IssueCodes.MISSING_METHOD_IMPLEMENTATION,
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
					"Add unimplemented actions",
					"Add the following unimplemented actions:\n- def fct(a : int=4)\n",
					//
					// Expected fixed code:
					//
					multilineString(
							"capacity C1 {",
							"	def fct(a : int=4)",
							"}",
							"skill S1 implements C1 {",
							"	def fct(a : int=4) {",
							"		// TODO Auto-generated action.",
							"	}",
							"}"));
		}

		/**
		 */
		@Test
		public void fixParameterDefaultValue_1() {
			assertQuickFix(
					IssueCodes.MISSING_METHOD_IMPLEMENTATION,
					//
					// Code to fix:
					//
					multilineString(
							"capacity C1 {",
							"	def fct(a : int=4, b : java.lang.String)",
							"}",
							"skill S1 implements C1 { }"),
					//
					// Label and description:
					//
					"Add unimplemented actions",
					"Add the following unimplemented actions:\n- def fct(a : int=4, b : java.lang.String)\n",
					//
					// Expected fixed code:
					//
					multilineString(
							"capacity C1 {",
							"	def fct(a : int=4, b : java.lang.String)",
							"}",
							"skill S1 implements C1 {",
							"	def fct(a : int=4, b : java.lang.String) {",
							"		// TODO Auto-generated action.",
							"	}",
							"}"));
		}

		/**
		 */
		@Test
		public void fixParameterDefaultValueAndVariadicParameter() {
			assertQuickFix(
					IssueCodes.MISSING_METHOD_IMPLEMENTATION,
					//
					// Code to fix:
					//
					multilineString(
							"capacity C1 {",
							"	def fct(a : int=4, b : java.lang.String, c : int*)",
							"}",
							"skill S1 implements C1 { }"),
					//
					// Label and description:
					//
					"Add unimplemented actions",
					"Add the following unimplemented actions:\n- def fct(a : int=4, b : java.lang.String, c : int*)\n",
					//
					// Expected fixed code:
					//
					multilineString(
							"capacity C1 {",
							"	def fct(a : int=4, b : java.lang.String, c : int*)",
							"}",
							"skill S1 implements C1 {",
							"	def fct(a : int=4, b : java.lang.String, c : int*) {",
							"		// TODO Auto-generated action.",
							"	}",
							"}"));
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
					multilineString("agent A1 {", "	def fct1 : int {", "		123",
							"	}", "}", "agent A2 extends A1 {",
							"	def fct1 : boolean {", "		true", "	}", "}"),
					//
					// Label and description:
					//
					"Replace the type by 'int'", "Replace the type by 'int'",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {", "	def fct1 : int {", "		123",
							"	}", "}", "agent A2 extends A1 {",
							"	def fct1 : int {", "		true", "	}", "}"));
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
					multilineString("agent A1 {",
							"	def fct1(a : int, b : boolean = true *) {", "	}",
							"}"));
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Remove the variadic parameter",
					"Remove the variadic parameter",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {",
							"	def fct1(a : int, b : boolean = true) {", "	}",
							"}"));
			asserts.assertQuickFix(
			//
			// Label and description:
			//
					"Remove the default value", "Remove the default value",
					//
					// Expected fixed code:
					//
					multilineString("agent A1 {",
							"	def fct1(a : int, b : boolean *) {", "	}", "}"));
			asserts.assertNoQuickFix();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class InconsistentTypeHierarchy extends
			AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_0() {
			assertQuickFix(IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
			//
			// Code to fix:
			//
					multilineString("capacity C1 extends C1 { }"),
					//
					// Label and description:
					//
					"Remove the type 'C1'", "Remove the type 'C1'",
					//
					// Expected fixed code:
					//
					"capacity C1 { }");
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_1() {
			assertQuickFix(IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
			//
			// Code to fix:
			//
					multilineString("capacity C1 extends C2 { }",
							"capacity C2 extends C1 { }"),
					//
					// Label and description:
					//
					"Remove the type 'C2'", "Remove the type 'C2'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { }",
							"capacity C2 extends C1 { }"));
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_3() {
			assertQuickFix(IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
			//
			// Code to fix:
			//
					multilineString("capacity C1 extends C2 { }",
							"capacity C2 extends C3 { }",
							"capacity C3 extends C1 { }"),
					//
					// Label and description:
					//
					"Remove the type 'C2'", "Remove the type 'C2'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { }",
							"capacity C2 extends C3 { }",
							"capacity C3 extends C1 { }"));
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_4() {
			assertQuickFix(IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
			//
			// Code to fix:
			//
					multilineString("capacity C3 extends C1 { }",
							"capacity C2 extends C3 { }",
							"capacity C1 extends C2 { }"),
					//
					// Label and description:
					//
					"Remove the type 'C1'", "Remove the type 'C1'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C3 { }",
							"capacity C2 extends C3 { }",
							"capacity C1 extends C2 { }"));
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_5() {
			assertQuickFix(IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
			//
			// Code to fix:
			//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"capacity C3 extends C1, C2, C3 { }"),
					//
					// Label and description:
					//
					"Remove the type 'C3'", "Remove the type 'C3'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"capacity C3 extends C1, C2 { }"));
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_6() {
			assertQuickFix(IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
			//
			// Code to fix:
			//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"capacity C3 extends C1, C3, C2 { }"),
					//
					// Label and description:
					//
					"Remove the type 'C3'", "Remove the type 'C3'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"capacity C3 extends C1, C2 { }"));
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_7() {
			assertQuickFix(IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
			//
			// Code to fix:
			//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"capacity C3 extends C3, C1, C2 { }"),
					//
					// Label and description:
					//
					"Remove the type 'C3'", "Remove the type 'C3'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { }", "capacity C2 { }",
							"capacity C3 extends C1, C2 { }"));
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
					multilineString("import foo.MockFinalAgent",
							"agent A1 extends MockFinalAgent { }"),
					//
					// Label and description:
					//
					"Remove the type 'MockFinalAgent'",
					"Remove the type 'MockFinalAgent'",
					//
					// Expected fixed code:
					//
					multilineString("import foo.MockFinalAgent", "agent A1 { }"));
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
			assertQuickFix(IssueCodes.UNUSED_AGENT_CAPACITY,
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
					"Remove the capacity 'C2'", "Remove the capacity 'C2'",
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
			assertQuickFix(IssueCodes.UNUSED_AGENT_CAPACITY,
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
					"Remove the capacity 'C2'", "Remove the capacity 'C2'",
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
			assertQuickFix(IssueCodes.UNUSED_AGENT_CAPACITY,
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
					"Remove the capacity 'C2'", "Remove the capacity 'C2'",
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
					IssueCodes.UNUSED_AGENT_CAPACITY,
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
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
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
					IssueCodes.UNUSED_AGENT_CAPACITY,
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
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
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
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					multilineString("capacity Lifecycle { def killMe }",
							"event Initialize", "agent AgentB {",
							"	uses Lifecycle", "	on Initialize {", "	}", "}"),
					//
					// Label and description:
					//
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
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
			assertQuickFix(IssueCodes.UNUSED_AGENT_CAPACITY,
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
					"Remove the capacity 'C2'", "Remove the capacity 'C2'",
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
			assertQuickFix(IssueCodes.UNUSED_AGENT_CAPACITY,
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
					"Remove the capacity 'C2'", "Remove the capacity 'C2'",
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
			assertQuickFix(IssueCodes.UNUSED_AGENT_CAPACITY,
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
					"Remove the capacity 'C2'", "Remove the capacity 'C2'",
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
					IssueCodes.UNUSED_AGENT_CAPACITY,
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
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
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
					IssueCodes.UNUSED_AGENT_CAPACITY,
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
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
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
					IssueCodes.UNUSED_AGENT_CAPACITY,
					//
					// Code to fix:
					//
					multilineString("capacity Lifecycle { def killMe }",
							"event Initialize", "behavior B {",
							"	uses Lifecycle", "	on Initialize {", "	}", "}"),
					//
					// Label and description:
					//
					"Remove the capacity 'Lifecycle'",
					"Remove the capacity 'Lifecycle'",
					//
					// Expected fixed code:
					//
					multilineString("capacity Lifecycle { def killMe }",
							"event Initialize", "behavior B {",
							"	on Initialize {", "	}", "}"));
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
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
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
			//
			// Code to fix:
			//
					multilineString("capacity C1 { def myfct }", "agent A1 {",
							"	uses C1, C1", "	def testfct { myfct }", "}"),
					//
					// Label and description:
					//
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
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
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
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
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
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
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
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
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
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
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
			//
			// Code to fix:
			//
					multilineString("capacity C1 { def myfct }",
							"behavior B1 {", "	uses C1, C1",
							"	def testfct { myfct }", "}"),
					//
					// Label and description:
					//
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
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
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
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
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
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
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
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
			assertQuickFix(IssueCodes.REDUNDANT_CAPACITY_USE,
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
					"Remove the capacity 'C1'", "Remove the capacity 'C1'",
					//
					// Expected fixed code:
					//
					multilineString("capacity C1 { def myfct }",
							"capacity C2 { def iddle }", "behavior B1 {",
							"	uses C1, C2", "	def testfct { myfct; iddle }",
							"	uses C2, C1", "}"));
		}

	}

}
