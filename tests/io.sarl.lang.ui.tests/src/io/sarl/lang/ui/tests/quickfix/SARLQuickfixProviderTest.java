/*
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import org.eclipse.xtext.ui.editor.quickfix.IssueResolution;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import static org.junit.Assert.*;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	SARLQuickfixProviderTest.PackageDeclaration.class,
	SARLQuickfixProviderTest.Duplicates.class,
	SARLQuickfixProviderTest.MemberNames.class,
	SARLQuickfixProviderTest.Overrides.class,
	SARLQuickfixProviderTest.BehaviorUnits.class,
	SARLQuickfixProviderTest.FiredEvents.class,
	SARLQuickfixProviderTest.SuperTypes.class,
	SARLQuickfixProviderTest.Capacities.class,
	SARLQuickfixProviderTest.ReturnTypes.class,
	//	SARLQuickfixProviderTest.MissingMethodImplementation.class,
})
@SuppressWarnings("all")
public class SARLQuickfixProviderTest {

	public static class PackageDeclaration extends AbstractSARLQuickfixTest {

		@Test
		public void fixPackageName() {
			assertQuickFix(
					org.eclipse.xtend.core.validation.IssueCodes.WRONG_PACKAGE,
					// Code to fix:
					"package foo.^package",
					//
					// Label and description:
					//
					"Change package declaration to 'io.sarl.tests.quickfix.org_eclipse_xtend_core_validation_issuecodes_wrong_package'",
					//
					// Expected fixed code:
					//
					"package io.sarl.tests.quickfix.org_eclipse_xtend_core_validation_issuecodes_wrong_package");
		}

	}

	public static class Duplicates extends AbstractSARLQuickfixTest {

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

	public static class MemberNames extends AbstractSARLQuickfixTest {

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
			asserts.assertNoQuickFix();
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
							"	var ___FORMAL_PARAMETER_DEFAULT_VALUE_myattr1 : boolean",
							"	var myattr2 : float",
							"}"));
			asserts.assertQuickFix(
					//
					// Label and description:
					//
					"Rename to myattr1",
					//
					// Expected fixed code:
					//
					multilineString(
							"agent A1 {",
							"	var myattr1 : boolean",
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
			asserts.assertNoQuickFix();
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
					"Rename to handleMethod1",
					//
					// Expected fixed code:
					//
					multilineString(
							"agent A1 {",
							"	def handleMethod1(a : int) : boolean {",
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
					"Rename to method1",
					//
					// Expected fixed code:
					//
					multilineString(
							"agent A1 {",
							"	def method1(a : int) : boolean {",
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
					"Rename to guardMethod1",
					//
					// Expected fixed code:
					//
					multilineString(
							"agent A1 {",
							"	def guardMethod1(a : int) : boolean {",
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
					"Rename to method1",
					//
					// Expected fixed code:
					//
					multilineString(
							"agent A1 {",
							"	def method1(a : int) : boolean {",
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
					"Rename to handlebodyMethod1",
					//
					// Expected fixed code:
					//
					multilineString(
							"agent A1 {",
							"	def handlebodyMethod1(a : int) : boolean {",
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
					"Rename to method1",
					//
					// Expected fixed code:
					//
					multilineString(
							"agent A1 {",
							"	def method1(a : int) : boolean {",
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

	public static class Overrides extends AbstractSARLQuickfixTest {

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

	public static class BehaviorUnits extends AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixDiscouragedBooleanExpression() {
			assertQuickFix(io.sarl.lang.validation.IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION,
					//
					// Code to fix:
					//
					multilineString(
							"event E1",
							"agent A1 {",
							"	on E1 [true] { [ false ] }",
							"}"),
							//
							// Label and description:
							//
							"Remove",
							//
							// Expected fixed code:
							//
							multilineString(
									"event E1",
									"agent A1 {",
									"	on E1 { [ false ] }",
									"}"));
		}

		/**
		 */
		@Test
		public void fixUnreachableBehaviorUnit() {
			assertQuickFix(
					io.sarl.lang.validation.IssueCodes.UNREACHABLE_BEHAVIOR_UNIT,
					//
					// Code to fix:
					//
					multilineString(
							"event E1",
							"agent A1 {",
							"	on E1 [false] { [ false ] }",
							"}"),
							//
							// Label and description:
							//
							"Remove",
							//
							// Expected fixed code:
							//
							multilineString(
									"event E1",
									"agent A1 {",
									"}"));
		}

	}

	public static class FiredEvents extends AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixInvalidFiringEventType_0() {
			assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_FIRING_EVENT_TYPE,
					//
					// Code to fix:
					//
					multilineString("event E1", "event E2", "capacity C1 { }",
							"agent A1 {", "	def myfct fires C1, E1, E2 { }",
							"}"),
							//
							// Label and description:
							//
							"Remove",
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
			assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_FIRING_EVENT_TYPE,
					//
					// Code to fix:
					//
					multilineString("event E1", "event E2", "capacity C1 { }",
							"agent A1 {", "	def myfct fires E1, C1, E2 { }",
							"}"),
							//
							// Label and description:
							//
							"Remove",
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
			assertQuickFix(io.sarl.lang.validation.IssueCodes.INVALID_FIRING_EVENT_TYPE,
					//
					// Code to fix:
					//
					multilineString("event E1", "event E2", "capacity C1 { }",
							"agent A1 {", "	def myfct fires E1, E2, C1 { }",
							"}"),
							//
							// Label and description:
							//
							"Remove",
							//
							// Expected fixed code:
							//
							multilineString("event E1", "event E2", "capacity C1 { }",
									"agent A1 {", "	def myfct fires E1, E2 { }", "}"));
		}

	}

	public static class SuperTypes extends AbstractSARLQuickfixTest {

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

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_1() {
			assertQuickFix(org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					//
					// Code to fix:
					//
					multilineString(
							"capacity C1 extends C2 { }",
							"capacity C2 extends C1 { }"),
					//
					// Label and description:
					//
					"Remove C2",
					//
					// Expected fixed code:
					//
					multilineString(
							"capacity C1 { }",
							"capacity C2 extends C1 { }"));
		}

		/**
		 */
		@Test
		public void fixInconsistentTypeHierarchy_2() {
			assertQuickFix(org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					//
					// Code to fix:
					//
					multilineString(
							"capacity C1 extends C2 { }",
							"capacity C2 extends C3 { }",
							"capacity C3 extends C1 { }"),
					//
					// Label and description:
					//
					"Remove C2",
					//
					// Expected fixed code:
					//
					multilineString(
							"capacity C1 { }",
							"capacity C2 extends C3 { }",
							"capacity C3 extends C1 { }"));
		}

	}

	public static class Capacities extends AbstractSARLQuickfixTest {

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
			asserts.assertNoQuickFix();
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

	public static class ReturnTypes extends AbstractSARLQuickfixTest {

		/**
		 */
		@Test
		public void fixIncompatibleReturnType() {
			assertQuickFix(
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					//
					// Code to fix:
					//
					multilineString(
							"class A1 {",
							"	def fct1 : int {",
							"		123",
							"	}",
							"}",
							"class A2 extends A1 {",
							"	def fct1 : boolean {",
							"		true",
							"	}",
							"}"),
							//
							// Label and description:
							//
							"Replace by int",
							//
							// Expected fixed code:
							//
							multilineString(
									"class A1 {",
									"	def fct1 : int {",
									"		123",
									"	}",
									"}",
									"class A2 extends A1 {",
									"	def fct1 : int {",
									"		true",
									"	}",
									"}"));
		}

		/**
		 */
		@Test
		public void fixReturnTypeRecommended() {
			assertQuickFix(
					io.sarl.lang.validation.IssueCodes.RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED,
					//
					// Code to fix:
					//
					multilineString(
							"class A1 {",
							"	def fct1 : int {",
							"		123",
							"	}",
							"}",
							"class A2 extends A1 {",
							"	def fct1 {",
							"		456",
							"	}",
							"}"),
							//
							// Label and description:
							//
							"Add the return type int",
							//
							// Expected fixed code:
							//
							multilineString(
									"class A1 {",
									"	def fct1 : int {",
									"		123",
									"	}",
									"}",
									"class A2 extends A1 {",
									"	def fct1 : int {",
									"		456",
									"	}",
									"}"));
		}

	}

}
