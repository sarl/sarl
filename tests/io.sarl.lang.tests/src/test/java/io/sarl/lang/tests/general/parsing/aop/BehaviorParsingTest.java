/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
package io.sarl.lang.tests.general.parsing.aop;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.google.common.base.Strings;
import org.eclipse.xtext.common.types.JvmTypeConstraint;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	BehaviorParsingTest.TopElementTest.class,
	BehaviorParsingTest.ActionTest.class,
	BehaviorParsingTest.FieldTest.class,
	BehaviorParsingTest.ConstructorTest.class,
	BehaviorParsingTest.CapacityUsesTest.class,
	BehaviorParsingTest.GenericTest.class,
})
@SuppressWarnings("all")
public class BehaviorParsingTest {

	public static class TopElementTest extends AbstractSarlTest {

		@Test
		public void invalidExtend_0() throws Exception {
			SarlScript mas = file(multilineString(
				"capacity C1 {",
				"}",
				"behavior B1 extends C1 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED,
				"Invalid supertype. Expecting a class");
		}

		@Test
		public void invalidExtend_1() throws Exception {
			SarlScript mas = file(multilineString(
				"agent A1 {",
				"}",
				"behavior B3 extends A1 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				IssueCodes.INVALID_EXTENDED_TYPE,
				"Supertype must be of type 'io.sarl.lang.core.Behavior'");
		}

		@Test
		public void invalidExtend_2() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 extends B1 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
				"The inheritance hierarchy of 'B1' is inconsistent");
		}

		@Test
		public void invalidExtend_3() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 extends B2 {",
				"}",
				"behavior B2 extends B1 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				IssueCodes.INVALID_EXTENDED_TYPE,
				"Supertype must be of type 'io.sarl.lang.core.Behavior'");
		}

		@Test
		public void invalidExtend_4() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 extends B2 {",
				"}",
				"behavior B2 extends B1 {",
				"}",
				"behavior B3 extends B2 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				IssueCodes.INVALID_EXTENDED_TYPE,
				"Supertype must be of type 'io.sarl.lang.core.Behavior'");
		}

		@Test
		public void invalidExtend_5() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B3 extends B2 {",
				"}",
				"behavior B2 extends B1 {",
				"}",
				"behavior B1 extends B2 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				IssueCodes.INVALID_EXTENDED_TYPE,
				"Supertype must be of type 'io.sarl.lang.core.Behavior'");
		}

		@Test
		public void duplicateTypeNames() throws Exception {
			SarlScript mas = file(multilineString(
				"package io.sarl.test",
				"behavior B1 {",
				"}",
				"behavior B2 {",
				"}",
				"behavior B1 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_TYPE_NAME,
				"Duplicate type B1");
		}

		@Test
		public void behaviormodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior B1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(JvmVisibility.PUBLIC, beh.getVisibility());
			assertEquals(0, beh.getMembers().size());
			assertFalse(beh.isAbstract());
			assertFalse(beh.isFinal());
			assertFalse(beh.isStrictFloatingPoint());
		}

		@Test
		public void behaviormodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(JvmVisibility.PUBLIC, beh.getVisibility());
			assertEquals(0, beh.getMembers().size());
			assertFalse(beh.isAbstract());
			assertFalse(beh.isFinal());
			assertFalse(beh.isStrictFloatingPoint());
		}

		@Test
		public void behaviormodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"private behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void behaviormodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"protected behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void behaviormodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"package behavior B1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(JvmVisibility.DEFAULT, beh.getVisibility());
			assertEquals(0, beh.getMembers().size());
			assertFalse(beh.isAbstract());
			assertFalse(beh.isFinal());
			assertFalse(beh.isStrictFloatingPoint());
		}

		@Test
		public void behaviormodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract behavior B1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(JvmVisibility.PUBLIC, beh.getVisibility());
			assertEquals(0, beh.getMembers().size());
			assertTrue(beh.isAbstract());
			assertFalse(beh.isFinal());
			assertFalse(beh.isStrictFloatingPoint());
		}

		@Test
		public void behaviormodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"static behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void behaviormodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void behaviormodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"final behavior B1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(JvmVisibility.PUBLIC, beh.getVisibility());
			assertEquals(0, beh.getMembers().size());
			assertFalse(beh.isAbstract());
			assertTrue(beh.isFinal());
			assertFalse(beh.isStrictFloatingPoint());
		}

		@Test
		public void behaviormodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void behaviormodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"native behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void behaviormodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"volatile behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void behaviormodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void behaviormodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"transient behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void behaviormodifier_abstract_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract final behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"The definition of B1 can either be abstract or final, not both");
		}

		@Test
		public void modifier_abstract_action_error_0() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	def name",
					"}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT);
		}

		@Test
		public void modifier_abstract_action_error_1() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	abstract def name",
					"}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT);
		}
		
		@Test
		public void modifier_abstract_action_warning() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract behavior B1 {",
					"	def name",
					"}"
					), true);
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT);
			//
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(JvmVisibility.PUBLIC, beh.getVisibility());
			assertEquals(1, beh.getMembers().size());
			assertTrue(beh.isAbstract());
			assertFalse(beh.isFinal());
			assertFalse(beh.isStrictFloatingPoint());
		}

		@Test
		public void modifier_abstract_action() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract behavior B1 {",
					"	abstract def name",
					"}"
					), true);
			validate(mas).assertNoIssues();
			//
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(JvmVisibility.PUBLIC, beh.getVisibility());
			assertEquals(1, beh.getMembers().size());
			assertTrue(beh.isAbstract());
			assertFalse(beh.isFinal());
			assertFalse(beh.isStrictFloatingPoint());
		}

		@Test
		public void behaviormodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public package behavior B1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"The definition of B1 can only set one of public / package / protected / private");
		}

	}

	public static class ActionTest extends AbstractSarlTest {

		@Test
		public void modifier_override_notRecommended() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract behavior B1 {",
					"	abstract def name",
					"}",
					"behavior B2 extends B1 {",
					"	def name { }",
					"}"), false);
			validate(mas).assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
		}

		@Test
		public void modifier_override_invalid() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"}",
					"behavior B2 extends B1 {",
					"	override name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.OBSOLETE_OVERRIDE,
					"The method name() of type B2 must override a superclass method");
		}

		@Test
		public void modifier_override_valid() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract behavior B1 {",
					"	abstract def name",
					"}",
					"behavior B2 extends B1 {",
					"	override name { }",
					"}"), false);
			validate(mas).assertNoIssues();
		}

		@Test
		public void multipleActionDefinition() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	def myaction(a : int, b : int) { }",
				"	def myaction(a : int) { }",
				"	def myaction(a : int) { }",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_METHOD,
				"Duplicate method myaction(int) in type B1");
		}

		@Test
		public void invalidActionName() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	def myaction {",
				"		System.out.println(\"ok\")",
				"	}",
				"	def $handle_myaction {",
				"		System.out.println(\"ko\")",
				"	}",
				"	def myaction2 {",
				"		System.out.println(\"ok\")",
				"	}",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME,
				"Invalid action name '$handle_myaction'.");
		}

		@Test
		public void incompatibleReturnType_0() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	def myaction(a : int) : int {",
				"		return 0",
				"	}",
				"}",
				"behavior B2 extends B1 {",
				"	def myaction(a : int) : float {",
				"		return 0f",
				"	}",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
		}

		@Test
		public void incompatibleReturnType_1() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	def myaction(a : int) {",
				"		// void",
				"	}",
				"}",
				"behavior B2 extends B1 {",
				"	def myaction(a : int) : int {",
				"		return 0",
				"	}",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
		}

		@Test
		public void incompatibleReturnType_2() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	def myaction(a : int) : int {",
				"		return 0",
				"	}",
				"}",
				"behavior B2 extends B1 {",
				"	def myaction(a : int) : void {",
				"		// void",
				"	}",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
		}

		@Test
		public void incompatibleReturnType_3() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	def myaction(a : int) : int {",
				"		return 0",
				"	}",
				"}",
				"behavior B2 extends B1 {",
				"	def myaction(a : int) {",
				"		// int return type is inferred",
				"	}",
				"}"
			));
			validate(mas).assertError(
				XbasePackage.eINSTANCE.getXBlockExpression(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
				"Type mismatch: cannot convert from null to int");
		}

		@Test
		public void compatibleReturnType_0() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	def myaction(a : int) : Number {",
				"		return 0.0",
				"	}",
				"}",
				"behavior B2 extends B1 {",
				"	def myaction(a : int) : Double {",
				"		return 0.0",
				"	}",
				"}"
			), true);
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlBehavior behavior1 = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", behavior1.getName());
			assertNull(behavior1.getExtends());
			assertEquals(1, behavior1.getMembers().size());
			//
			SarlAction action1 = (SarlAction) behavior1.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTrue(action1.getFiredEvents().isEmpty());
			assertTypeReferenceIdentifier(action1.getReturnType(), "java.lang.Number");
			assertParameterNames(action1.getParameters(), "a");
			assertParameterTypes(action1.getParameters(), "int");
			assertParameterDefaultValues(action1.getParameters(), (Object) null);
			//
			SarlBehavior behavior2= (SarlBehavior) mas.getXtendTypes().get(1);
			assertEquals("B2", behavior2.getName());
			assertTypeReferenceIdentifier(behavior2.getExtends(), "B1");
			assertEquals(1, behavior2.getMembers().size());
			//
			SarlAction action2 = (SarlAction) behavior2.getMembers().get(0);
			assertEquals("myaction", action2.getName());
			assertTrue(action2.getFiredEvents().isEmpty());
			assertTypeReferenceIdentifier(action2.getReturnType(), "java.lang.Double");
			assertParameterNames(action2.getParameters(), "a");
			assertParameterTypes(action2.getParameters(), "int");
			assertParameterDefaultValues(action2.getParameters(), (Object) null);
		}

		@Test
		public void compatibleReturnType_1() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	def myaction(a : int) : float {",
				"		return 0f",
				"	}",
				"}",
				"behavior B2 extends B1 {",
				"	def myaction(a : int) : float {",
				"		return 0f",
				"	}",
				"}"
			), true);
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlBehavior behavior1 = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", behavior1.getName());
			assertNull(behavior1.getExtends());
			assertEquals(1, behavior1.getMembers().size());
			//
			SarlAction action1 = (SarlAction) behavior1.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTrue(action1.getFiredEvents().isEmpty());
			assertTypeReferenceIdentifier(action1.getReturnType(), "float");
			assertParameterNames(action1.getParameters(), "a");
			assertParameterTypes(action1.getParameters(), "int");
			assertParameterDefaultValues(action1.getParameters(), (Object) null);
			//
			SarlBehavior behavior2= (SarlBehavior) mas.getXtendTypes().get(1);
			assertEquals("B2", behavior2.getName());
			assertTypeReferenceIdentifier(behavior2.getExtends(), "B1");
			assertEquals(1, behavior2.getMembers().size());
			//
			SarlAction action2 = (SarlAction) behavior2.getMembers().get(0);
			assertEquals("myaction", action2.getName());
			assertTrue(action2.getFiredEvents().isEmpty());
			assertTypeReferenceIdentifier(action2.getReturnType(), "float");
			assertParameterNames(action2.getParameters(), "a");
			assertParameterTypes(action2.getParameters(), "int");
			assertParameterDefaultValues(action2.getParameters(), (Object) null);
		}

		@Test
		public void multipleParameterNames() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	def myaction(a : int, b : int, c : int, b : int) {",
				"	}",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlFormalParameter(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				"Duplicate local variable b");
		}

		@Test
		public void modifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	public def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		public void modifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	private def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PRIVATE, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		public void modifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	protected def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PROTECTED, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		public void modifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	package def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.DEFAULT, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		public void modifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract behavior B1 {",
					"	abstract def name",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertTrue(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		public void modifier_no_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract behavior B1 {",
					"	def name",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertTrue(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		public void modifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	static def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	dispatch def name { }",
					"}"), false);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertTrue(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		public void modifier_final_var() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	final def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertTrue(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	strictfp def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	native def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	volatile def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	synchronized def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertTrue(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		public void modifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	transient def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_protected_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	protected private def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"public / package / protected / private");
		}

		@Test
		public void modifier_dispatch_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	dispatch final def name(a : Integer) { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction act1 = (SarlAction) beh.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertTrue(act1.isDispatch());
			assertTrue(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

	}

	public static class FieldTest extends AbstractSarlTest {

		@Test
		public void multipleVariableDefinition() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	var myfield : int",
				"	var myfield1 : String",
				"	var myfield : double",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
				"Duplicate field myfield");
		}

		@Test
		public void multipleValueDefinition() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	val myfield : int = 4",
				"	val myfield1 : String = \"\"",
				"	val myfield : double = 5",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
				"Duplicate field myfield");
		}

		@Test
		public void invalidAttributeName_0() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	var myfield1 = 4.5",
				"	var $FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
				"	var myfield2 = true",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid attribute name '$FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'");
		}

		@Test
		public void invalidAttributeName_1() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	val myfield1 = 4.5",
				"	val $FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
				"	val myfield2 = true",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid attribute name '$FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'");
		}

		@Test
		public void invalidAttributeName_2() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	val myfield1 = 4.5",
				"	val this = \"String\"",
				"	val myfield2 = true",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_IDENTIFIER,
				"'this' is not a valid identifier");
		}

		@Test
		public void invalidAttributeName_3() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	val myfield1 = 4.5",
				"	val const = \"String\"",
				"	val myfield2 = true",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_IDENTIFIER,
				"'const' is not a valid identifier.");
		}

		@Test
		public void invalidAttributeName_4() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	val myfield1 = 4.5",
				"	val self = \"String\"",
				"	val myfield2 = true",
				"}"
			));
			validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISCOURAGED,
				"'self' is a discouraged name");
		}

		@Test
		public void missedFinalFieldInitialization() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	val field1 : int = 5",
				"	val field2 : String",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtend.core.validation.IssueCodes.FIELD_NOT_INITIALIZED,
				"The blank final field field2 may not have been initialized");
		}

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	val field1 : int = 5",
				"	val field2 : String = \"\"",
				"}"
			), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", behavior.getName());
			assertNull(behavior.getExtends());
			assertEquals(2, behavior.getMembers().size());
			//
			SarlField attr1 = (SarlField) behavior.getMembers().get(0);
			assertEquals("field1", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertXExpression(attr1.getInitialValue(), XNumberLiteral.class, "5");
			//
			SarlField attr2 = (SarlField) behavior.getMembers().get(1);
			assertEquals("field2", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.String");
			assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "");
		}

		@Test
		public void fieldNameShadowing() throws Exception {
			SarlScript mas = file(multilineString(
				"behavior B1 {",
				"	protected val field1 : int = 5",
				"	def myaction(a : int) { }",
				"}",
				"behavior B2 extends B1 {",
				"	val field1 : int = 5",
				"	def myaction(a : int) { }",
				"}"
			));
			validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				"The field 'field1' in 'B2' is hidding the inherited field 'B1.field1'.");
		}

		@Test
		public void modifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	public var field : int",
					"}"), false);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlField attr1 = (SarlField) beh.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.PUBLIC, attr1.getVisibility());
			assertFalse(attr1.isExtension());
			assertFalse(attr1.isFinal());
			assertFalse(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
		}

		@Test
		public void modifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	private var field : int",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlField attr1 = (SarlField) beh.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
			assertFalse(attr1.isExtension());
			assertFalse(attr1.isFinal());
			assertFalse(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
		}

		@Test
		public void modifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	protected var field : int",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlField attr1 = (SarlField) beh.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
			assertFalse(attr1.isExtension());
			assertFalse(attr1.isFinal());
			assertFalse(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
		}

		@Test
		public void modifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	package var field : int",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlField attr1 = (SarlField) beh.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.DEFAULT, attr1.getVisibility());
			assertFalse(attr1.isExtension());
			assertFalse(attr1.isFinal());
			assertFalse(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
		}

		@Test
		public void modifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	var field : int",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlField attr1 = (SarlField) beh.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
			assertFalse(attr1.isExtension());
			assertFalse(attr1.isFinal());
			assertFalse(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
		}

		@Test
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	abstract var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	static var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	dispatch var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_final_var() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	final var field : int = 5",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"var or val / final, not both");
		}

		@Test
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	strictfp var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	native var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	volatile var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	synchronized var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	transient var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_protected_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	protected private var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"public / package / protected / private");
		}

	}

	public static class ConstructorTest extends AbstractSarlTest {

		@Test
		public void validImplicitSuperConstructor() throws Exception {
			SarlScript mas = file(multilineString(
				"package io.sarl.test",
				"behavior B1 {",
				"}",
				"behavior B2 extends B1 {",
				"	new (a : int) {",
				"		super(null)",
				"	}",
				"}"
			), true);
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.test", mas.getPackage());
			//
			SarlBehavior behavior1 = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", behavior1.getName());
			assertNull(behavior1.getExtends());
			assertEquals(0, behavior1.getMembers().size());
			//
			SarlBehavior behavior2 = (SarlBehavior) mas.getXtendTypes().get(1);
			assertEquals("B2", behavior2.getName());
			assertTypeReferenceIdentifier(behavior2.getExtends(), "io.sarl.test.B1");
			assertEquals(1, behavior2.getMembers().size());
			//
			SarlConstructor constructor = (SarlConstructor) behavior2.getMembers().get(0);
			assertParameterNames(constructor.getParameters(), "a");
			assertParameterTypes(constructor.getParameters(), "int");
			assertParameterDefaultValues(constructor.getParameters(), (Object) null);
		}

		@Test
		public void missedImplicitSuperConstructor_1() throws Exception {
			SarlScript mas = file(multilineString(
				"package io.sarl.test",
				"behavior B1 {",
				"}",
				"behavior B2 extends B1 {",
				"	new (a : int) {",
				"	}",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlConstructor(),
				org.eclipse.xtend.core.validation.IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR,
				"Undefined default constructor in the super-type");
		}

		@Test
		public void missedImplicitSuperConstructor_2() throws Exception {
			SarlScript mas = file(multilineString(
				"package io.sarl.test",
				"behavior B1 {",
				"	new (a : int) {",
				"		super(null)",
				"	}",
				"}",
				"behavior B2 extends B1 {",
				"	new (a : int) {",
				"	}",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlConstructor(),
				org.eclipse.xtend.core.validation.IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR,
				"Undefined default constructor in the super-type");
		}

		@Test
		public void notMissedImplicitSuperConstructor() throws Exception {
			SarlScript mas = file(multilineString(
				"package io.sarl.test",
				"behavior B1 {",
				"	new (a : int) {",
				"		super(null)",
				"	}",
				"}",
				"behavior B2 extends B1 {",
				"}"
			));
			validate(mas).assertNoErrors();
		}


		@Test
		public void invalidArgumentTypeToSuperConstructor() throws Exception {
			SarlScript mas = file(multilineString(
				"package io.sarl.test",
				"behavior B1 {",
				"	new (a : int) {",
				"		super(null)",
				"	}",
				"}",
				"behavior B2 extends B1 {",
				"	new (a : int) {",
				"		super(\"\")",
				"	}",
				"}"
			));
			validate(mas).assertError(
				XbasePackage.eINSTANCE.getXStringLiteral(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
				"Type mismatch: cannot convert from String to int");
		}

		@Test
		public void modifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	public new { super(null) }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) beh.getMembers().get(0);
			assertEquals(JvmVisibility.PUBLIC, cons.getVisibility());
			assertFalse(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		public void modifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	private new { super(null) }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) beh.getMembers().get(0);
			assertEquals(JvmVisibility.PRIVATE, cons.getVisibility());
			assertFalse(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		public void modifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	protected new { super(null) }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) beh.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, cons.getVisibility());
			assertFalse(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		public void modifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	package new { super(null) }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) beh.getMembers().get(0);
			assertEquals(JvmVisibility.DEFAULT, cons.getVisibility());
			assertFalse(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		public void modifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	new { super(null) }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			//
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", beh.getName());
			assertNull(beh.getExtends());
			assertEquals(1, beh.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) beh.getMembers().get(0);
			assertEquals(JvmVisibility.PUBLIC, cons.getVisibility());
			assertFalse(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	abstract new { super(null) }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	static new { super(null) }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	dispatch new { super(null) }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	final new { super(null) }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	strictfp new { super(null) }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	native new { super(null) }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	volatile new { super(null) }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	synchronized new { super(null) }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	transient new { super(null) }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void modifier_protected_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	protected private new { super(null) }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"public / package / protected / private");
		}

	}

	public static class CapacityUsesTest extends AbstractSarlTest {

		@Test
		public void multipleCapacityUses_0() throws Exception {
			SarlScript mas = file(multilineString(
				"capacity C1 {}",
				"capacity C2 {}",
				"behavior B1 {",
				"	uses C1, C2, C1",
				"	def testFct { }",
				"}"
			));

			validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlCapacityUses(),
				IssueCodes.REDUNDANT_CAPACITY_USE,
				"Redundant use of the capacity 'C1'");
		}

		@Test
		public void multipleCapacityUses_1() throws Exception {
			SarlScript mas = file(multilineString(
				"capacity C1 {}",
				"capacity C2 {}",
				"behavior B1 {",
				"	uses C2",
				"	def testFct { }",
				"	uses C2, C1",
				"}"
			));

			validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlCapacityUses(),
				IssueCodes.REDUNDANT_CAPACITY_USE,
				"Redundant use of the capacity 'C2'");
		}

	}

	public static class GenericTest extends AbstractSarlTest {

		@Test
		public void functionGeneric_X_sarlNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	def setX(param : X) : void with X { var xxx : X }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertNotNull(beh);
			//
			assertEquals("B1", beh.getName());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction action = (SarlAction) beh.getMembers().get(0);
			assertEquals("setX", action.getName());
			assertEquals(1, action.getTypeParameters().size());
			//
			JvmTypeParameter parameter = action.getTypeParameters().get(0);
			assertEquals("X", parameter.getName());
			assertNullOrEmpty(parameter.getConstraints());
		}

		@Test
		public void functionGeneric_X_javaNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	def <X> setX(param : X) : void { var xxx : X }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertNotNull(beh);
			//
			assertEquals("B1", beh.getName());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction action = (SarlAction) beh.getMembers().get(0);
			assertEquals("setX", action.getName());
			assertEquals(1, action.getTypeParameters().size());
			//
			JvmTypeParameter parameter = action.getTypeParameters().get(0);
			assertEquals("X", parameter.getName());
			assertNullOrEmpty(parameter.getConstraints());
		}

		@Test
		public void functionGeneric_XextendsNumber_sarlNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	def setX(param : X) : void with X extends Number { var xxx : X }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertNotNull(beh);
			//
			assertEquals("B1", beh.getName());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction action = (SarlAction) beh.getMembers().get(0);
			assertEquals("setX", action.getName());
			assertEquals(1, action.getTypeParameters().size());
			//
			JvmTypeParameter parameter = action.getTypeParameters().get(0);
			assertEquals("X", parameter.getName());
			assertEquals(1, parameter.getConstraints().size());
			//
			JvmTypeConstraint constraint = parameter.getConstraints().get(0);
			assertEquals("java.lang.Number", constraint.getTypeReference().getIdentifier());
			assertTrue(constraint.getIdentifier().startsWith("extends"));
		}

		@Test
		public void functionGeneric_XextendsNumber_javaNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	def <X extends Number> setX(param : X) : void { var xxx : X }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertNotNull(beh);
			//
			assertEquals("B1", beh.getName());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction action = (SarlAction) beh.getMembers().get(0);
			assertEquals("setX", action.getName());
			assertEquals(1, action.getTypeParameters().size());
			//
			JvmTypeParameter parameter = action.getTypeParameters().get(0);
			assertEquals("X", parameter.getName());
			assertEquals(1, parameter.getConstraints().size());
			//
			JvmTypeConstraint constraint = parameter.getConstraints().get(0);
			assertEquals("java.lang.Number", constraint.getTypeReference().getIdentifier());
			assertTrue(constraint.getIdentifier().startsWith("extends"));
		}

		@Test
		public void functionGeneric_XY_sarlNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	def setX(param : X) : void with X, Y { var xxx : X; var yyy : Y }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertNotNull(beh);
			//
			assertEquals("B1", beh.getName());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction action = (SarlAction) beh.getMembers().get(0);
			assertEquals("setX", action.getName());
			assertEquals(2, action.getTypeParameters().size());
			//
			JvmTypeParameter parameter1 = action.getTypeParameters().get(0);
			assertEquals("X", parameter1.getName());
			assertNullOrEmpty(parameter1.getConstraints());
			//
			JvmTypeParameter parameter2 = action.getTypeParameters().get(1);
			assertEquals("Y", parameter2.getName());
			assertNullOrEmpty(parameter2.getConstraints());
		}

		@Test
		public void functionGeneric_XY_javaNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	def <X, Y> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertNotNull(beh);
			//
			assertEquals("B1", beh.getName());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction action = (SarlAction) beh.getMembers().get(0);
			assertEquals("setX", action.getName());
			assertEquals(2, action.getTypeParameters().size());
			//
			JvmTypeParameter parameter1 = action.getTypeParameters().get(0);
			assertEquals("X", parameter1.getName());
			assertNullOrEmpty(parameter1.getConstraints());
			//
			JvmTypeParameter parameter2 = action.getTypeParameters().get(1);
			assertEquals("Y", parameter2.getName());
			assertNullOrEmpty(parameter2.getConstraints());
		}

		@Test
		public void functionGeneric_XYextendsX_sarlNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	def setX(param : X) : void with X, Y extends X { var xxx : X; var yyy : Y }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertNotNull(beh);
			//
			assertEquals("B1", beh.getName());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction action = (SarlAction) beh.getMembers().get(0);
			assertEquals("setX", action.getName());
			assertEquals(2, action.getTypeParameters().size());
			//
			JvmTypeParameter parameter1 = action.getTypeParameters().get(0);
			assertEquals("X", parameter1.getName());
			assertNullOrEmpty(parameter1.getConstraints());
			//
			JvmTypeParameter parameter2 = action.getTypeParameters().get(1);
			assertEquals("Y", parameter2.getName());
			assertEquals(1, parameter2.getConstraints().size());
			//
			JvmTypeConstraint constraint = parameter2.getConstraints().get(0);
			assertEquals("X", constraint.getTypeReference().getIdentifier());
			assertTrue(constraint.getIdentifier().startsWith("extends"));
		}

		@Test
		public void functionGeneric_XYextendsX_javaNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"behavior B1 {",
					"	def <X, Y extends X> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlBehavior beh = (SarlBehavior) mas.getXtendTypes().get(0);
			assertNotNull(beh);
			//
			assertEquals("B1", beh.getName());
			assertEquals(1, beh.getMembers().size());
			//
			SarlAction action = (SarlAction) beh.getMembers().get(0);
			assertEquals("setX", action.getName());
			assertEquals(2, action.getTypeParameters().size());
			//
			JvmTypeParameter parameter1 = action.getTypeParameters().get(0);
			assertEquals("X", parameter1.getName());
			assertNullOrEmpty(parameter1.getConstraints());
			//
			JvmTypeParameter parameter2 = action.getTypeParameters().get(1);
			assertEquals("Y", parameter2.getName());
			assertEquals(1, parameter2.getConstraints().size());
			//
			JvmTypeConstraint constraint = parameter2.getConstraints().get(0);
			assertEquals("X", constraint.getTypeReference().getIdentifier());
			assertTrue(constraint.getIdentifier().startsWith("extends"));
		}

	}

}
