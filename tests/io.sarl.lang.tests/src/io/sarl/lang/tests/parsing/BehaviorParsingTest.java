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
package io.sarl.lang.tests.parsing;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.google.common.base.Strings;
import com.google.inject.Inject;

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
	BehaviorParsingTest.AttributeTest.class,
	BehaviorParsingTest.ConstructorTest.class,
	BehaviorParsingTest.CapacityUsesTest.class,
})
@SuppressWarnings("all")
public class BehaviorParsingTest {

	public static class TopElementTest extends AbstractSarlTest {

		@Test
		public void invalidExtend_0() throws Exception {
			XtendFile mas = file(multilineString(
				"capacity C1 {",
				"}",
				"behavior B1 extends C1 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED,
				36, 2,
				"Invalid supertype. Expecting a class");
		}

		@Test
		public void invalidExtend_1() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
				"behavior B1 extends B2 {",
				"}",
				"behavior B2 extends B1 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
				"The inheritance hierarchy of 'B1' is inconsistent");
		}

		@Test
		public void invalidExtend_4() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 extends B2 {",
				"}",
				"behavior B2 extends B1 {",
				"}",
				"behavior B3 extends B2 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
				"The inheritance hierarchy of 'B1' is inconsistent");
		}

		@Test
		public void invalidExtend_5() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B3 extends B2 {",
				"}",
				"behavior B2 extends B1 {",
				"}",
				"behavior B1 extends B2 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
				"The inheritance hierarchy of 'B3' is inconsistent");
		}

		@Test
		public void duplicateTypeNames() throws Exception {
			XtendFile mas = file(multilineString(
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
				62, 2,
				"The type B1 is already defined");
		}

	}

	public static class ActionTest extends AbstractSarlTest {

		@Test
		public void multipleActionDefinition() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	def myaction(a : int, b : int) { }",
				"	def myaction(a : int) { }",
				"	def myaction(a : int) { }",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_METHOD,
				82, 8,
				"Duplicate method myaction(int) in type B1");
		}

		@Test
		public void invalidActionName() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	def myaction {",
				"		System.out.println(\"ok\")",
				"	}",
				"	def _handle_myaction {",
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
				"Invalid action name '_handle_myaction'.");
		}

		@Test
		public void incompatibleReturnType_0() throws Exception {
			XtendFile mas = file(multilineString(
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
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
				111, 5,
				"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_1() throws Exception {
			XtendFile mas = file(multilineString(
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
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
				104, 3,
				"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_2() throws Exception {
			XtendFile mas = file(multilineString(
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
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
				"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_3() throws Exception {
			XtendFile mas = file(multilineString(
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
				109, 37,
				"Type mismatch: cannot convert from null to int");
		}

		@Test
		public void compatibleReturnType_0() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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

	}

	public static class AttributeTest extends AbstractSarlTest {

		@Test
		public void multipleVariableDefinition() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	var myfield : int",
				"	var myfield1 : String",
				"	var myfield : double",
				"}"
			));
			validate(mas).assertError(
				XtendPackage.eINSTANCE.getXtendField(),
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
				61, 7,
				"Duplicate field myfield");
		}

		@Test
		public void multipleValueDefinition() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	val myfield : int = 4",
				"	val myfield1 : String = \"\"",
				"	val myfield : double = 5",
				"}"
			));
			validate(mas).assertError(
				XtendPackage.eINSTANCE.getXtendField(),
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
				70, 7,
				"Duplicate field myfield");
		}

		@Test
		public void invalidAttributeName_0() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	var myfield1 = 4.5",
				"	var ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
				"	var myfield2 = true",
				"}"
			));
			validate(mas).assertError(
				XtendPackage.eINSTANCE.getXtendField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
				39, 41,
				"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void invalidAttributeName_1() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	val myfield1 = 4.5",
				"	val ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
				"	val myfield2 = true",
				"}"
			));
			validate(mas).assertError(
				XtendPackage.eINSTANCE.getXtendField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
				39, 41,
				"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void invalidAttributeName_2() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	val myfield1 = 4.5",
				"	val this = \"String\"",
				"	val myfield2 = true",
				"}"
			));
			validate(mas).assertError(
				XtendPackage.eINSTANCE.getXtendField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_IDENTIFIER,
				39, 4,
				"'this' is not a valid identifier");
		}

		@Test
		public void invalidAttributeName_3() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	val myfield1 = 4.5",
				"	val const = \"String\"",
				"	val myfield2 = true",
				"}"
			));
			validate(mas).assertError(
				XtendPackage.eINSTANCE.getXtendField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_IDENTIFIER,
				39, 5,
				"'const' is not a valid identifier.");
		}

		@Test
		public void invalidAttributeName_4() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	val myfield1 = 4.5",
				"	val self = \"String\"",
				"	val myfield2 = true",
				"}"
			));
			validate(mas).assertWarning(
				XtendPackage.eINSTANCE.getXtendField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISCOURAGED,
				39, 4,
				"'self' is a discouraged name");
		}

		@Test
		public void missedFinalFieldInitialization() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	val field1 : int = 5",
				"	val field2 : String",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.FIELD_NOT_INITIALIZED,
				0, 58,
				"The blank final field field2 may not have been initialized");
		}

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendField attr1 = (XtendField) behavior.getMembers().get(0);
			assertEquals("field1", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertXExpression(attr1.getInitialValue(), XNumberLiteral.class, "5");
			//
			XtendField attr2 = (XtendField) behavior.getMembers().get(1);
			assertEquals("field2", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.String");
			assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "");
		}

		@Test
		public void fieldNameShadowing() throws Exception {
			XtendFile mas = file(multilineString(
				"behavior B1 {",
				"	val field1 : int = 5",
				"	def myaction(a : int) { }",
				"}",
				"behavior B2 extends B1 {",
				"	val field1 : int = 5",
				"	def myaction(a : int) { }",
				"}"
			));
			validate(mas).assertWarning(
				XtendPackage.eINSTANCE.getXtendField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				"The field 'field1' in 'B2' is hidding the inherited field 'B1.field1'.");
		}

		@Test
		public void variableModifier_public() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"public var name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertNoErrors();
		}

		@Test
		public void variableModifier_protected() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"protected var name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) behavior.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

		@Test
		public void variableModifier_package() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"package var name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertNoErrors();
		}

		@Test
		public void variableModifier_private() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"private var name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) behavior.getMembers().get(0);
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
		}

		@Test
		public void variableModifier_default() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"var name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) behavior.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

		@Test
		public void valueModifier_public() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"public val name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertNoErrors();
		}

		@Test
		public void valueModifier_protected() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"protected val name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) behavior.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

		@Test
		public void valueModifier_package() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"package val name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertNoErrors();
		}

		@Test
		public void valueModifier_private() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"private val name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) behavior.getMembers().get(0);
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
		}

		@Test
		public void valueModifier_default() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"val name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) behavior.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

	}

	public static class ConstructorTest extends AbstractSarlTest {

		@Test
		public void validImplicitSuperConstructor() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendConstructor constructor = (XtendConstructor) behavior2.getMembers().get(0);
			assertParameterNames(constructor.getParameters(), "a");
			assertParameterTypes(constructor.getParameters(), "int");
			assertParameterDefaultValues(constructor.getParameters(), (Object) null);
		}

		@Test
		public void missedImplicitSuperConstructor_1() throws Exception {
			XtendFile mas = file(multilineString(
				"package io.sarl.test",
				"behavior B1 {",
				"}",
				"behavior B2 extends B1 {",
				"	new (a : int) {",
				"	}",
				"}"
			));
			validate(mas).assertError(
				XtendPackage.eINSTANCE.getXtendConstructor(),
				org.eclipse.xtend.core.validation.IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR,
				63, 18,
				"Undefined default constructor in the super-type");
		}

		@Test
		public void missedImplicitSuperConstructor_2() throws Exception {
			XtendFile mas = file(multilineString(
				"package io.sarl.test",
				"behavior B1 {",
				"	new (a : int) {",
				"		super(null)",
				"	}",
				"}",
				"behavior B2 extends B1 {",
				"}"
			));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_CONSTRUCTOR,
				80, 2,
				"Undefined default constructor in the super-type");
		}

		@Test
		public void missedImplicitSuperConstructor_3() throws Exception {
			XtendFile mas = file(multilineString(
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
				XtendPackage.eINSTANCE.getXtendConstructor(),
				org.eclipse.xtend.core.validation.IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR,
				97, 18,
				"Undefined default constructor in the super-type");
		}

		@Test
		public void invalidArgumentTypeToSuperConstructor() throws Exception {
			XtendFile mas = file(multilineString(
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

	}

	public static class CapacityUsesTest extends AbstractSarlTest {

		@Test
		public void multipleCapacityUses_0() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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

}
