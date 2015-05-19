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
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.google.common.base.Strings;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	EventParsingTest.TopElementTest.class,
	EventParsingTest.SarlFieldTest.class,
	EventParsingTest.ConstructorTest.class,
	EventParsingTest.AttributeTest.class,
})
@SuppressWarnings("all")
public class EventParsingTest extends AbstractSarlTest {

	public static class TopElementTest extends AbstractSarlTest {

		@Test
		public void invalidExtend_0() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 {",
					"}",
					"event E1 extends C1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED,
					33, 2,
					"Invalid supertype. Expecting a class.");
		}

		@Test
		public void invalidExtend_1() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"}",
					"event E1 extends A1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					30, 2,
					"Supertype must be of type 'io.sarl.lang.core.Event'.");
		}

		@Test
		public void invalidExtend_2() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"}",
					"agent A1 extends E1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					30, 2,
					"Supertype must be of type 'io.sarl.lang.core.Agent'.");
		}

		@Test
		public void invalidExtend_3() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"}",
					"behavior B1 extends E1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					33, 2,
					"Supertype must be of type 'io.sarl.lang.core.Behavior'.");
		}

		@Test
		public void invalidExtend_4() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"}",
					"skill S1 extends E1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					30, 2,
					"Supertype must be of type 'io.sarl.lang.core.Skill'.");
		}

	}

	public static class SarlFieldTest extends AbstractSarlTest {

		@Test
		public void missedFinalFieldInitialization() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"val field1 : int = 5",
					"val field2 : String",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.FIELD_NOT_INITIALIZED,
					"The blank final field field2 may not have been initialized");
		}

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"val field1 : int = 5",
					"val field2 : String = \"\"",
					"}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(2, event.getMembers().size());
			//
			SarlField attr1 = (SarlField) event.getMembers().get(0);
			assertEquals("field1", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertXExpression(attr1.getInitialValue(), XNumberLiteral.class, "5");
			//
			SarlField attr2 = (SarlField) event.getMembers().get(1);
			assertEquals("field2", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.String");
			assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "");
		}

		@Test
		public void invalidSarlFieldName_0() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"var myfield1 = 4.5",
					"var ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"var myfield2 = true",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
					34, 41,
					"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void invalidSarlFieldName_1() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"val myfield1 = 4.5",
					"val ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"val myfield2 = true",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
					34, 41,
					"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void invalidSarlFieldName_2() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"var myfield1 = 4.5",
					"var const = \"String\"",
					"var myfield2 = true",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_IDENTIFIER,
					34, 5,
					"'const' is not a valid identifier.");
		}

		@Test
		public void invalidSarlFieldName_3() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"var myfield1 = 4.5",
					"var this = \"String\"",
					"var myfield2 = true",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_IDENTIFIER,
					34, 4,
					"'this' is not a valid identifier");
		}

		@Test
		public void discouragedSarlFieldName_0() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"val myfield1 = 4.5",
					"val self = \"String\"",
					"val myfield2 = true",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISCOURAGED,
					34, 4,
					"'self' is a discouraged name");
		}

		@Test
		public void multipleVariableDefinition() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"var myfield : int",
					"var myfield1 : String",
					"var myfield : double",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					55, 7,
					"Duplicate field myfield");
		}

		@Test
		public void multipleValueDefinition() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"val myfield : int = 4",
					"val myfield1 : String = \"\"",
					"val myfield : double = 5",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					64, 7,
					"Duplicate field myfield");
		}

		@Test
		public void fieldNameShadowing() throws Exception {
			SarlScript mas = file(multilineString(
					"event E0 {",
					"val field1 : int = 5",
					"val field2 : int = 6",
					"}",
					"event E1 extends E0 {",
					"val field1 : int = 5",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
					81, 6,
					"The field 'field1' in 'E1' is hidding the inherited field 'E0.field1'.");
		}

	}

	public static class ConstructorTest extends AbstractSarlTest {

		@Test
		public void validImplicitSuperConstructor() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.test",
					"event E1 {",
					"}",
					"event E2 extends E1 {",
					"new (a : int) {",
					"}",
					"}"
					), true);
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.test", mas.getPackage());
			//
			SarlEvent event1 = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event1.getName());
			assertNull(event1.getExtends());
			assertEquals(0, event1.getMembers().size());
			//
			SarlEvent event2 = (SarlEvent) mas.getXtendTypes().get(1);
			assertEquals("E2", event2.getName());
			assertTypeReferenceIdentifier(event2.getExtends(), "io.sarl.test.E1");
			assertEquals(1, event2.getMembers().size());
			//
			SarlConstructor constructor = (SarlConstructor) event2.getMembers().get(0);
			assertParameterNames(constructor.getParameters(), "a");
			assertParameterTypes(constructor.getParameters(), "int");
			assertParameterDefaultValues(constructor.getParameters(), (Object) null);
		}

		@Test
		public void missedImplicitSuperConstructor_1() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.test",
					"event E1 {",
					"new (a : char) {",
					"}",
					"}",
					"event E2 extends E1 {",
					"new (a : int) {",
					"}",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR,
					75, 17,
					"Undefined default constructor in the super-type");
		}

		@Test
		public void missedImplicitSuperConstructor_2() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.test",
					"event E1 {",
					"new (a : int) {",
					"}",
					"}",
					"event E2 extends E1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_CONSTRUCTOR,
					58, 2,
					"Undefined default constructor in the super-type.");
		}

		@Test
		public void missedImplicitSuperConstructor_3() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.test",
					"event E1 {",
					"new (a : int) {",
					"}",
					"}",
					"event E2 extends E1 {",
					"new (a : int) {",
					"}",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR,
					74, 17,
					"Undefined default constructor in the super-type");
		}

		@Test
		public void invalidArgumentTypeToSuperConstructor() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.test",
					"event E1 {",
					"new (a : int) {",
					"}",
					"}",
					"event E2 extends E1 {",
					"new (a : int) {",
					"super(\"\")",
					"}",
					"}"
					));
			validate(mas).assertError(
					XbasePackage.eINSTANCE.getXStringLiteral(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
					96, 2,
					"Type mismatch: cannot convert from String to int");
		}

	}

	public static class AttributeTest extends AbstractSarlTest {

		@Test
		public void variableModifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"public var name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			SarlField attr1 = (SarlField) event.getMembers().get(0);
			assertEquals(JvmVisibility.PUBLIC, attr1.getVisibility());
		}

		@Test
		public void variableModifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"package var name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of E1; only public, final, val & var are permitted");
		}

		@Test
		public void variableModifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"protected var name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of E1; only public, final, val & var are permitted");
		}

		@Test
		public void variableModifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"private var name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of E1; only public, final, val & var are permitted");
		}

		@Test
		public void variableModifier_default() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"var name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			SarlField attr1 = (SarlField) event.getMembers().get(0);
			assertEquals(JvmVisibility.PUBLIC, attr1.getVisibility());
		}

		@Test
		public void valueModifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"public val name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			SarlField attr1 = (SarlField) event.getMembers().get(0);
			assertEquals(JvmVisibility.PUBLIC, attr1.getVisibility());
		}

		@Test
		public void valueModifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"package val name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of E1; only public, final, val & var are permitted");
		}

		@Test
		public void valueModifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"protected val name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of E1; only public, final, val & var are permitted");
		}

		@Test
		public void valueModifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"private val name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of E1; only public, final, val & var are permitted");
		}

		@Test
		public void valueModifier_default() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 {",
					"val name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			SarlField attr1 = (SarlField) event.getMembers().get(0);
			assertEquals(JvmVisibility.PUBLIC, attr1.getVisibility());
		}

	}

}
