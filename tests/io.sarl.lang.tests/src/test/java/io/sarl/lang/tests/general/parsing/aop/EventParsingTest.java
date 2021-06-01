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
package io.sarl.lang.tests.general.parsing.aop;

import static io.sarl.tests.api.tools.TestAssertions.assertParameterDefaultValues;
import static io.sarl.tests.api.tools.TestAssertions.assertParameterNames;
import static io.sarl.tests.api.tools.TestAssertions.assertParameterTypes;
import static io.sarl.tests.api.tools.TestAssertions.assertTypeReferenceIdentifier;
import static io.sarl.tests.api.tools.TestAssertions.assertXExpression;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.common.base.Strings;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: event")
@Tag("core")
public class EventParsingTest extends AbstractSarlTest {

	@Nested
	public class TopElementTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void invalidExtend_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"}",
					"event E1 extends C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED,
					"Invalid supertype. Expecting a class.");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidExtend_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"}",
					"event E1 extends A1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Event'.");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidExtend_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"}",
					"agent A1 extends E1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Agent'.");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidExtend_3() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"}",
					"behavior B1 extends E1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehavior(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Behavior'.");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidExtend_4() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"}",
					"skill S1 extends E1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Skill'.");
		}

		@Test
		@Tag("sarlParsing")
		public void eventmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public event E1"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(JvmVisibility.PUBLIC, event.getVisibility());
			assertEquals(0, event.getMembers().size());
			assertFalse(event.isAbstract());
			assertFalse(event.isFinal());
			assertFalse(event.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void eventmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(JvmVisibility.PUBLIC, event.getVisibility());
			assertEquals(0, event.getMembers().size());
			assertFalse(event.isAbstract());
			assertFalse(event.isFinal());
			assertFalse(event.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"private event E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"protected event E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void eventmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"package event E1"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(JvmVisibility.DEFAULT, event.getVisibility());
			assertEquals(0, event.getMembers().size());
			assertFalse(event.isAbstract());
			assertFalse(event.isFinal());
			assertFalse(event.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"abstract event E1"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(JvmVisibility.PUBLIC, event.getVisibility());
			assertEquals(0, event.getMembers().size());
			assertTrue(event.isAbstract());
			assertFalse(event.isFinal());
			assertFalse(event.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"static event E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch event E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void eventmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"final event E1"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(JvmVisibility.PUBLIC, event.getVisibility());
			assertEquals(0, event.getMembers().size());
			assertFalse(event.isAbstract());
			assertTrue(event.isFinal());
			assertFalse(event.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp event E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"native event E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"volatile event E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized event E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"transient event E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void eventmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public package event E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"public / package / protected / private");
		}

	}

	@Nested
	public class FieldTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void modifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	public var field : int",
					"}"));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(1, event.getMembers().size());
			//
			SarlField attr = (SarlField) event.getMembers().get(0);
			assertEquals("field", attr.getName());
			assertTypeReferenceIdentifier(attr.getType(), "int");
			assertNull(attr.getInitialValue());
			assertEquals(JvmVisibility.PUBLIC, attr.getVisibility());
			assertFalse(attr.isFinal());
			assertFalse(attr.isStatic());
			assertFalse(attr.isTransient());
			assertFalse(attr.isVolatile());
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	private var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	protected var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	package var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	var field : int",
					"}"));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(1, event.getMembers().size());
			//
			SarlField attr = (SarlField) event.getMembers().get(0);
			assertEquals("field", attr.getName());
			assertTypeReferenceIdentifier(attr.getType(), "int");
			assertNull(attr.getInitialValue());
			assertEquals(JvmVisibility.PUBLIC, attr.getVisibility());
			assertFalse(attr.isFinal());
			assertFalse(attr.isStatic());
			assertFalse(attr.isTransient());
			assertFalse(attr.isVolatile());
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	abstract var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	static var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	dispatch var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_final_var() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	final var field : int = 5",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"var or val / final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	strictfp var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	native var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	volatile var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	synchronized var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	transient var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_protected_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	protected private var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"public / package / protected / private");
		}

		@Test
		@Tag("sarlValidation")
		public void missedFinalFieldInitialization() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"val field1 : int = 5",
					"val field2 : String",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.FIELD_NOT_INITIALIZED,
					"The blank final field field2 may not have been initialized");
		}

		@Test
		@Tag("sarlParsing")
		public void completeFinalFieldInitialization() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"event E1 {",
					"val field1 : int = 5",
					"val field2 : String = \"\"",
					"}"
					));
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
		@Tag("sarlValidation")
		public void invalidSarlFieldName_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"var myfield1 = 4.5",
					"var $FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"var myfield2 = true",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
					"Invalid name '$FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidSarlFieldName_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"val myfield1 = 4.5",
					"val $FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"val myfield2 = true",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
					"Invalid name '$FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidSarlFieldName_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"var myfield1 = 4.5",
					"var const = \"String\"",
					"var myfield2 = true",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_IDENTIFIER,
					"'const' is not a valid identifier.");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidSarlFieldName_3() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"var myfield1 = 4.5",
					"var this = \"String\"",
					"var myfield2 = true",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_IDENTIFIER,
					"'this' is not a valid identifier");
		}

		@Test
		@Tag("sarlValidation")
		public void discouragedSarlFieldName_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"val myfield1 = 4.5",
					"val self = \"String\"",
					"val myfield2 = true",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISCOURAGED,
					"'self' is a discouraged name");
		}

		@Test
		@Tag("sarlValidation")
		public void multipleVariableDefinition() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"var myfield : int",
					"var myfield1 : String",
					"var myfield : double",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					"Duplicate field myfield");
		}

		@Test
		@Tag("sarlValidation")
		public void multipleValueDefinition() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"val myfield : int = 4",
					"val myfield1 : String = \"\"",
					"val myfield : double = 5",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					"Duplicate field myfield");
		}

		@Test
		@Tag("sarlValidation")
		public void fieldNameShadowing() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E0 {",
					"val field1 : int = 5",
					"val field2 : int = 6",
					"}",
					"event E1 extends E0 {",
					"val field1 : int = 5",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
					"The field 'field1' in 'E1' is hidding the inherited field 'E0.field1'.");
		}

	}

	@Nested
	public class ConstructorTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void modifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	public new { super(null) }",
					"}"));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(1, event.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) event.getMembers().get(0);
			assertEquals(JvmVisibility.PUBLIC, cons.getVisibility());
			assertFalse(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	private new { super(null) }",
					"}"));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(1, event.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) event.getMembers().get(0);
			assertEquals(JvmVisibility.PRIVATE, cons.getVisibility());
			assertFalse(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	protected new { super(null) }",
					"}"));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(1, event.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) event.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, cons.getVisibility());
			assertFalse(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	package new { super(null) }",
					"}"));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(1, event.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) event.getMembers().get(0);
			assertEquals(JvmVisibility.DEFAULT, cons.getVisibility());
			assertFalse(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	new { super(null) }",
					"}"));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(1, event.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) event.getMembers().get(0);
			assertEquals(JvmVisibility.PUBLIC, cons.getVisibility());
			assertFalse(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	abstract new { super(null) }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	static new { }",
					"}"));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(1, event.getMembers().size());
			//
			SarlConstructor cons = (SarlConstructor) event.getMembers().get(0);
			assertEquals(JvmVisibility.PUBLIC, cons.getVisibility());
			assertTrue(cons.isStatic());
			assertFalse(cons.isFinal());
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	dispatch new { super(null) }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	final new { super(null) }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	strictfp new { super(null) }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	native new { super(null) }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	volatile new { super(null) }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	synchronized new { super(null) }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	transient new { super(null) }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_protected_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"event E1 {",
					"	protected private new { super(null) }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"public / package / protected / private");
		}

		@Test
		@Tag("sarlParsing")
		public void validImplicitSuperConstructor() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.test",
					"event E1 {",
					"}",
					"event E2 extends E1 {",
					"new (a : int) {",
					"}",
					"}"
					));
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
		@Tag("sarlValidation")
		public void missedImplicitSuperConstructor_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
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
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR,
					"No default constructor in super type E1");
		}

		@Test
		@Tag("sarlValidation")
		public void missedImplicitSuperConstructor_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
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
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					org.eclipse.xtend.core.validation.IssueCodes.MUST_INVOKE_SUPER_CONSTRUCTOR,
					"No default constructor in super type E1");
		}

		@Test
		@Tag("sarlValidation")
		public void notMissedImplicitSuperConstructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.test",
					"event E1 {",
					"new (a : int) {",
					"}",
					"}",
					"event E2 extends E1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		public void invalidArgumentTypeToSuperConstructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
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
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XbasePackage.eINSTANCE.getXStringLiteral(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from String to int");
		}

	}

}
