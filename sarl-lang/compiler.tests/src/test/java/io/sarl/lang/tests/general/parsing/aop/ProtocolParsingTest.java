/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import static io.sarl.lang.validation.IssueCodes.*;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.eclipse.xtend.core.validation.IssueCodes.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static io.sarl.tests.api.tools.TestAssertions.assertContains;

import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlProtocol;
import io.sarl.lang.sarl.SarlProtocolMessage;
import io.sarl.lang.sarl.SarlProtocolParameter;
import io.sarl.lang.sarl.SarlProtocolRole;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@SuppressWarnings("all")
@DisplayName("Syntax: protocol")
@Tag("core")
public class ProtocolParsingTest {

	@Nested
	@DisplayName("protocol entity")
	public class TopElementTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Empty declaration")
		public void emptyDeclaration() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					PROTOCOL_EMPTY,
					"Discouraged protocol definition");
		}

		@Test
		@Tag("sarlParsing")
		@DisplayName("Modifier: public")
		public void modifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public protocol P1 { }"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals("P1", protocol.getName());
			assertNull(protocol.getExtends());
			assertEquals(JvmVisibility.PUBLIC, protocol.getVisibility());
			assertEquals(0, protocol.getMembers().size());
			assertFalse(protocol.isAbstract());
			assertFalse(protocol.isFinal());
			assertFalse(protocol.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		@DisplayName("No modifier")
		public void modifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"protocol P1 { }"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals("P1", protocol.getName());
			assertNull(protocol.getExtends());
			assertEquals(JvmVisibility.PUBLIC, protocol.getVisibility());
			assertEquals(0, protocol.getMembers().size());
			assertFalse(protocol.isAbstract());
			assertFalse(protocol.isFinal());
			assertFalse(protocol.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifier: private")
		public void modifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"private protocol P1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifier: protected")
		public void modifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"protected protocol P1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		@DisplayName("Modifier: package")
		public void modifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"package protocol P1 { }"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals("P1", protocol.getName());
			assertNull(protocol.getExtends());
			assertEquals(JvmVisibility.DEFAULT, protocol.getVisibility());
			assertEquals(0, protocol.getMembers().size());
			assertFalse(protocol.isAbstract());
			assertFalse(protocol.isFinal());
			assertFalse(protocol.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifier: abstract")
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"abstract protocol P1 { }"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlProtocol protocol = (SarlProtocol) mas.getXtendTypes().get(0);
			assertEquals("P1", protocol.getName());
			assertNull(protocol.getExtends());
			assertEquals(JvmVisibility.PUBLIC, protocol.getVisibility());
			assertEquals(0, protocol.getMembers().size());
			assertTrue(protocol.isAbstract());
			assertFalse(protocol.isFinal());
			assertFalse(protocol.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifier: static")
		public void modifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"static protocol P1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifier: dispatch")
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch protocol P1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		@DisplayName("Modifier: final")
		public void modifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"final protocol P1 { }"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals("P1", protocol.getName());
			assertNull(protocol.getExtends());
			assertEquals(JvmVisibility.PUBLIC, protocol.getVisibility());
			assertEquals(0, protocol.getMembers().size());
			assertFalse(protocol.isAbstract());
			assertTrue(protocol.isFinal());
			assertFalse(protocol.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifier: strictfp")
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp protocol P1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifier: native")
		public void modifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"native protocol P1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifier: volatile")
		public void modifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"volatile protocol P1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifier: synchronized")
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized protocol P1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifier: transient")
		public void modifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"transient protocol P1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Modifiers: public package")
		public void modifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public package protocol P1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocol(),
					INVALID_MODIFIER,
					"public, package, abstract or final");
		}

	}

	@Nested
	@DisplayName("Roles")
	public class RolesTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("One role")
		public void one() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolRole roles = assertInstanceOf(SarlProtocolRole.class, protocol.getMembers().get(0));
			assertContains(roles.getNames(), "R1");
			//
			var allRoles = protocol.getRoleNames();
			assertContains(allRoles, "R1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Two roles in sequence")
		public void twoSequence() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolRole roles0 = assertInstanceOf(SarlProtocolRole.class, protocol.getMembers().get(0));
			assertContains(roles0.getNames(), "R1", "R2");
			//
			var allRoles = protocol.getRoleNames();
			assertContains(allRoles, "R1", "R2");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Two roles in different statements")
		public void twoStatements() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  role R2",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(2, protocol.getMembers().size());
			SarlProtocolRole roles0 = assertInstanceOf(SarlProtocolRole.class, protocol.getMembers().get(0));
			assertContains(roles0.getNames(), "R1");
			SarlProtocolRole roles1 = assertInstanceOf(SarlProtocolRole.class, protocol.getMembers().get(1));
			assertContains(roles1.getNames(), "R2");
			//
			var allRoles = protocol.getRoleNames();
			assertContains(allRoles, "R1", "R2");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Three roles in sequence")
		public void threeSequence() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R3, R2",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolRole roles0 = assertInstanceOf(SarlProtocolRole.class, protocol.getMembers().get(0));
			assertContains(roles0.getNames(), "R1", "R3", "R2");
			//
			var allRoles = protocol.getRoleNames();
			assertContains(allRoles, "R1", "R3", "R2");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Three roles in different statements")
		public void threeStatements() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  role R3",
					"  role R2",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(3, protocol.getMembers().size());
			SarlProtocolRole roles0 = assertInstanceOf(SarlProtocolRole.class, protocol.getMembers().get(0));
			assertContains(roles0.getNames(), "R1");
			SarlProtocolRole roles1 = assertInstanceOf(SarlProtocolRole.class, protocol.getMembers().get(1));
			assertContains(roles1.getNames(), "R3");
			SarlProtocolRole roles2 = assertInstanceOf(SarlProtocolRole.class, protocol.getMembers().get(2));
			assertContains(roles2.getNames(), "R2");
			//
			var allRoles = protocol.getRoleNames();
			assertContains(allRoles, "R1", "R3", "R2");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Duplicate roles in sequence")
		public void duplicate1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R1",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolRole(),
					PROTOCOL_DUPLICATE_ROLE,
					"Multiple declaration of the role 'R1' in the protocol");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Duplicate roles in statements")
		public void duplicate2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  role R1",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolRole(),
					PROTOCOL_DUPLICATE_ROLE,
					"Multiple declaration of the role 'R1' in the protocol");
		}

	}

	@Nested
	@DisplayName("Parameters")
	public class ParametersTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Duplicate parameters")
		public void duplicate2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var P1",
					"  var P1",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					PROTOCOL_DUPLICATE_PARAMETER,
					"Multiple declaration of the parameter 'P1' in the protocol");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Two parameters in sequence, no modifier, no type")
		public void twoSequence_noModifier_noType() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var P1, P2",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ','");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, no modifier, no type")
		public void one_noModifier_noType() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertNull(parameter.getType());
			assertContains(parameter.getModifiers());
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Two parameters in statements, no modifier, no type")
		public void twoStatements_noModifier_noType() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1",
					"  var P2",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(2, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter0.getName());
			assertNull(parameter0.getType());
			assertContains(parameter0.getModifiers());
			SarlProtocolParameter parameter1 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(1));
			assertEquals("P2", parameter1.getName());
			assertNull(parameter1.getType());
			assertContains(parameter1.getModifiers());
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1", "P2");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Three parameters in statements, no modifier, no type")
		public void threeStatements_noModifier_noType() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1",
					"  var P3",
					"  var P2",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(3, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter0.getName());
			assertNull(parameter0.getType());
			assertContains(parameter0.getModifiers());
			SarlProtocolParameter parameter1 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(1));
			assertEquals("P3", parameter1.getName());
			assertNull(parameter1.getType());
			assertContains(parameter1.getModifiers());
			SarlProtocolParameter parameter2 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(2));
			assertEquals("P2", parameter2.getName());
			assertNull(parameter2.getType());
			assertContains(parameter2.getModifiers());
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1", "P3", "P2");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, in modifier before, no type")
		public void one_inModifierBefore_noType() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in P1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertNull(parameter.getType());
			assertContains(parameter.getModifiers(), "in");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, in modifier after, no type")
		public void one_inModifierAfter_noType() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1 in",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertNull(parameter.getType());
			assertContains(parameter.getModifiers(), "in");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before, no type")
		public void one_twoModifiersBefore_noType_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var in out P1",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before, no type")
		public void one_twoModifiersBefore_noType_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in key P1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertNull(parameter.getType());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before, no type")
		public void one_twoModifiersBefore_noType_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key in P1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertNull(parameter.getType());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}


		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers after, no type")
		public void one_twoModifiersAfter_noType_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var P1 in out",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers after, no type")
		public void one_twoModifiersAfter_noType_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1 in key",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertNull(parameter.getType());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers after, no type")
		public void one_twoModifiersAfter_noType_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1 key in",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertNull(parameter.getType());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}


		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before-after, no type")
		public void one_twoModifiersBeforeAfter_noType_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 out",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before-after, no type")
		public void one_twoModifiersBeforeAfter_noType_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 key",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertNull(parameter.getType());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before-after, no type")
		public void one_twoModifiersBeforeAfter_noType_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key P1 in",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertNull(parameter.getType());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Two parameters in statements, in modifier, no type")
		public void twoStatements_inModifier_noType() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in P1",
					"  var P2",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(2, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter0.getName());
			assertNull(parameter0.getType());
			assertContains(parameter0.getModifiers(), "in");
			SarlProtocolParameter parameter1 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(1));
			assertEquals("P2", parameter1.getName());
			assertNull(parameter1.getType());
			assertContains(parameter1.getModifiers());
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1", "P2");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Three parameters in statements, in modifier, no type")
		public void threeStatements_inModifierBefore_noType() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in P1",
					"  var P3",
					"  var P2 in",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(3, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter0.getName());
			assertNull(parameter0.getType());
			assertContains(parameter0.getModifiers(), "in");
			SarlProtocolParameter parameter1 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(1));
			assertEquals("P3", parameter1.getName());
			assertNull(parameter1.getType());
			assertContains(parameter1.getModifiers());
			SarlProtocolParameter parameter2 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(2));
			assertEquals("P2", parameter2.getName());
			assertNull(parameter2.getType());
			assertContains(parameter2.getModifiers(), "in");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1", "P3", "P2");
		}


		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, no modifier, type")
		public void one_noModifier_type() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1 : Double",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertEquals(Double.class.getName(), parameter.getType().getIdentifier());
			assertContains(parameter.getModifiers());
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Two parameters in statements, no modifier, type")
		public void twoStatements_noModifier_type() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1 : Integer",
					"  var P2 : Float",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(2, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter0.getName());
			assertEquals(Integer.class.getName(), parameter0.getType().getIdentifier());
			assertContains(parameter0.getModifiers());
			SarlProtocolParameter parameter1 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(1));
			assertEquals("P2", parameter1.getName());
			assertEquals(Float.class.getName(), parameter1.getType().getIdentifier());
			assertContains(parameter1.getModifiers());
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1", "P2");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Three parameters in statements, no modifier, type")
		public void threeStatements_noModifier_type() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1 : Integer",
					"  var P3 : String",
					"  var P2 : Double",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(3, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter0.getName());
			assertEquals(Integer.class.getName(), parameter0.getType().getIdentifier());
			assertContains(parameter0.getModifiers());
			SarlProtocolParameter parameter1 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(1));
			assertEquals("P3", parameter1.getName());
			assertEquals(String.class.getName(), parameter1.getType().getIdentifier());
			assertContains(parameter1.getModifiers());
			SarlProtocolParameter parameter2 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(2));
			assertEquals("P2", parameter2.getName());
			assertEquals(Double.class.getName(), parameter2.getType().getIdentifier());
			assertContains(parameter2.getModifiers());
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1", "P3", "P2");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, in modifier before, type")
		public void one_inModifierBefore_type() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 : Double",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertEquals(Double.class.getName(), parameter.getType().getIdentifier());
			assertContains(parameter.getModifiers(), "in");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, in modifier after, type")
		public void one_inModifierAfter_type() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1 in : Float",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertEquals(Float.class.getName(), parameter.getType().getIdentifier());
			assertContains(parameter.getModifiers(), "in");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before, type")
		public void one_twoModifiersBefore_type_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var in out P1 : Float",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before, type")
		public void one_twoModifiersBefore_type_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in key P1 : Double",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertEquals(Double.class.getName(), parameter.getType().getIdentifier());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before, type")
		public void one_twoModifiersBefore_type_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key in P1 : Integer",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertEquals(Integer.class.getName(), parameter.getType().getIdentifier());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}


		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers after, type")
		public void one_twoModifiersAfter_type_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var P1 in out : String",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers after, type")
		public void one_twoModifiersAfter_type_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1 in key : Integer",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertEquals(Integer.class.getName(), parameter.getType().getIdentifier());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers after, type")
		public void one_twoModifiersAfter_type_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var P1 key in : String",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertEquals(String.class.getName(), parameter.getType().getIdentifier());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}


		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before-after, type")
		public void one_twoModifiersBeforeAfter_type_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 out : Integer",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before-after, type")
		public void one_twoModifiersBeforeAfter_type_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 key : String",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertEquals(String.class.getName(), parameter.getType().getIdentifier());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("One parameter, two modifiers before-after, type")
		public void one_twoModifiersBeforeAfter_type_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key P1 in : Integer",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter.getName());
			assertEquals(Integer.class.getName(), parameter.getType().getIdentifier());
			assertContains(parameter.getModifiers(), "in", "key");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Two parameters in statements, in modifier, type")
		public void twoStatements_inModifier_type() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 : Integer",
					"  var P2 : String",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(2, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter0.getName());
			assertEquals(Integer.class.getName(), parameter0.getType().getIdentifier());
			assertContains(parameter0.getModifiers(), "in");
			SarlProtocolParameter parameter1 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(1));
			assertEquals("P2", parameter1.getName());
			assertEquals(String.class.getName(), parameter1.getType().getIdentifier());
			assertContains(parameter1.getModifiers());
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1", "P2");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Three parameters in statements, in modifier, type")
		public void threeStatements_inModifierBefore_type() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 : Integer",
					"  var P3 : String",
					"  var P2 in : Double",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			//
			assertEquals(3, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			assertEquals("P1", parameter0.getName());
			assertEquals(Integer.class.getName(), parameter0.getType().getIdentifier());
			assertContains(parameter0.getModifiers(), "in");
			SarlProtocolParameter parameter1 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(1));
			assertEquals("P3", parameter1.getName());
			assertEquals(String.class.getName(), parameter1.getType().getIdentifier());
			assertContains(parameter1.getModifiers());
			SarlProtocolParameter parameter2 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(2));
			assertEquals("P2", parameter2.getName());
			assertEquals(Double.class.getName(), parameter2.getType().getIdentifier());
			assertContains(parameter2.getModifiers(), "in");
			//
			var allParameters = protocol.getParameterNames();
			assertContains(allParameters, "P1", "P3", "P2");
		}

	}

	@Nested
	@DisplayName("Parameter Modifiers")
	public class ParameterModifersTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("in")
		public void in() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in P1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertTrue(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertFalse(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("out")
		public void out() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var out P1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertTrue(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertFalse(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("any")
		public void any() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var any P1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertTrue(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertFalse(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("nil")
		public void nil() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var nil P1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertTrue(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertFalse(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("opt")
		public void opt() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var opt P1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertTrue(parameter0.isOptional());
			assertFalse(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("key")
		public void key() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key P1",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("xxx")
		public void xxx() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var P1 xxx",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'xxx' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("in in")
		public void in_in() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 in",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					UNNECESSARY_MODIFIER,
					"Duplicate modifier 'in' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("in out")
		public void in_out() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 out",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("in any")
		public void in_any() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 any",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'any' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("in nil")
		public void in_nil() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 nil",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'nil' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("in opt")
		public void in_opt() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 opt",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'opt' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("in key")
		public void in_key() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 key",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertTrue(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("in xxx")
		public void in_xxx() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var in P1 xxx",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'xxx' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("out in")
		public void out_in() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var out P1 in",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'in' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("out out")
		public void out_out() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var out P1 out",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					UNNECESSARY_MODIFIER,
					"Duplicate modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("out any")
		public void out_any() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var out P1 any",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'any' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("out nil")
		public void out_nil() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var out P1 nil",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'nil' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("out opt")
		public void out_opt() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var out P1 opt",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'opt' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("out key")
		public void out_key() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var out P1 key",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertTrue(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("out xxx")
		public void out_xxx() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var out P1 xxx",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'xxx' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("any in")
		public void any_in() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var any P1 in",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'in' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("any out")
		public void any_out() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var any P1 out",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("any any")
		public void any_any() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var any P1 any",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					UNNECESSARY_MODIFIER,
					"Duplicate modifier 'any' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("any nil")
		public void any_nil() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var any P1 nil",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'nil' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("any opt")
		public void any_opt() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var any P1 opt",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'opt' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("any key")
		public void any_key() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var any P1 key",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertTrue(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("any xxx")
		public void any_xxx() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var any P1 xxx",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'xxx' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("nil in")
		public void nil_in() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var nil P1 in",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'in' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("nil out")
		public void nil_out() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var nil P1 out",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("nil any")
		public void nil_any() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var nil P1 any",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'any' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("nil nil")
		public void nil_nil() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var nil P1 nil",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					UNNECESSARY_MODIFIER,
					"Duplicate modifier 'nil' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("nil opt")
		public void nil_opt() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var nil P1 opt",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'opt' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("nil key")
		public void nil_key() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var nil P1 key",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertTrue(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("nil xxx")
		public void nil_xxx() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var nil P1 xxx",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'xxx' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("opt in")
		public void opt_in() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var opt P1 in",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'in' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("opt out")
		public void opt_out() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var opt P1 out",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'out' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("opt any")
		public void opt_any() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var opt P1 any",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'any' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("opt nil")
		public void opt_nil() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var opt P1 nil",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'nil' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("opt opt")
		public void opt_opt() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var opt P1 opt",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					UNNECESSARY_MODIFIER,
					"Duplicate modifier 'opt' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("opt key")
		public void opt_key() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var opt P1 key",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertTrue(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("opt xxx")
		public void opt_xxx() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var opt P1 xxx",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'xxx' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("key in")
		public void key_in() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key P1 in",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertTrue(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("key out")
		public void key_out() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key P1 out",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertTrue(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("key any")
		public void key_any() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key P1 any",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertTrue(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("key nil")
		public void key_nil() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key P1 nil",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertTrue(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("key opt")
		public void key_opt() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key P1 opt",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertTrue(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("key key")
		public void key_key() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  var key P1 key",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(1, protocol.getMembers().size());
			SarlProtocolParameter parameter0 = assertInstanceOf(SarlProtocolParameter.class, protocol.getMembers().get(0));
			//
			assertFalse(parameter0.isIn());
			assertFalse(parameter0.isOut());
			assertFalse(parameter0.isAny());
			assertFalse(parameter0.isNil());
			assertFalse(parameter0.isOptional());
			assertTrue(parameter0.isKey());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("key xxx")
		public void key_xxx() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var key P1 xxx",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'xxx' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("xxx in")
		public void xxx_in() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var xxx P1 in",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'P1' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("xxx out")
		public void xxx_out() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var xxx P1 out",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'P1' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("xxx any")
		public void xxx_any() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var xxx P1 any",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'P1' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("xxx nil")
		public void xxx_nil() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var xxx P1 nil",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'P1' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("xxx opt")
		public void xxx_opt() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var xxx P1 opt",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'P1' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("xxx key")
		public void xxx_key() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var xxx P1 key",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'P1' for the protocol parameter");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("key xxx")
		public void xxx_xxx() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var xxx P1 xxx",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolParameter(),
					INVALID_MODIFIER,
					"Invalid modifier 'P1' for the protocol parameter");
		}

	}

	@Nested
	@DisplayName("R -> R : M")
	public class MessageWithoutParameterV1Test extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Duplicate messages")
		public void duplicate2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  R1 -> R2 : Msg",
					"  R1 -> R2 : Msg",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_DUPLICATE_MESSAGE,
					"Multiple declaration of the message 'Msg' from 'R1' to 'R2' in the protocol");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing 2 roles")
		public void missingR1R2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  R1 -> R2 : Msg",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role")
		public void missingR1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  R1 -> R2 : Msg",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role")
		public void missingR2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  R1 -> R2 : Msg",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role")
		public void noMissing() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  R1 -> R2 : Msg",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(2, protocol.getMembers().size());
			SarlProtocolMessage message0 = assertInstanceOf(SarlProtocolMessage.class, protocol.getMembers().get(1));
			//
			assertEquals("R1", message0.getFrom());
			assertEquals("R2", message0.getTo());
			assertEquals("Msg", message0.getName());
			assertEquals(0, message0.getParameters().size());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Undefined role")
		public void undefinedRole() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  R1 -> R3 : Msg",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R3' in the protocol")
			.assertNoErrors();
		}

	}

	@Nested
	@DisplayName("R -> R : M()")
	public class MessageWithoutParameterV2Test extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Duplicate messages")
		public void duplicate2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  R1 -> R2 : Msg()",
					"  R1 -> R2 : Msg()",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_DUPLICATE_MESSAGE,
					"Multiple declaration of the message 'Msg' from 'R1' to 'R2' in the protocol");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing 2 roles")
		public void missingR1R2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  R1 -> R2 : Msg()",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role")
		public void missingR1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  R1 -> R2 : Msg()",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role")
		public void missingR2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  R1 -> R2 : Msg()",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role")
		public void noMissing() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  R1 -> R2 : Msg()",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(2, protocol.getMembers().size());
			SarlProtocolMessage message0 = assertInstanceOf(SarlProtocolMessage.class, protocol.getMembers().get(1));
			//
			assertEquals("R1", message0.getFrom());
			assertEquals("R2", message0.getTo());
			assertEquals("Msg", message0.getName());
			assertEquals(0, message0.getParameters().size());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Undefined role")
		public void undefinedRole() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  R1 -> R3 : Msg()",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R3' in the protocol")
			.assertNoErrors();
		}

	}

	@Nested
	@DisplayName("R -> R : M(P1)")
	public class MessageWithParameterNoModifierTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Duplicate messages")
		public void duplicate2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(P1)",
					"  R1 -> R2 : Msg(P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_DUPLICATE_MESSAGE,
					"Multiple declaration of the message 'Msg' from 'R1' to 'R2' in the protocol");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing 2 roles 1 parameter")
		public void missingR1R2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  R1 -> R2 : Msg(P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role")
		public void missingR1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  var P1",
					"  R1 -> R2 : Msg(P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role 1 parameter")
		public void missingR1P1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  R1 -> R2 : Msg(P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role")
		public void missingR2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P1",
					"  R1 -> R2 : Msg(P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role 1 parameter")
		public void missingR2P1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  R1 -> R2 : Msg(P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role 1 missing parameter")
		public void missingP1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  R1 -> R2 : Msg(P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role 1 missing parameter")
		public void noMissing() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P1",
					"  R1 -> R2 : Msg(P1)",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(3, protocol.getMembers().size());
			SarlProtocolMessage message0 = assertInstanceOf(SarlProtocolMessage.class, protocol.getMembers().get(2));
			//
			assertEquals("R1", message0.getFrom());
			assertEquals("R2", message0.getTo());
			assertEquals("Msg", message0.getName());
			assertEquals(1, message0.getParameters().size());
			var parameter0 = assertInstanceOf(SarlProtocolParameter.class, message0.getParameters().get(0));
			assertEquals("P1", parameter0.getName());
			assertNull(parameter0.getType());
			assertContains(parameter0.getModifiers());
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Undefined role")
		public void undefinedRole() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P1",
					"  R1 -> R3 : Msg(P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R3' in the protocol")
			.assertNoErrors();
		}

	}

	@Nested
	@DisplayName("R -> R : M(in P1)")
	public class MessageWithParameterInModifierBeforeTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Duplicate messages")
		public void duplicate2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(in P1)",
					"  R1 -> R2 : Msg(in P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_DUPLICATE_MESSAGE,
					"Multiple declaration of the message 'Msg' from 'R1' to 'R2' in the protocol");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing 2 roles 1 parameter")
		public void missingR1R2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  R1 -> R2 : Msg(in P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role")
		public void missingR1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  var P1",
					"  R1 -> R2 : Msg(in P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role 1 parameter")
		public void missingR1P1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  R1 -> R2 : Msg(in P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role")
		public void missingR2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P1",
					"  R1 -> R2 : Msg(in P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role 1 parameter")
		public void missingR2P1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  R1 -> R2 : Msg(in P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role 1 missing parameter")
		public void missingP1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  R1 -> R2 : Msg(in P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role 1 missing parameter")
		public void noMissing() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P1",
					"  R1 -> R2 : Msg(in P1)",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(3, protocol.getMembers().size());
			SarlProtocolMessage message0 = assertInstanceOf(SarlProtocolMessage.class, protocol.getMembers().get(2));
			//
			assertEquals("R1", message0.getFrom());
			assertEquals("R2", message0.getTo());
			assertEquals("Msg", message0.getName());
			assertEquals(1, message0.getParameters().size());
			var parameter0 = assertInstanceOf(SarlProtocolParameter.class, message0.getParameters().get(0));
			assertEquals("P1", parameter0.getName());
			assertNull(parameter0.getType());
			assertContains(parameter0.getModifiers(), "in");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Undefined role")
		public void undefinedRole() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P1",
					"  R1 -> R3 : Msg(in P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R3' in the protocol")
			.assertNoErrors();
		}

	}

	@Nested
	@DisplayName("R -> R : M(P1 in)")
	public class MessageWithParameterInModifierAfterTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Duplicate messages")
		public void duplicate2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(P1 in)",
					"  R1 -> R2 : Msg(P2 in)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_DUPLICATE_MESSAGE,
					"Multiple declaration of the message 'Msg' from 'R1' to 'R2' in the protocol");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing 2 roles 1 parameter")
		public void missingR1R2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  R1 -> R2 : Msg(P1 in)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role")
		public void missingR1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  var P1",
					"  R1 -> R2 : Msg(P1 in)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role 1 parameter")
		public void missingR1P1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  R1 -> R2 : Msg(P1 in)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role")
		public void missingR2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P1",
					"  R1 -> R2 : Msg(P1 in)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role 1 parameter")
		public void missingR2P1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  R1 -> R2 : Msg(P1 in)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role 1 missing parameter")
		public void missingP1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  R1 -> R2 : Msg(P1 in)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role 1 missing parameter")
		public void noMissing() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P1",
					"  R1 -> R2 : Msg(P1 in)",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(3, protocol.getMembers().size());
			SarlProtocolMessage message0 = assertInstanceOf(SarlProtocolMessage.class, protocol.getMembers().get(2));
			//
			assertEquals("R1", message0.getFrom());
			assertEquals("R2", message0.getTo());
			assertEquals("Msg", message0.getName());
			assertEquals(1, message0.getParameters().size());
			var parameter0 = assertInstanceOf(SarlProtocolParameter.class, message0.getParameters().get(0));
			assertEquals("P1", parameter0.getName());
			assertNull(parameter0.getType());
			assertContains(parameter0.getModifiers(), "in");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Undefined role")
		public void undefinedRole() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P1",
					"  R1 -> R3 : Msg(P1 in)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R3' in the protocol")
			.assertNoErrors();
		}

	}

	@Nested
	@DisplayName("R -> R : M(P1, out P2)")
	public class MessageWithParametersNoModifierTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Duplicate messages")
		public void duplicate2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(P1, out P2)",
					"  R1 -> R2 : Msg(P2, out P1)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_DUPLICATE_MESSAGE,
					"Multiple declaration of the message 'Msg' from 'R1' to 'R2' in the protocol");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing 2 roles 1 parameter")
		public void missingR1R2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var P2",
					"  R1 -> R2 : Msg(P1, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role")
		public void missingR1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(P1, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role 1 parameter")
		public void missingR1P1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  var P2",
					"  R1 -> R2 : Msg(P1, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role")
		public void missingR2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(P1, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role 1 parameter")
		public void missingR2P1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P2",
					"  R1 -> R2 : Msg(P1, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role 1 missing parameter")
		public void missingP1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P2",
					"  R1 -> R2 : Msg(P1, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role 1 missing parameter")
		public void noMissing() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(P1, out P2)",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(4, protocol.getMembers().size());
			SarlProtocolMessage message0 = assertInstanceOf(SarlProtocolMessage.class, protocol.getMembers().get(3));
			//
			assertEquals("R1", message0.getFrom());
			assertEquals("R2", message0.getTo());
			assertEquals("Msg", message0.getName());
			assertEquals(2, message0.getParameters().size());
			var parameter0 = assertInstanceOf(SarlProtocolParameter.class, message0.getParameters().get(0));
			assertEquals("P1", parameter0.getName());
			assertNull(parameter0.getType());
			assertContains(parameter0.getModifiers());
			var parameter1 = assertInstanceOf(SarlProtocolParameter.class, message0.getParameters().get(1));
			assertEquals("P2", parameter1.getName());
			assertNull(parameter1.getType());
			assertContains(parameter1.getModifiers(), "out");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Undefined role")
		public void undefinedRole() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P1",
					"  var P2",
					"  R1 -> R3 : Msg(P1, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R3' in the protocol")
			.assertNoErrors();
		}

	}

	@Nested
	@DisplayName("R -> R : M(P1 in, out P2)")
	public class MessageWithParametersModifiersTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Duplicate messages")
		public void duplicate2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(P1 in, out P2)",
					"  R1 -> R2 : Msg(P2 in, P1 out)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_DUPLICATE_MESSAGE,
					"Multiple declaration of the message 'Msg' from 'R1' to 'R2' in the protocol");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing 2 roles 1 parameter")
		public void missingR1R2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  var P2",
					"  R1 -> R2 : Msg(P1 in, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role")
		public void missingR1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(P1 in, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing first role 1 parameter")
		public void missingR1P1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R2",
					"  var P2",
					"  R1 -> R2 : Msg(P1 in, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R1' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role")
		public void missingR2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(P1 in, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Missing second role 1 parameter")
		public void missingR2P1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P2",
					"  R1 -> R2 : Msg(P1 in, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R2' in the protocol")
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role 1 missing parameter")
		public void missingP1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P2",
					"  R1 -> R2 : Msg(P1 in, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_PARAMETER,
					"Undefined parameter 'P1' in the protocol")
			.assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("No missing role 1 missing parameter")
		public void noMissing() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"protocol P1 {",
					"  role R1, R2",
					"  var P1",
					"  var P2",
					"  R1 -> R2 : Msg(P1 in, out P2)",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			SarlProtocol protocol = assertInstanceOf(SarlProtocol.class, mas.getXtendTypes().get(0));
			assertEquals(4, protocol.getMembers().size());
			SarlProtocolMessage message0 = assertInstanceOf(SarlProtocolMessage.class, protocol.getMembers().get(3));
			//
			assertEquals("R1", message0.getFrom());
			assertEquals("R2", message0.getTo());
			assertEquals("Msg", message0.getName());
			assertEquals(2, message0.getParameters().size());
			var parameter0 = assertInstanceOf(SarlProtocolParameter.class, message0.getParameters().get(0));
			assertEquals("P1", parameter0.getName());
			assertNull(parameter0.getType());
			assertContains(parameter0.getModifiers(), "in");
			var parameter1 = assertInstanceOf(SarlProtocolParameter.class, message0.getParameters().get(1));
			assertEquals("P2", parameter1.getName());
			assertNull(parameter1.getType());
			assertContains(parameter1.getModifiers(), "out");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Undefined role")
		public void undefinedRole() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"protocol P1 {",
					"  role R1",
					"  var P1",
					"  var P2",
					"  R1 -> R3 : Msg(P1 in, out P2)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
			.assertError(
					SarlPackage.eINSTANCE.getSarlProtocolMessage(),
					PROTOCOL_UNDEFINED_ROLE,
					"Undefined role 'R3' in the protocol")
			.assertNoErrors();
		}

	}

}
