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
package io.sarl.lang.tests.general.parsing.oop;

import static io.sarl.tests.api.tools.TestAssertions.assertNullOrEmpty;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.eclipse.xtext.common.types.JvmTypeConstraint;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: interface")
@Tag("core")
public class InterfaceParsingTest {

	@Nested
	@DisplayName("Syntax: interface as top element")
	public class TopInterfaceTest extends AbstractSarlTest {

		protected SarlInterface getInterface(SarlScript script) {
			return (SarlInterface) script.getXtendTypes().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public interface I1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertFalse(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertFalse(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"private interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlInterface(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the interface I1; only public, package, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"protected interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlInterface(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the interface I1; only public, package, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"package interface I1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.DEFAULT, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertFalse(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"abstract interface I1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertFalse(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"static interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlInterface(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the interface I1; only public, package, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"final interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlInterface(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the interface I1; only public, package, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp interface I1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertFalse(interf.isStatic());
			assertTrue(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"native interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"volatile interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"transient interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"abstract final interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The interface I1 can either be abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public private interface I1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The interface I1 can only set one of public / package / protected / private");
		}

		@Test
		@Tag("sarlValidation")
		public void method_0() throws Exception {
			SarlScript mas = file(getParseHelper(), "interface I1 { def fct(a : int) : float { a + 1f } }");
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		public void method_5() throws Exception {
			SarlScript mas = file(getParseHelper(), "interface I1 { def fct(a : int = 6) : float { a + 1f } }");
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

	}

	@Nested
	@DisplayName("Syntax: interface inside class")
	public class InsideClassTest extends AbstractSarlTest {

		protected SarlInterface getInterface(SarlScript script) {
			SarlClass enclosing = (SarlClass) script.getXtendTypes().get(0);
			return (SarlInterface) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  public interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  private interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PRIVATE, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  protected interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PROTECTED, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  package interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.DEFAULT, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  abstract interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  static interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  dispatch interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  final interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlInterface(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  strictfp interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertTrue(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  native interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  volatile interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  synchronized interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  transient interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  abstract final interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The interface I1 can either be abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  public private interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The interface I1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: interface inside agent")
	public class InsideAgentTest extends AbstractSarlTest {

		protected SarlInterface getInterface(SarlScript script) {
			SarlAgent enclosing = (SarlAgent) script.getXtendTypes().get(0);
			return (SarlInterface) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  public interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlInterface(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PROTECTED, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  private interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PRIVATE, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  protected interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PROTECTED, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  package interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.DEFAULT, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  abstract interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PROTECTED, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  static interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PROTECTED, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  dispatch interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  final interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlInterface(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  strictfp interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlInterface(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  native interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  volatile interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  synchronized interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  transient interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  abstract final interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  public private interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: interface inside behavior")
	public class InsideBehaviorTest extends AbstractSarlTest {

		protected SarlInterface getInterface(SarlScript script) {
			SarlBehavior enclosing = (SarlBehavior) script.getXtendTypes().get(0);
			return (SarlInterface) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  public interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  private interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PRIVATE, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  protected interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PROTECTED, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  package interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.DEFAULT, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  abstract interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  static interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  dispatch interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  final interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlInterface(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  strictfp interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertTrue(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  native interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  volatile interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  synchronized interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  transient interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  abstract final interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The interface I1 can either be abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  public private interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The interface I1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: interfacce inside skill")
	public class InsideSkillTest extends AbstractSarlTest {

		protected SarlInterface getInterface(SarlScript script) {
			SarlSkill enclosing = (SarlSkill) script.getXtendTypes().get(1);
			return (SarlInterface) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  public interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  private interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PRIVATE, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  protected interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PROTECTED, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  package interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.DEFAULT, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  abstract interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  static interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertFalse(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  dispatch interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  final interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlInterface(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  strictfp interface I1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = getInterface(mas);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertNullOrEmpty(interf.getExtends());
			assertEquals(JvmVisibility.PUBLIC, interf.getVisibility());
			assertEquals(0, interf.getMembers().size());
			assertFalse(interf.isAnonymous());
			assertFalse(interf.isFinal());
			assertFalse(interf.isLocal());
			assertTrue(interf.isStatic());
			assertTrue(interf.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  native interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  volatile interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  synchronized interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  transient interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the interface I1; only public, package, protected, private, static, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  abstract final interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The interface I1 can either be abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  public private interface I1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlInterface(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The interface I1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: interface general tests")
	public class GenericTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void interfaceGeneric_X() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1<X> {",
					"	def setX(param : X)",
					"	def getX : X",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			//
			assertEquals(1, interf.getTypeParameters().size());
			//
			JvmTypeParameter parameter = interf.getTypeParameters().get(0);
			assertEquals("X", parameter.getName());
			assertNullOrEmpty(parameter.getConstraints());
		}

		@Test
		@Tag("sarlParsing")
		public void interfaceGeneric_XextendsNumber() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1<X extends Number> {",
					"	def setX(param : X)",
					"	def getX : X",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			//
			assertEquals(1, interf.getTypeParameters().size());
			//
			JvmTypeParameter parameter = interf.getTypeParameters().get(0);
			assertEquals("X", parameter.getName());
			assertEquals(1, parameter.getConstraints().size());
			//
			JvmTypeConstraint constraint = parameter.getConstraints().get(0);
			assertEquals("java.lang.Number", constraint.getTypeReference().getIdentifier());
			assertTrue(constraint.getIdentifier().startsWith("extends"));
		}

		@Test
		@Tag("sarlParsing")
		public void interfaceGeneric_XY() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1<X,Y> {",
					"	def getY : Y",
					"	def setX(param : X)",
					"	def getX : X",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			//
			assertEquals(2, interf.getTypeParameters().size());
			//
			JvmTypeParameter parameter1 = interf.getTypeParameters().get(0);
			assertEquals("X", parameter1.getName());
			assertNullOrEmpty(parameter1.getConstraints());
			//
			JvmTypeParameter parameter2 = interf.getTypeParameters().get(1);
			assertEquals("Y", parameter2.getName());
			assertNullOrEmpty(parameter2.getConstraints());
		}

		@Test
		@Tag("sarlParsing")
		public void interfaceGeneric_XYextendsX() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1<X,Y extends X> {",
					"	def getY : Y",
					"	def setX(param : X)",
					"	def getX : X",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			//
			assertEquals(2, interf.getTypeParameters().size());
			//
			JvmTypeParameter parameter1 = interf.getTypeParameters().get(0);
			assertEquals("X", parameter1.getName());
			assertNullOrEmpty(parameter1.getConstraints());
			//
			JvmTypeParameter parameter2 = interf.getTypeParameters().get(1);
			assertEquals("Y", parameter2.getName());
			assertEquals(1, parameter2.getConstraints().size());
			//
			JvmTypeConstraint constraint = parameter2.getConstraints().get(0);
			assertEquals("X", constraint.getTypeReference().getIdentifier());
			assertTrue(constraint.getIdentifier().startsWith("extends"));
		}

		@Test
		@Tag("sarlValidation")
		public void interfaceGeneric_XextendsYY() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1<X extends Y,Y> {",
					"	def getY : Y",
					"	def setX(param : X)",
					"	def getX : X",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.TYPE_PARAMETER_FORWARD_REFERENCE,
					"Illegal forward reference to type parameter Y");
		}

		@Test
		@Tag("sarlParsing")
		public void functionGeneric_X_sarlNotation() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def setX(param : X) : void with X",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertEquals(1, interf.getMembers().size());
			//
			SarlAction action = (SarlAction) interf.getMembers().get(0);
			assertEquals("setX", action.getName());
			assertEquals(1, action.getTypeParameters().size());
			//
			JvmTypeParameter parameter = action.getTypeParameters().get(0);
			assertEquals("X", parameter.getName());
			assertNullOrEmpty(parameter.getConstraints());
		}

		@Test
		@Tag("sarlParsing")
		public void functionGeneric_X_javaNotation() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def <X> setX(param : X) : void",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertEquals(1, interf.getMembers().size());
			//
			SarlAction action = (SarlAction) interf.getMembers().get(0);
			assertEquals("setX", action.getName());
			assertEquals(1, action.getTypeParameters().size());
			//
			JvmTypeParameter parameter = action.getTypeParameters().get(0);
			assertEquals("X", parameter.getName());
			assertNullOrEmpty(parameter.getConstraints());
		}

		@Test
		@Tag("sarlParsing")
		public void functionGeneric_XextendsNumber_sarlNotation() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def setX(param : X) : void with X extends Number",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertEquals(1, interf.getMembers().size());
			//
			SarlAction action = (SarlAction) interf.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void functionGeneric_XextendsNumber_javaNotation() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def <X extends Number> setX(param : X) : void",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertEquals(1, interf.getMembers().size());
			//
			SarlAction action = (SarlAction) interf.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void functionGeneric_XY_sarlNotation() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def setX(param : X) : void with X, Y",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertEquals(1, interf.getMembers().size());
			//
			SarlAction action = (SarlAction) interf.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void functionGeneric_XY_javaNotation() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def <X, Y> setX(param : X) : void",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertEquals(1, interf.getMembers().size());
			//
			SarlAction action = (SarlAction) interf.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void functionGeneric_XYextendsX_sarlNotation() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def setX(param : X) : void with X, Y extends X",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertEquals(1, interf.getMembers().size());
			//
			SarlAction action = (SarlAction) interf.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void functionGeneric_XYextendsX_javaNotation() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"interface I1 {",
					"	def <X, Y extends X> setX(param : X) : void",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlInterface interf = (SarlInterface) mas.getXtendTypes().get(0);
			assertNotNull(interf);
			//
			assertEquals("I1", interf.getName());
			assertEquals(1, interf.getMembers().size());
			//
			SarlAction action = (SarlAction) interf.getMembers().get(0);
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
