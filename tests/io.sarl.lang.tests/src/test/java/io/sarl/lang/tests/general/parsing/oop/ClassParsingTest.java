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
import static org.junit.jupiter.api.Assertions.assertNull;
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
@DisplayName("Syntax: class")
@Tag("core")
public class ClassParsingTest {

	@Nested
	@DisplayName("Syntax: class as top element")
	public class TopClassTest extends AbstractSarlTest {

		protected SarlClass getClazz(SarlScript script) {
			return (SarlClass) script.getXtendTypes().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class C1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertFalse(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertFalse(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"private class C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"protected class C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"package class C1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.DEFAULT, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertFalse(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"abstract class C1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertTrue(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertFalse(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"static class C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch class C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"final class C1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertTrue(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertFalse(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp class C1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertFalse(clazz.isStatic());
			assertTrue(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"native class C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"volatile class C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized class C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"transient class C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"abstract final class C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The class C1 can either be abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 { abstract def name() }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT,
					"The abstract method name in type C1 can only be defined by an abstract class");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public private class C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The class C1 can only set one of public / package / protected / private");
		}

		@Test
		@Tag("sarlValidation")
		public void duplicateTypeNames() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"class B1 {",
				"}",
				"class B2 {",
				"}",
				"class B1 {",
				"}"
			));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_TYPE_NAME,
				"Duplicate type B1");
		}

	}

	@Nested
	@DisplayName("Syntax: class inside class")
	public class InsideClassTest extends AbstractSarlTest {

		protected SarlClass getClazz(SarlScript script) {
			SarlClass enclosing = (SarlClass) script.getXtendTypes().get(0);
			return (SarlClass) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  public static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
					"Nested classes must be static");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  private static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PRIVATE, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  protected static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PROTECTED, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  package static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.DEFAULT, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  abstract static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertTrue(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  dispatch static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  final static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertTrue(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  strictfp static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertTrue(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  native class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  volatile static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  synchronized static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  transient static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  abstract final static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The class C1 can either be abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  static class C1 { abstract def name() }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT,
					"The abstract method name in type C1 can only be defined by an abstract class");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  public private static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The class C1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: class inside agent")
	public class InsideAgentTest extends AbstractSarlTest {

		protected SarlClass getClazz(SarlScript script) {
			SarlAgent enclosing = (SarlAgent) script.getXtendTypes().get(0);
			return (SarlClass) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  public static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  private static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PRIVATE, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  protected static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PROTECTED, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  package static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.DEFAULT, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  abstract static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PROTECTED, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertTrue(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PROTECTED, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  dispatch static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  final static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PROTECTED, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertTrue(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  strictfp static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  native class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  volatile static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  synchronized static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  transient static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  abstract final static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"can either be abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  static class C1 { abstract def name() }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  public private static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: class inside behavior")
	public class InsideBehaviorTest extends AbstractSarlTest {

		protected SarlClass getClazz(SarlScript script) {
			SarlBehavior enclosing = (SarlBehavior) script.getXtendTypes().get(0);
			return (SarlClass) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  public static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  private static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PRIVATE, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  protected static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PROTECTED, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  package static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.DEFAULT, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  abstract static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertTrue(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  dispatch static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  final static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertTrue(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  strictfp static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertTrue(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  native class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  volatile static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  synchronized static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  transient static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  abstract final static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The class C1 can either be abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  static class C1 { abstract def name() }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  public private static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The class C1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: class inside skill")
	public class InsideSkillTest extends AbstractSarlTest {

		protected SarlClass getClazz(SarlScript script) {
			SarlSkill enclosing = (SarlSkill) script.getXtendTypes().get(1);
			return (SarlClass) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  public static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  private static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PRIVATE, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  protected static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PROTECTED, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  package static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.DEFAULT, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  abstract static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertTrue(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  dispatch static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  final static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertTrue(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertFalse(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  strictfp static class C1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = getClazz(mas);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertNull(clazz.getExtends());
			assertEquals(JvmVisibility.PUBLIC, clazz.getVisibility());
			assertEquals(0, clazz.getMembers().size());
			assertFalse(clazz.isAbstract());
			assertFalse(clazz.isAnonymous());
			assertFalse(clazz.isFinal());
			assertFalse(clazz.isLocal());
			assertTrue(clazz.isStatic());
			assertTrue(clazz.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  native class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  volatile static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  synchronized static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  transient static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  abstract final static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The class C1 can either be abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  static class C1 { abstract def name() }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  public private static class C1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The class C1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: class general tests")
	public class GenericTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void classGeneric_X() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X> {",
					"	var x : X",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			//
			assertEquals(1, clazz.getTypeParameters().size());
			//
			JvmTypeParameter parameter = clazz.getTypeParameters().get(0);
			assertEquals("X", parameter.getName());
			assertNullOrEmpty(parameter.getConstraints());
		}

		@Test
		@Tag("sarlParsing")
		public void classGeneric_XextendsNumber() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X extends Number> {",
					"	var x : X",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			//
			assertEquals(1, clazz.getTypeParameters().size());
			//
			JvmTypeParameter parameter = clazz.getTypeParameters().get(0);
			assertEquals("X", parameter.getName());
			assertEquals(1, parameter.getConstraints().size());
			//
			JvmTypeConstraint constraint = parameter.getConstraints().get(0);
			assertEquals("java.lang.Number", constraint.getTypeReference().getIdentifier());
			assertTrue(constraint.getIdentifier().startsWith("extends"));
		}

		@Test
		@Tag("sarlParsing")
		public void classGeneric_XY() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X,Y> {",
					"	var x : X",
					"	def getY : Y { null }",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			//
			assertEquals(2, clazz.getTypeParameters().size());
			//
			JvmTypeParameter parameter1 = clazz.getTypeParameters().get(0);
			assertEquals("X", parameter1.getName());
			assertNullOrEmpty(parameter1.getConstraints());
			//
			JvmTypeParameter parameter2 = clazz.getTypeParameters().get(1);
			assertEquals("Y", parameter2.getName());
			assertNullOrEmpty(parameter2.getConstraints());
		}

		@Test
		@Tag("sarlParsing")
		public void classGeneric_XYextendsX() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X,Y extends X> {",
					"	var x : X",
					"	def getY : Y { null }",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			//
			assertEquals(2, clazz.getTypeParameters().size());
			//
			JvmTypeParameter parameter1 = clazz.getTypeParameters().get(0);
			assertEquals("X", parameter1.getName());
			assertNullOrEmpty(parameter1.getConstraints());
			//
			JvmTypeParameter parameter2 = clazz.getTypeParameters().get(1);
			assertEquals("Y", parameter2.getName());
			assertEquals(1, parameter2.getConstraints().size());
			//
			JvmTypeConstraint constraint = parameter2.getConstraints().get(0);
			assertEquals("X", constraint.getTypeReference().getIdentifier());
			assertTrue(constraint.getIdentifier().startsWith("extends"));
		}

		@Test
		@Tag("sarlValidation")
		public void classGeneric_XextendsYY() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X extends Y,Y> {",
					"	var x : X",
					"	def getY : Y { null }",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
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
					"class C1 {",
					"	def setX(param : X) : void with X { var xxx : X }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertEquals(1, clazz.getMembers().size());
			//
			SarlAction action = (SarlAction) clazz.getMembers().get(0);
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
					"class C1 {",
					"	def <X> setX(param : X) : void { var xxx : X }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertEquals(1, clazz.getMembers().size());
			//
			SarlAction action = (SarlAction) clazz.getMembers().get(0);
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
					"class C1 {",
					"	def setX(param : X) : void with X extends Number { var xxx : X }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertEquals(1, clazz.getMembers().size());
			//
			SarlAction action = (SarlAction) clazz.getMembers().get(0);
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
					"class C1 {",
					"	def <X extends Number> setX(param : X) : void { var xxx : X }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertEquals(1, clazz.getMembers().size());
			//
			SarlAction action = (SarlAction) clazz.getMembers().get(0);
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
					"class C1 {",
					"	def setX(param : X) : void with X, Y { var xxx : X; var yyy : Y }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertEquals(1, clazz.getMembers().size());
			//
			SarlAction action = (SarlAction) clazz.getMembers().get(0);
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
					"class C1 {",
					"	def <X, Y> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertEquals(1, clazz.getMembers().size());
			//
			SarlAction action = (SarlAction) clazz.getMembers().get(0);
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
					"class C1 {",
					"	def setX(param : X) : void with X, Y extends X { var xxx : X; var yyy : Y }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertEquals(1, clazz.getMembers().size());
			//
			SarlAction action = (SarlAction) clazz.getMembers().get(0);
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
					"class C1 {",
					"	def <X, Y extends X> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlClass clazz = (SarlClass) mas.getXtendTypes().get(0);
			assertNotNull(clazz);
			//
			assertEquals("C1", clazz.getName());
			assertEquals(1, clazz.getMembers().size());
			//
			SarlAction action = (SarlAction) clazz.getMembers().get(0);
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
