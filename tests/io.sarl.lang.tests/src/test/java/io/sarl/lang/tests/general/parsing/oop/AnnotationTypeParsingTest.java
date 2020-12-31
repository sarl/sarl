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

import static io.sarl.tests.api.tools.TestAssertions.assertContains;
import static io.sarl.tests.api.tools.TestAssertions.assertTypeReferenceIdentifier;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlField;
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
@DisplayName("Syntax: annotation")
@Tag("core")
@Tag("sarlParsing")
public class AnnotationTypeParsingTest extends AbstractSarlTest {

	@Nested
	@DisplayName("Syntax: annotation as top element")
	public class TopAnnotationTypeTest extends AbstractSarlTest {

		protected SarlAnnotationType getAnnotationType(SarlScript script) {
			return (SarlAnnotationType) script.getXtendTypes().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public annotation A1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertFalse(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"annotation A1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertFalse(annotationType.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"private annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"protected annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package & abstract are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"package annotation A1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.DEFAULT, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertFalse(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"abstract annotation A1 { }"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertFalse(annotationType.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"static annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"final annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"native annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"volatile annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"transient annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"abstract final annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlAnnotationType(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The annotation type A1 can either be abstract or final, not both");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public private annotation A1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlAnnotationType(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The annotation type A1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: annotation inside class")
	public class InsideClassTest extends AbstractSarlTest {

		protected SarlAnnotationType getAnnotationType(SarlScript script) {
			SarlClass enclosing = (SarlClass) script.getXtendTypes().get(0);
			return (SarlAnnotationType) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  public annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  private annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PRIVATE, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  protected annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PROTECTED, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  package annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.DEFAULT, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  abstract annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  static annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  dispatch annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  final annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  strictfp annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  native annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  volatile annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  synchronized annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  transient annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  public private annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlAnnotationType(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The annotation type A1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: annotation inside agent")
	public class InsideAgentTest extends AbstractSarlTest {

		protected SarlAnnotationType getAnnotationType(SarlScript script) {
			SarlAgent enclosing = (SarlAgent) script.getXtendTypes().get(0);
			return (SarlAnnotationType) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  public annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PROTECTED, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  private annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PRIVATE, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  protected annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PROTECTED, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  package annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.DEFAULT, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  abstract annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PROTECTED, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  static annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PROTECTED, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  dispatch annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  final annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  strictfp annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  native annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  volatile annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  synchronized annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  transient annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  public private annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlAnnotationType(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: annotation inside behavior")
	public class InsideBehaviorTest extends AbstractSarlTest {

		protected SarlAnnotationType getAnnotationType(SarlScript script) {
			SarlBehavior enclosing = (SarlBehavior) script.getXtendTypes().get(0);
			return (SarlAnnotationType) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  public annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  private annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PRIVATE, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  protected annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PROTECTED, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  package annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.DEFAULT, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  abstract annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  static annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  dispatch annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  final annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  strictfp annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  native annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  volatile annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  synchronized annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  transient annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  public private annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlAnnotationType(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The annotation type A1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	@DisplayName("Syntax: annotation inside skill")
	public class InsideSkillTest extends AbstractSarlTest {

		protected SarlAnnotationType getAnnotationType(SarlScript script) {
			SarlSkill enclosing = (SarlSkill) script.getXtendTypes().get(1);
			return (SarlAnnotationType) enclosing.getMembers().get(0);
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  public annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  private annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PRIVATE, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  protected annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PROTECTED, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  package annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.DEFAULT, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  abstract annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  static annotation A1 { }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlAnnotationType annotationType = getAnnotationType(mas);
			assertNotNull(annotationType);
			//
			assertEquals("A1", annotationType.getName());
			assertEquals(JvmVisibility.PUBLIC, annotationType.getVisibility());
			assertEquals(0, annotationType.getMembers().size());
			assertFalse(annotationType.isAnonymous());
			assertFalse(annotationType.isFinal());
			assertFalse(annotationType.isLocal());
			assertTrue(annotationType.isStatic());
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  dispatch annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  final annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  strictfp annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  native annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  volatile annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  synchronized annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  transient annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAnnotationType(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the annotation type A1; only public, package, protected, private, static & abstract are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  public private annotation A1 { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlAnnotationType(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The annotation type A1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	public class UsageTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void varUsage() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"annotation A1 {",
					"	var field1 : String",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the field field1; only public, static, final & val are permitted");
		}
	
		@Test
		@Tag("sarlParsing")
		public void valUsage_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"annotation A1 {",
					"	val field1 : String",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAnnotationType annotation = (SarlAnnotationType) mas.getXtendTypes().get(0);
			assertEquals("A1", annotation.getName());
			assertEquals(1, annotation.getMembers().size());
			//
			SarlField field = (SarlField) annotation.getMembers().get(0);
			assertEquals("field1", field.getName());
			assertTypeReferenceIdentifier(field.getType(), "java.lang.String");
			assertContains(field.getModifiers(), "val");
		}
	
		@Test
		@Tag("sarlParsing")
		public void valUsage_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"annotation A1 {",
					"	val field1 = \"a\"",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAnnotationType annotation = (SarlAnnotationType) mas.getXtendTypes().get(0);
			assertEquals("A1", annotation.getName());
			assertEquals(1, annotation.getMembers().size());
			//
			SarlField field = (SarlField) annotation.getMembers().get(0);
			assertEquals("field1", field.getName());
			assertNull(field.getType());
			assertContains(field.getModifiers(), "val");
		}

	}

}
