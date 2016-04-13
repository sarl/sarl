/*
 * Copyright (C) 2014-2015 the original authors or authors.
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
package io.sarl.lang.tests.parsing.oop;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.eclipse.xtext.common.types.JvmTypeConstraint;

import org.eclipse.xtext.common.types.JvmTypeParameter;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
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
	ClassParsingTest.TopClassTest.class,
	ClassParsingTest.InsideClassTest.class,
	ClassParsingTest.InsideAgentTest.class,
	ClassParsingTest.InsideBehaviorTest.class,
	ClassParsingTest.InsideSkillTest.class,
	ClassParsingTest.GenericTest.class,
})
@SuppressWarnings("all")
public class ClassParsingTest {

	public static class TopClassTest extends AbstractSarlTest {

		protected SarlClass getClazz(SarlScript script) {
			return (SarlClass) script.getXtendTypes().get(0);
		}

		@Test
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class C1 { }"), true);
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
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 { }"), true);
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
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"private class C1 { }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 7,
					"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"protected class C1 { }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 9,
					"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"package class C1 { }"), true);
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
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract class C1 { }"), true);
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
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"static class C1 { }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 6,
					"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch class C1 { }"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				32, 8,
				"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"final class C1 { }"), true);
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
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp class C1 { }"), true);
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
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"native class C1 { }"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				32, 6,
				"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"volatile class C1 { }"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				32, 8,
				"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized class C1 { }"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				32, 12,
				"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"transient class C1 { }"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				32, 9,
				"Illegal modifier for the class C1; only public, package, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract final class C1 { }"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				41, 5,
				"The class C1 can either be abstract or final, not both");
		}

		@Test
		public void classmodifier_abstract_action() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 { abstract def name() }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT,
					56, 4,
					"Invalid implemented type", "name");
		}

		@Test
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public private class C1 { }"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				39, 7,
				"The class C1 can only set one of public / package / protected / private");
		}

	}

	public static class InsideClassTest extends AbstractSarlTest {

		protected SarlClass getClazz(SarlScript script) {
			SarlClass enclosing = (SarlClass) script.getXtendTypes().get(0);
			return (SarlClass) enclosing.getMembers().get(0);
		}

		@Test
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  public static class C1 { }",
					"}"), true);
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
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  class C1 { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
					70, 2,
					"Nested classes must be static");
		}

		@Test
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  private static class C1 { }",
					"}"), false);
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
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  protected static class C1 { }",
					"}"), true);
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
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  package static class C1 { }",
					"}"), true);
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
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  abstract static class C1 { }",
					"}"), true);
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
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  static class C1 { }",
					"}"), false);
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
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  dispatch static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				64, 8,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  final static class C1 { }",
					"}"), true);
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
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  strictfp static class C1 { }",
					"}"), true);
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
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  native class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				64, 6,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  volatile static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				64, 8,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  synchronized static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				64, 12,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  transient static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				64, 9,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  abstract final static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				73, 5,
				"The class C1 can either be abstract or final, not both");
		}

		@Test
		public void classmodifier_abstract_action() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  static class C1 { abstract def name() }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT,
					95, 4,
					"Invalid implemented type", "name");
		}

		@Test
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  public private static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				71, 7,
				"The class C1 can only set one of public / package / protected / private");
		}

	}

	public static class InsideAgentTest extends AbstractSarlTest {

		protected SarlClass getClazz(SarlScript script) {
			SarlAgent enclosing = (SarlAgent) script.getXtendTypes().get(0);
			return (SarlClass) enclosing.getMembers().get(0);
		}

		@Test
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  public static class C1 { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					64, 6);
		}

		@Test
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  class C1 { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
					70, 2);
		}

		@Test
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  private static class C1 { }",
					"}"), false);
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
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  protected static class C1 { }",
					"}"), true);
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
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  package static class C1 { }",
					"}"), true);
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
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  abstract static class C1 { }",
					"}"), true);
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
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  static class C1 { }",
					"}"), false);
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
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  dispatch static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				64, 8);
		}

		@Test
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  final static class C1 { }",
					"}"), true);
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
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  strictfp static class C1 { }",
					"}"), true);
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
			assertTrue(clazz.isStrictFloatingPoint());
		}

		@Test
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  native class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				64, 6);
		}

		@Test
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  volatile static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				64, 8);
		}

		@Test
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  synchronized static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				64, 12);
		}

		@Test
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  transient static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				64, 9);
		}

		@Test
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  abstract final static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				73, 5,
				"can either be abstract or final, not both");
		}

		@Test
		public void classmodifier_abstract_action() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  static class C1 { abstract def name() }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT,
					95, 4);
		}

		@Test
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  public private static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				71, 7,
				"public / package / protected / private");
		}

	}

	public static class InsideBehaviorTest extends AbstractSarlTest {

		protected SarlClass getClazz(SarlScript script) {
			SarlBehavior enclosing = (SarlBehavior) script.getXtendTypes().get(0);
			return (SarlClass) enclosing.getMembers().get(0);
		}

		@Test
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  public static class C1 { }",
					"}"), true);
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
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  class C1 { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
					76, 2);
		}

		@Test
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  private static class C1 { }",
					"}"), false);
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
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  protected static class C1 { }",
					"}"), true);
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
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  package static class C1 { }",
					"}"), true);
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
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  abstract static class C1 { }",
					"}"), true);
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
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  static class C1 { }",
					"}"), false);
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
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  dispatch static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				70, 8,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  final static class C1 { }",
					"}"), true);
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
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  strictfp static class C1 { }",
					"}"), true);
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
			assertTrue(clazz.isStrictFloatingPoint());
		}

		@Test
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  native class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				70, 6,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  volatile static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				70, 8,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  synchronized static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				70, 12,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  transient static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				70, 9,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  abstract final static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				79, 5,
				"The class C1 can either be abstract or final, not both");
		}

		@Test
		public void classmodifier_abstract_action() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  static class C1 { abstract def name() }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT,
					101, 4);
		}

		@Test
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  public private static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				77, 7,
				"The class C1 can only set one of public / package / protected / private");
		}

	}

	public static class InsideSkillTest extends AbstractSarlTest {

		protected SarlClass getClazz(SarlScript script) {
			SarlSkill enclosing = (SarlSkill) script.getXtendTypes().get(1);
			return (SarlClass) enclosing.getMembers().get(0);
		}

		@Test
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  public static class C1 { }",
					"}"), true);
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
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  class C1 { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlClass(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER,
					107, 2);
		}

		@Test
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  private static class C1 { }",
					"}"), false);
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
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  protected static class C1 { }",
					"}"), true);
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
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  package static class C1 { }",
					"}"), true);
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
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  abstract static class C1 { }",
					"}"), true);
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
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  static class C1 { }",
					"}"), false);
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
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  dispatch static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				101, 8,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  final static class C1 { }",
					"}"), true);
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
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  strictfp static class C1 { }",
					"}"), true);
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
			assertTrue(clazz.isStrictFloatingPoint());
		}

		@Test
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  native class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				101, 6,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  volatile static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				101, 8,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  synchronized static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				101, 12,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  transient static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				101, 9,
				"Illegal modifier for the class C1; only public, package, protected, private, static, final, abstract & strictfp are permitted");
		}

		@Test
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  abstract final static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				110, 5,
				"The class C1 can either be abstract or final, not both");
		}

		@Test
		public void classmodifier_abstract_action() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  static class C1 { abstract def name() }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT,
					132, 4);
		}

		@Test
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  public private static class C1 { }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				108, 7,
				"The class C1 can only set one of public / package / protected / private");
		}

	}

	public static class GenericTest extends AbstractSarlTest {

		@Test
		public void classGeneric_X() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X> {",
					"	var x : X",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}"), true);
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
		public void classGeneric_XextendsNumber() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X extends Number> {",
					"	var x : X",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}"), true);
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
		public void classGeneric_XY() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X,Y> {",
					"	var x : X",
					"	def getY : Y { null }",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}"), true);
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
		public void classGeneric_XYextendsX() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X,Y extends X> {",
					"	var x : X",
					"	def getY : Y { null }",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}"), true);
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
		public void classGeneric_XextendsYY() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X extends Y,Y> {",
					"	var x : X",
					"	def getY : Y { null }",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}"), false);
			validate(mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.TYPE_PARAMETER_FORWARD_REFERENCE,
					51, 1,
					"Illegal forward reference to type parameter Y");
		}

		@Test
		public void functionGeneric_X_sarlNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X { var xxx : X }",
					"}"), true);
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
		public void functionGeneric_X_javaNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X> setX(param : X) : void { var xxx : X }",
					"}"), true);
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
		public void functionGeneric_XextendsNumber_sarlNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X extends Number { var xxx : X }",
					"}"), true);
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
		public void functionGeneric_XextendsNumber_javaNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X extends Number> setX(param : X) : void { var xxx : X }",
					"}"), true);
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
		public void functionGeneric_XY_sarlNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X, Y { var xxx : X; var yyy : Y }",
					"}"), true);
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
		public void functionGeneric_XY_javaNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X, Y> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}"), true);
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
		public void functionGeneric_XYextendsX_sarlNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X, Y extends X { var xxx : X; var yyy : Y }",
					"}"), true);
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
		public void functionGeneric_XYextendsX_javaNotation() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X, Y extends X> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}"), true);
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
