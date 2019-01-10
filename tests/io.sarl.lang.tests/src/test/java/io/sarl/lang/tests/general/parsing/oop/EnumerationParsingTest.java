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
package io.sarl.lang.tests.general.parsing.oop;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.eclipse.xtext.common.types.JvmVisibility;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlEnumeration;
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
@RunWith(Suite.class)
@SuiteClasses({
	EnumerationParsingTest.TopEnumerationTest.class,
	EnumerationParsingTest.InsideClassTest.class,
	EnumerationParsingTest.InsideAgentTest.class,
	EnumerationParsingTest.InsideBehaviorTest.class,
	EnumerationParsingTest.InsideSkillTest.class,
})
@SuppressWarnings("all")
public class EnumerationParsingTest {

	public static class TopEnumerationTest extends AbstractSarlTest {

		protected SarlEnumeration getEnumeration(SarlScript script) {
			return (SarlEnumeration) script.getXtendTypes().get(0);
		}

		@Test
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public enum E1 { CST1 }"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertFalse(enumeration.isStatic());
		}

		@Test
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"enum E1 { CST1 }"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertFalse(enumeration.isStatic());
		}

		@Test
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"private enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"protected enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"package enum E1 { CST1 }"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.DEFAULT, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertFalse(enumeration.isStatic());
		}

		@Test
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"static enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"final enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"native enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"volatile enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"transient enum E1 { CST1 }"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public & package are permitted");
		}

		@Test
		public void classmodifier_abstract_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract final enum E1 { CST1 }"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlEnumeration(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The enum E1 can either be abstract or final, not both");
		}

		@Test
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public private enum E1 { CST1 }"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlEnumeration(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The enum E1 can only set one of public / package / protected / private");
		}

	}

	public static class InsideClassTest extends AbstractSarlTest {

		protected SarlEnumeration getEnumeration(SarlScript script) {
			SarlClass enclosing = (SarlClass) script.getXtendTypes().get(0);
			return (SarlEnumeration) enclosing.getMembers().get(0);
		}

		@Test
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  public enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  private enum E1 { CST1 }",
					"}"), false);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PRIVATE, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  protected enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PROTECTED, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  package enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.DEFAULT, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  abstract enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  static enum E1 { CST1 }",
					"}"), false);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  dispatch enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  final enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  strictfp enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  native enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  volatile enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  synchronized enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  transient enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public class EnclosingClass {",
					"  public private enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlEnumeration(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The enum E1 can only set one of public / package / protected / private");
		}

	}

	public static class InsideAgentTest extends AbstractSarlTest {

		protected SarlEnumeration getEnumeration(SarlScript script) {
			SarlAgent enclosing = (SarlAgent) script.getXtendTypes().get(0);
			return (SarlEnumeration) enclosing.getMembers().get(0);
		}

		@Test
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  public enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PROTECTED, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  private enum E1 { CST1 }",
					"}"), false);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PRIVATE, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  protected enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PROTECTED, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  package enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.DEFAULT, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  abstract enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  static enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PROTECTED, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  dispatch enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  final enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  strictfp enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  native enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  volatile enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  synchronized enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  transient enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent EnclosingAgent {",
					"  public private enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlEnumeration(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"public / package / protected / private");
		}

	}

	public static class InsideBehaviorTest extends AbstractSarlTest {

		protected SarlEnumeration getEnumeration(SarlScript script) {
			SarlBehavior enclosing = (SarlBehavior) script.getXtendTypes().get(0);
			return (SarlEnumeration) enclosing.getMembers().get(0);
		}

		@Test
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  public enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  private enum E1 { CST1 }",
					"}"), false);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PRIVATE, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  protected enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PROTECTED, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  package enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.DEFAULT, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  abstract enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  static enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  dispatch enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  final enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  strictfp enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  native enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  volatile enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  synchronized enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  transient enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public behavior EnclosingBehavior {",
					"  public private enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlEnumeration(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"The enum E1 can only set one of public / package / protected / private");
		}

	}

	public static class InsideSkillTest extends AbstractSarlTest {

		protected SarlEnumeration getEnumeration(SarlScript script) {
			SarlSkill enclosing = (SarlSkill) script.getXtendTypes().get(1);
			return (SarlEnumeration) enclosing.getMembers().get(0);
		}

		@Test
		public void classmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  public enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  private enum E1 { CST1 }",
					"}"), false);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PRIVATE, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  protected enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PROTECTED, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  package enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.DEFAULT, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  abstract enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  static enum E1 { CST1 }",
					"}"), true);
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlEnumeration enumeration = getEnumeration(mas);
			assertNotNull(enumeration);
			//
			assertEquals("E1", enumeration.getName());
			assertEquals(JvmVisibility.PUBLIC, enumeration.getVisibility());
			assertEquals(1, enumeration.getMembers().size());
			assertFalse(enumeration.isAnonymous());
			assertTrue(enumeration.isFinal());
			assertFalse(enumeration.isLocal());
			assertTrue(enumeration.isStatic());
		}

		@Test
		public void classmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  dispatch enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  final enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  strictfp enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  native enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  volatile enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  synchronized enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  transient enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlEnumeration(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the enum E1; only public, package, protected, private & static are permitted");
		}

		@Test
		public void classmodifier_public_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 { }",
					"public skill EnclosingSkill implements C1 {",
					"  public private enum E1 { CST1 }",
					"}"), false);
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlEnumeration(),
				org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
				"The enum E1 can only set one of public / package / protected / private");
		}

	}

}
