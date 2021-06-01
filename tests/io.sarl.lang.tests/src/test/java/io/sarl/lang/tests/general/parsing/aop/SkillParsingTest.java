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

import static io.sarl.tests.api.tools.TestAssertions.assertNoMoreIssues;
import static io.sarl.tests.api.tools.TestAssertions.assertNullOrEmpty;
import static io.sarl.tests.api.tools.TestAssertions.assertParameterDefaultValues;
import static io.sarl.tests.api.tools.TestAssertions.assertParameterNames;
import static io.sarl.tests.api.tools.TestAssertions.assertParameterTypes;
import static io.sarl.tests.api.tools.TestAssertions.assertTypeReferenceIdentifier;
import static io.sarl.tests.api.tools.TestAssertions.assertTypeReferenceIdentifiers;
import static io.sarl.tests.api.tools.TestAssertions.assertWarning;
import static io.sarl.tests.api.tools.TestAssertions.assertXExpression;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestEObjects.issues;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import com.google.common.base.Strings;
import org.eclipse.xtext.common.types.JvmTypeConstraint;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: skill")
@Tag("core")
public class SkillParsingTest {

	@Nested
	public class TopElementTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void capacityDirectImplementation() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Capacity",
					"skill S1 implements Capacity {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					"Invalid implemented type: 'io.sarl.lang.core.Capacity'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		@Tag("sarlValidation")
		public void redundantCapacity_fromSuperType() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"skill S1 implements C1 { }",
					"skill S2 extends S1 implements C2, C1 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
					"The feature 'C1' is already implemented by the super-type 'S1'.");
		}

		@Test
		@Tag("sarlValidation")
		public void redundantCapacity_duplicate() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 {}",
					"skill S1 implements C1 { }",
					"skill S2 extends S1 implements C2, C3, C2 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
					"Duplicate implemented feature 'C2'");
		}

		@Test
		@Tag("sarlValidation")
		public void redundantCapacity_fromPreviousCapacity() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 extends C2 {}",
					"skill S1 implements C1 { }",
					"skill S2 extends S1 implements C3, C2 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
					"The feature 'C2' is already implemented by the super-type 'C3'.");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidSkillExtend_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"}",
					"agent A1 {",
					"}",
					"skill S1 extends A1 implements C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Skill'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidSkillExtend_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 extends C1 implements C2 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED,
					"Invalid supertype. Expecting a class");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidSkillImplement_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"}",
					"skill S1 implements B1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidSkillImplement_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 implements B1, C1, C2 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidSkillImplement_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 implements C1, B1, C2 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidSkillImplement_3() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 implements C1, C2, B1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		@Tag("sarlParsing")
		public void skillImplementCapacity() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def myaction",
					"}",
					"skill S1 implements C1 {",
					"	def myaction { }",
					"}"
					));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getExtends());
			assertEquals(1, capacity.getMembers().size());
			//
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertEquals("myaction", signature.getName());
			assertTypeReferenceIdentifiers(signature.getFiredEvents());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(signature.getParameters());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action = (SarlAction) skill.getMembers().get(0);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			assertParameterNames(action.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void skillExtendSkill() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def myaction",
					"}",
					"skill S1 implements C1 {",
					"	def myaction { }",
					"}",
					"skill S2 extends S1 {",
					"	def myaction { }",
					"}"
					));
			assertEquals(3, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity.getName());
			assertTrue(capacity.getExtends().isEmpty());
			assertEquals(1, capacity.getMembers().size());
			//
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertEquals("myaction", signature.getName());
			assertTrue(signature.getFiredEvents().isEmpty());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(signature.getParameters());
			//
			SarlSkill skill1 = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill1.getName());
			assertTrue(capacity.getExtends().isEmpty());
			assertTypeReferenceIdentifiers(skill1.getImplements(), "C1");
			assertEquals(1, skill1.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill1.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTrue(signature.getFiredEvents().isEmpty());
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters());
			//
			SarlSkill skill2 = (SarlSkill) mas.getXtendTypes().get(2);
			assertEquals("S2", skill2.getName());
			assertTypeReferenceIdentifier(skill2.getExtends(), "S1");
			assertTrue(skill2.getImplements().isEmpty());
			assertEquals(1, skill2.getMembers().size());
			//
			SarlAction action2 = (SarlAction) skill2.getMembers().get(0);
			assertEquals("myaction", action2.getName());
			assertTrue(signature.getFiredEvents().isEmpty());
			assertTypeReferenceIdentifier(action2.getReturnType(), "void");
			assertParameterNames(action2.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void skillExtendSkillImplementCapacity() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def myaction",
					"}",
					"capacity C2 {",
					"	def myaction2",
					"}",
					"skill S1 implements C1 {",
					"	def myaction { }",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	def myaction { }",
					"	def myaction2 { }",
					"}"
					));
			assertEquals(4, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity1 = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity1.getName());
			assertTrue(capacity1.getExtends().isEmpty());
			assertEquals(1, capacity1.getMembers().size());
			//
			SarlAction signature1 = (SarlAction) capacity1.getMembers().get(0);
			assertEquals("myaction", signature1.getName());
			assertTrue(signature1.getFiredEvents().isEmpty());
			assertTypeReferenceIdentifier(signature1.getReturnType(), "void");
			assertParameterNames(signature1.getParameters());
			//
			SarlCapacity capacity2 = (SarlCapacity) mas.getXtendTypes().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getExtends());
			assertEquals(1, capacity2.getMembers().size());
			//
			SarlAction signature2 = (SarlAction) capacity2.getMembers().get(0);
			assertEquals("myaction2", signature2.getName());
			assertTypeReferenceIdentifiers(signature2.getFiredEvents());
			assertTypeReferenceIdentifier(signature2.getReturnType(), "void");
			assertParameterNames(signature2.getParameters());
			//
			SarlSkill skill1 = (SarlSkill) mas.getXtendTypes().get(2);
			assertEquals("S1", skill1.getName());
			assertNull(skill1.getExtends());
			assertTypeReferenceIdentifiers(skill1.getImplements(), "C1");
			assertEquals(1, skill1.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill1.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters());
			//
			SarlSkill skill2 = (SarlSkill) mas.getXtendTypes().get(3);
			assertEquals("S2", skill2.getName());
			assertTypeReferenceIdentifier(skill2.getExtends(), "S1");
			assertTypeReferenceIdentifiers(skill2.getImplements(), "C2");
			assertEquals(2, skill2.getMembers().size());
			//
			SarlAction action2 = (SarlAction) skill2.getMembers().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getReturnType(), "void");
			assertParameterNames(action2.getParameters());
			//
			SarlAction action3 = (SarlAction) skill2.getMembers().get(1);
			assertEquals("myaction2", action3.getName());
			assertTypeReferenceIdentifiers(action3.getFiredEvents());
			assertTypeReferenceIdentifier(action3.getReturnType(), "void");
			assertParameterNames(action3.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void skillNoExtendSkillNoImplementCapacity() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"skill S1 {",
					"	def myaction { }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_TYPE,
					"Missing implemented type 'io.sarl.lang.core.Capacity' for 'S1'");
		}

		@Test
		@Tag("sarlParsing")
		public void skillmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"public skill S1 implements C1 {}"
					));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(JvmVisibility.PUBLIC, skill.getVisibility());
			assertEquals(0, skill.getMembers().size());
			assertFalse(skill.isAbstract());
			assertFalse(skill.isFinal());
			assertFalse(skill.isStatic());
			assertFalse(skill.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void skillmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"skill S1 implements C1 {}"
					));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(JvmVisibility.PUBLIC, skill.getVisibility());
			assertEquals(0, skill.getMembers().size());
			assertFalse(skill.isAbstract());
			assertFalse(skill.isFinal());
			assertFalse(skill.isStatic());
			assertFalse(skill.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void skillmodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"private skill S1 implements C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void skillmodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"protected skill S1 implements C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void skillmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"package skill S1 implements C1 {}"
					));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(JvmVisibility.DEFAULT, skill.getVisibility());
			assertEquals(0, skill.getMembers().size());
			assertFalse(skill.isAbstract());
			assertFalse(skill.isFinal());
			assertFalse(skill.isStatic());
			assertFalse(skill.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlParsing")
		public void skillmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"abstract skill S1 implements C1 { }"
					));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(JvmVisibility.PUBLIC, skill.getVisibility());
			assertEquals(0, skill.getMembers().size());
			assertTrue(skill.isAbstract());
			assertFalse(skill.isFinal());
			assertFalse(skill.isStatic());
			assertFalse(skill.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void skillmodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"static skill S1 implements C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void skillmodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"dispatch skill S1 implements C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void skillmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"final skill S1 implements C1 {}"
					));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(JvmVisibility.PUBLIC, skill.getVisibility());
			assertEquals(0, skill.getMembers().size());
			assertFalse(skill.isAbstract());
			assertTrue(skill.isFinal());
			assertFalse(skill.isStatic());
			assertFalse(skill.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void skillmodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"strictfp skill S1 implements C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void skillmodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"native skill S1 implements C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void skillmodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"volatile skill S1 implements C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void skillmodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"synchronized skill S1 implements C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void skillmodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"transient skill S1 implements C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void skillmodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}",
					"public package skill S1 implements C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"public / package / protected / private");
		}

		@Test
		@Tag("sarlValidation")
		public void abstractSkill() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def fct(a : int = 4)",
					"}",
					"skill S1 implements C1 { }"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
					"class S1 must be defined abstract");
		}

	}

	@Nested
	public class ActionTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void modifier_override_possible_but_no_warning() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { def fakefct() }",
					"abstract skill S1 implements C1 {",
					"   override fakefct() { }",
					"	abstract def name",
					"}",
					"skill S2 extends S1 {",
					"	def name { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_override_invalid() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { def fakefct() }",
					"skill S1 implements C1 {",
					"  override fakefct() { }",
					"}",
					"skill S2 extends S1 {",
					"	override name { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.OBSOLETE_OVERRIDE,
					"The method name() of type S2 must override a superclass method");
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_override_valid() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { def fakefct() }",
					"abstract skill S1 implements C1 {",
					"   override fakefct() { }",
					"	abstract def name()",
					"}",
					"skill S2 extends S1 {",
					"   override fakefct() { }",
					"	override name() { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	public def name { }",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void modifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	private def name { }",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void modifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	protected def name { }",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void modifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	package def name { }",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void modifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def name { }",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"abstract skill S1 implements C1 {",
					"	abstract def name",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void modifier_no_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"abstract skill S1 implements C1 {",
					"	def name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT);
			//
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
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
		@Tag("sarlValidation")
		public void modifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	static def name { }",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill1 = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill1.getName());
			assertNull(skill1.getExtends());
			assertEquals(1, skill1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertNull(act1.getReturnType());
			assertEquals(0, act1.getParameters().size());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertFalse(act1.isFinal());
			assertTrue(act1.isStatic());
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	dispatch def name { }",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
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
		@Tag("sarlParsing")
		public void modifier_final_var() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	final def name { }",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
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
		@Tag("sarlValidation")
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	strictfp def name { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	native def name { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	volatile def name { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	synchronized def name { }",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
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
		@Tag("sarlValidation")
		public void modifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	transient def name { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_protected_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	protected private def name { }",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_dispatch_final() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	dispatch final def name(a : Integer) { }",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction act1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertTrue(act1.isDispatch());
			assertTrue(act1.isFinal());
			assertFalse(act1.isSynchonized());
			assertFalse(act1.isStrictFloatingPoint());
		}

		@Test
		@Tag("sarlValidation")
		public void multipleActionDefinitionInCapacity() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(a : int, b : int)",
					"	def myaction(a : int)",
					"	def myaction(a : int)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_METHOD,
					"Duplicate method myaction(int) in type C1");
		}

		@Test
		@Tag("sarlValidation")
		public void multipleActionDefinitionInSkill() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int, b : int) { }",
					"	def myaction(a : int) { }",
					"	def myaction(a : int) { }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_METHOD,
					"Duplicate method myaction(int) in type S1");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidActionNameInCapacity() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myaction",
					"	def $handle_myaction",
					"	def myaction2",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME,
					"Invalid action name '$handle_myaction'.");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidActionNameInSkill() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
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
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME,
					"Invalid action name '$handle_myaction'.");
		}

		@Test
		@Tag("sarlParsing")
		public void missedActionImplementation_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def myaction1(a : int)",
					"}",
					"capacity C2 {",
					"	def myaction2(b : float, c : boolean)",
					"}",
					"skill S1 implements C1, C2 {",
					"	def myaction1(x : int) { }",
					"	def myaction2(y : float, z : boolean) { }",
					"}"
					));
			assertEquals(3, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity1 = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getExtends());
			assertEquals(1, capacity1.getMembers().size());
			//
			SarlAction signature1 = (SarlAction) capacity1.getMembers().get(0);
			assertEquals("myaction1", signature1.getName());
			assertTypeReferenceIdentifiers(signature1.getFiredEvents());
			assertTypeReferenceIdentifier(signature1.getReturnType(), "void");
			assertParameterNames(signature1.getParameters(), "a");
			assertParameterTypes(signature1.getParameters(), "int");
			assertParameterDefaultValues(signature1.getParameters(), (Object) null);
			//
			SarlCapacity capacity2 = (SarlCapacity) mas.getXtendTypes().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getExtends());
			assertEquals(1, capacity2.getMembers().size());
			//
			SarlAction signature2 = (SarlAction) capacity2.getMembers().get(0);
			assertEquals("myaction2", signature2.getName());
			assertTypeReferenceIdentifiers(signature2.getFiredEvents());
			assertTypeReferenceIdentifier(signature2.getReturnType(), "void");
			assertParameterNames(signature2.getParameters(), "b", "c");
			assertParameterTypes(signature2.getParameters(), "float", "boolean");
			assertParameterDefaultValues(signature2.getParameters(), null, null);
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(2);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1", "C2");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("myaction1", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters(), "x");
			assertParameterTypes(action1.getParameters(), "int");
			assertParameterDefaultValues(action1.getParameters(), (Object) null);
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction2", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "y", "z");
			assertParameterTypes(action2.getParameters(), "float", "boolean");
			assertParameterDefaultValues(action2.getParameters(), null, null);
		}

		@Test
		@Tag("sarlValidation")
		public void missedActionImplementation_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myaction1(a : int)",
					"}",
					"capacity C2 {",
					"	def myaction2(b : float, c : boolean)",
					"}",
					"skill S1 implements C1, C2 {",
					"	def myaction2(b : float, c : boolean) { }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
					"The class S1 must be defined abstract because it does not implement myaction1(int)");
		}

		@Test
		@Tag("sarlValidation")
		public void missedActionImplementation_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myaction1(a : int)",
					"}",
					"capacity C2 {",
					"	def myaction2(b : float, c : boolean)",
					"}",
					"skill S1 implements C1, C2 {",
					"	def myaction1(x : float) { }",
					"	def myaction2(y : float, z : boolean) { }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
					"The class S1 must be defined abstract because it does not implement myaction1(int)");
		}

		@Test
		@Tag("sarlValidation")
		public void incompatibleReturnType_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int) : int {",
					"		return 0",
					"	}",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	def myaction(a : int) : float {",
					"		return 0f",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
		}

		@Test
		@Tag("sarlValidation")
		public void incompatibleReturnType_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int) {",
					"		// void",
					"	}",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	def myaction(a : int) : int {",
					"		return 0",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
		}

		@Test
		@Tag("sarlValidation")
		public void incompatibleReturnType_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int) : int {",
					"		return 0",
					"	}",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	def myaction(a : int) : void{",
					"		// void",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
		}

		@Test
		@Tag("sarlValidation")
		public void incompatibleReturnType_3() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int) : int {",
					"		return 0",
					"	}",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	def myaction(a : int) {",
					"		// int is inferred",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XbasePackage.eINSTANCE.getXBlockExpression(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from null to int");
		}

		@Test
		@Tag("sarlValidation")
		public void incompatibleReturnType_4() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : int",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : float {",
					"		return 0f",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
		}

		@Test
		@Tag("sarlValidation")
		public void incompatibleReturnType_5() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(a : int) // void",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : int {",
					"		return 0",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
		}

		@Test
		@Tag("sarlValidation")
		public void incompatibleReturnType_6() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : int",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : void {",
					"		// void",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
		}

		@Test
		@Tag("sarlValidation")
		public void incompatibleReturnType_7() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : int",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) {",
					"		// int is inferred",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XbasePackage.eINSTANCE.getXBlockExpression(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from null to int");
		}

		@Test
		@Tag("sarlValidation")
		public void expectingReturnType_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int) : int {",
					"		return 0",
					"	}",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	def myaction(a : int) {",
					"		1",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlAction(),
					IssueCodes.RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED,
					"int");
		}

		@Test
		@Tag("sarlParsing")
		public void compatibleReturnType_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int) : Number {",
					"		return 0.0",
					"	}",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	def myaction(a : int) : Double {",
					"		return 0.0",
					"	}",
					"}"
					));
			assertEquals(4, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity1 = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getExtends());
			assertEquals(0, capacity1.getMembers().size());
			//
			SarlCapacity capacity2 = (SarlCapacity) mas.getXtendTypes().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getExtends());
			assertEquals(0, capacity2.getMembers().size());
			//
			SarlSkill skill1 = (SarlSkill) mas.getXtendTypes().get(2);
			assertEquals("S1", skill1.getName());
			assertNull(skill1.getExtends());
			assertTypeReferenceIdentifiers(skill1.getImplements(), "C1");
			assertEquals(1, skill1.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill1.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getReturnType(), "java.lang.Number");
			assertParameterNames(action1.getParameters(), "a");
			assertParameterTypes(action1.getParameters(), "int");
			assertParameterDefaultValues(action1.getParameters(), (Object) null);
			//
			SarlSkill skill2 = (SarlSkill) mas.getXtendTypes().get(3);
			assertEquals("S2", skill2.getName());
			assertTypeReferenceIdentifier(skill2.getExtends(), "S1");
			assertTypeReferenceIdentifiers(skill2.getImplements(), "C2");
			assertEquals(1, skill2.getMembers().size());
			//
			SarlAction action2 = (SarlAction) skill2.getMembers().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getReturnType(), "java.lang.Double");
			assertParameterNames(action2.getParameters(), "a");
			assertParameterTypes(action2.getParameters(), "int");
			assertParameterDefaultValues(action2.getParameters(), (Object) null);
		}

		@Test
		@Tag("sarlParsing")
		public void compatibleReturnType_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int) : float {",
					"		return 0f",
					"	}",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	def myaction(a : int) : float {",
					"		return 0f",
					"	}",
					"}"
					));
			assertEquals(4, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity1 = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getExtends());
			assertEquals(0, capacity1.getMembers().size());
			//
			SarlCapacity capacity2 = (SarlCapacity) mas.getXtendTypes().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getExtends());
			assertEquals(0, capacity2.getMembers().size());
			//
			SarlSkill skill1 = (SarlSkill) mas.getXtendTypes().get(2);
			assertEquals("S1", skill1.getName());
			assertNull(skill1.getExtends());
			assertTypeReferenceIdentifiers(skill1.getImplements(), "C1");
			assertEquals(1, skill1.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill1.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getReturnType(), "float");
			assertParameterNames(action1.getParameters(), "a");
			assertParameterTypes(action1.getParameters(), "int");
			assertParameterDefaultValues(action1.getParameters(), (Object) null);
			//
			SarlSkill skill2 = (SarlSkill) mas.getXtendTypes().get(3);
			assertEquals("S2", skill2.getName());
			assertTypeReferenceIdentifier(skill2.getExtends(), "S1");
			assertTypeReferenceIdentifiers(skill2.getImplements(), "C2");
			assertEquals(1, skill2.getMembers().size());
			//
			SarlAction action2 = (SarlAction) skill2.getMembers().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getReturnType(), "float");
			assertParameterNames(action2.getParameters(), "a");
			assertParameterTypes(action2.getParameters(), "int");
			assertParameterDefaultValues(action2.getParameters(), (Object) null);
		}

		@Test
		@Tag("sarlParsing")
		public void compatibleReturnType_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : Number",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : Double {",
					"		return 0.0",
					"	}",
					"}"
					));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getExtends());
			assertEquals(1, capacity.getMembers().size());
			//
			SarlAction action1 = (SarlAction) capacity.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getReturnType(), "java.lang.Number");
			assertParameterNames(action1.getParameters(), "a");
			assertParameterTypes(action1.getParameters(), "int");
			assertParameterDefaultValues(action1.getParameters(), (Object) null);
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S2", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getReturnType(), "java.lang.Double");
			assertParameterNames(action2.getParameters(), "a");
			assertParameterTypes(action2.getParameters(), "int");
			assertParameterDefaultValues(action2.getParameters(), (Object) null);
		}

		@Test
		@Tag("sarlParsing")
		public void compatibleReturnType_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : float",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : float {",
					"		return 0f",
					"	}",
					"}"
					));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getExtends());
			assertEquals(1, capacity.getMembers().size());
			//
			SarlAction action1 = (SarlAction) capacity.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getReturnType(), "float");
			assertParameterNames(action1.getParameters(), "a");
			assertParameterTypes(action1.getParameters(), "int");
			assertParameterDefaultValues(action1.getParameters(), (Object) null);
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S2", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getReturnType(), "float");
			assertParameterNames(action2.getParameters(), "a");
			assertParameterTypes(action2.getParameters(), "int");
			assertParameterDefaultValues(action2.getParameters(), (Object) null);
		}

	}

	@Nested
	public class FieldTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void modifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	public var field : int",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.PUBLIC, attr1.getVisibility());
			assertFalse(attr1.isFinal());
			assertFalse(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
			assertFalse(attr1.isExtension());
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	private var field : int",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
			assertFalse(attr1.isFinal());
			assertFalse(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
			assertFalse(attr1.isExtension());
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	protected var field : int",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
			assertFalse(attr1.isFinal());
			assertFalse(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
			assertFalse(attr1.isExtension());
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	package var field : int",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.DEFAULT, attr1.getVisibility());
			assertFalse(attr1.isFinal());
			assertFalse(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
			assertFalse(attr1.isExtension());
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	var field : int",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertEquals(1, skill.getMembers().size());
			//
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
			assertFalse(attr1.isFinal());
			assertFalse(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
			assertFalse(attr1.isExtension());
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	static var field : int",
					"}"));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlSkill skill1 = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill1.getName());
			assertNull(skill1.getExtends());
			assertEquals(1, skill1.getMembers().size());
			//
			SarlField attr1 = (SarlField) skill1.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
			assertFalse(attr1.isExtension());
			assertFalse(attr1.isFinal());
			assertTrue(attr1.isStatic());
			assertFalse(attr1.isTransient());
			assertFalse(attr1.isVolatile());
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	final var field : int = 5",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	protected private var field : int",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"public / package / protected / private");
		}

		@Test
		@Tag("sarlValidation")
		public void multipleVariableDefinitionInSkill() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	var myfield : int",
					"	var myfield1 : String",
					"	var myfield : double",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					"Duplicate field myfield");
		}

		@Test
		@Tag("sarlValidation")
		public void multipleValueDefinitionInSkill() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	val myfield : int = 4",
					"	val myfield1 : String = \"\"",
					"	val myfield : double = 5",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					"Duplicate field myfield");
		}

		@Test
		@Tag("sarlValidation")
		public void missedFinalFieldInitialization() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	val field1 : int = 5",
					"	val field2 : String",
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	val field1 : int = 5",
					"	val field2 : String = \"\"",
					"}"
					));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getExtends());
			assertEquals(0, capacity.getMembers().size());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals("field1", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertXExpression(attr1.getInitialValue(), XNumberLiteral.class, "5");
			//
			SarlField attr2 = (SarlField) skill.getMembers().get(1);
			assertEquals("field2", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.String");
			assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "");
		}

		@Test
		@Tag("sarlValidation")
		public void fieldNameShadowingInSkill() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"	protected val field1 : int = 5",
					"	def myaction(a : int) { }",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	val field1 : int = 5",
					"	def myaction(a : int) { }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
					"The field 'field1' in 'S2' is hidding the inherited field 'S1.field1'.");
		}

		@Test
		@Tag("sarlValidation")
		public void variableModifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"public var name : String = \"Hello\"",
					"}"
					));
			//
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlParsing")
		public void variableModifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"protected var name : String = \"Hello\"",
					"}"
					));
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

		@Test
		@Tag("sarlValidation")
		public void variableModifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"package var name : String = \"Hello\"",
					"}"
					));
			//
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlParsing")
		public void variableModifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"private var name : String = \"Hello\"",
					"}"
					));
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
		}

		@Test
		@Tag("sarlParsing")
		public void variableModifier_default() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"var name : String = \"Hello\"",
					"}"
					));
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
		}

		@Test
		@Tag("sarlValidation")
		public void valueModifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"public val name : String = \"Hello\"",
					"}"
					));
			//
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlParsing")
		public void valueModifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"protected val name : String = \"Hello\"",
					"}"
					));
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

		@Test
		@Tag("sarlValidation")
		public void valueModifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"package val name : String = \"Hello\"",
					"}"
					));
			//
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlParsing")
		public void valueModifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"private val name : String = \"Hello\"",
					"}"
					));
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
		}

		@Test
		@Tag("sarlParsing")
		public void valueModifier_default() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"val name : String = \"Hello\"",
					"}"
					));
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			SarlField attr1 = (SarlField) skill.getMembers().get(0);
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
		}

	}

	@Nested
	public class CapacityUsesTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityTypeForUses() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : float",
					"}",
					"event E1 {",
					"	var abc : int",
					"}",
					"behavior B1 {",
					"	uses C1, E1",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_CAPACITY_TYPE,
					"Invalid type: 'E1'. Only capacities can be used after the keyword 'uses'");
		}

		@Test
		@Tag("sarlValidation")
		public void agentUnsuedCapacity_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myfct",
					"}",
					"capacity C2 {",
					"	def myfct2",
					"}",
					"agent A1 {",
					"	uses C2, C1",
					"	def myaction {",
					"		myfct2",
					"	}",
					"}"
					));
			List<Issue> issues = issues(getValidationHelper(), mas);
			assertWarning(
					issues,
					mas,
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.UNUSED_AGENT_CAPACITY,
					"The capacity 'C1' is not used");
			assertNoMoreIssues(issues, mas);
		}

		@Test
		@Tag("sarlValidation")
		public void agentUnsuedCapacity_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myfct",
					"}",
					"capacity C2 {",
					"	def myfct2",
					"}",
					"agent A1 {",
					"	uses C2, C1",
					"	def myaction {",
					"	}",
					"}"
					));
			List<Issue> issues = issues(getValidationHelper(), mas);
			assertWarning(
					issues,
					mas,
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.UNUSED_AGENT_CAPACITY,
					"The capacity 'C1' is not used");
			assertWarning(
					issues,
					mas,
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.UNUSED_AGENT_CAPACITY,
					"The capacity 'C2' is not used");
			assertNoMoreIssues(issues, mas);
		}

		@Test
		@Tag("sarlParsing")
		public void agentUnsuedCapacity_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def myfct",
					"}",
					"capacity C2 {",
					"	def myfct2",
					"}",
					"agent A1 {",
					"	uses C2, C1",
					"	def myaction {",
					"		myfct",
					"		myfct2",
					"	}",
					"}"
					));
			assertEquals(3, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity1 = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getExtends());
			assertEquals(1, capacity1.getMembers().size());
			//
			SarlAction signature1 = (SarlAction) capacity1.getMembers().get(0);
			assertEquals("myfct", signature1.getName());
			assertTypeReferenceIdentifiers(signature1.getFiredEvents());
			assertTypeReferenceIdentifier(signature1.getReturnType(), "void");
			assertParameterNames(signature1.getParameters());
			//
			SarlCapacity capacity2 = (SarlCapacity) mas.getXtendTypes().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getExtends());
			assertEquals(1, capacity2.getMembers().size());
			//
			SarlAction signature2 = (SarlAction) capacity2.getMembers().get(0);
			assertEquals("myfct2", signature2.getName());
			assertTypeReferenceIdentifiers(signature2.getFiredEvents());
			assertTypeReferenceIdentifier(signature2.getReturnType(), "void");
			assertParameterNames(signature2.getParameters());
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(2);
			assertEquals("A1", agent.getName());
			assertNull(agent.getExtends());
			assertEquals(2, agent.getMembers().size());
			//
			SarlCapacityUses uses = (SarlCapacityUses) agent.getMembers().get(0);
			assertTypeReferenceIdentifiers(uses.getCapacities(), "C2", "C1");
			//
			SarlAction action = (SarlAction) agent.getMembers().get(1);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			assertParameterNames(action.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void agentUnsuedCapacity_3() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myfct",
					"}",
					"capacity C2 extends C1 {",
					"	def myfct2",
					"}",
					"skill S1 implements C2 {",
					"	uses C2",
					"	def myfct {}",
					"	def myfct2 {}",
					"   def myaction {",
					"     myfct",
					"   }",
					"}"
					));
			List<Issue> issues = issues(getValidationHelper(), mas);
			assertWarning(
					issues,
					mas,
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.UNUSED_AGENT_CAPACITY,
					"Unnecessary use of the capacity 'C2'");
			assertNoMoreIssues(issues, mas);
		}

		@Test
		@Tag("sarlValidation")
		public void agentUnsuedCapacity_4() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myfct",
					"}",
					"capacity C2 extends C1 {",
					"	def myfct2",
					"}",
					"skill S1 implements C2 {",
					"	uses C1",
					"	def myfct {}",
					"	def myfct2 {}",
					"   def myaction {",
					"     myfct",
					"   }",
					"}"
					));
			List<Issue> issues = issues(getValidationHelper(), mas);
			assertWarning(
					issues,
					mas,
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.UNUSED_AGENT_CAPACITY,
					"Unnecessary use of the capacity 'C1'");
			assertNoMoreIssues(issues, mas);
		}

		@Test
		@Tag("sarlValidation")
		public void multipleCapacityUses_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 { def testFct }",
					"skill S1 implements C3 {",
					"	uses C1, C2, C1",
					"	def testFct { }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.REDUNDANT_CAPACITY_USE,
					"Redundant use of the capacity 'C1'");
		}

		@Test
		@Tag("sarlValidation")
		public void multipleCapacityUses_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 { def testFct }",
					"skill S1 implements C3 {",
					"	uses C2",
					"	def testFct { }",
					"	uses C2, C1",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.REDUNDANT_CAPACITY_USE,
					"Redundant use of the capacity 'C2'");
		}

	}

	@Nested
	public class GenericTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void functionGeneric_X_sarlNotation() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def setX(param : X) : void with X { var xxx : X }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertNotNull(skill);
			//
			assertEquals("S1", skill.getName());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action = (SarlAction) skill.getMembers().get(0);
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def <X> setX(param : X) : void { var xxx : X }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertNotNull(skill);
			//
			assertEquals("S1", skill.getName());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action = (SarlAction) skill.getMembers().get(0);
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def setX(param : X) : void with X extends Number { var xxx : X }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertNotNull(skill);
			//
			assertEquals("S1", skill.getName());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action = (SarlAction) skill.getMembers().get(0);
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def <X extends Number> setX(param : X) : void { var xxx : X }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertNotNull(skill);
			//
			assertEquals("S1", skill.getName());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action = (SarlAction) skill.getMembers().get(0);
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def setX(param : X) : void with X, Y { var xxx : X; var yyy : Y }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertNotNull(skill);
			//
			assertEquals("S1", skill.getName());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action = (SarlAction) skill.getMembers().get(0);
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def <X, Y> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertNotNull(skill);
			//
			assertEquals("S1", skill.getName());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action = (SarlAction) skill.getMembers().get(0);
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def setX(param : X) : void with X, Y extends X { var xxx : X; var yyy : Y }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertNotNull(skill);
			//
			assertEquals("S1", skill.getName());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action = (SarlAction) skill.getMembers().get(0);
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
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def <X, Y extends X> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertNotNull(skill);
			//
			assertEquals("S1", skill.getName());
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action = (SarlAction) skill.getMembers().get(0);
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
