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
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
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
@DisplayName("Syntax: capacity")
@Tag("core")
public class CapacityParsingTest {
	
	@Nested
	public class TopElementTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"}",
					"capacity C1 extends A1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 extends A1, C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 extends C1, A1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_3() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends A1, C1, C2 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_4() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, A1, C2 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_5() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, C2, A1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_6() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 extends java.lang.Cloneable {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_7() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 extends java.lang.Cloneable, C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_8() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 extends C1, java.lang.Cloneable {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_9() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends java.lang.Cloneable, C1, C2 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_10() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, java.lang.Cloneable, C2 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_11() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, C2, java.lang.Cloneable {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_12() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 extends C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of C1 contains cycles");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_13() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 extends C2 {",
					"}",
					"capacity C2 extends C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_14() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 extends C3 {",
					"}",
					"capacity C2 extends C1 {",
					"}",
					"capacity C3 extends C2 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_15() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"capacity C3 extends C1, C2, C3 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of C3 contains cycles");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_16() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"capacity C3 extends C1, C3, C2 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of C3 contains cycles");
		}

		@Test
		@Tag("sarlValidation")
		public void invalidCapacityExtend_17() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"capacity C3 extends C3, C1, C3 { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of C3 contains cycles");
		}

		@Test
		@Tag("sarlParsing")
		public void inheritance() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity CapTest1 {",
					"	def func1 : int",
					"}",
					"capacity CapTest2 extends CapTest1 {",
					"	def func2(a : int)",
					"}"
					));
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity1 = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("CapTest1", capacity1.getName());
			assertTrue(capacity1.getExtends().isEmpty());
			assertEquals(1, capacity1.getMembers().size());
			//
			SarlAction signature1 = (SarlAction) capacity1.getMembers().get(0);
			assertEquals("func1", signature1.getName());
			assertTrue(signature1.getFiredEvents().isEmpty());
			assertTypeReferenceIdentifier(signature1.getReturnType(), "int");
			assertParameterNames(signature1.getParameters());
			//
			SarlCapacity capacity2 = (SarlCapacity) mas.getXtendTypes().get(1);
			assertEquals("CapTest2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getExtends(), "CapTest1");
			assertEquals(1, capacity2.getMembers().size());
			//
			SarlAction signature2 = (SarlAction) capacity2.getMembers().get(0);
			assertEquals("func2", signature2.getName());
			assertTrue(signature2.getFiredEvents().isEmpty());
			assertTypeReferenceIdentifier(signature2.getReturnType(), "void");
			assertParameterNames(signature2.getParameters(), "a");
			assertParameterTypes(signature2.getParameters(), "int");
			assertParameterDefaultValues(signature2.getParameters(), (Object) null);
		}

		@Test
		@Tag("sarlValidation")
		public void emptyCapacity() throws Exception {
			SarlScript mas = file(getParseHelper(), "capacity C1 { }");
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.DISCOURAGED_CAPACITY_DEFINITION,
					"Discouraged capacity definition. A capacity without actions defined inside is not useful since it cannot be called by an agent or a behavior.");
		}

		@Test
		@Tag("sarlParsing")
		public void capacitymodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 {}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", cap.getName());
			assertNullOrEmpty(cap.getExtends());
			assertEquals(JvmVisibility.PUBLIC, cap.getVisibility());
			assertEquals(0, cap.getMembers().size());
			assertFalse(cap.isFinal());
		}

		@Test
		@Tag("sarlParsing")
		public void capacitymodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", cap.getName());
			assertNullOrEmpty(cap.getExtends());
			assertEquals(JvmVisibility.PUBLIC, cap.getVisibility());
			assertEquals(0, cap.getMembers().size());
			assertFalse(cap.isFinal());
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"private capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"protected capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlParsing")
		public void capacitymodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"package capacity C1 {}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", cap.getName());
			assertNullOrEmpty(cap.getExtends());
			assertEquals(JvmVisibility.DEFAULT, cap.getVisibility());
			assertEquals(0, cap.getMembers().size());
			assertFalse(cap.isFinal());
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"abstract capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"static capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"final capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_native() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"native capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_volatile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"volatile capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"transient capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of C1; only public & package are permitted");
		}

		@Test
		@Tag("sarlValidation")
		public void capacitymodifier_public_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"public package capacity C1 {}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"The definition of C1 can only set one of public / package / protected / private");
		}

	}

	@Nested
	public class ActionTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void modifier_override_notRecommended() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	def name",
					"}",
					"capacity C2 extends C1 {",
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
					"capacity C1 {",
					"}",
					"capacity C2 extends C1 {",
					"	override name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.OBSOLETE_OVERRIDE,
					"The method name() of type C2 must override a superclass method");
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_override_valid() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	def name",
					"}",
					"capacity C2 extends C1 {",
					"	override name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
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
		@Tag("sarlParsing")
		public void modifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	public def name",
					"}"));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", cap.getName());
			assertNullOrEmpty(cap.getExtends());
			assertEquals(1, cap.getMembers().size());
			//
			SarlAction act1 = (SarlAction) cap.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertTrue(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	private def name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	protected def name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	package def name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlParsing")
		public void modifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	def name",
					"}"));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", cap.getName());
			assertNullOrEmpty(cap.getExtends());
			assertEquals(1, cap.getMembers().size());
			//
			SarlAction act1 = (SarlAction) cap.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PUBLIC, act1.getVisibility());
			assertTrue(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	abstract def name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_static() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	static def name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	dispatch def name(a : Integer)",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	final def name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	strictfp def name",
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
					"capacity C1 {",
					"	native def name",
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
					"capacity C1 {",
					"	volatile def name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	synchronized def name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER);
		}

		@Test
		@Tag("sarlValidation")
		public void modifier_transient() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	transient def name",
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
					"capacity C1 {",
					"	protected private def name",
					"}"));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"public / package / protected / private");
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
					"capacity C1 {",
					"	def setX(param : X) : void with X",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertNotNull(cap);
			//
			assertEquals("C1", cap.getName());
			assertEquals(1, cap.getMembers().size());
			//
			SarlAction action = (SarlAction) cap.getMembers().get(0);
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
					"capacity C1 {",
					"	def <X> setX(param : X) : void",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertNotNull(cap);
			//
			assertEquals("C1", cap.getName());
			assertEquals(1, cap.getMembers().size());
			//
			SarlAction action = (SarlAction) cap.getMembers().get(0);
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
					"capacity C1 {",
					"	def setX(param : X) : void with X extends Number",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertNotNull(cap);
			//
			assertEquals("C1", cap.getName());
			assertEquals(1, cap.getMembers().size());
			//
			SarlAction action = (SarlAction) cap.getMembers().get(0);
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
					"capacity C1 {",
					"	def <X extends Number> setX(param : X) : void",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertNotNull(cap);
			//
			assertEquals("C1", cap.getName());
			assertEquals(1, cap.getMembers().size());
			//
			SarlAction action = (SarlAction) cap.getMembers().get(0);
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
					"capacity C1 {",
					"	def setX(param : X) : void with X, Y",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertNotNull(cap);
			//
			assertEquals("C1", cap.getName());
			assertEquals(1, cap.getMembers().size());
			//
			SarlAction action = (SarlAction) cap.getMembers().get(0);
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
					"capacity C1 {",
					"	def <X, Y> setX(param : X) : void",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertNotNull(cap);
			//
			assertEquals("C1", cap.getName());
			assertEquals(1, cap.getMembers().size());
			//
			SarlAction action = (SarlAction) cap.getMembers().get(0);
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
					"capacity C1 {",
					"	def setX(param : X) : void with X, Y extends X",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertNotNull(cap);
			//
			assertEquals("C1", cap.getName());
			assertEquals(1, cap.getMembers().size());
			//
			SarlAction action = (SarlAction) cap.getMembers().get(0);
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
					"capacity C1 {",
					"	def <X, Y extends X> setX(param : X) : void",
					"}"));
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertNotNull(cap);
			//
			assertEquals("C1", cap.getName());
			assertEquals(1, cap.getMembers().size());
			//
			SarlAction action = (SarlAction) cap.getMembers().get(0);
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
