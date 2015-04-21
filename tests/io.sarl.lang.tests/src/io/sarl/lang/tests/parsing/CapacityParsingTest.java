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

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.inject.Inject;

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.validation.IssueCodes;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import java.util.Iterator;
import java.util.List;

import static org.junit.Assert.*;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	CapacityParsingTest.TopElementTest.class,
	CapacityParsingTest.ActionTest.class,
	CapacityParsingTest.AttributeTest.class,
	CapacityParsingTest.CapacityUsesTest.class,
})
@SuppressWarnings("all")
public class CapacityParsingTest {

	/**
	 * FIXME: Issue #260. Move to Xbase.
	 */
	public static StringBuilder getIssuesAsString(EObject model, Iterable<Issue> issues, StringBuilder result) {
		for(Issue issue : issues) {
			URI uri = issue.getUriToProblem();
			result.append(issue.getSeverity());
			result.append(" (");
			result.append(issue.getCode());
			result.append(") '");
			result.append(issue.getMessage());
			result.append("'");
			if (uri != null) {
				EObject eObject = model.eResource().getResourceSet().getEObject(uri, true);
				result.append(" on ");
				result.append(eObject.eClass().getName());
			}
			result.append("\n");
		}
		return result;
	}

	/**
	 * FIXME: Issue #260. Move to Xbase.
	 */
	public static boolean isIssueMessage(Issue issue, String... messageParts) {
		for (String messagePart : messageParts) {
			if (!issue.getMessage().toLowerCase().contains(messagePart.toLowerCase())) {
				return false;
			}
		}
		return true;
	}

	/**
	 * FIXME: Issue #260. Move to Xbase.
	 */
	public static void assertIssue(List<Issue> issues, Severity severity, EObject model,
			EClass objectType, String code, String... messageParts) {
		Iterator<Issue> iterator = issues.iterator();
		while (iterator.hasNext()) {
			Issue issue = iterator.next();
			if (Objects.equal(issue.getCode(), code) && issue.getSeverity() == severity) {
				EObject object = model.eResource().getResourceSet().getEObject(issue.getUriToProblem(), true);
				if (objectType.isInstance(object)) {
					if (isIssueMessage(issue, messageParts)) {
						iterator.remove();
						return;
					}
				}
			}
		}
		StringBuilder message = new StringBuilder("Expected ");
		message.append(severity);
		message.append(" '");
		message.append(code);
		message.append("' on ");
		message.append(objectType.getName());
		message.append(" but got\n");
		getIssuesAsString(model, issues, message);
		fail(message.toString());
	}

	/**
	 * FIXME: Issue #260. Move to Xbase.
	 */
	public static void assertWarning(List<Issue> issues, EObject model, EClass objectType, String code,
			String... messageParts) {
		assertIssue(issues, Severity.WARNING, model, objectType, code, messageParts);
	}

	/**
	 * FIXME: Issue #260. Move to Xbase.
	 */
	public static void assertNoMoreIssues(List<Issue> issues, EObject model) {
		if (!issues.isEmpty()) {
			StringBuilder message = new StringBuilder("Expecting no issue but got\n");
			getIssuesAsString(model, issues, message);
			fail(message.toString());
		}
	}

	public static class TopElementTest extends AbstractSarlTest {

		@Test
		public void capacityDirectImplementation() throws Exception {
			XtendFile mas = file(multilineString(
					"import io.sarl.lang.core.Capacity",
					"skill S1 implements Capacity {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					54, 8,
					"Invalid implemented type: 'io.sarl.lang.core.Capacity'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		public void redundantCapacity_fromSuperType() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"skill S1 implements C1 { }",
					"skill S2 extends S1 implements C2, C1 { }"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
					"The feature 'C1' is already implemented by the super-type 'S1'.");
		}

		@Test
		public void redundantCapacity_duplicate() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 {}",
					"skill S1 implements C1 { }",
					"skill S2 extends S1 implements C2, C3, C2 { }"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
					"Duplicate implemented feature 'C2'");
		}

		@Test
		public void redundantCapacity_fromPreviousCapacity() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 extends C2 {}",
					"skill S1 implements C1 { }",
					"skill S2 extends S1 implements C3, C2 { }"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
					"Duplicate implemented feature 'C2'");
		}

		@Test
		public void invalidCapacityExtend_0() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 extends A1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		public void invalidCapacityExtend_1() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 extends A1, C1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					49, 2,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		public void invalidCapacityExtend_2() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 extends C1, A1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					53, 2,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		public void invalidCapacityExtend_3() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends A1, C1, C2 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					65, 2,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		public void invalidCapacityExtend_4() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, A1, C2 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					69, 2,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		public void invalidCapacityExtend_5() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, C2, A1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INTERFACE_EXPECTED,
					73, 2,
					"Invalid supertype. Expecting an interface");
		}

		@Test
		public void invalidCapacityExtend_6() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 extends java.lang.Cloneable {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_7() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 extends java.lang.Cloneable, C1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_8() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 extends C1, java.lang.Cloneable {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_9() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends java.lang.Cloneable, C1, C2 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_10() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, java.lang.Cloneable, C2 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_11() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, C2, java.lang.Cloneable {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_12() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 extends C1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'C1' is inconsistent");
		}

		@Test
		public void invalidCapacityExtend_13() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 extends C2 {",
					"}",
					"capacity C2 extends C1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'C1' is inconsistent");
		}

		@Test
		public void invalidCapacityExtend_14() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 extends C3 {",
					"}",
					"capacity C2 extends C1 {",
					"}",
					"capacity C3 extends C2 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'C1' is inconsistent");
		}

		@Test
		public void invalidCapacityExtend_15() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"capacity C3 extends C1, C2, C3 { }"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'C3' is inconsistent");
		}

		@Test
		public void invalidCapacityExtend_16() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"capacity C3 extends C1, C3, C2 { }"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'C3' is inconsistent");
		}

		@Test
		public void invalidCapacityExtend_17() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"capacity C3 extends C3, C1, C3 { }"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'C3' is inconsistent");
		}

		@Test
		public void invalidSkillExtend_0() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"}",
					"agent A1 {",
					"}",
					"skill S1 extends A1 implements C1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Skill'");
		}

		@Test
		public void invalidSkillExtend_1() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 extends C1 implements C2 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED,
					49, 2,
					"Invalid supertype. Expecting a class");
		}

		@Test
		public void invalidSkillImplement_0() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"}",
					"skill S1 implements B1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					36, 2,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		public void invalidSkillImplement_1() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 implements B1, C1, C2 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					68, 2,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		public void invalidSkillImplement_2() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 implements C1, B1, C2 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					72, 2,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		public void invalidSkillImplement_3() throws Exception {
			XtendFile mas = file(multilineString(
					"behavior B1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 implements C1, C2, B1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					76, 2,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		public void inheritance() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity CapTest1 {",
					"	def func1 : int",
					"}",
					"capacity CapTest2 extends CapTest1 {",
					"	def func2(a : int)",
					"}"
					), true);
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
		public void emptyCapacity() throws Exception {
			XtendFile mas = file("capacity C1 { }");
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.DISCOURAGED_CAPACITY_DEFINITION,
					"Discouraged capacity definition. A capacity without actions defined inside is not useful since it cannot be called by an agent or a behavior.");
		}

		@Test
		public void skillImplementCapacity() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction",
					"}",
					"skill S1 implements C1 {",
					"	def myaction { }",
					"}"
					), true);
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
		public void skillExtendSkill() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction",
					"}",
					"skill S1 implements C1 {",
					"	def myaction { }",
					"}",
					"skill S2 extends S1 {",
					"	def myaction { }",
					"}"
					), true);
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
		public void skillExtendSkillImplementCapacity() throws Exception {
			XtendFile mas = file(multilineString(
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
					), true);
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
		public void skillNoExtendSkillNoImplementCapacity() throws Exception {
			XtendFile mas = file(multilineString(
					"skill S1 {",
					"	def myaction { }",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_TYPE,
					"Missing implemented type 'io.sarl.lang.core.Capacity' for 'S1'");
		}

	}

	public static class ActionTest extends AbstractSarlTest {

		@Test
		public void multipleActionDefinitionInCapacity() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction(a : int, b : int)",
					"	def myaction(a : int)",
					"	def myaction(a : int)",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_METHOD,
					74, 8,
					"Duplicate method myaction(int) in type C1");
		}

		@Test
		public void multipleActionDefinitionInSkill() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int, b : int) { }",
					"	def myaction(a : int) { }",
					"	def myaction(a : int) { }",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_METHOD,
					109, 8,
					"Duplicate method myaction(int) in type S1");
		}

		@Test
		public void invalidActionNameInCapacity() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction",
					"	def _handle_myaction",
					"	def myaction2",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME,
					"Invalid action name '_handle_myaction'.");
		}

		@Test
		public void invalidActionNameInSkill() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def myaction {",
					"		System.out.println(\"ok\")",
					"	}",
					"	def _handle_myaction {",
					"		System.out.println(\"ko\")",
					"	}",
					"	def myaction2 {",
					"		System.out.println(\"ok\")",
					"	}",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME,
					"Invalid action name '_handle_myaction'.");
		}

		@Test
		public void missedActionImplementation_0() throws Exception {
			XtendFile mas = file(multilineString(
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
					), true);
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
		public void missedActionImplementation_1() throws Exception {
			XtendFile mas = file(multilineString(
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
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
					101, 2,
					"The class S1 must be defined abstract because it does not implement myaction1(int)");
		}

		@Test
		public void missedActionImplementation_2() throws Exception {
			XtendFile mas = file(multilineString(
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
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
					101, 2,
					"The class S1 must be defined abstract because it does not implement myaction1(int)");
		}

		@Test
		public void incompatibleReturnType_0() throws Exception {
			XtendFile mas = file(multilineString(
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
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					165, 5,
					"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_1() throws Exception {
			XtendFile mas = file(multilineString(
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
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					158, 3,
					"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_2() throws Exception {
			XtendFile mas = file(multilineString(
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
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					165, 4,
					"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_3() throws Exception {
			XtendFile mas = file(multilineString(
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
			validate(mas).assertError(
					XbasePackage.eINSTANCE.getXBlockExpression(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from null to int");
		}

		@Test
		public void incompatibleReturnType_4() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : int",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : float {",
					"		return 0f",
					"	}",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					95, 5,
					"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_5() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) // void",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : int {",
					"		return 0",
					"	}",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					97, 3,
					"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_6() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : int",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : void {",
					"		// void",
					"	}",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_7() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : int",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) {",
					"		// int is inferred",
					"	}",
					"}"
					));
			validate(mas).assertError(
					XbasePackage.eINSTANCE.getXBlockExpression(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
					93, 25,
					"Type mismatch: cannot convert from null to int");
		}

		@Test
		public void expectingReturnType_0() throws Exception {
			XtendFile mas = file(multilineString(
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
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlAction(),
					IssueCodes.RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED,
					141, 30,
					"Expecting the return type int. It is recommended to write the return type, even if it is inferred from the overridden function");
		}

		@Test
		public void compatibleReturnType_0() throws Exception {
			XtendFile mas = file(multilineString(
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
					), true);
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
		public void compatibleReturnType_1() throws Exception {
			XtendFile mas = file(multilineString(
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
					), true);
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
		public void compatibleReturnType_2() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : Number",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : Double {",
					"		return 0.0",
					"	}",
					"}"
					), true);
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
		public void compatibleReturnType_3() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : float",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : float {",
					"		return 0f",
					"	}",
					"}"
					), true);
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

	public static class AttributeTest extends AbstractSarlTest {

		@Test
		public void multipleVariableDefinitionInSkill() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	var myfield : int",
					"	var myfield1 : String",
					"	var myfield : double",
					"}"
					));
			validate(mas).assertError(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					88, 7,
					"Duplicate field myfield");
		}

		@Test
		public void multipleValueDefinitionInSkill() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	val myfield : int = 4",
					"	val myfield1 : String = \"\"",
					"	val myfield : double = 5",
					"}"
					));
			validate(mas).assertError(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					97, 7,
					"Duplicate field myfield");
		}

		@Test
		public void missedFinalFieldInitialization() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	val field1 : int = 5",
					"	val field2 : String",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.FIELD_NOT_INITIALIZED,
					16, 69,
					"The blank final field field2 may not have been initialized");
		}

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	val field1 : int = 5",
					"	val field2 : String = \"\"",
					"}"
					), true);
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
			XtendField attr1 = (XtendField) skill.getMembers().get(0);
			assertEquals("field1", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertXExpression(attr1.getInitialValue(), XNumberLiteral.class, "5");
			//
			XtendField attr2 = (XtendField) skill.getMembers().get(1);
			assertEquals("field2", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.String");
			assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "");
		}

		@Test
		public void fieldNameShadowingInSkill() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"	val field1 : int = 5",
					"	def myaction(a : int) { }",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	val field1 : int = 5",
					"	def myaction(a : int) { }",
					"}"
					));
			validate(mas).assertWarning(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
					"The field 'field1' in 'S2' is hidding the inherited field 'S1.field1'.");
		}

	}

	public static class CapacityUsesTest extends AbstractSarlTest {

		@Test
		public void invalidCapacityTypeForUses() throws Exception {
			XtendFile mas = file(multilineString(
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
			validate(mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_CAPACITY_TYPE,
					"Invalid type: 'E1'. Only capacities can be used after the keyword 'uses'");
		}

		@Test
		public void invalidCapacityTypeForRequires() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : float",
					"}",
					"event E1 {",
					"	var abc : int",
					"}",
					"behavior B1 {",
					"	requires C1, E1",
					"}"
					));
			validate(mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_CAPACITY_TYPE,
					"Invalid type: 'E1'. Only capacities can be used after the keyword 'requires'");
		}

		@Test
		public void agentUnsuedCapacity_0() throws Exception {
			XtendFile mas = file(multilineString(
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
			List<Issue> issues = issues(mas);
			assertWarning(
					issues,
					mas,
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.UNUSED_AGENT_CAPACITY,
					"The capacity 'C1' is not used");
			assertNoMoreIssues(issues, mas);
		}

		@Test
		public void agentUnsuedCapacity_1() throws Exception {
			XtendFile mas = file(multilineString(
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
			List<Issue> issues = issues(mas);
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
		public void agentUnsuedCapacity_2() throws Exception {
			XtendFile mas = file(multilineString(
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
					), true);
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
		public void multipleCapacityUses_0() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 { def testFct }",
					"skill S1 implements C3 {",
					"	uses C1, C2, C1",
					"	def testFct { }",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.REDUNDANT_CAPACITY_USE,
					"Redundant use of the capacity 'C1'");
		}

		@Test
		public void multipleCapacityUses_1() throws Exception {
			XtendFile mas = file(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 { def testFct }",
					"skill S1 implements C3 {",
					"	uses C2",
					"	def testFct { }",
					"	uses C2, C1",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.REDUNDANT_CAPACITY_USE,
					"Redundant use of the capacity 'C2'");
		}

	}

}
