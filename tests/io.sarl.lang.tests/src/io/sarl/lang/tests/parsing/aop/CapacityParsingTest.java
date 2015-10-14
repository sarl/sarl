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
package io.sarl.lang.tests.parsing.aop;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.inject.Inject;

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.validation.IssueCodes;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmVisibility;
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
	
	/** Check if the given value is <code>null</code> or empty.
	 *
	 * @param actual
	 */
	public static void assertNullOrEmpty(Iterable<?> actual) {
		if (actual != null) {
			assertFalse("Not null nor empty", actual.iterator().hasNext());
		}
	}

	public static class TopElementTest extends AbstractSarlTest {

		@Test
		public void invalidCapacityExtend_0() throws Exception {
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
		public void inheritance() throws Exception {
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file("capacity C1 { }");
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					IssueCodes.DISCOURAGED_CAPACITY_DEFINITION,
					"Discouraged capacity definition. A capacity without actions defined inside is not useful since it cannot be called by an agent or a behavior.");
		}

		@Test
		public void capacitymodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public capacity C1 {}"
					), true);
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
		public void capacitymodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {}"
					), true);
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
		public void capacitymodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"private capacity C1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlCapacity cap = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", cap.getName());
			assertNullOrEmpty(cap.getExtends());
			assertEquals(JvmVisibility.PRIVATE, cap.getVisibility());
			assertEquals(0, cap.getMembers().size());
			assertFalse(cap.isFinal());
		}

		@Test
		public void capacitymodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"protected capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 9,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"package capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 7,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 8,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"static capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 6,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 8,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"final capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 5,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 8,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"native capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 6,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"volatile capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 8,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 12,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"transient capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 9,
					"Illegal modifier for the capacity C1; only public & private are permitted");
		}

		@Test
		public void capacitymodifier_public_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public private capacity C1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlCapacity(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					39, 7,
					"The capacity C1 can only set one of public / package / protected / private");
		}

	}

	public static class ActionTest extends AbstractSarlTest {

		@Test
		public void multipleActionDefinitionInCapacity() throws Exception {
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
		public void modifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	public def name",
					"}"), true);
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
		public void modifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	private def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 7,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	protected def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 9,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	package def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 7,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	def name",
					"}"), true);
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
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	abstract def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 8,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	static def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 6,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	dispatch def name(a : Integer)",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 8,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	final def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 5,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	strictfp def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 8,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	native def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 6,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	volatile def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 8,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	synchronized def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 12,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	transient def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					47, 9,
					"Illegal modifier for the definition of name in C1; only public, def & override are permitted");
		}

		@Test
		public void modifier_protected_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"capacity C1 {",
					"	protected private def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					57, 7,
					"The definition of name in C1 can only set one of public / package / protected / private");
		}

	}

	public static class CapacityUsesTest extends AbstractSarlTest {

		@Test
		public void invalidCapacityTypeForUses() throws Exception {
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
			SarlScript mas = file(multilineString(
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
