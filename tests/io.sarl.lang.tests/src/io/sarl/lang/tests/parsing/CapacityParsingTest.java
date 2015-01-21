/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.CapacityUses;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.validation.IssueCodes;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
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

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class TopElementTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void capacityDirectImplementation() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"import io.sarl.lang.core.Capacity",
					"skill S1 implements Capacity {",
					"}"
					));
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					"Invalid implemented type: 'io.sarl.lang.core.Capacity'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed for 'S1'");
		}

		@Test
		public void redundantCapacity_fromSuperType() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"skill S1 implements C1 { }",
					"skill S2 extends S1 implements C2, C1 { }"
					));
			this.validator.assertWarning(mas,
					SarlPackage.eINSTANCE.getSkill(),
					IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
					"The feature 'C1' is already implemented by the super-type 'S1'.");
		}

		@Test
		public void redundantCapacity_duplicate() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 {}",
					"skill S1 implements C1 { }",
					"skill S2 extends S1 implements C2, C3, C2 { }"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getSkill(),
					IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
					"Duplicate implemented feature 'C2'");
		}

		@Test
		public void redundantCapacity_fromPreviousCapacity() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 extends C2 {}",
					"skill S1 implements C1 { }",
					"skill S2 extends S1 implements C3, C2 { }"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getSkill(),
					IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION,
					"Duplicate implemented feature 'C2'");
		}

		@Test
		public void invalidCapacityExtend_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 extends A1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Invalid supertype. Expecting: interface");
		}

		@Test
		public void invalidCapacityExtend_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 extends A1, C1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Invalid supertype. Expecting: interface");
		}

		@Test
		public void invalidCapacityExtend_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 extends C1, A1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Invalid supertype. Expecting: interface");
		}

		@Test
		public void invalidCapacityExtend_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends A1, C1, C2 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Invalid supertype. Expecting: interface");
		}

		@Test
		public void invalidCapacityExtend_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, A1, C2 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Invalid supertype. Expecting: interface");
		}

		@Test
		public void invalidCapacityExtend_5() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, C2, A1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Invalid supertype. Expecting: interface");
		}

		@Test
		public void invalidCapacityExtend_6() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 extends java.lang.Cloneable {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_7() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 extends java.lang.Cloneable, C1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_8() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 extends C1, java.lang.Cloneable {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_9() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends java.lang.Cloneable, C1, C2 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_10() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, java.lang.Cloneable, C2 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_11() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"capacity C3 extends C1, C2, java.lang.Cloneable {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Capacity'");
		}

		@Test
		public void invalidCapacityExtend_12() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 extends C1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'C1' is inconsistent");
		}

		@Test
		public void invalidCapacityExtend_13() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 extends C2 {",
					"}",
					"capacity C2 extends C1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'C1' is inconsistent");
		}

		@Test
		public void invalidCapacityExtend_14() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 extends C3 {",
					"}",
					"capacity C2 extends C1 {",
					"}",
					"capacity C3 extends C2 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'C1' is inconsistent");
		}

		@Test
		public void invalidCapacityExtend_15() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"capacity C3 extends C1, C2, C3 { }"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'C3' is inconsistent");
		}

		@Test
		public void invalidCapacityExtend_16() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"capacity C3 extends C1, C3, C2 { }"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'C3' is inconsistent");
		}

		@Test
		public void invalidCapacityExtend_17() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"capacity C3 extends C3, C1, C3 { }"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'C3' is inconsistent");
		}

		@Test
		public void invalidSkillExtend_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"}",
					"agent A1 {",
					"}",
					"skill S1 extends A1 implements C1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getSkill(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Skill'");
		}

		@Test
		public void invalidSkillExtend_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 extends C1 implements C2 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getSkill(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Invalid supertype. Expecting: class");
		}

		@Test
		public void invalidSkillImplement_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"}",
					"skill S1 implements B1 {",
					"}"
					));
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed");
		}

		@Test
		public void invalidSkillImplement_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 implements B1, C1, C2 {",
					"}"
					));
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed");
		}

		@Test
		public void invalidSkillImplement_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 implements C1, B1, C2 {",
					"}"
					));
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed");
		}

		@Test
		public void invalidSkillImplement_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"}",
					"capacity C1 {",
					"}",
					"capacity C2 {",
					"}",
					"skill S1 implements C1, C2, B1 {",
					"}"
					));
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_IMPLEMENTED_TYPE,
					"Invalid implemented type: 'B1'. Only subtypes of 'io.sarl.lang.core.Capacity' are allowed");
		}

		@Test
		public void inheritance() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity CapTest1 {",
					"	def func1 : int",
					"}",
					"capacity CapTest2 extends CapTest1 {",
					"	def func2(a : int)",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity1 = (Capacity) mas.getElements().get(0);
			assertEquals("CapTest1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getSuperTypes());
			assertEquals(1, capacity1.getFeatures().size());
			//
			ActionSignature signature1 = (ActionSignature) capacity1.getFeatures().get(0);
			assertEquals("func1", signature1.getName());
			assertTypeReferenceIdentifiers(signature1.getFiredEvents());
			assertTypeReferenceIdentifier(signature1.getType(), "int");
			assertParameterNames(signature1.getParams());
			//
			Capacity capacity2 = (Capacity) mas.getElements().get(1);
			assertEquals("CapTest2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getSuperTypes(), "CapTest1");
			assertEquals(1, capacity2.getFeatures().size());
			//
			ActionSignature signature2 = (ActionSignature) capacity2.getFeatures().get(0);
			assertEquals("func2", signature2.getName());
			assertTypeReferenceIdentifiers(signature2.getFiredEvents());
			assertTypeReferenceIdentifier(signature2.getType(), "void");
			assertParameterNames(signature2.getParams(), "a");
			assertParameterTypes(signature2.getParams(), "int");
			assertParameterDefaultValues(signature2.getParams(), (Object) null);
		}

		@Test
		public void emptyCapacity() throws Exception {
			SarlScript mas = this.parser.parse("capacity C1 { }");
			this.validator.assertWarning(mas,
					SarlPackage.eINSTANCE.getCapacity(),
					IssueCodes.DISCOURAGED_CAPACITY_DEFINITION,
					"Discouraged capacity definition. A capacity without actions defined inside is not useful since it cannot be called by an agent or a behavior.");
		}

		@Test
		public void skillImplementCapacity() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction",
					"}",
					"skill S1 implements C1 {",
					"	def myaction { }",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity = (Capacity) mas.getElements().get(0);
			assertEquals("C1", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getSuperTypes());
			assertEquals(1, capacity.getFeatures().size());
			//
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertEquals("myaction", signature.getName());
			assertTypeReferenceIdentifiers(signature.getFiredEvents());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(signature.getParams());
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(1, skill.getFeatures().size());
			//
			Action action = (Action) skill.getFeatures().get(0);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getType(), "void");
			assertParameterNames(action.getParams());
		}

		@Test
		public void skillExtendSkill() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertNoErrors(mas);
			assertEquals(3, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity = (Capacity) mas.getElements().get(0);
			assertEquals("C1", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getSuperTypes());
			assertEquals(1, capacity.getFeatures().size());
			//
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertEquals("myaction", signature.getName());
			assertTypeReferenceIdentifiers(signature.getFiredEvents());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(signature.getParams());
			//
			Skill skill1 = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill1.getName());
			assertTypeReferenceIdentifiers(skill1.getSuperTypes());
			assertTypeReferenceIdentifiers(skill1.getImplementedTypes(), "C1");
			assertEquals(1, skill1.getFeatures().size());
			//
			Action action1 = (Action) skill1.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams());
			//
			Skill skill2 = (Skill) mas.getElements().get(2);
			assertEquals("S2", skill2.getName());
			assertTypeReferenceIdentifiers(skill2.getSuperTypes(), "S1");
			assertTypeReferenceIdentifiers(skill2.getImplementedTypes());
			assertEquals(1, skill2.getFeatures().size());
			//
			Action action2 = (Action) skill2.getFeatures().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getType(), "void");
			assertParameterNames(action2.getParams());
		}

		@Test
		public void skillExtendSkillImplementCapacity() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertNoErrors(mas);
			assertEquals(4, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity1 = (Capacity) mas.getElements().get(0);
			assertEquals("C1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getSuperTypes());
			assertEquals(1, capacity1.getFeatures().size());
			//
			ActionSignature signature1 = (ActionSignature) capacity1.getFeatures().get(0);
			assertEquals("myaction", signature1.getName());
			assertTypeReferenceIdentifiers(signature1.getFiredEvents());
			assertTypeReferenceIdentifier(signature1.getType(), "void");
			assertParameterNames(signature1.getParams());
			//
			Capacity capacity2 = (Capacity) mas.getElements().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getSuperTypes());
			assertEquals(1, capacity2.getFeatures().size());
			//
			ActionSignature signature2 = (ActionSignature) capacity2.getFeatures().get(0);
			assertEquals("myaction2", signature2.getName());
			assertTypeReferenceIdentifiers(signature2.getFiredEvents());
			assertTypeReferenceIdentifier(signature2.getType(), "void");
			assertParameterNames(signature2.getParams());
			//
			Skill skill1 = (Skill) mas.getElements().get(2);
			assertEquals("S1", skill1.getName());
			assertTypeReferenceIdentifiers(skill1.getSuperTypes());
			assertTypeReferenceIdentifiers(skill1.getImplementedTypes(), "C1");
			assertEquals(1, skill1.getFeatures().size());
			//
			Action action1 = (Action) skill1.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams());
			//
			Skill skill2 = (Skill) mas.getElements().get(3);
			assertEquals("S2", skill2.getName());
			assertTypeReferenceIdentifiers(skill2.getSuperTypes(), "S1");
			assertTypeReferenceIdentifiers(skill2.getImplementedTypes(), "C2");
			assertEquals(2, skill2.getFeatures().size());
			//
			Action action2 = (Action) skill2.getFeatures().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getType(), "void");
			assertParameterNames(action2.getParams());
			//
			Action action3 = (Action) skill2.getFeatures().get(1);
			assertEquals("myaction2", action3.getName());
			assertTypeReferenceIdentifiers(action3.getFiredEvents());
			assertTypeReferenceIdentifier(action3.getType(), "void");
			assertParameterNames(action3.getParams());
		}

		@Test
		public void skillNoExtendSkillNoImplementCapacity() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"skill S1 {",
					"	def myaction { }",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getSkill(),
					org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_TYPE,
					"Missing implemented type 'io.sarl.lang.core.Capacity' for 'S1'");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class ActionTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void multipleActionDefinitionInCapacity() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction(a : int, b : int)",
					"	def myaction(a : int)",
					"	def myaction(a : int)",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getActionSignature(),
					IssueCodes.DUPLICATE_METHOD,
					"Duplicate action in 'C1': myaction(a : int)");
		}

		@Test
		public void multipleActionDefinitionInSkill() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int, b : int) { }",
					"	def myaction(a : int) { }",
					"	def myaction(a : int) { }",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					IssueCodes.DUPLICATE_METHOD,
					"Duplicate action in 'S1': myaction(a : int)");
		}

		@Test
		public void invalidActionNameInCapacity() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction",
					"	def _handle_myaction",
					"	def myaction2",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getActionSignature(),
					IssueCodes.INVALID_MEMBER_NAME,
					"Invalid action name '_handle_myaction'.");
		}

		@Test
		public void invalidActionNameInSkill() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					IssueCodes.INVALID_MEMBER_NAME,
					"Invalid action name '_handle_myaction'.");
		}

		@Test
		public void missedActionImplementation_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertNoErrors(mas);
			assertEquals(3, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity1 = (Capacity) mas.getElements().get(0);
			assertEquals("C1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getSuperTypes());
			assertEquals(1, capacity1.getFeatures().size());
			//
			ActionSignature signature1 = (ActionSignature) capacity1.getFeatures().get(0);
			assertEquals("myaction1", signature1.getName());
			assertTypeReferenceIdentifiers(signature1.getFiredEvents());
			assertTypeReferenceIdentifier(signature1.getType(), "void");
			assertParameterNames(signature1.getParams(), "a");
			assertParameterTypes(signature1.getParams(), "int");
			assertParameterDefaultValues(signature1.getParams(), (Object) null);
			//
			Capacity capacity2 = (Capacity) mas.getElements().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getSuperTypes());
			assertEquals(1, capacity2.getFeatures().size());
			//
			ActionSignature signature2 = (ActionSignature) capacity2.getFeatures().get(0);
			assertEquals("myaction2", signature2.getName());
			assertTypeReferenceIdentifiers(signature2.getFiredEvents());
			assertTypeReferenceIdentifier(signature2.getType(), "void");
			assertParameterNames(signature2.getParams(), "b", "c");
			assertParameterTypes(signature2.getParams(), "float", "boolean");
			assertParameterDefaultValues(signature2.getParams(), null, null);
			//
			Skill skill = (Skill) mas.getElements().get(2);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1", "C2");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("myaction1", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams(), "x");
			assertParameterTypes(action1.getParams(), "int");
			assertParameterDefaultValues(action1.getParams(), (Object) null);
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction2", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getType(), "void");
			assertParameterNames(action2.getParams(), "y", "z");
			assertParameterTypes(action2.getParams(), "float", "boolean");
			assertParameterDefaultValues(action2.getParams(), null, null);
		}

		@Test
		public void missedActionImplementation_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getSkill(),
					IssueCodes.MISSING_METHOD_IMPLEMENTATION,
					"The operation myaction1(int) must be implemented.");
		}

		@Test
		public void missedActionImplementation_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getSkill(),
					IssueCodes.MISSING_METHOD_IMPLEMENTATION,
					"The operation myaction1(int) must be implemented.");
		}

		@Test
		public void incompatibleReturnType_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					"Incompatible return type between 'float' and 'int' for myaction(int).");
		}

		@Test
		public void incompatibleReturnType_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					"Incompatible return type between 'int' and 'void' for myaction(int).");
		}

		@Test
		public void incompatibleReturnType_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 { }",
					"capacity C2 { }",
					"skill S1 implements C1 {",
					"	def myaction(a : int) : int {",
					"		return 0",
					"	}",
					"}",
					"skill S2 extends S1 implements C2 {",
					"	def myaction(a : int) {",
					"		// void",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					"Incompatible return type between 'void' and 'int' for myaction(int).");
		}

		@Test
		public void incompatibleReturnType_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : int",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : float {",
					"		return 0f",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					"Incompatible return type between 'float' and 'int' for myaction(int).");
		}

		@Test
		public void incompatibleReturnType_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) // void",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : int {",
					"		return 0",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					"Incompatible return type between 'int' and 'void' for myaction(int).");
		}

		@Test
		public void incompatibleReturnType_5() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : int",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) {",
					"		// void",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					"Incompatible return type between 'void' and 'int' for myaction(int).");
		}

		@Test
		public void compatibleReturnType_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertNoErrors(mas);
			assertEquals(4, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity1 = (Capacity) mas.getElements().get(0);
			assertEquals("C1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getSuperTypes());
			assertEquals(0, capacity1.getFeatures().size());
			//
			Capacity capacity2 = (Capacity) mas.getElements().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getSuperTypes());
			assertEquals(0, capacity2.getFeatures().size());
			//
			Skill skill1 = (Skill) mas.getElements().get(2);
			assertEquals("S1", skill1.getName());
			assertTypeReferenceIdentifiers(skill1.getSuperTypes());
			assertTypeReferenceIdentifiers(skill1.getImplementedTypes(), "C1");
			assertEquals(1, skill1.getFeatures().size());
			//
			Action action1 = (Action) skill1.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "java.lang.Number");
			assertParameterNames(action1.getParams(), "a");
			assertParameterTypes(action1.getParams(), "int");
			assertParameterDefaultValues(action1.getParams(), (Object) null);
			//
			Skill skill2 = (Skill) mas.getElements().get(3);
			assertEquals("S2", skill2.getName());
			assertTypeReferenceIdentifiers(skill2.getSuperTypes(), "S1");
			assertTypeReferenceIdentifiers(skill2.getImplementedTypes(), "C2");
			assertEquals(1, skill2.getFeatures().size());
			//
			Action action2 = (Action) skill2.getFeatures().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getType(), "java.lang.Double");
			assertParameterNames(action2.getParams(), "a");
			assertParameterTypes(action2.getParams(), "int");
			assertParameterDefaultValues(action2.getParams(), (Object) null);
		}

		@Test
		public void compatibleReturnType_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertNoErrors(mas);
			assertEquals(4, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity1 = (Capacity) mas.getElements().get(0);
			assertEquals("C1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getSuperTypes());
			assertEquals(0, capacity1.getFeatures().size());
			//
			Capacity capacity2 = (Capacity) mas.getElements().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getSuperTypes());
			assertEquals(0, capacity2.getFeatures().size());
			//
			Skill skill1 = (Skill) mas.getElements().get(2);
			assertEquals("S1", skill1.getName());
			assertTypeReferenceIdentifiers(skill1.getSuperTypes());
			assertTypeReferenceIdentifiers(skill1.getImplementedTypes(), "C1");
			assertEquals(1, skill1.getFeatures().size());
			//
			Action action1 = (Action) skill1.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "float");
			assertParameterNames(action1.getParams(), "a");
			assertParameterTypes(action1.getParams(), "int");
			assertParameterDefaultValues(action1.getParams(), (Object) null);
			//
			Skill skill2 = (Skill) mas.getElements().get(3);
			assertEquals("S2", skill2.getName());
			assertTypeReferenceIdentifiers(skill2.getSuperTypes(), "S1");
			assertTypeReferenceIdentifiers(skill2.getImplementedTypes(), "C2");
			assertEquals(1, skill2.getFeatures().size());
			//
			Action action2 = (Action) skill2.getFeatures().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getType(), "float");
			assertParameterNames(action2.getParams(), "a");
			assertParameterTypes(action2.getParams(), "int");
			assertParameterDefaultValues(action2.getParams(), (Object) null);
		}

		@Test
		public void compatibleReturnType_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : Number",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : Double {",
					"		return 0.0",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity = (Capacity) mas.getElements().get(0);
			assertEquals("C1", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getSuperTypes());
			assertEquals(1, capacity.getFeatures().size());
			//
			ActionSignature action1 = (ActionSignature) capacity.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "java.lang.Number");
			assertParameterNames(action1.getParams(), "a");
			assertParameterTypes(action1.getParams(), "int");
			assertParameterDefaultValues(action1.getParams(), (Object) null);
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S2", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(1, skill.getFeatures().size());
			//
			Action action2 = (Action) skill.getFeatures().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getType(), "java.lang.Double");
			assertParameterNames(action2.getParams(), "a");
			assertParameterTypes(action2.getParams(), "int");
			assertParameterDefaultValues(action2.getParams(), (Object) null);
		}

		@Test
		public void compatibleReturnType_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction(a : int) : float",
					"}",
					"skill S2 implements C1 {",
					"	def myaction(a : int) : float {",
					"		return 0f",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity = (Capacity) mas.getElements().get(0);
			assertEquals("C1", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getSuperTypes());
			assertEquals(1, capacity.getFeatures().size());
			//
			ActionSignature action1 = (ActionSignature) capacity.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "float");
			assertParameterNames(action1.getParams(), "a");
			assertParameterTypes(action1.getParams(), "int");
			assertParameterDefaultValues(action1.getParams(), (Object) null);
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S2", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(1, skill.getFeatures().size());
			//
			Action action2 = (Action) skill.getFeatures().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getType(), "float");
			assertParameterNames(action2.getParams(), "a");
			assertParameterTypes(action2.getParams(), "int");
			assertParameterDefaultValues(action2.getParams(), (Object) null);
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class AttributeTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void multipleVariableDefinitionInSkill() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	var myfield : int",
					"	var myfield1 : String",
					"	var myfield : double",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					IssueCodes.DUPLICATE_FIELD,
					"Duplicate field in 'S1': myfield");
		}

		@Test
		public void multipleValueDefinitionInSkill() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	val myfield : int = 4",
					"	val myfield1 : String = \"\"",
					"	val myfield : double = 5",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					IssueCodes.DUPLICATE_FIELD,
					"Duplicate field in 'S1': myfield");
		}

		@Test
		public void missedFinalFieldInitialization() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	val field1 : int = 5",
					"	val field2 : String",
					"}"
					));
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmConstructor(),
					org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_INITIALIZATION,
					"The blank final field 'field2' may not have been initialized");
		}

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	val field1 : int = 5",
					"	val field2 : String = \"\"",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity = (Capacity) mas.getElements().get(0);
			assertEquals("C1", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getSuperTypes());
			assertEquals(0, capacity.getFeatures().size());
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Attribute attr1 = (Attribute) skill.getFeatures().get(0);
			assertEquals("field1", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertXExpression(attr1.getInitialValue(), XNumberLiteral.class, "5");
			//
			Attribute attr2 = (Attribute) skill.getFeatures().get(1);
			assertEquals("field2", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.String");
			assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "");
		}

		@Test
		public void fieldNameShadowingInSkill() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertWarning(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
					"The field 'field1' in 'S2' is hidding the inherited field 'S1.field1'.");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class CapacityUsesTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void invalidCapacityTypeForUses() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_CAPACITY_TYPE,
					"Invalid type: 'E1'. Only capacities can be used after the keyword 'uses'");
		}

		@Test
		public void invalidCapacityTypeForRequires() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_CAPACITY_TYPE,
					"Invalid type: 'E1'. Only capacities can be used after the keyword 'requires'");
		}

		@Test
		public void agentUnsuedCapacity_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			List<Issue> issues = this.validator.validate(mas);
			assertWarning(
					issues,
					mas,
					SarlPackage.eINSTANCE.getCapacityUses(),
					IssueCodes.UNUSED_AGENT_CAPACITY,
					"The capacity 'C1' is not used");
			assertNoMoreIssues(issues, mas);
		}

		@Test
		public void agentUnsuedCapacity_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			List<Issue> issues = this.validator.validate(mas);
			assertWarning(
					issues,
					mas,
					SarlPackage.eINSTANCE.getCapacityUses(),
					IssueCodes.UNUSED_AGENT_CAPACITY,
					"The capacity 'C1' is not used");
			assertWarning(
					issues,
					mas,
					SarlPackage.eINSTANCE.getCapacityUses(),
					IssueCodes.UNUSED_AGENT_CAPACITY,
					"The capacity 'C2' is not used");
			assertNoMoreIssues(issues, mas);
		}

		@Test
		public void agentUnsuedCapacity_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertNoIssues(mas);
			assertEquals(3, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity1 = (Capacity) mas.getElements().get(0);
			assertEquals("C1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getSuperTypes());
			assertEquals(1, capacity1.getFeatures().size());
			//
			ActionSignature signature1 = (ActionSignature) capacity1.getFeatures().get(0);
			assertEquals("myfct", signature1.getName());
			assertTypeReferenceIdentifiers(signature1.getFiredEvents());
			assertTypeReferenceIdentifier(signature1.getType(), "void");
			assertParameterNames(signature1.getParams());
			//
			Capacity capacity2 = (Capacity) mas.getElements().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getSuperTypes());
			assertEquals(1, capacity2.getFeatures().size());
			//
			ActionSignature signature2 = (ActionSignature) capacity2.getFeatures().get(0);
			assertEquals("myfct2", signature2.getName());
			assertTypeReferenceIdentifiers(signature2.getFiredEvents());
			assertTypeReferenceIdentifier(signature2.getType(), "void");
			assertParameterNames(signature2.getParams());
			//
			Agent agent = (Agent) mas.getElements().get(2);
			assertEquals("A1", agent.getName());
			assertTypeReferenceIdentifiers(agent.getSuperTypes());
			assertEquals(2, agent.getFeatures().size());
			//
			CapacityUses uses = (CapacityUses) agent.getFeatures().get(0);
			assertTypeReferenceIdentifiers(uses.getCapacitiesUsed(), "C2", "C1");
			//
			Action action = (Action) agent.getFeatures().get(1);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getType(), "void");
			assertParameterNames(action.getParams());
		}

		@Test
		public void multipleCapacityUses_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 { def testFct }",
					"skill S1 implements C3 {",
					"	uses C1, C2, C1",
					"	def testFct { }",
					"}"
					));
			this.validator.assertWarning(mas,
					SarlPackage.eINSTANCE.getCapacityUses(),
					IssueCodes.REDUNDANT_CAPACITY_USE,
					"Redundant use of the capacity 'C1'");
		}

		@Test
		public void multipleCapacityUses_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"capacity C3 { def testFct }",
					"skill S1 implements C3 {",
					"	uses C2",
					"	def testFct { }",
					"	uses C2, C1",
					"}"
					));
			this.validator.assertWarning(mas,
					SarlPackage.eINSTANCE.getCapacityUses(),
					IssueCodes.REDUNDANT_CAPACITY_USE,
					"Redundant use of the capacity 'C2'");
		}

	}

}
