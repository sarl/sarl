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

import com.google.common.base.Strings;
import com.google.inject.Inject;

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.Skill;

import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import static org.junit.Assert.*;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	VarArgsParsingTest.AgentTest.class,
	VarArgsParsingTest.BehaviorTest.class,
	VarArgsParsingTest.SkillTest.class,
	VarArgsParsingTest.CapacityTest.class,
	VarArgsParsingTest.EventTest.class,
})
@SuppressWarnings("all")
public class VarArgsParsingTest {

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class AgentTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void action_singleParam() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"	def myaction(arg : int*) {",
					"		System.out.println(arg)",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Agent agent = (Agent) mas.getElements().get(0);
			assertEquals("A1", agent.getName());
			assertTypeReferenceIdentifiers(agent.getSuperTypes());
			assertEquals(1, agent.getFeatures().size());
			//
			Action action = (Action) agent.getFeatures().get(0);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getType(), "void");
			assertParameterNames(action.getParams(), "arg");
			assertParameterTypes(action.getParams(), "int");
			assertParameterDefaultValues(action.getParams(), (Object) null);
		}

		@Test
		public void inAgentAction() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"	def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Agent agent = (Agent) mas.getElements().get(0);
			assertEquals("A1", agent.getName());
			assertTypeReferenceIdentifiers(agent.getSuperTypes());
			assertEquals(1, agent.getFeatures().size());
			//
			Action action = (Action) agent.getFeatures().get(0);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getType(), "void");
			assertParameterNames(action.getParams(), "arg1", "arg2", "arg3");
			assertParameterTypes(action.getParams(), "char", "boolean", "int");
			assertTrue(action.isVarargs());
			assertParameterDefaultValues(action.getParams(), null, null, null);
		}

		@Test
		public void action_invalid() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"	def myaction(arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ',' expecting ')'");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class BehaviorTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void action_singleParam() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	def myaction(arg : int*) {",
					"		System.out.println(arg)",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Behavior behavior = (Behavior) mas.getElements().get(0);
			assertEquals("B1", behavior.getName());
			assertTypeReferenceIdentifiers(behavior.getSuperTypes());
			assertEquals(1, behavior.getFeatures().size());
			//
			Action action = (Action) behavior.getFeatures().get(0);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getType(), "void");
			assertParameterNames(action.getParams(), "arg");
			assertParameterTypes(action.getParams(), "int");
			assertTrue(action.isVarargs());
			assertParameterDefaultValues(action.getParams(), (Object) null);
		}

		@Test
		public void action() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Behavior behavior = (Behavior) mas.getElements().get(0);
			assertEquals("B1", behavior.getName());
			assertTypeReferenceIdentifiers(behavior.getSuperTypes());
			assertEquals(1, behavior.getFeatures().size());
			//
			Action action = (Action) behavior.getFeatures().get(0);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getType(), "void");
			assertParameterNames(action.getParams(), "arg1", "arg2", "arg3");
			assertParameterTypes(action.getParams(), "char", "boolean", "int");
			assertTrue(action.isVarargs());
			assertParameterDefaultValues(action.getParams(), null, null, null);
		}

		@Test
		public void action_invalid() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	def myaction(arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ',' expecting ')'");
		}

		@Test
		public void constructor_singleParam() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	new(arg : int*) {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg)",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Behavior behavior = (Behavior) mas.getElements().get(0);
			assertEquals("B1", behavior.getName());
			assertTypeReferenceIdentifiers(behavior.getSuperTypes());
			assertEquals(1, behavior.getFeatures().size());
			//
			Constructor constructor = (Constructor) behavior.getFeatures().get(0);
			assertParameterNames(constructor.getParams(), "arg");
			assertParameterTypes(constructor.getParams(), "int");
			assertTrue(constructor.isVarargs());
			assertParameterDefaultValues(constructor.getParams(), (Object) null);
		}

		@Test
		public void constructor() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	new (arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Behavior behavior = (Behavior) mas.getElements().get(0);
			assertEquals("B1", behavior.getName());
			assertTypeReferenceIdentifiers(behavior.getSuperTypes());
			assertEquals(1, behavior.getFeatures().size());
			//
			Constructor constructor = (Constructor) behavior.getFeatures().get(0);
			assertParameterNames(constructor.getParams(), "arg1", "arg2", "arg3");
			assertParameterTypes(constructor.getParams(), "char", "boolean", "int");
			assertTrue(constructor.isVarargs());
			assertParameterDefaultValues(constructor.getParams(), null, null, null);
		}

		@Test
		public void constructor_invalid() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	new (arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getConstructor(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ',' expecting ')'");
		}

		@Test
		public void multipleActionDefinitionsInBehavior_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int, arg1 : int*) {",
					"		System.out.println(\"invalid\")",
					"	}",
					"	def myaction {",
					"		System.out.println(\"invalid\")",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Behavior behavior = (Behavior) mas.getElements().get(0);
			assertEquals("B1", behavior.getName());
			assertTypeReferenceIdentifiers(behavior.getSuperTypes());
			assertEquals(2, behavior.getFeatures().size());
			//
			Action action1 = (Action) behavior.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams(), "arg0", "arg1");
			assertParameterTypes(action1.getParams(), "int", "int");
			assertTrue(action1.isVarargs());
			assertParameterDefaultValues(action1.getParams(), null, null);
			//
			Action action2 = (Action) behavior.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getType(), "void");
			assertParameterNames(action2.getParams());
		}

		@Test
		public void multipleActionDefinitionsInBehavior_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int, arg1 : int*) {",
					"		System.out.println(\"invalid\")",
					"	}",
					"	def myaction(arg0 : int) {",
					"		System.out.println(\"invalid\")",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Behavior behavior = (Behavior) mas.getElements().get(0);
			assertEquals("B1", behavior.getName());
			assertTypeReferenceIdentifiers(behavior.getSuperTypes());
			assertEquals(2, behavior.getFeatures().size());
			//
			Action action1 = (Action) behavior.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams(), "arg0", "arg1");
			assertParameterTypes(action1.getParams(), "int", "int");
			assertTrue(action1.isVarargs());
			assertParameterDefaultValues(action1.getParams(), null, null);
			//
			Action action2 = (Action) behavior.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0");
			assertParameterTypes(action2.getParams(), "int");
			assertParameterDefaultValues(action2.getParams(), (Object) null);
		}

		@Test
		public void multipleActionDefinitionsInBehavior_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int, arg1 : int*) {",
					"		System.out.println(\"invalid\")",
					"	}",
					"	def myaction(arg0 : int, arg1 : int) {",
					"		System.out.println(\"invalid\")",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					io.sarl.lang.validation.IssueCodes.DUPLICATE_METHOD,
					"Duplicate action in 'B1': myaction(arg0 : int, arg1 : int)");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class SkillTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void action_singleParam() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	def myaction(arg : int*) {",
					"		System.out.println(arg)",
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
			assertEquals(0, capacity.getFeatures().size());
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(1, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams(), "arg");
			assertParameterTypes(action1.getParams(), "int");
			assertTrue(action1.isVarargs());
			assertParameterDefaultValues(action1.getParams(), (Object) null);
		}

		@Test
		public void action() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		System.out.println(arg3)",
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
			assertEquals(0, capacity.getFeatures().size());
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(1, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams(), "arg1", "arg2", "arg3");
			assertParameterTypes(action1.getParams(), "char", "boolean", "int");
			assertTrue(action1.isVarargs());
			assertParameterDefaultValues(action1.getParams(), null, null, null);
		}

		@Test
		public void action_invalid() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	def myaction(arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ',' expecting ')'");
		}

		@Test
		public void constructor_singleParam() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	new(arg : int*) {",
					"		System.out.println(arg)",
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
			assertEquals(0, capacity.getFeatures().size());
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(1, skill.getFeatures().size());
			//
			Constructor constructor = (Constructor) skill.getFeatures().get(0);
			assertParameterNames(constructor.getParams(), "arg");
			assertParameterTypes(constructor.getParams(), "int");
			assertTrue(constructor.isVarargs());
			assertParameterDefaultValues(constructor.getParams(), (Object) null);
		}

		@Test
		public void constructor() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	new (arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		System.out.println(arg3)",
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
			assertEquals(0, capacity.getFeatures().size());
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(1, skill.getFeatures().size());
			//
			Constructor constructor = (Constructor) skill.getFeatures().get(0);
			assertParameterNames(constructor.getParams(), "arg1", "arg2", "arg3");
			assertParameterTypes(constructor.getParams(), "char", "boolean", "int");
			assertTrue(constructor.isVarargs());
			assertParameterDefaultValues(constructor.getParams(), null, null, null);
		}

		@Test
		public void constructor_invalid() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	new (arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getConstructor(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ',' expecting ')'");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class CapacityTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void action_singleParam() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction(arg : int*)",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
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
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams(), "arg");
			assertParameterTypes(action1.getParams(), "int");
			assertTrue(action1.isVarargs());
			assertParameterDefaultValues(action1.getParams(), (Object) null);
		}

		@Test
		public void action() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction(arg1 : char, arg2 : boolean, arg3 : int*)",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
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
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams(), "arg1", "arg2", "arg3");
			assertParameterTypes(action1.getParams(), "char", "boolean", "int");
			assertTrue(action1.isVarargs());
			assertParameterDefaultValues(action1.getParams(), null, null, null);
		}

		@Test
		public void action_invalid() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction(arg1 : char, arg2 : boolean*, arg3 : int)",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getActionSignature(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ',' expecting ')'");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class EventTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void constructor_singleParam() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1 {",
					"	new(arg : int*) {",
					"		System.out.println(arg)",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Event event = (Event) mas.getElements().get(0);
			assertEquals("E1", event.getName());
			assertTypeReferenceIdentifiers(event.getSuperTypes());
			assertEquals(1, event.getFeatures().size());
			//
			Constructor constructor = (Constructor) event.getFeatures().get(0);
			assertParameterNames(constructor.getParams(), "arg");
			assertParameterTypes(constructor.getParams(), "int");
			assertTrue(constructor.isVarargs());
			assertParameterDefaultValues(constructor.getParams(), (Object) null);
		}

		@Test
		public void constructor() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1 {",
					"	new (arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Event event = (Event) mas.getElements().get(0);
			assertEquals("E1", event.getName());
			assertTypeReferenceIdentifiers(event.getSuperTypes());
			assertEquals(1, event.getFeatures().size());
			//
			Constructor constructor = (Constructor) event.getFeatures().get(0);
			assertParameterNames(constructor.getParams(), "arg1", "arg2", "arg3");
			assertParameterTypes(constructor.getParams(), "char", "boolean", "int");
			assertTrue(constructor.isVarargs());
			assertParameterDefaultValues(constructor.getParams(), null, null, null);
		}

		@Test
		public void constructor_invalid() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1 {",
					"	new (arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getConstructor(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ',' expecting ')'");
		}

	}

}
