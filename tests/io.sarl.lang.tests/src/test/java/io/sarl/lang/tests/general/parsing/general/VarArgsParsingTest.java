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
package io.sarl.lang.tests.general.parsing.general;

import static io.sarl.tests.api.tools.TestAssertions.assertParameterDefaultValues;
import static io.sarl.tests.api.tools.TestAssertions.assertParameterNames;
import static io.sarl.tests.api.tools.TestAssertions.assertParameterTypes;
import static io.sarl.tests.api.tools.TestAssertions.assertParameterVarArg;
import static io.sarl.tests.api.tools.TestAssertions.assertTypeReferenceIdentifier;
import static io.sarl.tests.api.tools.TestAssertions.assertTypeReferenceIdentifiers;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.common.base.Strings;
import org.eclipse.xtend.core.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
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
@DisplayName("Syntax: variadic parameters")
@Tag("core")
public class VarArgsParsingTest {

	@Nested
	public class AgentTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void action_singleParam() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"	def myaction(arg : int*) {",
					"		System.out.println(arg)",
					"	}",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent.getName());
			assertNull(agent.getExtends());
			assertEquals(1, agent.getMembers().size());
			//
			SarlAction action = (SarlAction) agent.getMembers().get(0);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			assertParameterNames(action.getParameters(), "arg");
			assertParameterTypes(action.getParameters(), "int");
			assertParameterDefaultValues(action.getParameters(), (Object) null);
		}

		@Test
		@Tag("sarlParsing")
		public void inAgentAction() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"	def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent.getName());
			assertNull(agent.getExtends());
			assertEquals(1, agent.getMembers().size());
			//
			SarlAction action = (SarlAction) agent.getMembers().get(0);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			assertParameterNames(action.getParameters(), "arg1", "arg2", "arg3");
			assertParameterTypes(action.getParameters(), "char", "boolean", "int");
			assertParameterDefaultValues(action.getParameters(), null, null, null);
			assertParameterVarArg(action.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void action_invalid() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"	def myaction(arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlFormalParameter(),
					IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A vararg must be the last parameter");
		}

	}

	@Nested
	public class BehaviorTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void action_singleParam() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	def myaction(arg : int*) {",
					"		System.out.println(arg)",
					"	}",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", behavior.getName());
			assertNull(behavior.getExtends());
			assertEquals(1, behavior.getMembers().size());
			//
			SarlAction action = (SarlAction) behavior.getMembers().get(0);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			assertParameterNames(action.getParameters(), "arg");
			assertParameterTypes(action.getParameters(), "int");
			assertParameterDefaultValues(action.getParameters(), (Object) null);
			assertParameterVarArg(action.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void action() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", behavior.getName());
			assertNull(behavior.getExtends());
			assertEquals(1, behavior.getMembers().size());
			//
			SarlAction action = (SarlAction) behavior.getMembers().get(0);
			assertEquals("myaction", action.getName());
			assertTypeReferenceIdentifiers(action.getFiredEvents());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			assertParameterNames(action.getParameters(), "arg1", "arg2", "arg3");
			assertParameterTypes(action.getParameters(), "char", "boolean", "int");
			assertParameterDefaultValues(action.getParameters(), null, null, null);
			assertParameterVarArg(action.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void action_invalid() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def myaction(arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlFormalParameter(),
					IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A vararg must be the last parameter");
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_singleParam() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	new(arg : int*) {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg)",
					"	}",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", behavior.getName());
			assertNull(behavior.getExtends());
			assertEquals(1, behavior.getMembers().size());
			//
			SarlConstructor constructor = (SarlConstructor) behavior.getMembers().get(0);
			assertParameterNames(constructor.getParameters(), "arg");
			assertParameterTypes(constructor.getParameters(), "int");
			assertParameterDefaultValues(constructor.getParameters(), (Object) null);
			assertParameterVarArg(constructor.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	new (arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", behavior.getName());
			assertNull(behavior.getExtends());
			assertEquals(1, behavior.getMembers().size());
			//
			SarlConstructor constructor = (SarlConstructor) behavior.getMembers().get(0);
			assertParameterNames(constructor.getParameters(), "arg1", "arg2", "arg3");
			assertParameterTypes(constructor.getParameters(), "char", "boolean", "int");
			assertParameterDefaultValues(constructor.getParameters(), null, null, null);
			assertParameterVarArg(constructor.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void constructor_invalid() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	new (arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlFormalParameter(),
					IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A vararg must be the last parameter");
		}

		@Test
		@Tag("sarlParsing")
		public void multipleActionDefinitionsInBehavior_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int, arg1 : int*) {",
					"		System.out.println(\"invalid\")",
					"	}",
					"	def myaction {",
					"		System.out.println(\"invalid\")",
					"	}",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", behavior.getName());
			assertNull(behavior.getExtends());
			assertEquals(2, behavior.getMembers().size());
			//
			SarlAction action1 = (SarlAction) behavior.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters(), "arg0", "arg1");
			assertParameterTypes(action1.getParameters(), "int", "int");
			assertParameterDefaultValues(action1.getParameters(), null, null);
			assertParameterVarArg(action1.getParameters());
			//
			SarlAction action2 = (SarlAction) behavior.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getReturnType(), "void");
			assertParameterNames(action2.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void multipleActionDefinitionsInBehavior_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int, arg1 : int*) {",
					"		System.out.println(\"invalid\")",
					"	}",
					"	def myaction(arg0 : int) {",
					"		System.out.println(\"invalid\")",
					"	}",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlBehavior behavior = (SarlBehavior) mas.getXtendTypes().get(0);
			assertEquals("B1", behavior.getName());
			assertNull(behavior.getExtends());
			assertEquals(2, behavior.getMembers().size());
			//
			SarlAction action1 = (SarlAction) behavior.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters(), "arg0", "arg1");
			assertParameterTypes(action1.getParameters(), "int", "int");
			assertParameterDefaultValues(action1.getParameters(), null, null);
			assertParameterVarArg(action1.getParameters());
			//
			SarlAction action2 = (SarlAction) behavior.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertTypeReferenceIdentifier(action2.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0");
			assertParameterTypes(action2.getParameters(), "int");
			assertParameterDefaultValues(action2.getParameters(), (Object) null);
		}

		@Test
		@Tag("sarlValidation")
		public void multipleActionDefinitionsInBehavior_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int, arg1 : int*) {",
					"		System.out.println(\"invalid\")",
					"	}",
					"	def myaction(arg0 : int, arg1 : int*) {",
					"		System.out.println(\"invalid\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					IssueCodes.DUPLICATE_METHOD,
					"Duplicate method myaction(int, int[]) in type B1");
		}

	}

	@Nested
	public class SkillTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void action_singleParam() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	def myaction(arg : int*) {",
					"		System.out.println(arg)",
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
			assertEquals(0, capacity.getMembers().size());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters(), "arg");
			assertParameterTypes(action1.getParameters(), "int");
			assertParameterDefaultValues(action1.getParameters(), (Object) null);
			assertParameterVarArg(action1.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void action() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	def myaction(arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		System.out.println(arg3)",
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
			assertEquals(0, capacity.getMembers().size());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(1, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters(), "arg1", "arg2", "arg3");
			assertParameterTypes(action1.getParameters(), "char", "boolean", "int");
			assertParameterDefaultValues(action1.getParameters(), null, null, null);
			assertParameterVarArg(action1.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void action_invalid() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	def myaction(arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlFormalParameter(),
					IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A vararg must be the last parameter");
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_singleParam() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	new(arg : int*) {",
					"		System.out.println(arg)",
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
			assertEquals(0, capacity.getMembers().size());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(1, skill.getMembers().size());
			//
			SarlConstructor constructor = (SarlConstructor) skill.getMembers().get(0);
			assertParameterNames(constructor.getParameters(), "arg");
			assertParameterTypes(constructor.getParameters(), "int");
			assertParameterDefaultValues(constructor.getParameters(), (Object) null);
			assertParameterVarArg(constructor.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	new (arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		System.out.println(arg3)",
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
			assertEquals(0, capacity.getMembers().size());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(1, skill.getMembers().size());
			//
			SarlConstructor constructor = (SarlConstructor) skill.getMembers().get(0);
			assertParameterNames(constructor.getParameters(), "arg1", "arg2", "arg3");
			assertParameterTypes(constructor.getParameters(), "char", "boolean", "int");
			assertParameterDefaultValues(constructor.getParameters(), null, null, null);
			assertParameterVarArg(constructor.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void constructor_invalid() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	new (arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlFormalParameter(),
					IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A vararg must be the last parameter");
		}

	}

	@Nested
	public class CapacityTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void action_singleParam() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(arg : int*)",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
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
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters(), "arg");
			assertParameterTypes(action1.getParameters(), "int");
			assertParameterDefaultValues(action1.getParameters(), (Object) null);
			assertParameterVarArg(action1.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void action() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(arg1 : char, arg2 : boolean, arg3 : int*)",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
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
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters(), "arg1", "arg2", "arg3");
			assertParameterTypes(action1.getParameters(), "char", "boolean", "int");
			assertParameterDefaultValues(action1.getParameters(), null, null, null);
			assertParameterVarArg(action1.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void action_invalid() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def myaction(arg1 : char, arg2 : boolean*, arg3 : int)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlFormalParameter(),
					IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A vararg must be the last parameter");
		}

	}

	@Nested
	public class EventTest extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void constructor_singleParam() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"event E1 {",
					"	new(arg : int*) {",
					"		System.out.println(arg)",
					"	}",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(1, event.getMembers().size());
			//
			SarlConstructor constructor = (SarlConstructor) event.getMembers().get(0);
			assertParameterNames(constructor.getParameters(), "arg");
			assertParameterTypes(constructor.getParameters(), "int");
			assertParameterDefaultValues(constructor.getParameters(), (Object) null);
			assertParameterVarArg(constructor.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"event E1 {",
					"	new (arg1 : char, arg2 : boolean, arg3 : int*) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E1", event.getName());
			assertNull(event.getExtends());
			assertEquals(1, event.getMembers().size());
			//
			SarlConstructor constructor = (SarlConstructor) event.getMembers().get(0);
			assertParameterNames(constructor.getParameters(), "arg1", "arg2", "arg3");
			assertParameterTypes(constructor.getParameters(), "char", "boolean", "int");
			assertParameterDefaultValues(constructor.getParameters(), null, null, null);
			assertParameterVarArg(constructor.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void constructor_invalid() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 {",
					"	new (arg1 : char, arg2 : boolean*, arg3 : int) {",
					"		System.out.println(arg3)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlFormalParameter(),
					IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A vararg must be the last parameter");
		}

	}

}
