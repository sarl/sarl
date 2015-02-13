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
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.Skill;

import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
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
	ArgDefaultValueParsingTest.AgentAction.class,
	ArgDefaultValueParsingTest.BehaviorConstructor.class,
	ArgDefaultValueParsingTest.CapacityAction.class,
	ArgDefaultValueParsingTest.SkillAction.class,
	ArgDefaultValueParsingTest.BehaviorAction.class,
})
@SuppressWarnings("all")
public class ArgDefaultValueParsingTest extends AbstractSarlTest {

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class AgentAction extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void action_1p() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg : int=4) {",
					"System.out.println(arg)",
					"}",
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
			assertParameterNames(action.getParams(), "arg");
			assertParameterTypes(action.getParams(), "int");
			assertParameterDefaultValues(action.getParams(), XNumberLiteral.class, "4");
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_1p_invalid1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg : int=4*) {",
					"System.out.println(arg)",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					io.sarl.lang.validation.IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A default value cannot be declared for the variadic formal parameter");
		}

		@Test
		public void action_1p_invalid2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg : int*=4) {",
					"System.out.println(arg)",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '*=' expecting ')'");
		}

		@Test
		public void action_1p_returnValue() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg : int=4) : boolean {",
					"System.out.println(arg)",
					"return true",
					"}",
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
			assertParameterNames(action.getParams(), "arg");
			assertParameterTypes(action.getParams(), "int");
			assertParameterDefaultValues(action.getParams(), XNumberLiteral.class, "4");
			assertTypeReferenceIdentifier(action.getType(), "boolean");
		}

		@Test
		public void action_5p_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParams(), XNumberLiteral.class, "4", null, null, null, null);
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_5p_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParams(), null, XStringLiteral.class, "abc", null, null, null);
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_5p_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"	def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {",
					"		System.out.println(arg0)",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParams(), null, null, XNumberLiteral.class, "18", null, null);
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_5p_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParams(), null, null, null, XNumberLiteral.class, "34", null);
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_5p_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParams(), null, null, null, null, XStringLiteral.class, "xyz");
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_5p_0_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParams(), XNumberLiteral.class, "4", null, null, XNumberLiteral.class, "56", null);
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_5p_0_3_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\") {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParams(), XNumberLiteral.class, "4", null, null, XNumberLiteral.class, "56", XStringLiteral.class, "def");
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_5p_0_2_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\") {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParams(), XNumberLiteral.class, "4", null, XNumberLiteral.class, "18", null, XStringLiteral.class, "def");
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_5p_0_1_2_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParams(), XNumberLiteral.class, "4", XStringLiteral.class, "ghj", XNumberLiteral.class, "18", XNumberLiteral.class, "98", null);
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_5p_0_1_2_3_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\") {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParams(), XNumberLiteral.class, "4", XStringLiteral.class, "ghj", XNumberLiteral.class, "18", XNumberLiteral.class, "98", XStringLiteral.class, "klm");
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_3p_vararg_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int, arg1 : int, arg2 : int=45*) {",
					"System.out.println(arg0)",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					io.sarl.lang.validation.IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A default value cannot be declared for the variadic formal parameter");
		}

		@Test
		public void action_3p_vararg_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"	def myaction(arg0 : int, arg1 : int=45, arg2 : int*) {",
					"		System.out.println(arg0)",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action.getParams(), "int", "int", "int");
			assertTrue(action.isVarargs());
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "45",
					null);
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_3p_vararg_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"	def myaction(arg0 : int=45, arg1 : int, arg2 : int*) {",
					"		System.out.println(arg0)",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action.getParams(), "int", "int", "int");
			assertTrue(action.isVarargs());
			assertParameterDefaultValues(action.getParams(),
					XNumberLiteral.class, "45",
					null,
					null);
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void action_3p_vararg_0_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action.getParams(), "int", "int", "int");
			assertTrue(action.isVarargs());
			assertParameterDefaultValues(action.getParams(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "56",
					null);
			assertTypeReferenceIdentifier(action.getType(), "void");
		}

		@Test
		public void multipleActionDefinitionsInAgent() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"	def myaction(arg0 : int, arg1 : int=42, arg2 : int*) {",
					"		System.out.println(\"valid\")",
					"	}",
					"	def myaction(arg0 : int, arg1 : int*) {",
					"		System.out.println(\"invalid\")",
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
			assertEquals(2, agent.getFeatures().size());
			//
			Action action1 = (Action) agent.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertParameterNames(action1.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action1.getParams(), "int", "int", "int");
			assertTrue(action1.isVarargs());
			assertParameterDefaultValues(action1.getParams(),
					null,
					XNumberLiteral.class, "42",
					null);
			assertTypeReferenceIdentifier(action1.getType(), "void");
			//
			Action action2 = (Action) agent.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertParameterNames(action2.getParams(), "arg0", "arg1");
			assertParameterTypes(action2.getParams(), "int", "int");
			assertTrue(action2.isVarargs());
			assertParameterDefaultValues(action2.getParams(),
					null,
					null);
			assertTypeReferenceIdentifier(action2.getType(), "void");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class BehaviorConstructor extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void constructor_1p() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg : int=4) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg)",
					"}",
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
			assertParameterDefaultValues(constructor.getParams(), XNumberLiteral.class, "4");
		}

		@Test
		public void constructor_1p_invalid1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg : int=4*) {",
					"System.out.println(arg)",
					"}",
					"}"
					)); 
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getConstructor(),
					io.sarl.lang.validation.IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A default value cannot be declared for the variadic formal parameter 'arg'");
		}

		@Test
		public void constructor_1p_invalid2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg : int*=4) {",
					"System.out.println(arg)",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getConstructor(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '*=' expecting ')'");
		}

		@Test
		public void constructor_5p_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParams(), XNumberLiteral.class, "4", null, null, null, null);
		}

		@Test
		public void constructor_5p_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParams(), null, XStringLiteral.class, "abc", null, null, null);
		}

		@Test
		public void constructor_5p_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParams(), null, null, XNumberLiteral.class, "18", null, null);
		}

		@Test
		public void constructor_5p_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParams(), null, null, null, XNumberLiteral.class, "34", null);
		}

		@Test
		public void constructor_5p_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParams(), null, null, null, null, XStringLiteral.class, "xyz");
		}

		@Test
		public void constructor_5p_0_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParams(), XNumberLiteral.class, "4", null, null, XNumberLiteral.class, "56", null);
		}

		@Test
		public void constructor_5p_0_3_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\") {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg0)",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParams(),
					XNumberLiteral.class, "4",
					null,
					null,
					XNumberLiteral.class, "56",
					XStringLiteral.class, "def");
		}

		@Test
		public void constructor_5p_0_2_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	new(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\") {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg0)",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParams(),
					XNumberLiteral.class, "4",
					null,
					XNumberLiteral.class, "18",
					null,
					XStringLiteral.class, "def");
		}

		@Test
		public void constructor_5p_0_1_2_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParams(), XNumberLiteral.class, "4", XStringLiteral.class, "ghj", XNumberLiteral.class, "18", XNumberLiteral.class, "98", null);
		}

		@Test
		public void constructor_5p_0_1_2_3_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\") {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParams(), XNumberLiteral.class, "4", XStringLiteral.class, "ghj", XNumberLiteral.class, "18", XNumberLiteral.class, "98", XStringLiteral.class, "klm");
		}

		@Test
		public void constructor_3p_vararg_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : int, arg2 : int=45*) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getConstructor(),
					io.sarl.lang.validation.IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A default value cannot be declared for the variadic formal parameter 'arg2'");
		}

		@Test
		public void constructor_3p_vararg_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : int=45, arg2 : int*) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(constructor.getParams(), "int", "int", "int");
			assertTrue(constructor.isVarargs());
			assertParameterDefaultValues(constructor.getParams(),
					null,
					XNumberLiteral.class, "45",
					null);
		}

		@Test
		public void constructor_3p_vararg_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int=45, arg1 : int, arg2 : int*) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(constructor.getParams(), "int", "int", "int");
			assertTrue(constructor.isVarargs());
			assertParameterDefaultValues(constructor.getParams(),
					XNumberLiteral.class, "45",
					null,
					null);
		}

		@Test
		public void constructor_3p_vararg_0_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	new(arg0 : int=45, arg1 : int=56, arg2 : int*) {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg0)",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(constructor.getParams(), "int", "int", "int");
			assertTrue(constructor.isVarargs());
			assertParameterDefaultValues(constructor.getParams(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "56",
					null);
		}

		@Test
		public void constructorCast_String2int() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"package io.sarl.test",
					"behavior B1 {",
					"new(arg0 : int=45, arg1 : int=\"S\", arg2 : int) {",
					"System.out.println(arg0)",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getFormalParameter(),
					IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from String to int");
		}

		@Test
		public void constructorCast_int2double() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int=45, arg1 : double=18, arg2 : int) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(constructor.getParams(), "int", "double", "int");
			assertParameterDefaultValues(constructor.getParams(), XNumberLiteral.class, "45", XNumberLiteral.class, "18", null);
		}

		@Test
		public void constructorCast_double2int() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"new(arg0 : int=45, arg1 : int=18.0, arg2 : int) {",
					"System.out.println(arg0)",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					XbasePackage.eINSTANCE.getXNumberLiteral(),
					IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from double to int");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class CapacityAction extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void action_1p() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg : int=4)",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg");
			assertParameterTypes(signature.getParams(), "int");
			assertParameterDefaultValues(signature.getParams(), XNumberLiteral.class, "4");
		}

		@Test
		public void action_1p_invalid1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg : int=4*)",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getActionSignature(),
					io.sarl.lang.validation.IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A default value cannot be declared for the variadic formal parameter");
		}

		@Test
		public void action_1p_invalid2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg : int*=4)",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getActionSignature(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '*=' expecting ')'");
		}

		@Test
		public void action_5p_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String)",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParams(), XNumberLiteral.class, "4", null, null, null, null);
		}

		@Test
		public void action_5p_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String)",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParams(), null, XStringLiteral.class, "abc", null, null, null);
		}

		@Test
		public void action_5p_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String)",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParams(), null, null, XNumberLiteral.class, "18", null, null);
		}

		@Test
		public void action_5p_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String)",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParams(), null, null, null, XNumberLiteral.class, "34", null);
		}

		@Test
		public void action_5p_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\")",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParams(), null, null, null, null, XStringLiteral.class, "xyz");
		}

		@Test
		public void action_5p_0_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String)",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParams(), XNumberLiteral.class, "4", null, null, XNumberLiteral.class, "56", null);
		}

		@Test
		public void action_5p_0_3_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\")",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParams(), XNumberLiteral.class, "4", null, null, XNumberLiteral.class, "56", XStringLiteral.class, "def");
		}

		@Test
		public void action_5p_0_2_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\")",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParams(), XNumberLiteral.class, "4", null, XNumberLiteral.class, "18", null, XStringLiteral.class, "def");
		}

		@Test
		public void action_5p_0_1_2_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String)",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParams(),
					XNumberLiteral.class, "4",
					XStringLiteral.class, "ghj",
					XNumberLiteral.class, "18",
					XNumberLiteral.class, "98",
					null);
		}

		@Test
		public void action_5p_0_1_2_3_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\")",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParams(), XNumberLiteral.class, "4", XStringLiteral.class, "ghj", XNumberLiteral.class, "18", XNumberLiteral.class, "98", XStringLiteral.class, "klm");
		}

		@Test
		public void action_3p_vararg_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : int, arg2 : int=45*)",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getActionSignature(),
					io.sarl.lang.validation.IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A default value cannot be declared for the variadic formal parameter");
		}

		@Test
		public void action_3p_vararg_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : int=45, arg2 : int*)",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(signature.getParams(), "int", "int", "int");
			assertTrue(signature.isVarargs());
			assertParameterDefaultValues(signature.getParams(), null, XNumberLiteral.class, "45", null);
		}

		@Test
		public void action_3p_vararg_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=45, arg1 : int, arg2 : int*)",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(signature.getParams(), "int", "int", "int");
			assertTrue(signature.isVarargs());
			assertParameterDefaultValues(signature.getParams(), XNumberLiteral.class, "45", null, null);
		}

		@Test
		public void action_3p_vararg_0_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*)",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(signature.getParams(), "int", "int", "int");
			assertTrue(signature.isVarargs());
			assertParameterDefaultValues(signature.getParams(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "56",
					null);
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class SkillAction extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void action_1p() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg : int=4) {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg");
			assertParameterTypes(action2.getParams(), "int");
			assertParameterDefaultValues(action2.getParams(),
					XNumberLiteral.class, "4");
		}

		@Test
		public void action_1p_invalid1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg : int=4*) {}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					io.sarl.lang.validation.IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A default value cannot be declared for the variadic formal parameter");
		}

		@Test
		public void action_1p_invalid2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg : int*=4) {}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '*=' expecting ')'");
		}

		@Test
		public void action_5p_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParams(),
					XNumberLiteral.class, "4",
					null,
					null,
					null,
					null);
		}

		@Test
		public void action_5p_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParams(),
					null,
					XStringLiteral.class, "abc",
					null,
					null,
					null);
		}

		@Test
		public void action_5p_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParams(),
					null,
					null,
					XNumberLiteral.class, "18",
					null,
					null);
		}

		@Test
		public void action_5p_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParams(),
					null,
					null,
					null,
					XNumberLiteral.class, "34",
					null);
		}

		@Test
		public void action_5p_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParams(), 
					null,
					null,
					null,
					null,
					XStringLiteral.class, "xyz");
		}

		@Test
		public void action_5p_0_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParams(),
					XNumberLiteral.class, "4",
					null,
					null,
					XNumberLiteral.class, "56",
					null);
		}

		@Test
		public void action_5p_0_3_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\") {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParams(),
					XNumberLiteral.class, "4",
					null,
					null,
					XNumberLiteral.class, "56",
					XStringLiteral.class, "def");
		}

		@Test
		public void action_5p_0_2_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\") {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParams(),
					XNumberLiteral.class, "4",
					null,
					XNumberLiteral.class, "18",
					null,
					XStringLiteral.class, "def");
		}

		@Test
		public void action_5p_0_1_2_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String) {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParams(),
					XNumberLiteral.class, "4",
					XStringLiteral.class, "ghj",
					XNumberLiteral.class, "18",
					XNumberLiteral.class, "98",
					null);
		}

		@Test
		public void action_5p_0_1_2_3_4() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\") {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParams(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParams(),
					XNumberLiteral.class, "4",
					XStringLiteral.class, "ghj",
					XNumberLiteral.class, "18",
					XNumberLiteral.class, "98",
					XStringLiteral.class, "klm");
		}

		@Test
		public void action_3p_vararg_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : int, arg2 : int=45*) {}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					io.sarl.lang.validation.IssueCodes.INVALID_USE_OF_VAR_ARG,
					"A default value cannot be declared for the variadic formal parameter");
		}

		@Test
		public void action_3p_vararg_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : int=45, arg2 : int*) {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action2.getParams(), "int", "int", "int");
			assertTrue(action2.isVarargs());
			assertParameterDefaultValues(action2.getParams(),
					null,
					XNumberLiteral.class, "45",
					null);
		}

		@Test
		public void action_3p_vararg_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=45, arg1 : int, arg2 : int*) {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action2.getParams(), "int", "int", "int");
			assertTrue(action2.isVarargs());
			assertParameterDefaultValues(action2.getParams(),
					XNumberLiteral.class, "45",
					null,
					null);
		}

		@Test
		public void action_3p_vararg_0_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*) {}",
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
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action2.getParams(), "int", "int", "int");
			assertTrue(action2.isVarargs());
			assertParameterDefaultValues(action2.getParams(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "56",
					null);
		}

		@Test
		public void overridingCapacitySkill() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*)",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {",
					"		System.out.println(\"ok\");",
					"	}",
					"	def myaction(arg0 : int, arg1 : int, arg2 : int*) {",
					"		System.out.println(\"ok\");",
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
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(signature.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(signature.getParams(), "int", "int", "int");
			assertTrue(signature.isVarargs());
			assertParameterDefaultValues(signature.getParams(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "56",
					null);
			//
			Skill skill = (Skill) mas.getElements().get(1);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action2.getParams(), "int", "int", "int");
			assertTrue(action2.isVarargs());
			assertParameterDefaultValues(action2.getParams(), null, null, null);
		}

		@Test
		public void multipleActionDefinitionsInSkill() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"skill S1 implements C1 {",
					"	def myaction(arg0 : int, arg1 : int=42, arg2 : int*) {",
					"		System.out.println(\"valid\")",
					"	}",
					"	def myaction(arg0 : int, arg1 : int*) {",
					"		System.out.println(\"invalid\")",
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
			assertEquals(2, skill.getFeatures().size());
			//
			Action action1 = (Action) skill.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action1.getParams(), "int", "int", "int");
			assertTrue(action1.isVarargs());
			assertParameterDefaultValues(action1.getParams(),
					null,
					XNumberLiteral.class, "42",
					null);
			//
			Action action2 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(action2.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1");
			assertParameterTypes(action2.getParams(), "int", "int");
			assertTrue(action2.isVarargs());
			assertParameterDefaultValues(action2.getParams(), null, null);
		}

		@Test
		public void missedActionImplementation_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction1(a : int=4)",
					"}",
					"capacity C2 {",
					"	def myaction2(b : float=6, c : boolean)",
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
			ActionSignature action1 = (ActionSignature) capacity1.getFeatures().get(0);
			assertEquals("myaction1", action1.getName());
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams(), "a");
			assertParameterTypes(action1.getParams(), "int");
			assertParameterDefaultValues(action1.getParams(), XNumberLiteral.class, "4");
			//
			Capacity capacity2 = (Capacity) mas.getElements().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getSuperTypes());
			assertEquals(1, capacity2.getFeatures().size());
			//
			ActionSignature action2 = (ActionSignature) capacity2.getFeatures().get(0);
			assertEquals("myaction2", action2.getName());
			assertTypeReferenceIdentifier(action2.getType(), "void");
			assertParameterNames(action2.getParams(), "b", "c");
			assertParameterTypes(action2.getParams(), "float", "boolean");
			assertParameterDefaultValues(action2.getParams(), XNumberLiteral.class, "6", null);
			//
			Skill skill = (Skill) mas.getElements().get(2);
			assertEquals("S1", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "C1", "C2");
			assertEquals(2, skill.getFeatures().size());
			//
			Action action3 = (Action) skill.getFeatures().get(0);
			assertEquals("myaction1", action3.getName());
			assertTypeReferenceIdentifier(action3.getType(), "void");
			assertParameterNames(action3.getParams(), "x");
			assertParameterTypes(action3.getParams(), "int");
			assertParameterDefaultValues(action3.getParams(), (Object) null);
			//
			Action action4 = (Action) skill.getFeatures().get(1);
			assertEquals("myaction2", action4.getName());
			assertTypeReferenceIdentifier(action4.getType(), "void");
			assertParameterNames(action4.getParams(), "y", "z");
			assertParameterTypes(action4.getParams(), "float", "boolean");
			assertParameterDefaultValues(action4.getParams(), null, null);
		}

		@Test
		public void missedActionImplementation_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction1(a : int=4)",
					"}",
					"capacity C2 {",
					"	def myaction2(b : float=6, c : boolean)",
					"}",
					"skill S1 implements C1, C2 {",
					"	def myaction2(b : float, c : boolean) { }",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getSkill(),
					io.sarl.lang.validation.IssueCodes.MISSING_METHOD_IMPLEMENTATION,
					"The operation myaction1(a : int=4) must be implemented.");
		}

		@Test
		public void missedActionImplementation_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"	def myaction1(a : int=4)",
					"}",
					"capacity C2 {",
					"	def myaction2(b : float=6, c : boolean)",
					"}",
					"skill S1 implements C1, C2 {",
					"	def myaction1(x : float) { }",
					"	def myaction2(y : float, z : boolean) { }",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getSkill(),
					io.sarl.lang.validation.IssueCodes.MISSING_METHOD_IMPLEMENTATION,
					"The operation myaction1(a : int=4) must be implemented.");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class BehaviorAction extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void actionCast_String2int() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int=45, arg1 : int=\"S\", arg2 : int) {",
					"		System.out.println(arg0)",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getFormalParameter(),
					IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from String to int");
		}

		@Test
		public void actionCast_int2double() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int=45, arg1 : double=18, arg2 : int) {",
					"		System.out.println(arg0)",
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
			assertTypeReferenceIdentifier(action.getType(), "void");
			assertParameterNames(action.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action.getParams(), "int", "double", "int");
			assertFalse(action.isVarargs());
			assertParameterDefaultValues(action.getParams(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "18",
					null);
		}

		@Test
		public void actionCast_double2int() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int=45, arg1 : int=18.0, arg2 : int) {",
					"		System.out.println(arg0)",
					"	}",
					"}"
					));
			this.validator.assertError(mas,
					XbasePackage.eINSTANCE.getXNumberLiteral(),
					IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from double to int");
		}

		@Test
		public void multipleActionDefinitionsInBehavior() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int, arg1 : int=42, arg2 : int*) {",
					"		System.out.println(\"valid\")",
					"	}",
					"	def myaction(arg0 : int, arg1 : int*) {",
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
			assertTypeReferenceIdentifier(action1.getType(), "void");
			assertParameterNames(action1.getParams(), "arg0", "arg1", "arg2");
			assertParameterTypes(action1.getParams(), "int", "int", "int");
			assertTrue(action1.isVarargs());
			assertParameterDefaultValues(action1.getParams(), null, XNumberLiteral.class, "42", null);
			//
			Action action2 = (Action) behavior.getFeatures().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(action2.getType(), "void");
			assertParameterNames(action2.getParams(), "arg0", "arg1");
			assertParameterTypes(action2.getParams(), "int", "int");
			assertTrue(action2.isVarargs());
			assertParameterDefaultValues(action2.getParams(), null, null);
		}

	}

}
