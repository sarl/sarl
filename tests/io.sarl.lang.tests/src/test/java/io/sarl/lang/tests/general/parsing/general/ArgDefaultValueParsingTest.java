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

import static io.sarl.tests.api.tools.TestAssertions.assertNoParameterVarArg;
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
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlConstructor;
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
@DisplayName("Syntax: parameter default values")
@Tag("core")
public class ArgDefaultValueParsingTest extends AbstractSarlTest {

	@Nested
	public class AgentAction extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void action_1p() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg : int=4) {",
					"System.out.println(arg)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg");
			assertParameterTypes(action.getParameters(), "int");
			assertParameterDefaultValues(action.getParameters(), XNumberLiteral.class, "4");
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlValidation")
		public void action_1p_invalid1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg : int=4*) {",
					"System.out.println(arg)",
					"}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input");
		}

		@Test
		@Tag("sarlValidation")
		public void action_1p_invalid2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg : int*=4) {",
					"System.out.println(arg)",
					"}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '*=' expecting ')'");
		}

		@Test
		@Tag("sarlParsing")
		public void action_1p_returnValue() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg : int=4) : boolean {",
					"System.out.println(arg)",
					"return true",
					"}",
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
			assertParameterNames(action.getParameters(), "arg");
			assertParameterTypes(action.getParameters(), "int");
			assertParameterDefaultValues(action.getParameters(), XNumberLiteral.class, "4");
			assertTypeReferenceIdentifier(action.getReturnType(), "boolean");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParameters(), XNumberLiteral.class, "4", null, null, null, null);
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParameters(), null, XStringLiteral.class, "abc", null, null, null);
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"	def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {",
					"		System.out.println(arg0)",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParameters(), null, null, XNumberLiteral.class, "18", null, null);
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParameters(), null, null, null, XNumberLiteral.class, "34", null);
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParameters(), null, null, null, null, XStringLiteral.class, "xyz");
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParameters(), XNumberLiteral.class, "4", null, null, XNumberLiteral.class, "56", null);
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_3_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\") {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParameters(), XNumberLiteral.class, "4", null, null, XNumberLiteral.class, "56", XStringLiteral.class, "def");
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_2_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\") {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParameters(), XNumberLiteral.class, "4", null, XNumberLiteral.class, "18", null, XStringLiteral.class, "def");
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_1_2_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParameters(), XNumberLiteral.class, "4", XStringLiteral.class, "ghj", XNumberLiteral.class, "18", XNumberLiteral.class, "98", null);
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_1_2_3_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\") {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action.getParameters(), XNumberLiteral.class, "4", XStringLiteral.class, "ghj", XNumberLiteral.class, "18", XNumberLiteral.class, "98", XStringLiteral.class, "klm");
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlValidation")
		public void action_3p_vararg_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int, arg1 : int, arg2 : int=45*) {",
					"System.out.println(arg0)",
					"}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input");
		}

		@Test
		@Tag("sarlParsing")
		public void action_3p_vararg_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"	def myaction(arg0 : int, arg1 : int=45, arg2 : int*) {",
					"		System.out.println(arg0)",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "45",
					null);
			assertParameterVarArg(action.getParameters());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_3p_vararg_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"	def myaction(arg0 : int=45, arg1 : int, arg2 : int*) {",
					"		System.out.println(arg0)",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(action.getParameters(),
					XNumberLiteral.class, "45",
					null,
					null);
			assertParameterVarArg(action.getParameters());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void action_3p_vararg_0_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*) {",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(action.getParameters(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "56",
					null);
			assertParameterVarArg(action.getParameters());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
		}

		@Test
		@Tag("sarlParsing")
		public void multipleActionDefinitionsInAgent() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"agent A1 {",
					"	def myaction(arg0 : int, arg1 : int=42, arg2 : int*) {",
					"		System.out.println(\"valid\")",
					"	}",
					"	def myaction(arg0 : int, arg1 : int*) {",
					"		System.out.println(\"invalid\")",
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
			assertEquals(2, agent.getMembers().size());
			//
			SarlAction action1 = (SarlAction) agent.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertParameterNames(action1.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action1.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(action1.getParameters(),
					null,
					XNumberLiteral.class, "42",
					null);
			assertParameterVarArg(action1.getParameters());
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) agent.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertParameterNames(action2.getParameters(), "arg0", "arg1");
			assertParameterTypes(action2.getParameters(), "int", "int");
			assertParameterDefaultValues(action2.getParameters(),
					null,
					null);
			assertParameterVarArg(action2.getParameters());
			assertTypeReferenceIdentifier(action2.getReturnType(), "void");
		}

	}

	@Nested
	public class BehaviorConstructor extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void constructor_1p() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg : int=4) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg)",
					"}",
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
			assertParameterDefaultValues(constructor.getParameters(), XNumberLiteral.class, "4");
		}

		@Test
		@Tag("sarlValidation")
		public void constructor_1p_invalid1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"new(arg : int=4*) {",
					"System.out.println(arg)",
					"}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input");
		}

		@Test
		@Tag("sarlValidation")
		public void constructor_1p_invalid2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"new(arg : int*=4) {",
					"System.out.println(arg)",
					"}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '*=' expecting ')'");
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_5p_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParameters(), XNumberLiteral.class, "4", null, null, null, null);
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_5p_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParameters(), null, XStringLiteral.class, "abc", null, null, null);
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_5p_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParameters(), null, null, XNumberLiteral.class, "18", null, null);
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_5p_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParameters(), null, null, null, XNumberLiteral.class, "34", null);
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_5p_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParameters(), null, null, null, null, XStringLiteral.class, "xyz");
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_5p_0_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParameters(), XNumberLiteral.class, "4", null, null, XNumberLiteral.class, "56", null);
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_5p_0_3_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\") {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg0)",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParameters(),
					XNumberLiteral.class, "4",
					null,
					null,
					XNumberLiteral.class, "56",
					XStringLiteral.class, "def");
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_5p_0_2_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	new(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\") {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg0)",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParameters(),
					XNumberLiteral.class, "4",
					null,
					XNumberLiteral.class, "18",
					null,
					XStringLiteral.class, "def");
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_5p_0_1_2_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParameters(), XNumberLiteral.class, "4", XStringLiteral.class, "ghj", XNumberLiteral.class, "18", XNumberLiteral.class, "98", null);
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_5p_0_1_2_3_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\") {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(constructor.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(constructor.getParameters(), XNumberLiteral.class, "4", XStringLiteral.class, "ghj", XNumberLiteral.class, "18", XNumberLiteral.class, "98", XStringLiteral.class, "klm");
		}

		@Test
		@Tag("sarlValidation")
		public void constructor_3p_vararg_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : int, arg2 : int=45*) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlConstructor(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input");
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_3p_vararg_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int, arg1 : int=45, arg2 : int*) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(constructor.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(constructor.getParameters(),
					null,
					XNumberLiteral.class, "45",
					null);
			assertParameterVarArg(constructor.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_3p_vararg_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int=45, arg1 : int, arg2 : int*) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(constructor.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(constructor.getParameters(),
					XNumberLiteral.class, "45",
					null,
					null);
			assertParameterVarArg(constructor.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void constructor_3p_vararg_0_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	new(arg0 : int=45, arg1 : int=56, arg2 : int*) {",
					"		super(null) // must be never null during runtime",
					"		System.out.println(arg0)",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(constructor.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(constructor.getParameters(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "56",
					null);
			assertParameterVarArg(constructor.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void constructorCast_String2int() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package io.sarl.test",
					"behavior B1 {",
					"new(arg0 : int=45, arg1 : int=\"S\", arg2 : int) {",
					"System.out.println(arg0)",
					"}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XbasePackage.eINSTANCE.getXStringLiteral(),
					IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from String to int");
		}

		@Test
		@Tag("sarlParsing")
		public void constructorCast_int2double() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int=45, arg1 : double=18, arg2 : int) {",
					"super(null) // must be never null during runtime",
					"System.out.println(arg0)",
					"}",
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
			assertParameterNames(constructor.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(constructor.getParameters(), "int", "double", "int");
			assertParameterDefaultValues(constructor.getParameters(), XNumberLiteral.class, "45", XNumberLiteral.class, "18", null);
		}

		@Test
		@Tag("sarlValidation")
		public void constructorCast_double2int() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"new(arg0 : int=45, arg1 : int=18.0, arg2 : int) {",
					"System.out.println(arg0)",
					"}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XbasePackage.eINSTANCE.getXNumberLiteral(),
					IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from double to int");
		}

	}

	@Nested
	public class CapacityAction extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void action_1p() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg : int=4)",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg");
			assertParameterTypes(signature.getParameters(), "int");
			assertParameterDefaultValues(signature.getParameters(), XNumberLiteral.class, "4");
		}

		@Test
		@Tag("sarlValidation")
		public void action_1p_invalid1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg : int=4*)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input");
		}

		@Test
		@Tag("sarlValidation")
		public void action_1p_invalid2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg : int*=4)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '*=' expecting ')'");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String)",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParameters(), XNumberLiteral.class, "4", null, null, null, null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String)",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParameters(), null, XStringLiteral.class, "abc", null, null, null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String)",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParameters(), null, null, XNumberLiteral.class, "18", null, null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String)",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParameters(), null, null, null, XNumberLiteral.class, "34", null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\")",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParameters(), null, null, null, null, XStringLiteral.class, "xyz");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String)",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParameters(), XNumberLiteral.class, "4", null, null, XNumberLiteral.class, "56", null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_3_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\")",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParameters(), XNumberLiteral.class, "4", null, null, XNumberLiteral.class, "56", XStringLiteral.class, "def");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_2_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\")",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParameters(), XNumberLiteral.class, "4", null, XNumberLiteral.class, "18", null, XStringLiteral.class, "def");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_1_2_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String)",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParameters(),
					XNumberLiteral.class, "4",
					XStringLiteral.class, "ghj",
					XNumberLiteral.class, "18",
					XNumberLiteral.class, "98",
					null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_1_2_3_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\")",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(signature.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(signature.getParameters(), XNumberLiteral.class, "4", XStringLiteral.class, "ghj", XNumberLiteral.class, "18", XNumberLiteral.class, "98", XStringLiteral.class, "klm");
		}

		@Test
		@Tag("sarlValidation")
		public void action_3p_vararg_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : int, arg2 : int=45*)",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input");
		}

		@Test
		@Tag("sarlParsing")
		public void action_3p_vararg_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int, arg1 : int=45, arg2 : int*)",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(signature.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(signature.getParameters(), null, XNumberLiteral.class, "45", null);
			assertParameterVarArg(signature.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void action_3p_vararg_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=45, arg1 : int, arg2 : int*)",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(signature.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(signature.getParameters(), XNumberLiteral.class, "45", null, null);
			assertParameterVarArg(signature.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void action_3p_vararg_0_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*)",
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
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(signature.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(signature.getParameters(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "56",
					null);
			assertParameterVarArg(signature.getParameters());
		}

	}

	@Nested
	public class SkillAction extends AbstractSarlTest {

		@Test
		@Tag("sarlParsing")
		public void action_1p() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg : int=4) {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg");
			assertParameterTypes(action2.getParameters(), "int");
			assertParameterDefaultValues(action2.getParameters(),
					XNumberLiteral.class, "4");
		}

		@Test
		@Tag("sarlValidation")
		public void action_1p_invalid1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg : int=4*) {}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input");
		}

		@Test
		@Tag("sarlValidation")
		public void action_1p_invalid2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg : int*=4) {}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '*=' expecting ')'");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParameters(),
					XNumberLiteral.class, "4",
					null,
					null,
					null,
					null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParameters(),
					null,
					XStringLiteral.class, "abc",
					null,
					null,
					null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_2() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParameters(),
					null,
					null,
					XNumberLiteral.class, "18",
					null,
					null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParameters(),
					null,
					null,
					null,
					XNumberLiteral.class, "34",
					null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParameters(), 
					null,
					null,
					null,
					null,
					XStringLiteral.class, "xyz");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParameters(),
					XNumberLiteral.class, "4",
					null,
					null,
					XNumberLiteral.class, "56",
					null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_3_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\") {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParameters(),
					XNumberLiteral.class, "4",
					null,
					null,
					XNumberLiteral.class, "56",
					XStringLiteral.class, "def");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_2_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\") {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParameters(),
					XNumberLiteral.class, "4",
					null,
					XNumberLiteral.class, "18",
					null,
					XStringLiteral.class, "def");
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_1_2_3() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String) {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParameters(),
					XNumberLiteral.class, "4",
					XStringLiteral.class, "ghj",
					XNumberLiteral.class, "18",
					XNumberLiteral.class, "98",
					null);
		}

		@Test
		@Tag("sarlParsing")
		public void action_5p_0_1_2_3_4() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\") {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2", "arg3", "arg4");
			assertParameterTypes(action2.getParameters(), "int", "java.lang.String", "int", "int", "java.lang.String");
			assertParameterDefaultValues(action2.getParameters(),
					XNumberLiteral.class, "4",
					XStringLiteral.class, "ghj",
					XNumberLiteral.class, "18",
					XNumberLiteral.class, "98",
					XStringLiteral.class, "klm");
		}

		@Test
		@Tag("sarlParsing")
		public void action_3p_vararg_2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : int, arg2 : int=45*) {}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input");
		}

		@Test
		@Tag("sarlParsing")
		public void action_3p_vararg_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int, arg1 : int=45, arg2 : int*) {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action2.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(action2.getParameters(),
					null,
					XNumberLiteral.class, "45",
					null);
			assertParameterVarArg(action2.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void action_3p_vararg_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=45, arg1 : int, arg2 : int*) {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action2.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(action2.getParameters(),
					XNumberLiteral.class, "45",
					null,
					null);
			assertParameterVarArg(action2.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void action_3p_vararg_0_1() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"capacity C1 {",
					"	def capAction",
					"}",
					"skill S1 implements C1 {",
					"	def capAction {}",
					"	def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*) {}",
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
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action2.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(action2.getParameters(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "56",
					null);
			assertParameterVarArg(action2.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void overridingCapacitySkill() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
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
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(signature.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(signature.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(signature.getParameters(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "56",
					null);
			assertParameterVarArg(signature.getParameters());
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(1);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("capAction", action1.getName());
			assertParameterNames(action1.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action2.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(action2.getParameters(), null, null, null);
			assertParameterVarArg(action2.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void multipleActionDefinitionsInSkill() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
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
			SarlAction action1 = (SarlAction) skill.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action1.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(action1.getParameters(),
					null,
					XNumberLiteral.class, "42",
					null);
			assertParameterVarArg(action1.getParameters());
			//
			SarlAction action2 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(action2.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1");
			assertParameterTypes(action2.getParameters(), "int", "int");
			assertParameterDefaultValues(action2.getParameters(), null, null);
			assertParameterVarArg(action2.getParameters());
		}

		@Test
		@Tag("sarlParsing")
		public void missedActionImplementation_0() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
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

			assertEquals(3, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity1 = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("C1", capacity1.getName());
			assertTypeReferenceIdentifiers(capacity1.getExtends());
			assertEquals(1, capacity1.getMembers().size());
			//
			SarlAction action1 = (SarlAction) capacity1.getMembers().get(0);
			assertEquals("myaction1", action1.getName());
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters(), "a");
			assertParameterTypes(action1.getParameters(), "int");
			assertParameterDefaultValues(action1.getParameters(), XNumberLiteral.class, "4");
			//
			SarlCapacity capacity2 = (SarlCapacity) mas.getXtendTypes().get(1);
			assertEquals("C2", capacity2.getName());
			assertTypeReferenceIdentifiers(capacity2.getExtends());
			assertEquals(1, capacity2.getMembers().size());
			//
			SarlAction action2 = (SarlAction) capacity2.getMembers().get(0);
			assertEquals("myaction2", action2.getName());
			assertTypeReferenceIdentifier(action2.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "b", "c");
			assertParameterTypes(action2.getParameters(), "float", "boolean");
			assertParameterDefaultValues(action2.getParameters(), XNumberLiteral.class, "6", null);
			//
			SarlSkill skill = (SarlSkill) mas.getXtendTypes().get(2);
			assertEquals("S1", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "C1", "C2");
			assertEquals(2, skill.getMembers().size());
			//
			SarlAction action3 = (SarlAction) skill.getMembers().get(0);
			assertEquals("myaction1", action3.getName());
			assertTypeReferenceIdentifier(action3.getReturnType(), "void");
			assertParameterNames(action3.getParameters(), "x");
			assertParameterTypes(action3.getParameters(), "int");
			assertParameterDefaultValues(action3.getParameters(), (Object) null);
			//
			SarlAction action4 = (SarlAction) skill.getMembers().get(1);
			assertEquals("myaction2", action4.getName());
			assertTypeReferenceIdentifier(action4.getReturnType(), "void");
			assertParameterNames(action4.getParameters(), "y", "z");
			assertParameterTypes(action4.getParameters(), "float", "boolean");
			assertParameterDefaultValues(action4.getParameters(), null, null);
		}

		@Test
		@Tag("sarlParsing")
		public void missedActionImplementation_1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
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
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlSkill(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_MUST_BE_ABSTRACT,
					"The class S1 must be defined abstract because it does not implement myaction1(int)");
		}

	}

	@Nested
	public class BehaviorAction extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void actionCast_String2int() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int=45, arg1 : int=\"S\", arg2 : int) {",
					"		System.out.println(arg0)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XbasePackage.eINSTANCE.getXStringLiteral(),
					IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from String to int");
		}

		@Test
		@Tag("sarlParsing")
		public void actionCast_int2double() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int=45, arg1 : double=18, arg2 : int) {",
					"		System.out.println(arg0)",
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
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			assertParameterNames(action.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action.getParameters(), "int", "double", "int");
			assertParameterDefaultValues(action.getParameters(),
					XNumberLiteral.class, "45",
					XNumberLiteral.class, "18",
					null);
			assertNoParameterVarArg(action.getParameters());
		}

		@Test
		@Tag("sarlValidation")
		public void actionCast_double2int() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int=45, arg1 : int=18.0, arg2 : int) {",
					"		System.out.println(arg0)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XbasePackage.eINSTANCE.getXNumberLiteral(),
					IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from double to int");
		}

		@Test
		@Tag("sarlParsing")
		public void multipleActionDefinitionsInBehavior() throws Exception {
			SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
					"behavior B1 {",
					"	def myaction(arg0 : int, arg1 : int=42, arg2 : int*) {",
					"		System.out.println(\"valid\")",
					"	}",
					"	def myaction(arg0 : int, arg1 : int*) {",
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
			assertTypeReferenceIdentifier(action1.getReturnType(), "void");
			assertParameterNames(action1.getParameters(), "arg0", "arg1", "arg2");
			assertParameterTypes(action1.getParameters(), "int", "int", "int");
			assertParameterDefaultValues(action1.getParameters(), null, XNumberLiteral.class, "42", null);
			assertParameterVarArg(action1.getParameters());
			//
			SarlAction action2 = (SarlAction) behavior.getMembers().get(1);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifier(action2.getReturnType(), "void");
			assertParameterNames(action2.getParameters(), "arg0", "arg1");
			assertParameterTypes(action2.getParameters(), "int", "int");
			assertParameterDefaultValues(action2.getParameters(), null, null);
			assertParameterVarArg(action2.getParameters());
		}

	}

}
