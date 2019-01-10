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
package io.sarl.lang.tests.general.parsing.general;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

import com.google.common.base.Strings;
import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class EarlyExistParsingTest extends AbstractSarlTest {

	@Inject
	private ISerializer serializer;
	
	@Test
	public void earlyExistFunction_inAction_lastExpression_0() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"class EarlyExitFunctionDefinitions {",
				"	@EarlyExit",
				"	static def killFunction2 {",
				"	}",
				"}",
				"agent A1 {",
				"	def caller {",
				"		EarlyExitFunctionDefinitions::killFunction2",
				"	}",
				"}"
				), true);
		assertEquals(2, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("caller", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParameters());
		assertTypeReferenceIdentifier(action.getReturnType(), "void");
	}

	@Test
	public void earlyExistFunction_inAction_lastExpression_1() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"class EarlyExitFunctionDefinitions {",
				"	@EarlyExit",
				"	def killFunction1 {",
				"	}",
				"}",
				"agent A1 {",
				"	def caller {",
				"		var inst = new EarlyExitFunctionDefinitions",
				"		inst.killFunction1",
				"	}",
				"}"
				), true);
		assertEquals(2, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("caller", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParameters());
		assertTypeReferenceIdentifier(action.getReturnType(), "void");
	}

	@Test
	public void earlyExistFunction_inAction_penultimateExpression_0() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"class EarlyExitFunctionDefinitions {",
				"	@EarlyExit",
				"	static def killFunction2 {",
				"	}",
				"}",
				"agent A1 {",
				"	def caller {",
				"		EarlyExitFunctionDefinitions::killFunction2",
				"		println(\"Hello\")",
				"	}",
				"}"
				));
		
		validate(mas).assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

	@Test
	public void earlyExistFunction_inAction_penultimateExpression_1() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"class EarlyExitFunctionDefinitions {",
				"	@EarlyExit",
				"	def killFunction1 {",
				"	}",
				"}",
				"agent A1 {",
				"	def caller {",
				"		var inst = new EarlyExitFunctionDefinitions",
				"		inst.killFunction1",
				"		println(\"Hello\")",
				"	}",
				"}"
				));
		validate(mas).assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

	@Test
	public void earlyExistFunction_inIf_0() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"class EarlyExitFunctionDefinitions {",
				"	@EarlyExit",
				"	static def killFunction2 {",
				"	}",
				"}",
				"agent A1 {",
				"	def caller {",
				"		if (true) {",
				"			EarlyExitFunctionDefinitions::killFunction2",
				"		} else {",
				"			EarlyExitFunctionDefinitions::killFunction2",
				"		}",
				"		println(\"Hello\")",
				"	}",
				"}"
				));
		validate(mas).assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

	@Test
	public void earlyExistFunction_inIf_1() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"class EarlyExitFunctionDefinitions {",
				"	@EarlyExit",
				"	static def killFunction2 {",
				"	}",
				"}",
				"agent A1 {",
				"	def caller {",
				"		if (true) {",
				"			EarlyExitFunctionDefinitions::killFunction2",
				"		} else {",
				"			println(\"Hello\")",
				"		}",
				"		println(\"Bye bye\")",
				"	}",
				"}"
				), true);
		assertEquals(2, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("caller", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParameters());
		assertTypeReferenceIdentifier(action.getReturnType(), "void");
	}

	@Test
	public void earlyExistFunction_inIf_2() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"class EarlyExitFunctionDefinitions {",
				"	@EarlyExit",
				"	static def killFunction2 {",
				"	}",
				"}",
				"agent A1 {",
				"	def caller {",
				"		if (true) {",
				"			println(\"Hello\")",
				"		} else {",
				"			EarlyExitFunctionDefinitions::killFunction2",
				"		}",
				"		println(\"Bye bye\")",
				"	}",
				"}"
				), true);
		assertEquals(2, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("caller", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParameters());
		assertTypeReferenceIdentifier(action.getReturnType(), "void");
	}

	@Test
	public void earlyExistFunction_inWhile_0() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"class EarlyExitFunctionDefinitions {",
				"	@EarlyExit",
				"	static def killFunction2 {",
				"	}",
				"}",
				"agent A1 {",
				"	def caller {",
				"		while (true) {",
				"			EarlyExitFunctionDefinitions::killFunction2",
				"		}",
				"		println(\"Hello\")",
				"	}",
				"}"
				));
		validate(mas).assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

}
