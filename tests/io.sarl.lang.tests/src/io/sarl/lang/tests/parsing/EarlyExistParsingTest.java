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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.TestClasspath;
import io.sarl.tests.api.TestScope;

import org.eclipse.xtext.junit4.util.ParseHelper;
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
@TestClasspath("io.sarl.tests.testdata")
public class EarlyExistParsingTest extends AbstractSarlUiTest {

	@Inject
	private ISerializer serializer;

	@Test
	@TestScope(tycho=false)
	public void earlyExistFunction_inAction_lastExpression_0() throws Exception {
		SarlScript mas = parseWithProjectClasspath(
				"agent A1 {",
				"	def caller {",
				"		foo.EarlyExitFunctionDefinitions::killFunction2",
				"	}",
				"}"
				);
		this.helper.getValidator().assertNoErrors(mas);
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
		assertEquals("caller", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "void");
	}

	@Test
	@TestScope(tycho=false)
	public void earlyExistFunction_inAction_lastExpression_1() throws Exception {
		ParseHelper<SarlScript> helper;
		SarlScript mas = parseWithProjectClasspath(
				"agent A1 {",
				"	def caller {",
				"		var inst = new foo.EarlyExitFunctionDefinitions",
				"		inst.killFunction1",
				"	}",
				"}"
				);
		this.helper.getValidator().assertNoErrors(mas);
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
		assertEquals("caller", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "void");
	}

	@Test
	@TestScope(tycho=false)
	public void earlyExistFunction_inAction_penultimateExpression_0() throws Exception {
		SarlScript mas = parseWithProjectClasspath(
				"agent A1 {",
				"	def caller {",
				"		foo.EarlyExitFunctionDefinitions::killFunction2",
				"		println(\"Hello\")",
				"	}",
				"}"
				);
		this.helper.getValidator().assertError(mas,
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.UNREACHABLE_CODE,
				77,
				16,
				"Unreachable expression");
	}

	@Test
	@TestScope(tycho=false)
	public void earlyExistFunction_inAction_penultimateExpression_1() throws Exception {
		SarlScript mas = parseWithProjectClasspath(
				"agent A1 {",
				"	def caller {",
				"		var inst = new foo.EarlyExitFunctionDefinitions",
				"		inst.killFunction1",
				"		println(\"Hello\")",
				"	}",
				"}"
				);
		this.helper.getValidator().assertError(mas,
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.UNREACHABLE_CODE,
				98,
				16,
				"Unreachable expression");
	}

	@Test
	@TestScope(tycho=false)
	public void earlyExistFunction_inIf_0() throws Exception {
		SarlScript mas = parseWithProjectClasspath(
				"agent A1 {",
				"	def caller {",
				"		if (true) {",
				"			foo.EarlyExitFunctionDefinitions::killFunction2",
				"		} else {",
				"			foo.EarlyExitFunctionDefinitions::killFunction2",
				"		}",
				"		println(\"Hello\")",
				"	}",
				"}"
				);
		this.helper.getValidator().assertError(mas,
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.UNREACHABLE_CODE,
				158,
				16,
				"Unreachable expression");
	}

	@Test
	@TestScope(tycho=false)
	public void earlyExistFunction_inIf_1() throws Exception {
		SarlScript mas = parseWithProjectClasspath(
				"agent A1 {",
				"	def caller {",
				"		if (true) {",
				"			foo.EarlyExitFunctionDefinitions::killFunction2",
				"		} else {",
				"			println(\"Hello\")",
				"		}",
				"		println(\"Bye bye\")",
				"	}",
				"}"
				);
		this.helper.getValidator().assertNoErrors(mas);
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
		assertEquals("caller", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "void");
	}

	@Test
	@TestScope(tycho=false)
	public void earlyExistFunction_inIf_2() throws Exception {
		SarlScript mas = parseWithProjectClasspath(
				"agent A1 {",
				"	def caller {",
				"		if (true) {",
				"			println(\"Hello\")",
				"		} else {",
				"			foo.EarlyExitFunctionDefinitions::killFunction2",
				"		}",
				"		println(\"Bye bye\")",
				"	}",
				"}"
				);
		this.helper.getValidator().assertNoErrors(mas);
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
		assertEquals("caller", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "void");
	}

	@Test
	@TestScope(tycho=false)
	public void earlyExistFunction_inWhile_0() throws Exception {
		SarlScript mas = parseWithProjectClasspath(
				"agent A1 {",
				"	def caller {",
				"		while (true) {",
				"			foo.EarlyExitFunctionDefinitions::killFunction2",
				"		}",
				"		println(\"Hello\")",
				"	}",
				"}"
				);
		this.helper.getValidator().assertError(mas,
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.UNREACHABLE_CODE,
				99,
				16,
				"Unreachable expression");
	}

}
