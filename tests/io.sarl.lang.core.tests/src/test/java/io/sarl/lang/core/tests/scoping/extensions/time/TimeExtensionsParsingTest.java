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
package io.sarl.lang.core.tests.scoping.extensions.time;

import static io.sarl.tests.api.tools.TestAssertions.assertParameterNames;
import static io.sarl.tests.api.tools.TestAssertions.assertTypeReferenceIdentifier;
import static io.sarl.tests.api.tools.TestAssertions.assertTypeReferenceIdentifiers;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("TimeExtensions - syntax")
@Tag("core")
@Tag("unit")
public class TimeExtensionsParsingTest extends AbstractSarlTest {

	@Test
	public void testMilliseconds() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.milliseconds",
					"}",
				"}"
			));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertEquals("io.sarl.lang.tests.ste", mas.getPackage());
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParameters());
		assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.Object");
	}

	@Test
	public void testSeconds() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.seconds",
					"}",
				"}"
			));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertEquals("io.sarl.lang.tests.ste", mas.getPackage());
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParameters());
		assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.Object");
	}

	@Test
	public void testMinutes() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.minutes",
					"}",
				"}"
			));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertEquals("io.sarl.lang.tests.ste", mas.getPackage());
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParameters());
		assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.Object");
	}

	@Test
	public void testHours() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.hours",
					"}",
				"}"
			));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertEquals("io.sarl.lang.tests.ste", mas.getPackage());
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParameters());
		assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.Object");
	}

	@Test
	public void testDays() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.days",
					"}",
				"}"
			));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertEquals("io.sarl.lang.tests.ste", mas.getPackage());
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParameters());
		assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.Object");
	}

	@Test
	public void testWeeks() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.weeks",
					"}",
				"}"
			));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertEquals("io.sarl.lang.tests.ste", mas.getPackage());
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParameters());
		assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.Object");
	}

}
