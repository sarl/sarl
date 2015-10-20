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
package io.sarl.lang.tests.parsing.general;

import static io.sarl.lang.scoping.batch.SARLTimeExtensions.days;
import static io.sarl.lang.scoping.batch.SARLTimeExtensions.hours;
import static io.sarl.lang.scoping.batch.SARLTimeExtensions.milliseconds;
import static io.sarl.lang.scoping.batch.SARLTimeExtensions.minutes;
import static io.sarl.lang.scoping.batch.SARLTimeExtensions.seconds;
import static io.sarl.lang.scoping.batch.SARLTimeExtensions.weeks;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

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
import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLTimeExtensionsParsingTest extends AbstractSarlTest {

	@Test
	public void testMilliseconds() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.milliseconds",
					"}",
				"}"
			), true);
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
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.seconds",
					"}",
				"}"
			), true);
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
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.minutes",
					"}",
				"}"
			), true);
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
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.hours",
					"}",
				"}"
			), true);
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
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.days",
					"}",
				"}"
			), true);
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
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.weeks",
					"}",
				"}"
			), true);
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
