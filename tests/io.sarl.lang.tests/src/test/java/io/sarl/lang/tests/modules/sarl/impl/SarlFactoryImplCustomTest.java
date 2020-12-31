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
package io.sarl.lang.tests.modules.sarl.impl;

import static io.sarl.tests.api.tools.TestAssertions.assertInstanceOf;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.impl.SarlActionImplCustom;
import io.sarl.lang.sarl.impl.SarlAgentImpl;
import io.sarl.lang.sarl.impl.SarlAgentImplCustom;
import io.sarl.lang.sarl.impl.SarlAnnotationTypeImplCustom;
import io.sarl.lang.sarl.impl.SarlBehaviorImpl;
import io.sarl.lang.sarl.impl.SarlBehaviorImplCustom;
import io.sarl.lang.sarl.impl.SarlBehaviorUnitImpl;
import io.sarl.lang.sarl.impl.SarlCapacityImpl;
import io.sarl.lang.sarl.impl.SarlCapacityUsesImpl;
import io.sarl.lang.sarl.impl.SarlClassImplCustom;
import io.sarl.lang.sarl.impl.SarlConstructorImpl;
import io.sarl.lang.sarl.impl.SarlEnumerationImplCustom;
import io.sarl.lang.sarl.impl.SarlFieldImplCustom;
import io.sarl.lang.sarl.impl.SarlInterfaceImplCustom;
import io.sarl.lang.sarl.impl.SarlRequiredCapacityImpl;
import io.sarl.lang.sarl.impl.SarlSkillImpl;
import io.sarl.lang.sarl.impl.SarlSkillImplCustom;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("ecore: custom SarlFactory")
@Tag("core")
@Tag("unit")
public class SarlFactoryImplCustomTest extends AbstractSarlTest {

	@Test
	public void agent() throws Exception {
		SarlScript mas = file(getParseHelper(), "agent A1 { }");
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlAgentImpl.class, element);
		assertEquals(SarlAgentImplCustom.class, element.getClass());
	}

	@Test
	public void behavior() throws Exception {
		SarlScript mas = file(getParseHelper(), "behavior B1 { }");
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlBehaviorImpl.class, element);
		assertEquals(SarlBehaviorImplCustom.class, element.getClass());
	}

	@Test
	public void capacity() throws Exception {
		SarlScript mas = file(getParseHelper(), "capacity C1 { }");
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlCapacityImpl.class, element);
		assertEquals(SarlCapacityImpl.class, element.getClass());
	}

	@Test
	public void skill() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"capacity C1 { }",
				"skill S1 implements C1 { }"));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(1);
		assertInstanceOf(SarlSkillImpl.class, element);
		assertEquals(SarlSkillImplCustom.class, element.getClass());
	}

	@Test
	public void clazz() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"class C1 { }"));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlClassImplCustom.class, element);
	}

	@Test
	public void interfaze() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"interface C1 { }"));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlInterfaceImplCustom.class, element);
	}

	@Test
	public void enumeration() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"enum E1 { CONST1, CONST2 }"));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlEnumerationImplCustom.class, element);
	}

	@Test
	public void annotationType() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"annotation A1 { }"));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlAnnotationTypeImplCustom.class, element);
	}

	@Test
	public void constructor() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"	new { super(null, null) }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlConstructorImpl.class, element);
	}

	@Test
	public void action() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"	def myfct { }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlActionImplCustom.class, element);
	}

	@Test
	public void behaviorUnit() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"event E1",
				"agent A1 {",
				"	on E1 { }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlBehaviorUnitImpl.class, element);
	}

	@Test
	public void capacityUses() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"capacity C1 { }",
				"agent A1 {",
				"	uses C1",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlCapacityUsesImpl.class, element);
	}

	@Test
	public void requireCapacity() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"capacity C1 { }",
				"agent A1 {",
				"	requires C1",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlRequiredCapacityImpl.class, element);
	}

	@Test
	public void value() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"	val field = 1",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlFieldImplCustom.class, element);
	}

	@Test
	public void variable() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"	var field = 1",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlFieldImplCustom.class, element);
	}

}
