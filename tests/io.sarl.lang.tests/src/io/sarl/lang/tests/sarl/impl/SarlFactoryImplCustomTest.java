/*
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.sarl.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.impl.SarlActionImplCustom;
import io.sarl.lang.sarl.impl.SarlAgentImpl;
import io.sarl.lang.sarl.impl.SarlAnnotationTypeImplCustom;
import io.sarl.lang.sarl.impl.SarlBehaviorImpl;
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
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.google.common.base.Strings;
import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SarlFactoryImplCustomTest extends AbstractSarlTest {

	@Test
	public void agent() throws Exception {
		SarlScript mas = file("agent A1 { }");
		validate(mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlAgentImpl.class, element);
	}

	@Test
	public void behavior() throws Exception {
		SarlScript mas = file("behavior B1 { }");
		validate(mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlBehaviorImpl.class, element);
	}

	@Test
	public void capacity() throws Exception {
		SarlScript mas = file("capacity C1 { }");
		validate(mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlCapacityImpl.class, element);
	}

	@Test
	public void skill() throws Exception {
		SarlScript mas = file(multilineString(
				"capacity C1 { }",
				"skill S1 implements C1 { }"));
		validate(mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(1);
		assertInstanceOf(SarlSkillImpl.class, element);
	}

	@Test
	public void clazz() throws Exception {
		SarlScript mas = file(multilineString(
				"class C1 { }"));
		validate(mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlClassImplCustom.class, element);
	}

	@Test
	public void interfaze() throws Exception {
		SarlScript mas = file(multilineString(
				"interface C1 { }"));
		validate(mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlInterfaceImplCustom.class, element);
	}

	@Test
	public void enumeration() throws Exception {
		SarlScript mas = file(multilineString(
				"enum E1 { CONST1, CONST2 }"));
		validate(mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlEnumerationImplCustom.class, element);
	}

	@Test
	public void annotationType() throws Exception {
		SarlScript mas = file(multilineString(
				"annotation A1 { }"));
		validate(mas).assertNoErrors();
		Object element = mas.getXtendTypes().get(0);
		assertInstanceOf(SarlAnnotationTypeImplCustom.class, element);
	}

	@Test
	public void constructor() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"	new { super(null) }",
				"}"
				));
		validate(mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlConstructorImpl.class, element);
	}

	@Test
	public void action() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"	def myfct { }",
				"}"
				));
		validate(mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlActionImplCustom.class, element);
	}

	@Test
	public void behaviorUnit() throws Exception {
		SarlScript mas = file(multilineString(
				"event E1",
				"agent A1 {",
				"	on E1 { }",
				"}"
				));
		validate(mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlBehaviorUnitImpl.class, element);
	}

	@Test
	public void capacityUses() throws Exception {
		SarlScript mas = file(multilineString(
				"capacity C1 { }",
				"agent A1 {",
				"	uses C1",
				"}"
				));
		validate(mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlCapacityUsesImpl.class, element);
	}

	@Test
	public void requireCapacity() throws Exception {
		SarlScript mas = file(multilineString(
				"capacity C1 { }",
				"agent A1 {",
				"	requires C1",
				"}"
				));
		validate(mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlRequiredCapacityImpl.class, element);
	}

	@Test
	public void value() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"	val field = 1",
				"}"
				));
		validate(mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlFieldImplCustom.class, element);
	}

	@Test
	public void variable() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"	var field = 1",
				"}"
				));
		validate(mas).assertNoErrors();
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertNotNull(agent);
		Object element = agent.getMembers().get(0);
		assertInstanceOf(SarlFieldImplCustom.class, element);
	}

}
