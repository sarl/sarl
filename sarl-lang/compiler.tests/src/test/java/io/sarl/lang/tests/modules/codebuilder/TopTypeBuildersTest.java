/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.tests.modules.codebuilder;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.when;
import static io.sarl.tests.api.tools.TestAssertions.*;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Generated;

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.xtend.core.xtend.XtendEnumLiteral;
import org.eclipse.xtext.common.types.JvmLowerBound;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.google.inject.Inject;

import foo.ecore.SubAgent;
import foo.ecore.SubBehavior;
import foo.ecore.SubCapacity;
import foo.ecore.SubCapacity2;
import foo.ecore.SubEvent;
import foo.ecore.SubSkill;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.builders.ISarlActionBuilder;
import io.sarl.lang.codebuilder.builders.ISarlAgentBuilder;
import io.sarl.lang.codebuilder.builders.ISarlAnnotationTypeBuilder;
import io.sarl.lang.codebuilder.builders.ISarlBehaviorBuilder;
import io.sarl.lang.codebuilder.builders.ISarlCapacityBuilder;
import io.sarl.lang.codebuilder.builders.ISarlClassBuilder;
import io.sarl.lang.codebuilder.builders.ISarlEnumerationBuilder;
import io.sarl.lang.codebuilder.builders.ISarlEventBuilder;
import io.sarl.lang.codebuilder.builders.ISarlInterfaceBuilder;
import io.sarl.lang.codebuilder.builders.ISarlSkillBuilder;
import io.sarl.lang.codebuilder.builders.IScriptBuilder;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Event;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEnumLiteral;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.tests.api.AbstractSarlTest;

/** Test the top-type builders of {@code CodeBuilderFactory}.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@DisplayName("Top-type builders of CodeBuilderFactory")
@SuppressWarnings("all")
@Tag("core")
@Tag("unit")
public class TopTypeBuildersTest {

	@DisplayName("SARL Script")
	@Nested
	public class SarlScriptTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private IScriptBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createScript("io.sarl.lang.tests.modules.codebuilder.toptype", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addSarlAgent")
		public void addSarlAgent() {
			var builder = this.builder.addSarlAgent("abc");
			assertNotNull(builder);

			final var script = this.builder.getScript();
			assertNotNull(script);
			final var members = script.getXtendTypes();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlAgent.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlBehavior")
		public void addSarlBehavior() {
			var builder = this.builder.addSarlBehavior("abc");
			assertNotNull(builder);

			final var script = this.builder.getScript();
			assertNotNull(script);
			final var members = script.getXtendTypes();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlBehavior.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlCapacity")
		public void addSarlCapacity() {
			var builder = this.builder.addSarlCapacity("abc");
			assertNotNull(builder);

			final var script = this.builder.getScript();
			assertNotNull(script);
			final var members = script.getXtendTypes();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlCapacity.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlSkill")
		public void addSarlSkill() {
			var builder = this.builder.addSarlSkill("abc");
			assertNotNull(builder);

			final var script = this.builder.getScript();
			assertNotNull(script);
			final var members = script.getXtendTypes();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlSkill.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlEvent")
		public void addSarlEvent() {
			var builder = this.builder.addSarlEvent("abc");
			assertNotNull(builder);

			final var script = this.builder.getScript();
			assertNotNull(script);
			final var members = script.getXtendTypes();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlEvent.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlClass")
		public void addSarlClass() {
			var builder = this.builder.addSarlClass("abc");
			assertNotNull(builder);

			final var script = this.builder.getScript();
			assertNotNull(script);
			final var members = script.getXtendTypes();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlClass.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlInterface")
		public void addSarlInterface() {
			var builder = this.builder.addSarlInterface("abc");
			assertNotNull(builder);

			final var script = this.builder.getScript();
			assertNotNull(script);
			final var members = script.getXtendTypes();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlInterface.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlAnnotationType")
		public void addSarlAnnotationType() {
			var builder = this.builder.addSarlAnnotationType("abc");
			assertNotNull(builder);

			final var script = this.builder.getScript();
			assertNotNull(script);
			final var members = script.getXtendTypes();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlAnnotationType.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlEnumeration")
		public void addSarlEnumeration() {
			var builder = this.builder.addSarlEnumeration("abc");
			assertNotNull(builder);

			final var script = this.builder.getScript();
			assertNotNull(script);
			final var members = script.getXtendTypes();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlEnumeration.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}
		
	}

	@DisplayName("agent")
	@Nested
	public class SarlAgentTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlAgentBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlAgent("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var modifiers = agent.getModifiers();
			assertContains(modifiers, "package");
		}

		@Test
		@DisplayName("addSarlAnnotationType")
		public void addSarlAnnotationType() {
			var type = this.builder.addSarlAnnotationType("abc");
			assertNotNull(type);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlAnnotationType.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlClass")
		public void addSarlClass() {
			var type = this.builder.addSarlClass("abc");
			assertNotNull(type);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlClass.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlEnumeration")
		public void addSarlEnumeration() {
			var type = this.builder.addSarlEnumeration("abc");
			assertNotNull(type);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlEnumeration.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlInterface")
		public void addSarlInterface() {
			var type = this.builder.addSarlInterface("abc");
			assertNotNull(type);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlInterface.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addDefSarlAction")
		public void addDefSarlAction() {
			var action = this.builder.addDefSarlAction("abc");
			assertNotNull(action);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addOverrideSarlAction")
		public void addOverrideSarlAction() {
			var action = this.builder.addOverrideSarlAction("abc");
			assertNotNull(action);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlAction")
		public void addSarlAction() {
			var action = this.builder.addSarlAction("abc");
			assertNotNull(action);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlBehaviorUnit")
		public void addSarlBehaviorUnit() {
			var type = this.builder.addSarlBehaviorUnit(Event.class.getName());
			assertNotNull(type);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlBehaviorUnit.class, members.get(0));
			assertEquals(Event.class.getName(), mbr.getName().getQualifiedName());
		}

		@Test
		@DisplayName("addSarlCapacityUses")
		public void addSarlCapacityUses() {
			this.builder.addSarlCapacityUses(Event.class.getName());

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlCapacityUses.class, members.get(0));
			assertEquals(1, mbr.getCapacities().size());
			var cap = mbr.getCapacities().get(0);
			assertEquals(Event.class.getName(), cap.getQualifiedName());
		}

		@Test
		@DisplayName("addSarlRequiredCapacity")
		public void addSarlRequiredCapacity() {
			this.builder.addSarlRequiredCapacity(Event.class.getName());

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlRequiredCapacity.class, members.get(0));
			assertEquals(1, mbr.getCapacities().size());
			var cap = mbr.getCapacities().get(0);
			assertEquals(Event.class.getName(), cap.getQualifiedName());
		}

		@Test
		@DisplayName("addSarlConstructor")
		public void addSarlConstructor() {
			var builder = this.builder.addSarlConstructor();
			assertNotNull(builder);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlConstructor.class, members.get(0));
		}

		@Test
		@DisplayName("addSarlField")
		public void addSarlField() {
			var builder = this.builder.addSarlField("abc");
			assertNotNull(builder);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("addValSarlField")
		public void addValSarlField() {
			var builder = this.builder.addValSarlField("abc");
			assertNotNull(builder);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "val");
		}

		@Test
		@DisplayName("addVarSarlField")
		public void addVarSarlField() {
			var builder = this.builder.addVarSarlField("abc");
			assertNotNull(builder);

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			final var members = agent.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("setExtends(no agent)")
		public void setExtends_0() {
			this.builder.setExtends(Number.class.getName());

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			assertNull(agent.getExtends());
		}

		@Test
		@DisplayName("setExtends(agent)")
		public void setExtends_agent() {
			this.builder.setExtends(SubAgent.class.getName());

			final var agent = this.builder.getSarlAgent();
			assertNotNull(agent);
			assertEquals(SubAgent.class.getName(), agent.getExtends().getQualifiedName());
		}
		
	}

	@DisplayName("behavior")
	@Nested
	public class SarlBehaviorTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlBehaviorBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlBehavior("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var modifiers = beh.getModifiers();
			assertContains(modifiers, "package");
		}

		@Test
		@DisplayName("addSarlAnnotationType")
		public void addSarlAnnotationType() {
			var type = this.builder.addSarlAnnotationType("abc");
			assertNotNull(type);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlAnnotationType.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlClass")
		public void addSarlClass() {
			var type = this.builder.addSarlClass("abc");
			assertNotNull(type);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlClass.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlEnumeration")
		public void addSarlEnumeration() {
			var type = this.builder.addSarlEnumeration("abc");
			assertNotNull(type);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlEnumeration.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlInterface")
		public void addSarlInterface() {
			var type = this.builder.addSarlInterface("abc");
			assertNotNull(type);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlInterface.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addDefSarlAction")
		public void addDefSarlAction() {
			var action = this.builder.addDefSarlAction("abc");
			assertNotNull(action);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addOverrideSarlAction")
		public void addOverrideSarlAction() {
			var action = this.builder.addOverrideSarlAction("abc");
			assertNotNull(action);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlAction")
		public void addSarlAction() {
			var action = this.builder.addSarlAction("abc");
			assertNotNull(action);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlBehaviorUnit")
		public void addSarlBehaviorUnit() {
			var type = this.builder.addSarlBehaviorUnit(Event.class.getName());
			assertNotNull(type);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlBehaviorUnit.class, members.get(0));
			assertEquals(Event.class.getName(), mbr.getName().getQualifiedName());
		}

		@Test
		@DisplayName("addSarlCapacityUses")
		public void addSarlCapacityUses() {
			this.builder.addSarlCapacityUses(Event.class.getName());

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlCapacityUses.class, members.get(0));
			assertEquals(1, mbr.getCapacities().size());
			var cap = mbr.getCapacities().get(0);
			assertEquals(Event.class.getName(), cap.getQualifiedName());
		}

		@Test
		@DisplayName("addSarlRequiredCapacity")
		public void addSarlRequiredCapacity() {
			this.builder.addSarlRequiredCapacity(Event.class.getName());

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlRequiredCapacity.class, members.get(0));
			assertEquals(1, mbr.getCapacities().size());
			var cap = mbr.getCapacities().get(0);
			assertEquals(Event.class.getName(), cap.getQualifiedName());
		}

		@Test
		@DisplayName("addSarlConstructor")
		public void addSarlConstructor() {
			var builder = this.builder.addSarlConstructor();
			assertNotNull(builder);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlConstructor.class, members.get(0));
		}

		@Test
		@DisplayName("addSarlField")
		public void addSarlField() {
			var builder = this.builder.addSarlField("abc");
			assertNotNull(builder);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("addValSarlField")
		public void addValSarlField() {
			var builder = this.builder.addValSarlField("abc");
			assertNotNull(builder);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "val");
		}

		@Test
		@DisplayName("addVarSarlField")
		public void addVarSarlField() {
			var builder = this.builder.addVarSarlField("abc");
			assertNotNull(builder);

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			final var members = beh.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("setExtends(no beh)")
		public void setExtends_0() {
			this.builder.setExtends(Number.class.getName());

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			assertNull(beh.getExtends());
		}

		@Test
		@DisplayName("setExtends(beh)")
		public void setExtends_agent() {
			this.builder.setExtends(SubBehavior.class.getName());

			final var beh = this.builder.getSarlBehavior();
			assertNotNull(beh);
			assertEquals(SubBehavior.class.getName(), beh.getExtends().getQualifiedName());
		}
		
	}

	@DisplayName("capacity")
	@Nested
	public class SarlCapacityTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlCapacityBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlCapacity("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}
		
		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");

			final var cap = this.builder.getSarlCapacity();
			assertNotNull(cap);
			final var modifiers = cap.getModifiers();
			assertContains(modifiers, "package");
		}

		@Test
		@DisplayName("addDefSarlAction")
		public void addDefSarlAction() {
			var action = this.builder.addDefSarlAction("abc");
			assertNotNull(action);

			final var cap = this.builder.getSarlCapacity();
			assertNotNull(cap);
			final var members = cap.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addOverrideSarlAction")
		public void addOverrideSarlAction() {
			var action = this.builder.addOverrideSarlAction("abc");
			assertNotNull(action);

			final var cap = this.builder.getSarlCapacity();
			assertNotNull(cap);
			final var members = cap.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlAction")
		public void addSarlAction() {
			var action = this.builder.addSarlAction("abc");
			assertNotNull(action);

			final var cap = this.builder.getSarlCapacity();
			assertNotNull(cap);
			final var members = cap.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addExtends(no cap)")
		public void addExtends_0() {
			this.builder.addExtends(Number.class.getName());

			final var cap = this.builder.getSarlCapacity();
			assertNotNull(cap);
			assertEquals(0, cap.getExtends().size());
		}

		@Test
		@DisplayName("addExtends(cap)")
		public void addExtends_capacity() {
			this.builder.addExtends(SubCapacity.class.getName());

			final var cap = this.builder.getSarlCapacity();
			assertNotNull(cap);
			assertEquals(1, cap.getExtends().size());
			var superCap = cap.getExtends().get(0);
			assertEquals(SubCapacity.class.getName(), superCap.getQualifiedName());
		}

		@Test
		@DisplayName("addExtends(cap, cap)")
		public void addExtends_capacity_capacity() {
			this.builder.addExtends(SubCapacity.class.getName());
			this.builder.addExtends(SubCapacity2.class.getName());

			final var cap = this.builder.getSarlCapacity();
			assertNotNull(cap);
			assertEquals(2, cap.getExtends().size());
			var superCap0 = cap.getExtends().get(0);
			assertEquals(SubCapacity.class.getName(), superCap0.getQualifiedName());
			var superCap1 = cap.getExtends().get(1);
			assertEquals(SubCapacity2.class.getName(), superCap1.getQualifiedName());
		}
		
	}

	@DisplayName("skill")
	@Nested
	public class SarlSkillTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlSkillBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlSkill("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var modifiers = skill.getModifiers();
			assertContains(modifiers, "package");
		}

		@Test
		@DisplayName("addSarlAnnotationType")
		public void addSarlAnnotationType() {
			var type = this.builder.addSarlAnnotationType("abc");
			assertNotNull(type);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlAnnotationType.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlClass")
		public void addSarlClass() {
			var type = this.builder.addSarlClass("abc");
			assertNotNull(type);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlClass.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlEnumeration")
		public void addSarlEnumeration() {
			var type = this.builder.addSarlEnumeration("abc");
			assertNotNull(type);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlEnumeration.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlInterface")
		public void addSarlInterface() {
			var type = this.builder.addSarlInterface("abc");
			assertNotNull(type);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlInterface.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addDefSarlAction")
		public void addDefSarlAction() {
			var action = this.builder.addDefSarlAction("abc");
			assertNotNull(action);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addOverrideSarlAction")
		public void addOverrideSarlAction() {
			var action = this.builder.addOverrideSarlAction("abc");
			assertNotNull(action);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlAction")
		public void addSarlAction() {
			var action = this.builder.addSarlAction("abc");
			assertNotNull(action);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlBehaviorUnit")
		public void addSarlBehaviorUnit() {
			var type = this.builder.addSarlBehaviorUnit(Event.class.getName());
			assertNotNull(type);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlBehaviorUnit.class, members.get(0));
			assertEquals(Event.class.getName(), mbr.getName().getQualifiedName());
		}

		@Test
		@DisplayName("addSarlCapacityUses")
		public void addSarlCapacityUses() {
			this.builder.addSarlCapacityUses(Event.class.getName());

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlCapacityUses.class, members.get(0));
			assertEquals(1, mbr.getCapacities().size());
			var cap = mbr.getCapacities().get(0);
			assertEquals(Event.class.getName(), cap.getQualifiedName());
		}

		@Test
		@DisplayName("addSarlRequiredCapacity")
		public void addSarlRequiredCapacity() {
			this.builder.addSarlRequiredCapacity(Event.class.getName());

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlRequiredCapacity.class, members.get(0));
			assertEquals(1, mbr.getCapacities().size());
			var cap = mbr.getCapacities().get(0);
			assertEquals(Event.class.getName(), cap.getQualifiedName());
		}

		@Test
		@DisplayName("addSarlConstructor")
		public void addSarlConstructor() {
			var builder = this.builder.addSarlConstructor();
			assertNotNull(builder);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlConstructor.class, members.get(0));
		}

		@Test
		@DisplayName("addSarlField")
		public void addSarlField() {
			var builder = this.builder.addSarlField("abc");
			assertNotNull(builder);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("addValSarlField")
		public void addValSarlField() {
			var builder = this.builder.addValSarlField("abc");
			assertNotNull(builder);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "val");
		}

		@Test
		@DisplayName("addVarSarlField")
		public void addVarSarlField() {
			var builder = this.builder.addVarSarlField("abc");
			assertNotNull(builder);

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			final var members = skill.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("setExtends(no skill)")
		public void setExtends_0() {
			this.builder.setExtends(Number.class.getName());

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			assertNull(skill.getExtends());
		}

		@Test
		@DisplayName("setExtends(skill)")
		public void setExtends_skill() {
			this.builder.setExtends(SubSkill.class.getName());

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			assertEquals(SubSkill.class.getName(), skill.getExtends().getQualifiedName());
		}

		@Test
		@DisplayName("addImplements(no cap)")
		public void addImplements_0() {
			this.builder.addImplements(Number.class.getName());

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			assertEquals(0, skill.getImplements().size());
		}

		@Test
		@DisplayName("addImplements(cap)")
		public void addImplements_capacity() {
			this.builder.addImplements(SubCapacity.class.getName());

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			assertEquals(1, skill.getImplements().size());
			var superCap = skill.getImplements().get(0);
			assertEquals(SubCapacity.class.getName(), superCap.getQualifiedName());
		}

		@Test
		@DisplayName("addImplements(cap, cap)")
		public void addImplements_capacity_capacity() {
			this.builder.addImplements(SubCapacity.class.getName());
			this.builder.addImplements(SubCapacity2.class.getName());

			final var skill = this.builder.getSarlSkill();
			assertNotNull(skill);
			assertEquals(2, skill.getImplements().size());
			var superCap0 = skill.getImplements().get(0);
			assertEquals(SubCapacity.class.getName(), superCap0.getQualifiedName());
			var superCap1 = skill.getImplements().get(1);
			assertEquals(SubCapacity2.class.getName(), superCap1.getQualifiedName());
		}
		
	}

	@DisplayName("event")
	@Nested
	public class SarlEventTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlEventBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlEvent("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");

			final var evt = this.builder.getSarlEvent();
			assertNotNull(evt);
			final var modifiers = evt.getModifiers();
			assertContains(modifiers, "package");
		}

		@Test
		@DisplayName("addSarlConstructor")
		public void addSarlConstructor() {
			var builder = this.builder.addSarlConstructor();
			assertNotNull(builder);

			final var evt = this.builder.getSarlEvent();
			assertNotNull(evt);
			final var members = evt.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlConstructor.class, members.get(0));
		}

		@Test
		@DisplayName("addSarlField")
		public void addSarlField() {
			var builder = this.builder.addSarlField("abc");
			assertNotNull(builder);

			final var evt = this.builder.getSarlEvent();
			assertNotNull(evt);
			final var members = evt.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("addValSarlField")
		public void addValSarlField() {
			var builder = this.builder.addValSarlField("abc");
			assertNotNull(builder);

			final var evt = this.builder.getSarlEvent();
			assertNotNull(evt);
			final var members = evt.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "val");
		}

		@Test
		@DisplayName("addVarSarlField")
		public void addVarSarlField() {
			var builder = this.builder.addVarSarlField("abc");
			assertNotNull(builder);

			final var evt = this.builder.getSarlEvent();
			assertNotNull(evt);
			final var members = evt.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("setExtends(no evt)")
		public void setExtends_0() {
			this.builder.setExtends(Number.class.getName());

			final var evt = this.builder.getSarlEvent();
			assertNotNull(evt);
			assertNull(evt.getExtends());
		}

		@Test
		@DisplayName("setExtends(evt)")
		public void setExtends_event() {
			this.builder.setExtends(SubEvent.class.getName());

			final var beh = this.builder.getSarlEvent();
			assertNotNull(beh);
			assertEquals(SubEvent.class.getName(), beh.getExtends().getQualifiedName());
		}

		@Test
		@DisplayName("addTypeParameter(P)")
		public void addTypeParameter_p() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);
			
			final var evt = this.builder.getSarlEvent();
			assertNotNull(evt);
			final var parameters = evt.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(0, param.getConstraints().size());
		}

		@Test
		@DisplayName("addTypeParameter(P extends Integer)")
		public void addTypeParameter_p_extends_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addUpperConstraint(Integer.class.getName());
			
			final var evt = this.builder.getSarlEvent();
			assertNotNull(evt);
			final var parameters = evt.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmUpperBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}

		@Test
		@DisplayName("addTypeParameter(P super Integer)")
		public void addTypeParameter_p_super_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addLowerConstraint(Integer.class.getName());
			
			final var evt = this.builder.getSarlEvent();
			assertNotNull(evt);
			final var parameters = evt.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmLowerBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}
		
	}

	@DisplayName("class")
	@Nested
	public class SarlClassTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlClassBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlClass("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var modifiers = cls.getModifiers();
			assertContains(modifiers, "package");
		}

		@Test
		@DisplayName("addSarlAnnotationType")
		public void addSarlAnnotationType() {
			var type = this.builder.addSarlAnnotationType("abc");
			assertNotNull(type);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlAnnotationType.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlClass")
		public void addSarlClass() {
			var type = this.builder.addSarlClass("abc");
			assertNotNull(type);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlClass.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlEnumeration")
		public void addSarlEnumeration() {
			var type = this.builder.addSarlEnumeration("abc");
			assertNotNull(type);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlEnumeration.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlInterface")
		public void addSarlInterface() {
			var type = this.builder.addSarlInterface("abc");
			assertNotNull(type);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlInterface.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addDefSarlAction")
		public void addDefSarlAction() {
			var action = this.builder.addDefSarlAction("abc");
			assertNotNull(action);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addOverrideSarlAction")
		public void addOverrideSarlAction() {
			var action = this.builder.addOverrideSarlAction("abc");
			assertNotNull(action);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlAction")
		public void addSarlAction() {
			var action = this.builder.addSarlAction("abc");
			assertNotNull(action);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlConstructor")
		public void addSarlConstructor() {
			var builder = this.builder.addSarlConstructor();
			assertNotNull(builder);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlConstructor.class, members.get(0));
		}

		@Test
		@DisplayName("addSarlField")
		public void addSarlField() {
			var builder = this.builder.addSarlField("abc");
			assertNotNull(builder);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("addValSarlField")
		public void addValSarlField() {
			var builder = this.builder.addValSarlField("abc");
			assertNotNull(builder);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "val");
		}

		@Test
		@DisplayName("addVarSarlField")
		public void addVarSarlField() {
			var builder = this.builder.addVarSarlField("abc");
			assertNotNull(builder);

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var members = cls.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("setExtends")
		public void setExtends_skill() {
			this.builder.setExtends(SubSkill.class.getName());

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			assertEquals(SubSkill.class.getName(), cls.getExtends().getQualifiedName());
		}

		@Test
		@DisplayName("addImplements(type)")
		public void addImplements_type() {
			this.builder.addImplements(SubCapacity.class.getName());

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			assertEquals(1, cls.getImplements().size());
			var superCap = cls.getImplements().get(0);
			assertEquals(SubCapacity.class.getName(), superCap.getQualifiedName());
		}

		@Test
		@DisplayName("addImplements(type, type)")
		public void addImplements_type_type() {
			this.builder.addImplements(SubCapacity.class.getName());
			this.builder.addImplements(SubCapacity2.class.getName());

			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			assertEquals(2, cls.getImplements().size());
			var superCap0 = cls.getImplements().get(0);
			assertEquals(SubCapacity.class.getName(), superCap0.getQualifiedName());
			var superCap1 = cls.getImplements().get(1);
			assertEquals(SubCapacity2.class.getName(), superCap1.getQualifiedName());
		}

		@Test
		@DisplayName("addTypeParameter(P)")
		public void addTypeParameter_p() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);
			
			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var parameters = cls.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(0, param.getConstraints().size());
		}

		@Test
		@DisplayName("addTypeParameter(P extends Integer)")
		public void addTypeParameter_p_extends_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addUpperConstraint(Integer.class.getName());
			
			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var parameters = cls.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmUpperBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}

		@Test
		@DisplayName("addTypeParameter(P super Integer)")
		public void addTypeParameter_p_super_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addLowerConstraint(Integer.class.getName());
			
			final var cls = this.builder.getSarlClass();
			assertNotNull(cls);
			final var parameters = cls.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmLowerBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}
		
	}

	@DisplayName("interface")
	@Nested
	public class SarlInterfaceTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlInterfaceBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlInterface("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var modifiers = intf.getModifiers();
			assertContains(modifiers, "package");
		}

		@Test
		@DisplayName("addSarlAnnotationType")
		public void addSarlAnnotationType() {
			var type = this.builder.addSarlAnnotationType("abc");
			assertNotNull(type);

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var members = intf.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlAnnotationType.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlClass")
		public void addSarlClass() {
			var type = this.builder.addSarlClass("abc");
			assertNotNull(type);

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var members = intf.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlClass.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlEnumeration")
		public void addSarlEnumeration() {
			var type = this.builder.addSarlEnumeration("abc");
			assertNotNull(type);

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var members = intf.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlEnumeration.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlInterface")
		public void addSarlInterface() {
			var type = this.builder.addSarlInterface("abc");
			assertNotNull(type);

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var members = intf.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlInterface.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addDefSarlAction")
		public void addDefSarlAction() {
			var action = this.builder.addDefSarlAction("abc");
			assertNotNull(action);

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var members = intf.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addOverrideSarlAction")
		public void addOverrideSarlAction() {
			var action = this.builder.addOverrideSarlAction("abc");
			assertNotNull(action);

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var members = intf.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlAction")
		public void addSarlAction() {
			var action = this.builder.addSarlAction("abc");
			assertNotNull(action);

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var members = intf.getMembers();
			assertEquals(1, members.size());
			var act = Assertions.assertInstanceOf(SarlAction.class, members.get(0));
			assertEquals("abc", act.getName());
		}

		@Test
		@DisplayName("addSarlField")
		public void addSarlField() {
			var builder = this.builder.addSarlField("abc");
			assertNotNull(builder);

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var members = intf.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("addValSarlField")
		public void addValSarlField() {
			var builder = this.builder.addValSarlField("abc");
			assertNotNull(builder);

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var members = intf.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "val");
		}

		@Test
		@DisplayName("addVarSarlField")
		public void addVarSarlField() {
			var builder = this.builder.addVarSarlField("abc");
			assertNotNull(builder);

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var members = intf.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("addExtends(type)")
		public void addExtends_type() {
			this.builder.addExtends(SubCapacity.class.getName());

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			assertEquals(1, intf.getExtends().size());
			var superCap = intf.getExtends().get(0);
			assertEquals(SubCapacity.class.getName(), superCap.getQualifiedName());
		}

		@Test
		@DisplayName("addExtends(type, type)")
		public void addExtends_type_type() {
			this.builder.addExtends(SubCapacity.class.getName());
			this.builder.addExtends(SubCapacity2.class.getName());

			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			assertEquals(2, intf.getExtends().size());
			var superCap0 = intf.getExtends().get(0);
			assertEquals(SubCapacity.class.getName(), superCap0.getQualifiedName());
			var superCap1 = intf.getExtends().get(1);
			assertEquals(SubCapacity2.class.getName(), superCap1.getQualifiedName());
		}

		@Test
		@DisplayName("addTypeParameter(P)")
		public void addTypeParameter_p() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);
			
			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var parameters = intf.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(0, param.getConstraints().size());
		}

		@Test
		@DisplayName("addTypeParameter(P extends Integer)")
		public void addTypeParameter_p_extends_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addUpperConstraint(Integer.class.getName());
			
			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var parameters = intf.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmUpperBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}

		@Test
		@DisplayName("addTypeParameter(P super Integer)")
		public void addTypeParameter_p_super_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addLowerConstraint(Integer.class.getName());
			
			final var intf = this.builder.getSarlInterface();
			assertNotNull(intf);
			final var parameters = intf.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmLowerBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}

	}

	@DisplayName("enum")
	@Nested
	public class SarlEnumerationTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlEnumerationBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlEnumeration("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");

			final var intf = this.builder.getSarlEnumeration();
			assertNotNull(intf);
			final var modifiers = intf.getModifiers();
			assertContains(modifiers, "package");
		}

		@Test
		@DisplayName("addSarlEnumLiteral(\"abc\")")
		public void addSarlEnumLiteral_1() {
			var type = this.builder.addSarlEnumLiteral("abc");
			assertNotNull(type);

			final var enm = this.builder.getSarlEnumeration();
			assertNotNull(enm);
			final var members = enm.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlEnumLiteral.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlEnumLiteral(\"abc\",\"def\")")
		public void addSarlEnumLiteral() {
			var type = this.builder.addSarlEnumLiteral("abc");
			assertNotNull(type);
			type = this.builder.addSarlEnumLiteral("def");
			assertNotNull(type);

			final var enm = this.builder.getSarlEnumeration();
			assertNotNull(enm);
			final var members = enm.getMembers();
			assertEquals(2, members.size());
			var mbr = Assertions.assertInstanceOf(SarlEnumLiteral.class, members.get(0));
			assertEquals("abc", mbr.getName());
			mbr = Assertions.assertInstanceOf(SarlEnumLiteral.class, members.get(1));
			assertEquals("def", mbr.getName());
		}

	}

	@DisplayName("annotation")
	@Nested
	public class SarlAnnotationTypeTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlAnnotationTypeBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlAnnotationType("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");

			final var anon = this.builder.getSarlAnnotationType();
			assertNotNull(anon);
			final var modifiers = anon.getModifiers();
			assertContains(modifiers, "package");
		}

		@Test
		@DisplayName("addSarlAnnotationType")
		public void addSarlAnnotationType() {
			var type = this.builder.addSarlAnnotationType("abc");
			assertNotNull(type);

			final var anon = this.builder.getSarlAnnotationType();
			assertNotNull(anon);
			final var members = anon.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlAnnotationType.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlClass")
		public void addSarlClass() {
			var type = this.builder.addSarlClass("abc");
			assertNotNull(type);

			final var anon = this.builder.getSarlAnnotationType();
			assertNotNull(anon);
			final var members = anon.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlClass.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlEnumeration")
		public void addSarlEnumeration() {
			var type = this.builder.addSarlEnumeration("abc");
			assertNotNull(type);

			final var anon = this.builder.getSarlAnnotationType();
			assertNotNull(anon);
			final var members = anon.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlEnumeration.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlInterface")
		public void addSarlInterface() {
			var type = this.builder.addSarlInterface("abc");
			assertNotNull(type);

			final var anon = this.builder.getSarlAnnotationType();
			assertNotNull(anon);
			final var members = anon.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlInterface.class, members.get(0));
			assertEquals("abc", mbr.getName());
		}

		@Test
		@DisplayName("addSarlField")
		public void addSarlField() {
			var builder = this.builder.addSarlField("abc");
			assertNotNull(builder);

			final var anon = this.builder.getSarlAnnotationType();
			assertNotNull(anon);
			final var members = anon.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

		@Test
		@DisplayName("addValSarlField")
		public void addValSarlField() {
			var builder = this.builder.addValSarlField("abc");
			assertNotNull(builder);

			final var anon = this.builder.getSarlAnnotationType();
			assertNotNull(anon);
			final var members = anon.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "val");
		}

		@Test
		@DisplayName("addVarSarlField")
		public void addVarSarlField() {
			var builder = this.builder.addVarSarlField("abc");
			assertNotNull(builder);

			final var anon = this.builder.getSarlAnnotationType();
			assertNotNull(anon);
			final var members = anon.getMembers();
			assertEquals(1, members.size());
			var mbr = Assertions.assertInstanceOf(SarlField.class, members.get(0));
			assertEquals("abc", mbr.getName());
			assertContains(mbr.getModifiers(), "var");
		}

	}
	
}
