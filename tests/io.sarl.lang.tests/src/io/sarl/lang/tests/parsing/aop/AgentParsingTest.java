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
package io.sarl.lang.tests.parsing.aop;

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
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
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
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	AgentParsingTest.TopElementTest.class,
	AgentParsingTest.BehaviorUnitTest.class,
	AgentParsingTest.FieldTest.class,
	AgentParsingTest.CapacityUseTest.class,
	AgentParsingTest.ActionTest.class,
})
@SuppressWarnings("all")
public class AgentParsingTest {

	public static class TopElementTest extends AbstractSarlTest {

		@Test
		public void declaration() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {}",
					"agent A2 {}"
					), true);
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(0, agent1.getMembers().size());
			//
			SarlAgent agent2 = (SarlAgent) mas.getXtendTypes().get(1);
			assertEquals("A2", agent2.getName());
			assertNull(agent2.getExtends());
			assertEquals(0, agent2.getMembers().size());
		}

		@Test
		public void invalidExtend() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 {",
					"}",
					"agent A1 extends C1 {",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED,
					33, 2,
					"Invalid supertype. Expecting a class.");
		}

		@Test
		public void recursiveAgentExtension_0() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 extends A1 {",
					"}"
					));

			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'A1' is inconsistent");
		}

		@Test
		public void recursiveAgentExtension_1() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 extends A2 {",
					"}",
					"agent A2 extends A1 {",
					"}"
					));

			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'A1' is inconsistent");
		}

		@Test
		public void recursiveAgentExtension_2() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 extends A3 {",
					"}",
					"agent A2 extends A1 {",
					"}",
					"agent A3 extends A2 {",
					"}"
					));

			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'A1' is inconsistent");
		}

		@Test
		public void sequenceAgentDefinition_invalid() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 extends A2 {",
					"}",
					"agent A2 extends A3 {",
					"}",
					"agent A3 {",
					"}"
					));

			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'A1' is inconsistent");
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.CYCLIC_INHERITANCE,
					"The inheritance hierarchy of 'A2' is inconsistent");
		}

		@Test
		public void sequenceAgentDefinition_valid() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A3 {",
					"}",
					"agent A2 extends A3 {",
					"}",
					"agent A1 extends A2 {",
					"}"
					), true);
			assertEquals(3, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A3", agent1.getName());
			assertNull(agent1.getExtends());
			assertTrue(agent1.getMembers().isEmpty());
			//
			SarlAgent agent2 = (SarlAgent) mas.getXtendTypes().get(1);
			assertEquals("A2", agent2.getName());
			assertTypeReferenceIdentifier(agent2.getExtends(), "A3");
			assertTrue(agent2.getMembers().isEmpty());
			//
			SarlAgent agent3 = (SarlAgent) mas.getXtendTypes().get(2);
			assertEquals("A1", agent3.getName());
			assertTypeReferenceIdentifier(agent3.getExtends(), "A2");
			assertTrue(agent3.getMembers().isEmpty());
		}

		@Test
		public void agentmodifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public agent A1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(JvmVisibility.PUBLIC, agent1.getVisibility());
			assertEquals(0, agent1.getMembers().size());
		}

		@Test
		public void agentmodifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(JvmVisibility.PUBLIC, agent1.getVisibility());
			assertEquals(0, agent1.getMembers().size());
		}

		@Test
		public void agentmodifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"private agent A1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(JvmVisibility.PRIVATE, agent1.getVisibility());
			assertEquals(0, agent1.getMembers().size());
		}

		@Test
		public void agentmodifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"protected agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 9,
					"Illegal modifier for the agent A1; only public, private, abstract & final are permitted");
		}

		@Test
		public void agentmodifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"package agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 7,
					"Illegal modifier for the agent A1; only public, private, abstract & final are permitted");
		}

		@Test
		public void agentmodifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract agent A1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(JvmVisibility.PUBLIC, agent1.getVisibility());
			assertTrue(agent1.isAbstract());
			assertEquals(0, agent1.getMembers().size());
		}

		@Test
		public void agentmodifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"static agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 6,
					"Illegal modifier for the agent A1; only public, private, abstract & final are permitted");
		}

		@Test
		public void agentmodifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"dispatch agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 8,
					"Illegal modifier for the agent A1; only public, private, abstract & final are permitted");
		}

		@Test
		public void agentmodifier_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"final agent A1 {}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(JvmVisibility.PUBLIC, agent1.getVisibility());
			assertTrue(agent1.isFinal());
			assertEquals(0, agent1.getMembers().size());
		}

		@Test
		public void agentmodifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"strictfp agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 8,
					"Illegal modifier for the agent A1; only public, private, abstract & final are permitted");
		}

		@Test
		public void agentmodifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"native agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 6,
					"Illegal modifier for the agent A1; only public, private, abstract & final are permitted");
		}

		@Test
		public void agentmodifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"volatile agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 8,
					"Illegal modifier for the agent A1; only public, private, abstract & final are permitted");
		}

		@Test
		public void agentmodifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"synchronized agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 12,
					"Illegal modifier for the agent A1; only public, private, abstract & final are permitted");
		}

		@Test
		public void agentmodifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"transient agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					32, 9,
					"Illegal modifier for the agent A1; only public, private, abstract & final are permitted");
		}

		@Test
		public void agentmodifier_abstract_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"abstract final agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					41, 5,
					"The agent A1 can either be abstract or final, not both");
		}

		@Test
		public void modifier_abstract_action() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	def name",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertFalse(agent1.isAbstract());
		}

		@Test
		public void agentmodifier_public_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"public private agent A1 {}"
					), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					39, 7,
					"The agent A1 can only set one of public / package / protected / private");
		}

	}

	public static class BehaviorUnitTest extends AbstractSarlTest {

		@Test
		public void itInsideEventHandlerGuards_explicitIt() throws Exception {
			SarlScript mas = file(multilineString(
					"event E {",
					"  var field : int",
					"}",
					"agent A1 {",
					"  on E [ it.field == 4 ] {",
					"  }",
					"}"
					), true);
		}

		@Test
		public void itInsideEventHandlerGuards_implicitIt() throws Exception {
			SarlScript mas = file(multilineString(
					"event E {",
					"  var field : int",
					"}",
					"agent A1 {",
					"  on E [ field == 4 ] {",
					"  }",
					"}"
					), true);
		}

		@Test
		public void synchronizedGuard() throws Exception {
			SarlScript mas = file(multilineString(
					"event E {}",
					"agent A1 {",
					"  var myfield = new Object",
					"  on E [ synchronized(this.myfield) { this.myfield.hashCode != 0 } ] {",
					"  }",
					"}"
					), false);
			validate(mas).assertError(
					XbasePackage.eINSTANCE.getXExpression(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_INNER_EXPRESSION,
					58, 57,
					"Expression with side effect is not allowed in guards");
		}

		@Test
		public void declarationWithoutGuard() throws Exception {
			SarlScript mas = file(multilineString(
					"event E {}",
					"agent A1 {",
					"on E {}",
					"}"
					), true);
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E", event.getName());
			assertNull(event.getExtends());
			assertTrue(event.getMembers().isEmpty());
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
			assertEquals("A1", agent.getName());
			assertNull(agent.getExtends());
			assertEquals(1, agent.getMembers().size());
			//
			SarlBehaviorUnit eventHandler = (SarlBehaviorUnit) agent.getMembers().get(0);
			assertTypeReferenceIdentifier(eventHandler.getName(), "E");
			assertNull(eventHandler.getGuard());
		}

		@Test
		public void declarationWithGuard() throws Exception {
			SarlScript mas = file(multilineString(
					"event E {}",
					"agent A1 {",
					"on E [ occurrence.source != null] {}",
					"}"
					), true);
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlEvent event = (SarlEvent) mas.getXtendTypes().get(0);
			assertEquals("E", event.getName());
			assertNull(event.getExtends());
			assertTrue(event.getMembers().isEmpty());
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
			assertEquals("A1", agent.getName());
			assertNull(agent.getExtends());
			assertEquals(1, agent.getMembers().size());
			//
			SarlBehaviorUnit eventHandler = (SarlBehaviorUnit) agent.getMembers().get(0);
			assertTypeReferenceIdentifier(eventHandler.getName(), "E");
			assertNotNull(eventHandler.getGuard());
		}

		@Test
		public void missedEventDeclaration() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"on E  {}",
					"}"
					));
			validate(mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.LINKING_DIAGNOSTIC,
					14, 1,
					"E cannot be resolved to a type");
		}

		@Test
		public void invalidBehaviorUnit_EventType() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"on String {}",
					"}"
					));

			validate(mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.TYPE_BOUNDS_MISMATCH,
					"Invalid type: 'java.lang.String'. Only events can be used after the keyword 'on'");
		}

		@Test
		public void invalidBehaviorUnit_GuardType() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1",
					"agent A1 {",
					"on E1 [ \"hello\" ] {}",
					"}"
					));

			validate(mas).assertError(
					XbasePackage.eINSTANCE.getXExpression(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from String to boolean");
		}

		@Test
		public void trueGuardBehaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1",
					"agent A1 {",
					"on E1 [ true ] {}",
					"}"
					));

			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXBooleanLiteral(),
					IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION,
					"Discouraged boolean value. The guard is always true.");
		}

		@Test
		public void falseGuardBehaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1",
					"agent A1 {",
					"on E1 [ false ] {}",
					"}"
					));

			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlBehaviorUnit(),
					IssueCodes.UNREACHABLE_BEHAVIOR_UNIT,
					"Dead code. The guard is always false.");
		}

		@Test
		public void invalidBehaviorUnit_SideEffect0() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1",
					"agent A1 {",
					"var t = 5",
					"on E1 [t += 5] {",
					"}"
					));

			validate(mas).assertError(
					XbasePackage.eINSTANCE.getXBinaryOperation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_INNER_EXPRESSION,
					37, 6,
					"Expression with side effect is not allowed in guards");
		}

		@Test
		public void invalidBehaviorUnit_SideEffect1() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1",
					"agent A1 {",
					"var t = 5",
					"on E1 [(t += 5) > 0] {",
					"}"
					));

			validate(mas).assertError(
					XbasePackage.eINSTANCE.getXBinaryOperation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_INNER_EXPRESSION,
					37, 12,
					"Expression with side effect is not allowed in guards");
		}

		@Test
		public void invalidBehaviorUnit_SideEffect2() throws Exception {
			SarlScript mas = file(multilineString(
					"import java.util.List",
					"event E1 { var parameters : List<Object> }",
					"agent A1 {",
					"	on E1 [ occurrence.parameters.empty ] {",
					"	}",
					"}"
					));

			validate(mas).assertNoIssues();
		}

	}

	public static class FieldTest extends AbstractSarlTest {

		@Test
		public void variableDefinition() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"var name : String = \"Hello\"",
					"var number : Integer",
					"}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent.getName());
			assertNull(agent.getExtends());
			assertEquals(2, agent.getMembers().size());
			//
			SarlField attr1 = (SarlField) agent.getMembers().get(0);
			assertEquals("name", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "java.lang.String");
			assertTrue(attr1.getInitialValue() instanceof XStringLiteral);
			assertEquals("Hello", ((XStringLiteral) attr1.getInitialValue()).getValue());
			assertFalse(attr1.isFinal());
			//
			SarlField attr2 = (SarlField) agent.getMembers().get(1);
			assertEquals("number", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.Integer");
			assertNull(attr2.getInitialValue());
			assertFalse(attr2.isFinal());
		}

		@Test
		public void valueDefinition() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"val name : String = \"Hello\"",
					"var number : Integer",
					"}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent.getName());
			assertNull(agent.getExtends());
			assertEquals(2, agent.getMembers().size());
			//
			SarlField attr1 = (SarlField) agent.getMembers().get(0);
			assertEquals("name", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "java.lang.String");
			assertXExpression(attr1.getInitialValue(), XStringLiteral.class, "Hello");
			assertTrue(attr1.isFinal());
			//
			SarlField attr2 = (SarlField) agent.getMembers().get(1);
			assertEquals("number", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.Integer");
			assertNull(attr2.getInitialValue());
			assertFalse(attr2.isFinal());
		}

		@Test
		public void multipleVariableDefinition() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"var myfield : int",
					"var myfield1 : String",
					"var myfield : double",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					55, 7,
					"Duplicate field myfield");
		}

		@Test
		public void multipleValueDefinition() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"val myfield : int = 4",
					"val myfield1 : String = \"\"",
					"val myfield : double = 5",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					64, 7,
					"Duplicate field myfield");
		}

		@Test
		public void invalidAttributeName_0() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"var myfield1 = 4.5",
					"var ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"var myfield2 = true",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
					"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void invalidAttributeName_1() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"val myfield1 = 4.5",
					"val ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"val myfield2 = true",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
					"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void variableUse() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"var name : String = \"Hello\"",
					"def myfct() { println(name) }",
					"}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent.getName());
			assertNull(agent.getExtends());
			assertEquals(2, agent.getMembers().size());
			//
			SarlField attr1 = (SarlField) agent.getMembers().get(0);
			assertEquals("name", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "java.lang.String");
			assertTrue(attr1.getInitialValue() instanceof XStringLiteral);
			assertEquals("Hello", ((XStringLiteral) attr1.getInitialValue()).getValue());
			assertFalse(attr1.isFinal());
			//
			SarlAction act = (SarlAction) agent.getMembers().get(1);
			assertEquals("myfct", act.getName());
		}

		@Test
		public void valueUse() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"val name : String = \"Hello\"",
					"def myfct() { println(name) }",
					"}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent.getName());
			assertNull(agent.getExtends());
			assertEquals(2, agent.getMembers().size());
			//
			SarlField attr1 = (SarlField) agent.getMembers().get(0);
			assertEquals("name", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "java.lang.String");
			assertXExpression(attr1.getInitialValue(), XStringLiteral.class, "Hello");
			assertTrue(attr1.isFinal());
			//
			SarlAction act = (SarlAction) agent.getMembers().get(1);
			assertEquals("myfct", act.getName());
		}

		@Test
		public void missedFinalFieldInitialization() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"val field1 : int = 5",
					"val field2 : String",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAgent(),
					org.eclipse.xtend.core.validation.IssueCodes.FIELD_NOT_INITIALIZED,
					0, 53,
					"The blank final field field2 may not have been initialized");
		}

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"val field1 : int = 5",
					"val field2 : String = \"\"",
					"}"
					), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent.getName());
			assertNull(agent.getExtends());
			assertEquals(2, agent.getMembers().size());
			//
			SarlField attr1 = (SarlField) agent.getMembers().get(0);
			assertEquals("field1", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertXExpression(attr1.getInitialValue(), XNumberLiteral.class, "5");
			assertTrue(attr1.isFinal());
			//
			SarlField attr2 = (SarlField) agent.getMembers().get(1);
			assertEquals("field2", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.String");
			assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "");
			assertTrue(attr2.isFinal());
		}

		@Test
		public void fieldNameShadowing() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"	val field1 : int = 5",
					"	def myaction(a : int) { }",
					"}",
					"agent A2 extends A1 {",
					"	val field1 : int = 5",
					"	def myaction(a : int) { }",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
					"The field 'field1' in 'A2' is hidding the inherited field 'A1.field1'.");
		}

		@Test
		public void modifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	public var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 6,
					"Illegal modifier for the definition of field in A1; only package, protected, private, final, val & var are permitted");
		}

		@Test
		public void modifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	private var field : int",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlField attr1 = (SarlField) agent1.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertFalse(attr1.isFinal());
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
		}

		@Test
		public void modifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	protected var field : int",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlField attr1 = (SarlField) agent1.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertFalse(attr1.isFinal());
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

		@Test
		public void modifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	package var field : int",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlField attr1 = (SarlField) agent1.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertFalse(attr1.isFinal());
			assertEquals(JvmVisibility.DEFAULT, attr1.getVisibility());
		}

		@Test
		public void modifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	var field : int",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlField attr1 = (SarlField) agent1.getMembers().get(0);
			assertEquals("field", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertNull(attr1.getInitialValue());
			assertFalse(attr1.isFinal());
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

		@Test
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	abstract var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 8,
					"Illegal modifier for the definition of field in A1; only package, protected, private, final, val & var are permitted");
		}

		@Test
		public void modifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	static var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 6,
					"Illegal modifier for the definition of field in A1; only package, protected, private, final, val & var are permitted");
		}

		@Test
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	dispatch var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 8,
					"Illegal modifier for the definition of field in A1; only package, protected, private, final, val & var are permitted");
		}

		@Test
		public void modifier_final_var() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	final var field : int = 5",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					50, 3,
					"The definition of field in A1 can either be var or val / final, not both");
		}

		@Test
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	strictfp var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 8,
					"Illegal modifier for the definition of field in A1; only package, protected, private, final, val & var are permitted");
		}

		@Test
		public void modifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	native var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 6,
					"Illegal modifier for the definition of field in A1; only package, protected, private, final, val & var are permitted");
		}

		@Test
		public void modifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	volatile var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 8,
					"Illegal modifier for the definition of field in A1; only package, protected, private, final, val & var are permitted");
		}

		@Test
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	synchronized var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 12,
					"Illegal modifier for the definition of field in A1; only package, protected, private, final, val & var are permitted");
		}

		@Test
		public void modifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	transient var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 9,
					"Illegal modifier for the definition of field in A1; only package, protected, private, final, val & var are permitted");
		}

		@Test
		public void modifier_protected_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	protected private var field : int",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					54, 7,
					"The definition of field in A1 can only set one of public / package / protected / private");
		}

	}

	public static class CapacityUseTest extends AbstractSarlTest {

		@Test
		public void declarationInAgent() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity MyCap {",
					"def my_operation",
					"}",
					"agent A1 {",
					"uses MyCap",
					"}"
					), true);
			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlCapacity capacity = (SarlCapacity) mas.getXtendTypes().get(0);
			assertEquals("MyCap", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getExtends());
			assertEquals(1, capacity.getMembers().size());
			//
			SarlAction signature = (SarlAction) capacity.getMembers().get(0);
			assertEquals("my_operation", signature.getName());
			assertTypeReferenceIdentifiers(signature.getFiredEvents());
			assertParameterNames(signature.getParameters());
			assertTypeReferenceIdentifier(signature.getReturnType(), "void");
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(1);
			assertEquals("A1", agent.getName());
			assertTypeReferenceIdentifiers(capacity.getExtends());
			assertEquals(1, agent.getMembers().size());
			//
			SarlCapacityUses uses = (SarlCapacityUses) agent.getMembers().get(0);
			assertTypeReferenceIdentifiers(uses.getCapacities(), "MyCap");
		}

		@Test
		public void missedCapacityDeclaration() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"uses MyCap",
					"}"
					));
			validate(mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.LINKING_DIAGNOSTIC,
					16, 5,
					"MyCap cannot be resolved to a type");
		}

		@Test
		public void multipleCapacityUses_0() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"agent A1 {",
					"uses C1, C2, C1",
					"def testFct { }",
					"}"
					));

			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.REDUNDANT_CAPACITY_USE,
					"Redundant use of the capacity 'C1'");
		}

		@Test
		public void multipleCapacityUses_1() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"agent A1 {",
					"uses C2",
					"def testFct { }",
					"uses C2, C1",
					"}"
					));

			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlCapacityUses(),
					IssueCodes.REDUNDANT_CAPACITY_USE,
					"Redundant use of the capacity 'C2'");
		}

	}

	public static class ActionTest extends AbstractSarlTest {

		@Test
		public void multipleActionDefinitionInAgent() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"def myaction(a : int, b : int) { }",
					"def myaction(a : int) { }",
					"def myaction(a : int) { }",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_METHOD,
					50, 8,
					"Duplicate method myaction(int) in type A1");
		}

		@Test
		public void invalidActionName() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"def myaction {",
					"System.out.println(\"ok\")",
					"}",
					"def _handle_myaction {",
					"System.out.println(\"ko\")",
					"}",
					"def myaction2 {",
					"System.out.println(\"ok\")",
					"}",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME,
					"Invalid action name '_handle_myaction'.");
		}

		@Test
		public void incompatibleReturnType_0() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"def myaction(a : int) : int {",
					"return 0",
					"}",
					"}",
					"agent A2 extends A1 {",
					"def myaction(a : int) : float {",
					"return 0f",
					"}",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					100, 5,
					"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_1() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"def myaction(a : int) {",
					"// void",
					"}",
					"}",
					"agent A2 extends A1 {",
					"def myaction(a : int) : int {",
					"return 0",
					"}",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					93, 3,
					"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_2() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"def myaction(a : int) : int {",
					"return 0",
					"}",
					"}",
					"agent A2 extends A1 {",
					"def myaction(a : int) : void {",
					"// void ",
					"}",
					"}"
					));
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					100, 4,
					"The return type is incompatible with myaction(int)");
		}

		@Test
		public void incompatibleReturnType_3() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"def myaction(a : int) : int {",
					"return 0",
					"}",
					"}",
					"agent A2 extends A1 {",
					"def myaction(a : int) {",
					"// int return type is inferred",
					"}",
					"}"
					));
			validate(mas).assertError(
					XbasePackage.eINSTANCE.getXBlockExpression(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
					98, 34,
					"Type mismatch: cannot convert from null to int");
		}

		@Test
		public void compatibleReturnType_0() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"def myaction(a : int) : Number {",
					"return 0.0",
					"}",
					"}",
					"agent A2 extends A1 {",
					"def myaction(a : int) : Double {",
					"return 0.0",
					"}",
					"}"
					), true);

			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction action1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertParameterNames(action1.getParameters(), "a");
			assertParameterTypes(action1.getParameters(), "int");
			assertTypeReferenceIdentifier(action1.getReturnType(), "java.lang.Number");
			//
			SarlAgent agent2 = (SarlAgent) mas.getXtendTypes().get(1);
			assertEquals("A2", agent2.getName());
			assertTypeReferenceIdentifier(agent2.getExtends(), "A1");
			assertEquals(1, agent2.getMembers().size());
			//
			SarlAction action2 = (SarlAction) agent2.getMembers().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertParameterNames(action2.getParameters(), "a");
			assertParameterTypes(action2.getParameters(), "int");
			assertTypeReferenceIdentifier(action2.getReturnType(), "java.lang.Double");
		}

		@Test
		public void compatibleReturnType_1() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"def myaction(a : int) : float {",
					"return 0f",
					"}",
					"}",
					"agent A2 extends A1 {",
					"def myaction(a : int) : float {",
					"return 0f",
					"}",
					"}"
					), true);

			assertEquals(2, mas.getXtendTypes().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction action1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertParameterNames(action1.getParameters(), "a");
			assertParameterTypes(action1.getParameters(), "int");
			assertTypeReferenceIdentifier(action1.getReturnType(), "float");
			//
			SarlAgent agent2 = (SarlAgent) mas.getXtendTypes().get(1);
			assertEquals("A2", agent2.getName());
			assertTypeReferenceIdentifier(agent2.getExtends(), "A1");
			assertEquals(1, agent2.getMembers().size());
			//
			SarlAction action2 = (SarlAction) agent2.getMembers().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertParameterNames(action1.getParameters(), "a");
			assertParameterTypes(action1.getParameters(), "int");
			assertTypeReferenceIdentifier(action1.getReturnType(), "float");
		}

		@Test
		public void invalidFires() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1",
					"behavior B1 { }",
					"agent A1 {",
					"def myaction1 fires E1, B1 { }",
					"}"
					));
			validate(mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_FIRING_EVENT_TYPE,
					"Invalid type: 'B1'. Only events can be used after the keyword 'fires'");
		}

		@Test
		public void modifier_public() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	public def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 6,
					"Illegal modifier for the definition of name in A1; only package, protected, private, static, dispatch, final, def, override & synchronized are permitted");
		}

		@Test
		public void modifier_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	private def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PRIVATE, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
		}

		@Test
		public void modifier_protected() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	protected def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PROTECTED, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
		}

		@Test
		public void modifier_package() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	package def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.DEFAULT, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
		}

		@Test
		public void modifier_none() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PROTECTED, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
		}

		@Test
		public void modifier_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	abstract def name",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 8,
					"Illegal modifier for the definition of name in A1; only package, protected, private, static, dispatch, final, def, override & synchronized are permitted");
		}

		@Test
		public void modifier_no_abstract() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	def name",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PROTECTED, act1.getVisibility());
			assertTrue(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
		}

		@Test
		public void modifier_static() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	static def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PROTECTED, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertTrue(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
		}

		@Test
		public void modifier_dispatch() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	dispatch def name(i : Integer) { }",
					"}"), false);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PROTECTED, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertTrue(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertFalse(act1.isSynchonized());
		}

		@Test
		public void modifier_final_var() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	final def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PROTECTED, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertTrue(act1.isFinal());
			assertFalse(act1.isSynchonized());
		}

		@Test
		public void modifier_strictfp() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	strictfp def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 8,
					"Illegal modifier for the definition of name in A1; only package, protected, private, static, dispatch, final, def, override & synchronized are permitted");
		}

		@Test
		public void modifier_native() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	native def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 6,
					"Illegal modifier for the definition of name in A1; only package, protected, private, static, dispatch, final, def, override & synchronized are permitted");
		}

		@Test
		public void modifier_volatile() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	volatile def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 8,
					"Illegal modifier for the definition of name in A1; only package, protected, private, static, dispatch, final, def, override & synchronized are permitted");
		}

		@Test
		public void modifier_synchronized() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	synchronized def name { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PROTECTED, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertFalse(act1.isDispatch());
			assertFalse(act1.isFinal());
			assertTrue(act1.isSynchonized());
		}

		@Test
		public void modifier_transient() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	transient def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					44, 9,
					"Illegal modifier for the definition of name in A1; only package, protected, private, static, dispatch, final, def, override & synchronized are permitted");
		}

		@Test
		public void modifier_protected_private() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	protected private def name { }",
					"}"), false);
			validate(mas).assertError(
					SarlPackage.eINSTANCE.getSarlAction(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					54, 7,
					"The definition of name in A1 can only set one of public / package / protected / private");
		}

		@Test
		public void modifier_dispatch_final() throws Exception {
			SarlScript mas = file(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {",
					"	dispatch final def name(a : Integer) { }",
					"}"), true);
			assertEquals(1, mas.getXtendTypes().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getPackage());
			//
			SarlAgent agent1 = (SarlAgent) mas.getXtendTypes().get(0);
			assertEquals("A1", agent1.getName());
			assertNull(agent1.getExtends());
			assertEquals(1, agent1.getMembers().size());
			//
			SarlAction act1 = (SarlAction) agent1.getMembers().get(0);
			assertEquals("name", act1.getName());
			assertEquals(JvmVisibility.PROTECTED, act1.getVisibility());
			assertFalse(act1.isAbstract());
			assertFalse(act1.isStatic());
			assertTrue(act1.isDispatch());
			assertTrue(act1.isFinal());
			assertFalse(act1.isSynchonized());
		}

	}

}
