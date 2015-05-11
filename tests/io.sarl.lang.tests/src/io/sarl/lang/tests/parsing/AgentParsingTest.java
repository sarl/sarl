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
package io.sarl.lang.tests.parsing;

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
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendPackage;
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
	AgentParsingTest.AttributeTest.class,
	AgentParsingTest.CapacityUseTest.class,
	AgentParsingTest.ActionTest.class,
})
@SuppressWarnings("all")
public class AgentParsingTest {

	public static class TopElementTest extends AbstractSarlTest {

		@Test
		public void declaration() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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

	}

	public static class BehaviorUnitTest extends AbstractSarlTest {

		@Test
		public void declarationWithoutGuard() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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

	}

	public static class AttributeTest extends AbstractSarlTest {

		@Test
		public void variableDefinition() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendField attr1 = (XtendField) agent.getMembers().get(0);
			assertEquals("name", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "java.lang.String");
			assertTrue(attr1.getInitialValue() instanceof XStringLiteral);
			assertEquals("Hello", ((XStringLiteral) attr1.getInitialValue()).getValue());
			assertFalse(attr1.isFinal());
			//
			XtendField attr2 = (XtendField) agent.getMembers().get(1);
			assertEquals("number", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.Integer");
			assertNull(attr2.getInitialValue());
			assertFalse(attr2.isFinal());
		}

		@Test
		public void valueDefinition() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendField attr1 = (XtendField) agent.getMembers().get(0);
			assertEquals("name", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "java.lang.String");
			assertXExpression(attr1.getInitialValue(), XStringLiteral.class, "Hello");
			assertTrue(attr1.isFinal());
			//
			XtendField attr2 = (XtendField) agent.getMembers().get(1);
			assertEquals("number", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.Integer");
			assertNull(attr2.getInitialValue());
			assertFalse(attr2.isFinal());
		}

		@Test
		public void multipleVariableDefinition() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"var myfield : int",
					"var myfield1 : String",
					"var myfield : double",
					"}"
					));
			validate(mas).assertError(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					55, 7,
					"Duplicate field myfield");
		}

		@Test
		public void multipleValueDefinition() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"val myfield : int = 4",
					"val myfield1 : String = \"\"",
					"val myfield : double = 5",
					"}"
					));
			validate(mas).assertError(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_FIELD,
					64, 7,
					"Duplicate field myfield");
		}

		@Test
		public void invalidAttributeName_0() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"var myfield1 = 4.5",
					"var ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"var myfield2 = true",
					"}"
					));
			validate(mas).assertError(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
					"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void invalidAttributeName_1() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"val myfield1 = 4.5",
					"val ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"val myfield2 = true",
					"}"
					));
			validate(mas).assertError(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED,
					"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void missedFinalFieldInitialization() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendField attr1 = (XtendField) agent.getMembers().get(0);
			assertEquals("field1", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertXExpression(attr1.getInitialValue(), XNumberLiteral.class, "5");
			assertTrue(attr1.isFinal());
			//
			XtendField attr2 = (XtendField) agent.getMembers().get(1);
			assertEquals("field2", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.String");
			assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "");
			assertTrue(attr2.isFinal());
		}

		@Test
		public void fieldNameShadowing() throws Exception {
			XtendFile mas = file(multilineString(
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
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
					"The field 'field1' in 'A2' is hidding the inherited field 'A1.field1'.");
		}

	}

	public static class CapacityUseTest extends AbstractSarlTest {

		@Test
		public void declarationInAgent() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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

		@Test
		public void variableModifier_public() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"public var name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertError(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of A1; only protected, private, static, final, val, var, volatile & transient are permitted");
		}

		@Test
		public void variableModifier_protected() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"protected var name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) agent.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

		@Test
		public void variableModifier_package() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"package var name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertError(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of A1; only protected, private, static, final, val, var, volatile & transient are permitted");
		}

		@Test
		public void variableModifier_private() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"private var name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) agent.getMembers().get(0);
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
		}

		@Test
		public void variableModifier_default() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"var name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) agent.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

		@Test
		public void valueModifier_public() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"public val name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertError(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of A1; only protected, private, static, final, val, var, volatile & transient are permitted");
		}

		@Test
		public void valueModifier_protected() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"protected val name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) agent.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

		@Test
		public void valueModifier_package() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"package val name : String = \"Hello\"",
					"}"
					), false);
			//
			validate(mas).assertError(
					XtendPackage.eINSTANCE.getXtendField(),
					org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER,
					"Illegal modifier for the definition of A1; only protected, private, static, final, val, var, volatile & transient are permitted");
		}

		@Test
		public void valueModifier_private() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"private val name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) agent.getMembers().get(0);
			assertEquals(JvmVisibility.PRIVATE, attr1.getVisibility());
		}

		@Test
		public void valueModifier_default() throws Exception {
			XtendFile mas = file(multilineString(
					"agent A1 {",
					"val name : String = \"Hello\"",
					"}"
					), true);
			//
			SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
			XtendField attr1 = (XtendField) agent.getMembers().get(0);
			assertEquals(JvmVisibility.PROTECTED, attr1.getVisibility());
		}

	}

	public static class ActionTest extends AbstractSarlTest {

		@Test
		public void multipleActionDefinitionInAgent() throws Exception {
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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
			XtendFile mas = file(multilineString(
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

	}

}
