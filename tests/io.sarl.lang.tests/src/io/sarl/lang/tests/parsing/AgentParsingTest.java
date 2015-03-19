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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.CapacityUses;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.TestClasspath;
import io.sarl.tests.api.TestScope;

import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.Assume;
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
	AgentParsingTest.TopElementUiBaseTest.class,
	AgentParsingTest.BehaviorUnitTest.class,
	AgentParsingTest.AttributeTest.class,
	AgentParsingTest.CapacityUseTest.class,
	AgentParsingTest.ActionTest.class,
})
@SuppressWarnings("all")
public class AgentParsingTest {

	public static class TopElementTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject 
		private ValidationTestHelper validator;

		@Test
		public void declaration() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"package io.sarl.lang.tests.test",
					"agent A1 {}",
					"agent A2 {}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertEquals("io.sarl.lang.tests.test", mas.getName());
			//
			Agent agent1 = (Agent) mas.getElements().get(0);
			assertEquals("A1", agent1.getName());
			assertTypeReferenceIdentifiers(agent1.getSuperTypes());
			assertEquals(0, agent1.getFeatures().size());
			//
			Agent agent2 = (Agent) mas.getElements().get(1);
			assertEquals("A2", agent2.getName());
			assertTypeReferenceIdentifiers(agent2.getSuperTypes());
			assertEquals(0, agent2.getFeatures().size());
		}

		@Test
		public void invalidExtend() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"}",
					"agent A1 extends C1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAgent(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Invalid supertype. Expecting: class");
		}

		@Test
		public void recursiveAgentExtension_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 extends A1 {",
					"}"
					));

			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAgent(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'A1' is inconsistent");
		}

		@Test
		public void recursiveAgentExtension_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 extends A2 {",
					"}",
					"agent A2 extends A1 {",
					"}"
					));

			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAgent(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'A1' is inconsistent");
		}

		@Test
		public void recursiveAgentExtension_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 extends A3 {",
					"}",
					"agent A2 extends A1 {",
					"}",
					"agent A3 extends A2 {",
					"}"
					));

			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAgent(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'A1' is inconsistent");
		}

		@Test
		public void sequenceAgentDefinition_invalid() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 extends A2 {",
					"}",
					"agent A2 extends A3 {",
					"}",
					"agent A3 {",
					"}"
					));

			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAgent(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'A1' is inconsistent");
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAgent(),
					IssueCodes.INCONSISTENT_TYPE_HIERARCHY,
					"The inheritance hierarchy of 'A2' is inconsistent");
		}

		@Test
		public void sequenceAgentDefinition_valid() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A3 {",
					"}",
					"agent A2 extends A3 {",
					"}",
					"agent A1 extends A2 {",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(3, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Agent agent1 = (Agent) mas.getElements().get(0);
			assertEquals("A3", agent1.getName());
			assertTypeReferenceIdentifiers(agent1.getSuperTypes());
			assertTrue(agent1.getFeatures().isEmpty());
			//
			Agent agent2 = (Agent) mas.getElements().get(1);
			assertEquals("A2", agent2.getName());
			assertTypeReferenceIdentifiers(agent2.getSuperTypes(), "A3");
			assertTrue(agent2.getFeatures().isEmpty());
			//
			Agent agent3 = (Agent) mas.getElements().get(2);
			assertEquals("A1", agent3.getName());
			assertTypeReferenceIdentifiers(agent3.getSuperTypes(), "A2");
			assertTrue(agent3.getFeatures().isEmpty());
		}

	}

	public static class TopElementUiBaseTest extends AbstractSarlUiTest {

		@Inject 
		private ValidationTestHelper _validator;

		@Test
		@TestClasspath("io.sarl.tests.testdata")
		@TestScope(tycho=false)
		public void invalidExtend() throws Exception {
			SarlScript script = parseWithProjectClasspath(
					"import foo.MockFinalAgent",
					"agent InvalidAgentDeclaration extends MockFinalAgent {",
					"}");
			this.helper.getValidator().assertError(script,
					SarlPackage.eINSTANCE.getAgent(),
					IssueCodes.OVERRIDDEN_FINAL_TYPE,
					"Attempt to override final class");
		}

	}

	public static class BehaviorUnitTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject 
		private ValidationTestHelper validator;

		@Test
		public void declarationWithoutGuard() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E {}",
					"agent A1 {",
					"on E {}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Event event = (Event) mas.getElements().get(0);
			assertEquals("E", event.getName());
			assertTypeReferenceIdentifiers(event.getSuperTypes());
			assertTrue(event.getFeatures().isEmpty());
			//
			Agent agent = (Agent) mas.getElements().get(1);
			assertEquals("A1", agent.getName());
			assertTypeReferenceIdentifiers(agent.getSuperTypes());
			assertEquals(1, agent.getFeatures().size());
			//
			BehaviorUnit eventHandler = (BehaviorUnit) agent.getFeatures().get(0);
			assertTypeReferenceIdentifier(eventHandler.getName(), "E");
			assertNull(eventHandler.getGuard());
		}

		@Test
		public void declarationWithGuard() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E {}",
					"agent A1 {",
					"on E [ occurrence.source != null] {}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Event event = (Event) mas.getElements().get(0);
			assertEquals("E", event.getName());
			assertTypeReferenceIdentifiers(event.getSuperTypes());
			assertTrue(event.getFeatures().isEmpty());
			//
			Agent agent = (Agent) mas.getElements().get(1);
			assertEquals("A1", agent.getName());
			assertTypeReferenceIdentifiers(agent.getSuperTypes());
			assertEquals(1, agent.getFeatures().size());
			//
			BehaviorUnit eventHandler = (BehaviorUnit) agent.getFeatures().get(0);
			assertTypeReferenceIdentifier(eventHandler.getName(), "E");
			assertNotNull(eventHandler.getGuard());
		}

		@Test
		public void missedEventDeclaration() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"on E  {}",
					"}"
					));
			this.validator.assertError(mas, 
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.LINKING_DIAGNOSTIC,
					"Couldn't resolve reference to JvmType 'E'.");
		}

		@Test
		public void invalidBehaviorUnit_EventType() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"on String {}",
					"}"
					));

			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.TYPE_BOUNDS_MISMATCH,
					"Invalid type: 'java.lang.String'. Only events can be used after the keyword 'on'");
		}

		@Test
		public void invalidBehaviorUnit_GuardType() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1",
					"agent A1 {",
					"on E1 [ \"hello\" ] {}",
					"}"
					));

			this.validator.assertError(mas,
					XbasePackage.eINSTANCE.getXExpression(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from String to boolean");
		}

		@Test
		public void trueGuardBehaviorUnit() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1",
					"agent A1 {",
					"on E1 [ true ] {}",
					"}"
					));

			this.validator.assertWarning(mas,
					XbasePackage.eINSTANCE.getXBooleanLiteral(),
					IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION,
					"Discouraged boolean value. The guard is always true.");
		}

		@Test
		public void falseGuardBehaviorUnit() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1",
					"agent A1 {",
					"on E1 [ false ] {}",
					"}"
					));

			this.validator.assertWarning(mas,
					SarlPackage.eINSTANCE.getBehaviorUnit(),
					IssueCodes.UNREACHABLE_BEHAVIOR_UNIT,
					"Dead code. The guard is always false.");
		}

	}

	public static class AttributeTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject 
		private ValidationTestHelper validator;

		@Test
		public void variableDefinition() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"var name : String = \"Hello\"",
					"var number : Integer",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Agent agent = (Agent) mas.getElements().get(0);
			assertEquals("A1", agent.getName());
			assertTypeReferenceIdentifiers(agent.getSuperTypes());
			assertEquals(2, agent.getFeatures().size());
			//
			Attribute attr1 = (Attribute) agent.getFeatures().get(0);
			assertEquals("name", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "java.lang.String");
			assertTrue(attr1.getInitialValue() instanceof XStringLiteral);
			assertEquals("Hello", ((XStringLiteral) attr1.getInitialValue()).getValue());
			assertTrue(attr1.isWriteable());
			//
			Attribute attr2 = (Attribute) agent.getFeatures().get(1);
			assertEquals("number", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.Integer");
			assertNull(attr2.getInitialValue());
			assertTrue(attr2.isWriteable());
		}

		@Test
		public void valueDefinition() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"val name : String = \"Hello\"",
					"var number : Integer",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Agent agent = (Agent) mas.getElements().get(0);
			assertEquals("A1", agent.getName());
			assertTypeReferenceIdentifiers(agent.getSuperTypes());
			assertEquals(2, agent.getFeatures().size());
			//
			Attribute attr1 = (Attribute) agent.getFeatures().get(0);
			assertEquals("name", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "java.lang.String");
			assertXExpression(attr1.getInitialValue(), XStringLiteral.class, "Hello");
			assertFalse(attr1.isWriteable());
			//
			Attribute attr2 = (Attribute) agent.getFeatures().get(1);
			assertEquals("number", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.Integer");
			assertNull(attr2.getInitialValue());
			assertTrue(attr2.isWriteable());
		}

		@Test
		public void multipleVariableDefinition() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"var myfield : int",
					"var myfield1 : String",
					"var myfield : double",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					IssueCodes.DUPLICATE_FIELD,
					"Duplicate field in 'A1': myfield");
		}

		@Test
		public void multipleValueDefinition() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"val myfield : int = 4",
					"val myfield1 : String = \"\"",
					"val myfield : double = 5",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					IssueCodes.DUPLICATE_FIELD,
					"Duplicate field in 'A1': myfield");
		}

		@Test
		public void invalidAttributeName_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"var myfield1 = 4.5",
					"var ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"var myfield2 = true",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					IssueCodes.INVALID_MEMBER_NAME,
					"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void invalidAttributeName_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"val myfield1 = 4.5",
					"val ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"val myfield2 = true",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					IssueCodes.INVALID_MEMBER_NAME,
					"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void missedFinalFieldInitialization() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"val field1 : int = 5",
					"val field2 : String",
					"}"
					));
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmConstructor(),
					org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_INITIALIZATION,
					"The blank final field 'field2' may not have been initialized");
		}

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"val field1 : int = 5",
					"val field2 : String = \"\"",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Agent agent = (Agent) mas.getElements().get(0);
			assertEquals("A1", agent.getName());
			assertTypeReferenceIdentifiers(agent.getSuperTypes());
			assertEquals(2, agent.getFeatures().size());
			//
			Attribute attr1 = (Attribute) agent.getFeatures().get(0);
			assertEquals("field1", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertXExpression(attr1.getInitialValue(), XNumberLiteral.class, "5");
			assertFalse(attr1.isWriteable());
			//
			Attribute attr2 = (Attribute) agent.getFeatures().get(1);
			assertEquals("field2", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.String");
			assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "");
			assertFalse(attr2.isWriteable());
		}

		@Test
		public void fieldNameShadowing() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"	val field1 : int = 5",
					"	def myaction(a : int) { }",
					"}",
					"agent A2 extends A1 {",
					"	val field1 : int = 5",
					"	def myaction(a : int) { }",
					"}"
					));
			this.validator.assertWarning(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
					"The field 'field1' in 'A2' is hidding the inherited field 'A1.field1'.");
		}

	}

	public static class CapacityUseTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject 
		private ValidationTestHelper validator;

		@Test
		public void declarationInAgent() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity MyCap {",
					"def my_operation",
					"}",
					"agent A1 {",
					"uses MyCap",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Capacity capacity = (Capacity) mas.getElements().get(0);
			assertEquals("MyCap", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getSuperTypes());
			assertEquals(1, capacity.getFeatures().size());
			//
			ActionSignature signature = (ActionSignature) capacity.getFeatures().get(0);
			assertEquals("my_operation", signature.getName());
			assertTypeReferenceIdentifiers(signature.getFiredEvents());
			assertParameterNames(signature.getParams());
			assertTypeReferenceIdentifier(signature.getType(), "void");
			//
			Agent agent = (Agent) mas.getElements().get(1);
			assertEquals("A1", agent.getName());
			assertTypeReferenceIdentifiers(capacity.getSuperTypes());
			assertEquals(1, agent.getFeatures().size());
			//
			CapacityUses uses = (CapacityUses) agent.getFeatures().get(0);
			assertTypeReferenceIdentifiers(uses.getCapacitiesUsed(), "MyCap");
		}

		@Test
		public void missedCapacityDeclaration() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"uses MyCap",
					"}"
					));		
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.LINKING_DIAGNOSTIC,
					"Couldn't resolve reference to JvmType 'MyCap'");
		}

		@Test
		public void multipleCapacityUses_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"agent A1 {",
					"uses C1, C2, C1",
					"def testFct { }",
					"}"
					));

			this.validator.assertWarning(mas,
					SarlPackage.eINSTANCE.getCapacityUses(),
					IssueCodes.REDUNDANT_CAPACITY_USE,
					"Redundant use of the capacity 'C1'");
		}

		@Test
		public void multipleCapacityUses_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {}",
					"capacity C2 {}",
					"agent A1 {",
					"uses C2",
					"def testFct { }",
					"uses C2, C1",
					"}"
					));

			this.validator.assertWarning(mas,
					SarlPackage.eINSTANCE.getCapacityUses(),
					IssueCodes.REDUNDANT_CAPACITY_USE,
					"Redundant use of the capacity 'C2'");
		}

	}

	public static class ActionTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject 
		private ValidationTestHelper validator;

		@Test
		public void multipleActionDefinitionInAgent() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(a : int, b : int) { }",
					"def myaction(a : int) { }",
					"def myaction(a : int) { }",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					IssueCodes.DUPLICATE_METHOD,
					"Duplicate action in 'A1': myaction(a : int)");
		}

		@Test
		public void invalidActionName() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					IssueCodes.INVALID_MEMBER_NAME,
					"Invalid action name '_handle_myaction'.");
		}

		@Test
		public void incompatibleReturnType_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					"Incompatible return type between 'float' and 'int' for myaction(int).");
		}

		@Test
		public void incompatibleReturnType_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					"Incompatible return type between 'int' and 'void' for myaction(int).");
		}

		@Test
		public void incompatibleReturnType_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"def myaction(a : int) : int {",
					"return 0",
					"}",
					"}",
					"agent A2 extends A1 {",
					"def myaction(a : int) {",
					"// void",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAction(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE,
					"Incompatible return type between 'void' and 'int' for myaction(int).");
		}

		@Test
		public void compatibleReturnType_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Agent agent1 = (Agent) mas.getElements().get(0);
			assertEquals("A1", agent1.getName());
			assertTypeReferenceIdentifiers(agent1.getSuperTypes());
			assertEquals(1, agent1.getFeatures().size());
			//
			Action action1 = (Action) agent1.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertParameterNames(action1.getParams(), "a");
			assertParameterTypes(action1.getParams(), "int");
			assertTypeReferenceIdentifier(action1.getType(), "java.lang.Number");
			//
			Agent agent2 = (Agent) mas.getElements().get(1);
			assertEquals("A2", agent2.getName());
			assertTypeReferenceIdentifiers(agent2.getSuperTypes(), "A1");
			assertEquals(1, agent2.getFeatures().size());
			//
			Action action2 = (Action) agent2.getFeatures().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertParameterNames(action2.getParams(), "a");
			assertParameterTypes(action2.getParams(), "int");
			assertTypeReferenceIdentifier(action2.getType(), "java.lang.Double");
		}

		@Test
		public void compatibleReturnType_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
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
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Agent agent1 = (Agent) mas.getElements().get(0);
			assertEquals("A1", agent1.getName());
			assertTypeReferenceIdentifiers(agent1.getSuperTypes());
			assertEquals(1, agent1.getFeatures().size());
			//
			Action action1 = (Action) agent1.getFeatures().get(0);
			assertEquals("myaction", action1.getName());
			assertTypeReferenceIdentifiers(action1.getFiredEvents());
			assertParameterNames(action1.getParams(), "a");
			assertParameterTypes(action1.getParams(), "int");
			assertTypeReferenceIdentifier(action1.getType(), "float");
			//
			Agent agent2 = (Agent) mas.getElements().get(1);
			assertEquals("A2", agent2.getName());
			assertTypeReferenceIdentifiers(agent2.getSuperTypes(), "A1");
			assertEquals(1, agent2.getFeatures().size());
			//
			Action action2 = (Action) agent2.getFeatures().get(0);
			assertEquals("myaction", action2.getName());
			assertTypeReferenceIdentifiers(action2.getFiredEvents());
			assertParameterNames(action1.getParams(), "a");
			assertParameterTypes(action1.getParams(), "int");
			assertTypeReferenceIdentifier(action1.getType(), "float");
		}

		@Test
		public void invalidFires() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1",
					"behavior B1 { }",
					"agent A1 {",
					"def myaction1 fires E1, B1 { }",
					"}"
					));
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_FIRING_EVENT_TYPE,
					"Invalid type: 'B1'. Only events can be used after the keyword 'fires'");
		}

	}

}
