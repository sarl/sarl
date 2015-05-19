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
package io.sarl.lang.tests.generator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.actionprototype.ActionPrototypeProvider;
import io.sarl.lang.generator.helper.BlockInnerDocumentationAdapter;
import io.sarl.lang.generator.helper.ECoreGeneratorHelper;
import io.sarl.lang.generator.helper.PostDocumentationAdapter;
import io.sarl.lang.generator.helper.SarlEcoreCode;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.compiler.DocumentationAdapter;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.google.inject.Inject;

/** This class tests the functions of {@link ECoreGeneratorHelper}.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	EcoreGeneratorHelperTest.InjectedAttributes.class,
	EcoreGeneratorHelperTest.DefaultTypeValue.class,
	EcoreGeneratorHelperTest.References.class,
	EcoreGeneratorHelperTest.Comments.class,
	EcoreGeneratorHelperTest.ScriptLevel.class,
	EcoreGeneratorHelperTest.AgentTopElement.class,
	EcoreGeneratorHelperTest.BehaviorTopElement.class,
	EcoreGeneratorHelperTest.CapacityTopElement.class,
	EcoreGeneratorHelperTest.EventTopElement.class,
	EcoreGeneratorHelperTest.SkillTopElement.class,
	EcoreGeneratorHelperTest.AgentFeatures.class,
	EcoreGeneratorHelperTest.BehaviorFeatures.class,
	EcoreGeneratorHelperTest.CapacityFeatures.class,
	EcoreGeneratorHelperTest.EventFeatures.class,
	EcoreGeneratorHelperTest.SkillFeatures.class,
	EcoreGeneratorHelperTest.Expressions.class,
	EcoreGeneratorHelperTest.FormalParameters.class,
	EcoreGeneratorHelperTest.CreateActionFromJvmElement.class,
	EcoreGeneratorHelperTest.CreateConstructorFromJvmElement.class,
	EcoreGeneratorHelperTest.CreateActionSignatureFromJvmElement.class
})
@SuppressWarnings("all")
public class EcoreGeneratorHelperTest {

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static abstract class AbstractCodeGeneratorTest extends AbstractSarlTest {
		
		@Inject
		protected ISerializer serializer;

		@Inject
		protected ECoreGeneratorHelper generator;

		@Nullable
		protected SarlEcoreCode code;

		@Before
		public void setUp() {
			XtextResource resource = new XtextResource();
			XtextResourceSet resourceSet = new XtextResourceSet();
			resourceSet.getResources().add(resource);
			resourceSet.setClasspathURIContext(getClass());
			this.code = generator.createScript(resource, "io.sarl.lang.tests");
		}

		protected void assertSerialize(String expected) {
			String text = serializer.serialize(this.code.getSarlScript());
			assertEquals(expected, text);
		}

		/** Check if the import manager contains the given elements.
		 *
		 * @param importManager - the import manager.
		 * @param importDeclarations - the expected import declarations.
		 */
		protected static void assertImports(ImportManager importManager, String... importDeclarations) {
			List<String> imports = importManager.getImports();
			assertEquals("Invalid number of import declarations", importDeclarations.length, imports.size());
			for (String declaration : importDeclarations) {
				assertTrue("Expecting import declaration: " + declaration,
						imports.contains(declaration));
			}
		}

	}

	public static class InjectedAttributes extends AbstractCodeGeneratorTest {

		@Inject
		private TypeReferences typeReferences;

		@Inject
		private TypesFactory typesFactory;

		@Inject
		private IResourceFactory resourceFactory;

		@Inject
		ActionPrototypeProvider actionSignatureProvider;

		@Test
		public void getSARLFileExtension() {
			assertEquals("sarl", generator.getSARLFileExtension());
		}

		@Test
		public void getTypeReferences() {
			assertSame(this.typeReferences, generator.getTypeReferences());
		}

		@Test
		public void getTypesFactory() {
			assertSame(this.typesFactory, generator.getTypesFactory());
		}

		@Test
		public void getResourceFactory() {
			IResourceFactory factory = generator.getResourceFactory();
			assertNotNull(factory);
			assertNotSame(this.resourceFactory, factory);
		}

		@Test
		public void getActionSignatureProvider() {
			assertSame(this.actionSignatureProvider, generator.getActionSignatureProvider());
		}

	}

	public static class DefaultTypeValue extends AbstractCodeGeneratorTest {

		private void assertSerialized(String expected, EObject actual) {
			String value = this.serializer.serialize(actual);
			assertNotNull(value);
			assertEquals(expected, value);
		}

		@Test
		public void getDefaultXExpressionForType_void() {
			assertNull(generator.getDefaultXExpressionForType(code, code.getSarlScript(), "void"));
		}

		@Test
		public void getDefaultXExpressionForType_Void() {
			assertNull(generator.getDefaultXExpressionForType(code, code.getSarlScript(), "java.lang.Void"));
		}

		@Test
		public void getDefaultXExpressionForType_boolean() {
			assertSerialized("false", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "boolean"));
		}

		@Test
		public void getDefaultXExpressionForType_Boolean() {
			assertSerialized("false", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "java.lang.Boolean"));
		}

		@Test
		public void getDefaultXExpressionForType_char() {
			assertSerialized("0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "char"));
		}

		@Test
		public void getDefaultXExpressionForType_Character() {
			assertSerialized("0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "java.lang.Character"));
		}

		@Test
		public void getDefaultXExpressionForType_byte() {
			assertSerialized("0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "byte"));
		}

		@Test
		public void getDefaultXExpressionForType_Byte() {
			assertSerialized("0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "java.lang.Byte"));
		}

		@Test
		public void getDefaultXExpressionForType_short() {
			assertSerialized("0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "short"));
		}

		@Test
		public void getDefaultXExpressionForType_Short() {
			assertSerialized("0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "java.lang.Short"));
		}

		@Test
		public void getDefaultXExpressionForType_int() {
			assertSerialized("0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "int"));
		}

		@Test
		public void getDefaultXExpressionForType_Integer() {
			assertSerialized("0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "java.lang.Integer"));
		}

		@Test
		public void getDefaultXExpressionForType_long() {
			assertSerialized("0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "long"));
		}

		@Test
		public void getDefaultXExpressionForType_Long() {
			assertSerialized("0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "java.lang.Long"));
		}

		@Test
		public void getDefaultXExpressionForType_float() {
			assertSerialized("0.0f", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "float"));
		}

		@Test
		public void getDefaultXExpressionForType_Float() {
			assertSerialized("0.0f", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "java.lang.Float"));
		}

		@Test
		public void getDefaultXExpressionForType_double() {
			assertSerialized("0.0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "double"));
		}

		@Test
		public void getDefaultXExpressionForType_Double() {
			assertSerialized("0.0", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "java.lang.Double"));
		}

		@Test
		public void getDefaultXExpressionForType_String() {
			assertSerialized("null", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "java.lang.String"));
		}

		@Test
		public void getDefaultXExpressionForType_AnyObject() {
			assertSerialized("null", generator.getDefaultXExpressionForType(code, code.getSarlScript(), "io.sarl.lang.tests.Dummy"));
		}

	}

	public static class References extends AbstractCodeGeneratorTest {

		@Test
		public void newTypeRef_javalangString() {
			JvmParameterizedTypeReference ref = generator.newTypeRef(code, "java.lang.String", this.code.getSarlScript());
			//
			assertNotNull(ref);
			assertEquals("java.lang.String", ref.getIdentifier());
			assertContains(this.code.getImportManager().getImports());
		}

		@Test
		public void newTypeRef_String() {
			JvmParameterizedTypeReference ref = generator.newTypeRef(code, "String", this.code.getSarlScript());
			//
			assertNotNull(ref);
			assertEquals("java.lang.String", ref.getIdentifier());
			assertContains(this.code.getImportManager().getImports());
		}

	}

	public static class Comments extends AbstractCodeGeneratorTest {

		@Nullable
		private XBlockExpression block;
		
		@Nullable
		private SarlAgent agent;

		@Before
		public void setUp() {
			super.setUp();
			agent = generator.createAgent(this.code, "Foo", null);
			block = XbaseFactory.eINSTANCE.createXBlockExpression();
			generator.createAction(this.code, this.agent, "foo", "int", this.block);
		}

		private <T extends Adapter> T assertAdapter(Class<T> expectedType, EObject actual) {
			for(Adapter adapter : actual.eAdapters()) {
				if (expectedType.isInstance(adapter)) {
					return expectedType.cast(adapter);
				}
			}
			fail("Expecting the adapter of type: " + expectedType.getName());
			return null;
		}

		@Test
		public void attachComment() {
			generator.attachComment(code, agent, "my comment");
			DocumentationAdapter adapter = assertAdapter(DocumentationAdapter.class, agent);
			assertEquals("my comment", adapter.getDocumentation());
		}

		@Test
		public void attachPostComment() {
			generator.attachPostComment(code, agent, "my comment");
			PostDocumentationAdapter adapter = assertAdapter(PostDocumentationAdapter.class, agent);
			assertEquals("my comment", adapter.getDocumentation());
		}

		@Test
		public void attachInnerComment() {
			generator.attachInnerComment(code, block, "my comment");
			BlockInnerDocumentationAdapter adapter = assertAdapter(BlockInnerDocumentationAdapter.class, block);
			assertEquals("my comment", adapter.getDocumentation());
		}

	}

	public static class ScriptLevel extends AbstractCodeGeneratorTest {

		@Test
		public void sarlScript()  {
			SarlScript script = code.getSarlScript();
			assertNotNull(script);
			assertEquals("io.sarl.lang.tests", script.getPackage());
			assertTrue(script.getXtendTypes().isEmpty());
			assertNull(script.getImportSection());
		}

		@Test
		public void codeGenerator()  {
			ECoreGeneratorHelper codeGenerator = code.getCodeGenerator();
			assertSame(generator, codeGenerator);
		}

		@Test
		public void importManager()  {
			ImportManager importManager = code.getImportManager();
			assertNotNull(importManager);
			assertTrue(importManager.getImports().isEmpty());
		}

		@Test
		public void resourceSet()  {
			ResourceSet r = code.getResourceSet();
			assertNotNull(r);
			assertSame(r, code.getResourceSet());
		}

	}

	public static class AgentTopElement extends AbstractCodeGeneratorTest {

		@Test
		public void nullSuperClass()  {
			SarlAgent agent = generator.createAgent(code, "MyAgent", null);
			assertNotNull(agent);
			assertEquals("MyAgent", agent.getName());
			assertNull(agent.getExtends());
			assertTrue(agent.getMembers().isEmpty());
		}

		@Test
		public void agentSuperClass()  {
			SarlAgent agent = generator.createAgent(code, "MyAgent", "io.sarl.lang.core.Agent");
			assertNotNull(agent);
			assertEquals("MyAgent", agent.getName());
			assertNull(agent.getExtends());
			assertTrue(agent.getMembers().isEmpty());
		}

		@Test
		public void subagentSuperClass()  {
			SarlAgent agent = generator.createAgent(code, "MyAgent", "foo.ecore.SubAgent");
			//
			assertNotNull(agent);
			assertEquals("MyAgent", agent.getName());
			assertTypeReferenceIdentifier(agent.getExtends(), "foo.ecore.SubAgent");
			assertTrue(agent.getMembers().isEmpty());
		}

		@Test
		public void otherSuperClass()  {
			SarlAgent agent = generator.createAgent(code, "MyAgent", "foo.Foo");
			//
			assertNotNull(agent);
			assertEquals("MyAgent", agent.getName());
			assertTypeReferenceIdentifier(agent.getExtends(), "foo.Foo");
			assertTrue(agent.getMembers().isEmpty());
		}

	}

	public static class BehaviorTopElement extends AbstractCodeGeneratorTest {

		@Test
		public void nullSuperClass()  {
			SarlBehavior behavior = generator.createBehavior(code, "MyBehavior", null);
			assertNotNull(behavior);
			assertEquals("MyBehavior", behavior.getName());
			assertNull(behavior.getExtends());
			assertTrue(behavior.getMembers().isEmpty());
		}

		@Test
		public void behaviorSuperClass()  {
			SarlBehavior behavior = generator.createBehavior(code, "MyBehavior", "io.sarl.lang.core.Behavior");
			assertNotNull(behavior);
			assertEquals("MyBehavior", behavior.getName());
			assertNull(behavior.getExtends());
			assertTrue(behavior.getMembers().isEmpty());
		}

		@Test
		public void subbehaviorSuperClass()  {
			SarlBehavior behavior = generator.createBehavior(code, "MyBehavior", "foo.ecore.SubBehavior");
			//
			assertNotNull(behavior);
			assertEquals("MyBehavior", behavior.getName());
			assertTypeReferenceIdentifier(behavior.getExtends(), "foo.ecore.SubBehavior");
			assertTrue(behavior.getMembers().isEmpty());
		}

		@Test
		public void otherSuperClass()  {
			SarlBehavior behavior = generator.createBehavior(code, "MyBehavior", "foo.Foo");
			//
			assertNotNull(behavior);
			assertEquals("MyBehavior", behavior.getName());
			assertTypeReferenceIdentifier(behavior.getExtends(), "foo.Foo");
			assertTrue(behavior.getMembers().isEmpty());
		}

	}

	public static class CapacityTopElement extends AbstractCodeGeneratorTest {

		@Test
		public void nullSuperClass()  {
			SarlCapacity capacity = generator.createCapacity(code, "MyCapacity", null);
			assertNotNull(capacity);
			assertEquals("MyCapacity", capacity.getName());
			assertTrue(capacity.getExtends().isEmpty());
			assertTrue(capacity.getMembers().isEmpty());
		}

		@Test
		public void behaviorSuperClass()  {
			SarlCapacity capacity = generator.createCapacity(code, "MyCapacity", "io.sarl.lang.core.Capacity");
			assertNotNull(capacity);
			assertEquals("MyCapacity", capacity.getName());
			assertTrue(capacity.getExtends().isEmpty());
			assertTrue(capacity.getMembers().isEmpty());
		}

		@Test
		public void subbehaviorSuperClass()  {
			SarlCapacity capacity = generator.createCapacity(code, "MyCapacity", "foo.ecore.SubCapacity");
			//
			assertNotNull(capacity);
			assertEquals("MyCapacity", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getExtends(), "foo.ecore.SubCapacity");
			assertTrue(capacity.getMembers().isEmpty());
		}

		@Test
		public void otherSuperClass()  {
			SarlCapacity capacity = generator.createCapacity(code, "MyCapacity", "foo.Foo");
			//
			assertNotNull(capacity);
			assertEquals("MyCapacity", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getExtends(), "foo.Foo");
			assertTrue(capacity.getMembers().isEmpty());
		}

	}

	public static class EventTopElement extends AbstractCodeGeneratorTest {

		@Test
		public void nullSuperClass()  {
			SarlEvent event = generator.createEvent(code, "MyEvent", null);
			assertNotNull(event);
			assertEquals("MyEvent", event.getName());
			assertNull(event.getExtends());
			assertTrue(event.getMembers().isEmpty());
		}

		@Test
		public void behaviorSuperClass()  {
			SarlEvent event = generator.createEvent(code, "MyEvent", "io.sarl.lang.core.Event");
			assertNotNull(event);
			assertEquals("MyEvent", event.getName());
			assertNull(event.getExtends());
			assertTrue(event.getMembers().isEmpty());
		}

		@Test
		public void subbehaviorSuperClass()  {
			SarlEvent event = generator.createEvent(code, "MyEvent", "foo.ecore.SubEvent");
			//
			assertNotNull(event);
			assertEquals("MyEvent", event.getName());
			assertTypeReferenceIdentifier(event.getExtends(), "foo.ecore.SubEvent");
			assertTrue(event.getMembers().isEmpty());
		}

		@Test
		public void otherSuperClass()  {
			SarlEvent event = generator.createEvent(code, "MyEvent", "foo.Foo");
			//
			assertNotNull(event);
			assertEquals("MyEvent", event.getName());
			assertTypeReferenceIdentifier(event.getExtends(), "foo.Foo");
			assertTrue(event.getMembers().isEmpty());
		}

	}

	public static class SkillTopElement extends AbstractCodeGeneratorTest {

		@Test
		public void nullSuperClass_noSuperInterfaces()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", null, Collections.<String>emptyList());
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertNull(skill.getExtends());
			assertTrue(skill.getImplements().isEmpty());
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void behaviorSuperClass_noSuperInterfaces()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", "io.sarl.lang.core.Skill", Collections.<String>emptyList());
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertNull(skill.getExtends());
			assertTrue(skill.getImplements().isEmpty());
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void subbehaviorSuperClass_noSuperInterface()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", "foo.ecore.SubSkill", Collections.<String>emptyList());
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifier(skill.getExtends(), "foo.ecore.SubSkill");
			assertTrue(skill.getImplements().isEmpty());
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void otherSuperClass_noSuperInterface()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", "foo.Foo", Collections.<String>emptyList());
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifier(skill.getExtends(), "foo.Foo");
			assertTrue(skill.getImplements().isEmpty());
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void nullSuperClass_oneSuperInterfaces()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", null, Collections.singleton("foo.ecore.SubCapacity"));
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "foo.ecore.SubCapacity");
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void behaviorSuperClass_oneSuperInterfaces()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", "io.sarl.lang.core.Skill", Collections.singleton("foo.ecore.SubCapacity"));
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "foo.ecore.SubCapacity");
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void subbehaviorSuperClass_oneSuperInterface()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", "foo.ecore.SubSkill", Collections.singleton("foo.ecore.SubCapacity"));
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifier(skill.getExtends(), "foo.ecore.SubSkill");
			assertTypeReferenceIdentifiers(skill.getImplements(), "foo.ecore.SubCapacity");
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void otherSuperClass_oneSuperInterface()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", "foo.Foo", Collections.singleton("foo.ecore.SubCapacity"));
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifier(skill.getExtends(), "foo.Foo");
			assertTypeReferenceIdentifiers(skill.getImplements(), "foo.ecore.SubCapacity");
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void nullSuperClass_twoSuperInterfaces()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", null, Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"));
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2");
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void behaviorSuperClass_twoSuperInterfaces()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", "io.sarl.lang.core.Skill", Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"));
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertNull(skill.getExtends());
			assertTypeReferenceIdentifiers(skill.getImplements(), "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2");
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void subbehaviorSuperClass_twoSuperInterface()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", "foo.ecore.SubSkill", Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"));
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifier(skill.getExtends(), "foo.ecore.SubSkill");
			assertTypeReferenceIdentifiers(skill.getImplements(), "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2");
			assertTrue(skill.getMembers().isEmpty());
		}

		@Test
		public void otherSuperClass_twoSuperInterface()  {
			SarlSkill skill = generator.createSkill(code, "MySkill", "foo.Foo", Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"));
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifier(skill.getExtends(), "foo.Foo");
			assertTypeReferenceIdentifiers(skill.getImplements(), "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2");
			assertTrue(skill.getMembers().isEmpty());
		}

	}

	public static class AgentFeatures extends AbstractCodeGeneratorTest {

		@Nullable
		private XBlockExpression block;

		@Nullable
		private SarlAgent agent;

		@Before
		public void setUp() {
			super.setUp();
			agent = this.generator.createAgent(this.code, "Foo", null);
			block = XbaseFactory.eINSTANCE.createXBlockExpression();
			this.generator.createAction(this.code, this.agent, "foo", "int", block);
		}

		@Test
		public void returnNull()  {
			SarlAction action = generator.createAction(code, agent, "myFct", null, block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertNull(action.getReturnType());
			assertTrue(action.getParameters().isEmpty());
			assertSame(block, action.getExpression());
		}

		@Test
		public void returnBoolean()  {
			SarlAction action = generator.createAction(code, agent, "myFct", "boolean", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "boolean");
			assertTrue(action.getParameters().isEmpty());
			assertSame(block, action.getExpression());
		}

		@Test
		public void returnObject()  {
			SarlAction action = generator.createAction(code, agent, "myFct", "java.lang.String", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			assertTrue(action.getParameters().isEmpty());
			assertSame(block, action.getExpression());
		}

		@Test
		public void createConstructor()  {
			SarlConstructor constructor = generator.createConstructor(code, agent, block);
			//
			assertNotNull(constructor);
			assertTrue(constructor.getParameters().isEmpty());
			assertSame(block, constructor.getExpression());
		}

		@Test
		public void createBehaviorUnit_noGuard()  {
			SarlBehaviorUnit unit = generator.createBehaviorUnit(code, agent, "foo.ecore.SubEvent", null, block);
			//
			assertNotNull(unit);
			assertTypeReferenceIdentifier(unit.getName(), "foo.ecore.SubEvent");
			assertNull(unit.getGuard());
			assertSame(block, unit.getExpression());
		}

		@Test
		public void createBehaviorUnit_aGuard()  {
			XBlockExpression guard = XbaseFactory.eINSTANCE.createXBlockExpression();
			//
			SarlBehaviorUnit unit = generator.createBehaviorUnit(code, agent, "foo.ecore.SubEvent", guard, block);
			//
			assertNotNull(unit);
			assertTypeReferenceIdentifier(unit.getName(), "foo.ecore.SubEvent");
			assertSame(guard, unit.getGuard());
			assertSame(block, unit.getExpression());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			generator.createVariable(code, agent, "myVar", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			generator.createVariable(code, agent, "myVar", (XExpression) null);
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringString()  {
			SarlField variable = generator.createVariable(code, agent, "myVar", "java.lang.String");
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertTypeReferenceIdentifier(variable.getType(), "java.lang.String");
			assertNull(variable.getInitialValue());
			assertFalse(variable.isFinal());
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			SarlField variable = generator.createVariable(code, agent, "myVar", numberLiteral);
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertNull(variable.getType());
			assertSame(numberLiteral, variable.getInitialValue());
			assertFalse(variable.isFinal());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			generator.createValue(code, agent, "myConst", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			generator.createValue(code, agent, "myConst", (XExpression) null);
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringString()  {
			SarlField value = generator.createValue(code, agent, "myConst", "java.lang.String");
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertTypeReferenceIdentifier(value.getType(), "java.lang.String");
			assertNull(value.getInitialValue());
			assertTrue(value.isFinal());
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral  numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			SarlField value = generator.createValue(code, agent, "myConst", numberLiteral);
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertNull(value.getType());
			assertSame(numberLiteral, value.getInitialValue());
			assertTrue(value.isFinal());
		}
		
		@Test
		public void isActionBodyAllowed() {
			assertTrue(generator.isActionBodyAllowed(this.agent));
		}

	}

	public static class BehaviorFeatures extends AbstractCodeGeneratorTest {

		@Nullable
		private XBlockExpression block;

		@Nullable
		private SarlBehavior behavior;

		@Before
		public void setUp() {
			super.setUp();
			behavior = this.generator.createBehavior(this.code, "Foo", null);
			block = XbaseFactory.eINSTANCE.createXBlockExpression();
			this.generator.createAction(this.code, this.behavior, "foo", "int", block);
		}

		@Test
		public void isActionBodyAllowed() {
			assertTrue(generator.isActionBodyAllowed(this.behavior));
		}

		@Test
		public void returnNull()  {
			SarlAction action = generator.createAction(code, behavior, "myFct", null, block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertNull(action.getReturnType());
			assertTrue(action.getParameters().isEmpty());
			assertSame(block, action.getExpression());
		}

		@Test
		public void returnBoolean()  {
			SarlAction action = generator.createAction(code, behavior, "myFct", "boolean", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "boolean");
			assertTrue(action.getParameters().isEmpty());
			assertSame(block, action.getExpression());
		}

		@Test
		public void returnObject()  {
			SarlAction action = generator.createAction(code, behavior, "myFct", "java.lang.String", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			assertTrue(action.getParameters().isEmpty());
			assertSame(block, action.getExpression());
		}

		@Test
		public void createConstructor()  {
			SarlConstructor constructor = generator.createConstructor(code, behavior, block);
			//
			assertNotNull(constructor);
			assertTrue(constructor.getParameters().isEmpty());
			assertSame(block, constructor.getExpression());
		}

		@Test
		public void createBehaviorUnit_noGuard()  {
			SarlBehaviorUnit unit = generator.createBehaviorUnit(code, behavior, "foo.ecore.SubEvent", null, block);
			//
			assertNotNull(unit);
			assertTypeReferenceIdentifier(unit.getName(), "foo.ecore.SubEvent");
			assertNull(unit.getGuard());
			assertSame(block, unit.getExpression());
		}

		@Test
		public void createBehaviorUnit_aGuard()  {
			XBlockExpression guard = XbaseFactory.eINSTANCE.createXBlockExpression();
			//
			SarlBehaviorUnit unit = generator.createBehaviorUnit(code, behavior, "foo.ecore.SubEvent", guard, block);
			//
			assertNotNull(unit);
			assertTypeReferenceIdentifier(unit.getName(), "foo.ecore.SubEvent");
			assertSame(guard, unit.getGuard());
			assertSame(block, unit.getExpression());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			generator.createVariable(code, behavior, "myVar", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			generator.createVariable(code, behavior, "myVar", (XExpression) null);
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringString()  {
			SarlField variable = generator.createVariable(code, behavior, "myVar", "java.lang.String");
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertTypeReferenceIdentifier(variable.getType(), "java.lang.String");
			assertNull(variable.getInitialValue());
			assertFalse(variable.isFinal());
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			SarlField variable = generator.createVariable(code, behavior, "myVar", numberLiteral);
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertNull(variable.getType());
			assertSame(numberLiteral, variable.getInitialValue());
			assertFalse(variable.isFinal());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			generator.createValue(code, behavior, "myConst", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			generator.createValue(code, behavior, "myConst", (XExpression) null);
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringString()  {
			SarlField value = generator.createValue(code, behavior, "myConst", "java.lang.String");
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertTypeReferenceIdentifier(value.getType(), "java.lang.String");
			assertNull(value.getInitialValue());
			assertTrue(value.isFinal());
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			SarlField value = generator.createValue(code, behavior, "myConst", numberLiteral);
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertNull(value.getType());
			assertSame(numberLiteral, value.getInitialValue());
			assertTrue(value.isFinal());
		}

	}

	public static class CapacityFeatures extends AbstractCodeGeneratorTest {

		@Nullable
		private SarlCapacity capacity;

		@Before
		public void setUp() {
			super.setUp();
			capacity = this.generator.createCapacity(this.code, "Foo", null);
			this.generator.createAction(this.code, this.capacity, "foo", "int", null);
		}

		@Test
		public void isActionBodyAllowed() {
			assertFalse(generator.isActionBodyAllowed(this.capacity));
		}

		@Test
		public void returnNull()  {
			SarlAction action = generator.createAction(code, capacity, "myFct", null, null);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertNull(action.getReturnType());
			assertTrue(action.getParameters().isEmpty());
			assertNull(action.getExpression());
		}

		@Test
		public void returnBoolean()  {
			SarlAction action = generator.createAction(code, capacity, "myFct", "boolean", null);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "boolean");
			assertTrue(action.getParameters().isEmpty());
			assertNull(action.getExpression());
		}

		@Test
		public void returnObject()  {
			SarlAction action = generator.createAction(code, capacity, "myFct", "java.lang.String", null);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			assertTrue(action.getParameters().isEmpty());
			assertNull(action.getExpression());
		}

	}

	public static class EventFeatures extends AbstractCodeGeneratorTest {

		@Nullable
		private XBlockExpression block;

		@Nullable
		private SarlEvent event;

		@Before
		public void setUp() {
			super.setUp();
			event = this.generator.createEvent(this.code, "Foo", null);
			block = XbaseFactory.eINSTANCE.createXBlockExpression();
			this.generator.createConstructor(this.code, this.event, block);
		}

		@Test
		public void isActionBodyAllowed() {
			assertFalse(generator.isActionBodyAllowed(this.event));
		}

		@Test
		public void createConstructor()  {
			SarlConstructor constructor = generator.createConstructor(code, event, block);
			//
			assertNotNull(constructor);
			assertTrue(constructor.getParameters().isEmpty());
			assertSame(block, constructor.getExpression());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			generator.createVariable(code, event, "myVar", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			generator.createVariable(code, event, "myVar", (XExpression) null);
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringString()  {
			SarlField variable = generator.createVariable(code, event, "myVar", "java.lang.String");
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertTypeReferenceIdentifier(variable.getType(), "java.lang.String");
			assertNull(variable.getInitialValue());
			assertFalse(variable.isFinal());
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			SarlField variable = generator.createVariable(code, event, "myVar", numberLiteral);
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertNull(variable.getType());
			assertSame(numberLiteral, variable.getInitialValue());
			assertFalse(variable.isFinal());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			generator.createValue(code, event, "myConst", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			generator.createValue(code, event, "myConst", (XExpression) null);
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringString()  {
			SarlField value = generator.createValue(code, event, "myConst", "java.lang.String");
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertTypeReferenceIdentifier(value.getType(), "java.lang.String");
			assertNull(value.getInitialValue());
			assertTrue(value.isFinal());
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			SarlField value = generator.createValue(code, event, "myConst", numberLiteral);
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertNull(value.getType());
			assertSame(numberLiteral, value.getInitialValue());
			assertTrue(value.isFinal());
		}

	}

	public static class SkillFeatures extends AbstractCodeGeneratorTest {

		@Nullable
		private XBlockExpression block;

		@Nullable
		private SarlSkill skill;

		@Before
		public void setUp() {
			super.setUp();
			skill = this.generator.createSkill(this.code, "Foo", null, Collections.<String>emptyList());
			block = XbaseFactory.eINSTANCE.createXBlockExpression();
			this.generator.createAction(this.code, this.skill, "foo", "int", block);
		}

		@Test
		public void isActionBodyAllowed() {
			assertTrue(generator.isActionBodyAllowed(this.skill));
		}

		@Test
		public void returnNull()  {
			SarlAction action = generator.createAction(code, skill, "myFct", null, block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertNull(action.getReturnType());
			assertTrue(action.getParameters().isEmpty());
			assertSame(block, action.getExpression());
		}

		@Test
		public void returnBoolean()  {
			SarlAction action = generator.createAction(code, skill, "myFct", "boolean", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "boolean");
			assertTrue(action.getParameters().isEmpty());
			assertSame(block, action.getExpression());
		}

		@Test
		public void returnObject()  {
			SarlAction action = generator.createAction(code, skill, "myFct", "java.lang.String", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			assertTrue(action.getParameters().isEmpty());
			assertSame(block, action.getExpression());
		}

		@Test
		public void createConstructor()  {
			SarlConstructor constructor = generator.createConstructor(code, skill, block);
			//
			assertNotNull(constructor);
			assertTrue(constructor.getParameters().isEmpty());
			assertSame(block, constructor.getExpression());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			generator.createVariable(code, skill, "myVar", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			generator.createVariable(code, skill, "myVar", (XExpression) null);
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringString()  {
			SarlField variable = generator.createVariable(code, skill, "myVar", "java.lang.String");
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertTypeReferenceIdentifier(variable.getType(), "java.lang.String");
			assertNull(variable.getInitialValue());
			assertFalse(variable.isFinal());
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			SarlField variable = generator.createVariable(code, skill, "myVar", numberLiteral);
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertNull(variable.getType());
			assertSame(numberLiteral, variable.getInitialValue());
			assertFalse(variable.isFinal());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			generator.createValue(code, skill, "myConst", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			generator.createValue(code, skill, "myConst", (XExpression) null);
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringString()  {
			SarlField value = generator.createValue(code, skill, "myConst", "java.lang.String");
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertTypeReferenceIdentifier(value.getType(), "java.lang.String");
			assertNull(value.getInitialValue());
			assertTrue(value.isFinal());
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			SarlField value = generator.createValue(code, skill, "myConst", numberLiteral);
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertNull(value.getType());
			assertSame(numberLiteral, value.getInitialValue());
			assertTrue(value.isFinal());
		}

	}

	public static class Expressions extends AbstractCodeGeneratorTest {

		@Test
		public void createXExpression_null()  {
			XExpression expr = generator.createXExpression(null, this.code.getResourceSet(),
					this.code.getImportManager());
			//
			assertNull(expr);
			assertImports(this.code.getImportManager());
		}

		@Test
		public void createXExpression_empty()  {
			XExpression expr = generator.createXExpression("", this.code.getResourceSet(),
					this.code.getImportManager());
			//
			assertNull(expr);
			assertImports(this.code.getImportManager());
		}

	}

	public static class FormalParameters extends AbstractCodeGeneratorTest {

		@Nullable
		private XtendExecutable container;

		@Before
		public void setUp() {
			super.setUp();
			SarlAgent agent = this.generator.createAgent(this.code, "Foo", null);
			this.container = this.generator.createAction(this.code, agent, "foo", "int", null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVarArgs_nullType()  {
			generator.createVarArgs(code, container, "myParam", null);
		}

		@Test
		public void createVarArgs()  {
			SarlFormalParameter param = generator.createVarArgs(code, container, "myParam", "boolean");
			//
			assertNotNull(param);
			assertEquals("myParam", param.getName());
			assertNull(param.getDefaultValue());
			assertTypeReferenceIdentifier(param.getParameterType(), "boolean");
			//
			assertParameterNames(container.getParameters(), "myParam");
			assertParameterVarArg(container.getParameters());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringStringResourceSet_nullType_noDefaultValue()  {
			generator.createFormalParameter(code, container, "myParam", null, null, code.getResourceSet());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringStringResourceSet_nullType_defaultValue()  {
			generator.createFormalParameter(code, container, "myParam", null, "true", code.getResourceSet());
		}

		@Test
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringStringResourceSet_noDefaultValue()  {
			SarlFormalParameter param = generator.createFormalParameter(code, container, "myParam", "java.lang.String", null, code.getResourceSet());
			//
			assertNotNull(param);
			assertEquals("myParam", param.getName());
			assertNull(param.getDefaultValue());
			assertTypeReferenceIdentifier(param.getParameterType(), "java.lang.String");
			//
			assertParameterNames(container.getParameters(), "myParam");
		}

		@Test(expected = IllegalArgumentException.class)
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_nullType_noDefaultValue()  {
			generator.createFormalParameter(code, container, "myParam", null, null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_nullType_defaultValue()  {
			XStringLiteral expr = XbaseFactory.eINSTANCE.createXStringLiteral();
			expr.setValue("abc");
			generator.createFormalParameter(code, container, "myParam", null, expr);
		}

		@Test
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_noDefaultValue()  {
			SarlFormalParameter param = generator.createFormalParameter(code, container, "myParam", "java.lang.String", null);
			//
			assertNotNull(param);
			assertEquals("myParam", param.getName());
			assertNull(param.getDefaultValue());
			assertTypeReferenceIdentifier(param.getParameterType(), "java.lang.String");
			//
			assertParameterNames(container.getParameters(), "myParam");
		}

		@Test
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_defaultValue()  {
			XStringLiteral expr = XbaseFactory.eINSTANCE.createXStringLiteral();
			expr.setValue("abc");
			SarlFormalParameter param = generator.createFormalParameter(code, container, "myParam", "java.lang.String", expr);
			//
			assertNotNull(param);
			assertEquals("myParam", param.getName());
			assertSame(expr, param.getDefaultValue());
			assertTypeReferenceIdentifier(param.getParameterType(), "java.lang.String");
			//
			assertParameterNames(container.getParameters(), "myParam");
		}

	}

	public static class CreateActionFromJvmElement extends AbstractCodeGeneratorTest {

		@Test
		public void noParam_noReturn() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct {}");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(0, action.getParameters().size());
			//
			assertImports(this.code.getImportManager());
		}

		@Test
		public void stdParam_noReturn() throws Exception {
			JvmOperation operation = jvmOperation(
				"def fct(a : URL, b : int) {}",
				"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_noReturn() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int*) {}",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void defaultValue_noReturn() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int=4, c : char) {}",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_defaultValue_noReturn() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int=4, c : char*) {}",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void noParam_returnValue() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct : String { null }");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(0, action.getParameters().size());
			//
			assertImports(this.code.getImportManager());
		}

		@Test
		public void stdParam_returnValue() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int) : String { null }",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_returnValue() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int*) : String { null }",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void defaultValue_returnValue() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int=4, c : char) : String { null }",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_defaultValue_returnValue() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int=4, c : char*) : String { null }",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void noParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct fires MyEvent {}",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(0, action.getParameters().size());
			//
			assertImports(this.code.getImportManager());
		}

		@Test
		public void stdParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int) fires MyEvent {}",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int*) fires MyEvent {}",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void defaultValue_noReturn_fireEvents() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int=4, c : char) fires MyEvent {}",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_defaultValue_noReturn_fireEvents() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int=4, c : char*) fires MyEvent {}",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void noParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct : String fires MyEvent { null }",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(0, action.getParameters().size());
			//
			assertImports(this.code.getImportManager());
		}

		@Test
		public void stdParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int) : String fires MyEvent { null }",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int*) : String fires MyEvent { null }",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void defaultValue_returnValue_fireEvents() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int=4, c : char) : String fires MyEvent { null }",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_defaultValue_returnValue_fireEvents() throws Exception {
			JvmOperation operation = jvmOperation(
					"def fct(a : URL, b : int=4, c : char*) : String fires MyEvent { null }",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

	}

	public static class CreateConstructorFromJvmElement extends AbstractCodeGeneratorTest {

		@Test
		public void noParam() throws Exception {
			JvmConstructor cons = jvmConstructor(
					"new() {}");
			//
			SarlConstructor action = this.generator.createConstructor(cons, this.code.getImportManager());
			//
			assertNotNull(action);
			//
			assertEquals(0, action.getParameters().size());
			//
			assertImports(this.code.getImportManager());
		}

		@Test
		public void stdParam() throws Exception {
			JvmConstructor cons = jvmConstructor(
					"new (a : URL, b : int) {}",
					"import java.net.URL");
			//
			SarlConstructor action = this.generator.createConstructor(cons, this.code.getImportManager());
			//
			assertNotNull(action);
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam() throws Exception {
			JvmConstructor cons = jvmConstructor(
					"new (a : URL, b : int*) {}",
					"import java.net.URL");
			//
			SarlConstructor action = this.generator.createConstructor(cons, this.code.getImportManager());
			//
			assertNotNull(action);
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void defaultValue() throws Exception {
			JvmConstructor cons = jvmConstructor(
					"new (a : URL, b : int=4, c : char) {}",
					"import java.net.URL");
			//
			SarlConstructor action = this.generator.createConstructor(cons, this.code.getImportManager());
			//
			assertNotNull(action);
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_defaultValue() throws Exception {
			JvmConstructor cons = jvmConstructor(
					"new (a : URL, b : int=4, c : char*) {}",
					"import java.net.URL");
			//
			SarlConstructor action = this.generator.createConstructor(cons, this.code.getImportManager());
			//
			assertNotNull(action);
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

	}

	public static class CreateActionSignatureFromJvmElement extends AbstractCodeGeneratorTest {

		@Test
		public void noParam_noReturn() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(0, action.getParameters().size());
			//
			assertImports(this.code.getImportManager());
		}

		@Test
		public void stdParam_noReturn() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int)",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_noReturn() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int*)",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void defaultValue_noReturn() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int=4, c : char)",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_defaultValue_noReturn() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int=4, c : char*)",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void noParam_returnValue() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct : String");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(0, action.getParameters().size());
			//
			assertImports(this.code.getImportManager());
		}

		@Test
		public void stdParam_returnValue() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int) : String",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_returnValue() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int*) : String",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void defaultValue_returnValue() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int=4, c : char) : String",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_defaultValue_returnValue() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int=4, c : char*) : String",
					"import java.net.URL");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void noParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct fires MyEvent",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(0, action.getParameters().size());
			//
			assertImports(this.code.getImportManager());
		}

		@Test
		public void stdParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int) fires MyEvent",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int*) fires MyEvent",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void defaultValue_noReturn_fireEvents() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int=4, c : char) fires MyEvent",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_defaultValue_noReturn_fireEvents() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int=4, c : char*) fires MyEvent",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void noParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct : String fires MyEvent",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(0, action.getParameters().size());
			//
			assertImports(this.code.getImportManager());
		}

		@Test
		public void stdParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int) : String fires MyEvent",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int*) : String fires MyEvent",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(2, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParameters(),
					null,
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void defaultValue_returnValue_fireEvents() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int=4, c : char) : String fires MyEvent",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertNoParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

		@Test
		public void variadicParam_defaultValue_returnValue_fireEvents() throws Exception {
			JvmOperation operation = jvmOperationSignature(
					"def fct(a : URL, b : int=4, c : char*) : String fires MyEvent",
					"import java.net.URL",
					"event MyEvent");
			//
			SarlAction action = this.generator.createAction(operation, this.code.getImportManager());
			//
			assertNotNull(action);
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getReturnType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertEquals(3, action.getParameters().size());
			assertParameterNames(action.getParameters(), "a", "b", "c");
			assertParameterTypes(action.getParameters(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParameters(),
					null,
					XNumberLiteral.class, "4",
					null);
			assertParameterVarArg(action.getParameters());
			//
			assertImports(this.code.getImportManager(), "java.net.URL");
		}

	}

}
