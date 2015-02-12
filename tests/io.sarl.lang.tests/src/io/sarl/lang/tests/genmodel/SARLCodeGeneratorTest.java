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
package io.sarl.lang.tests.genmodel;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.genmodel.SARLCodeGenerator;
import io.sarl.lang.genmodel.SARLCodeGenerator.BlockInnerDocumentationAdapter;
import io.sarl.lang.genmodel.SARLCodeGenerator.GeneratedCode;
import io.sarl.lang.genmodel.SARLCodeGenerator.PostDocumentationAdapter;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.FormalParameter;
import io.sarl.lang.sarl.ParameterizedFeature;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.sarl.TopElement;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.Nullable;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.compiler.DocumentationAdapter;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.impl.XBlockExpressionImpl;
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.mockito.Matchers;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	SARLCodeGeneratorTest.InjectedAttributes.class,
	SARLCodeGeneratorTest.DefaultTypeValue.class,
	SARLCodeGeneratorTest.References.class,
	SARLCodeGeneratorTest.Comments.class,
	SARLCodeGeneratorTest.ScriptLevel.class,
	SARLCodeGeneratorTest.AgentTopElement.class,
	SARLCodeGeneratorTest.BehaviorTopElement.class,
	SARLCodeGeneratorTest.CapacityTopElement.class,
	SARLCodeGeneratorTest.EventTopElement.class,
	SARLCodeGeneratorTest.SkillTopElement.class,
	SARLCodeGeneratorTest.AgentFeatures.class,
	SARLCodeGeneratorTest.BehaviorFeatures.class,
	SARLCodeGeneratorTest.CapacityFeatures.class,
	SARLCodeGeneratorTest.EventFeatures.class,
	SARLCodeGeneratorTest.SkillFeatures.class,
	SARLCodeGeneratorTest.Expressions.class,
	SARLCodeGeneratorTest.FormalParameters.class,
	SARLCodeGeneratorTest.CreateActionFromJvmElement.class,
	SARLCodeGeneratorTest.CreateConstructorFromJvmElement.class,
	SARLCodeGeneratorTest.CreateActionSignatureFromJvmElement.class
})
@SuppressWarnings("all")
public class SARLCodeGeneratorTest {

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class InjectedAttributes extends AbstractSarlTest {

		@Inject
		private TypeReferences typeReferences;

		@Inject
		private TypesFactory typesFactory;

		@Inject
		private IResourceFactory resourceFactory;

		@Inject
		ActionSignatureProvider actionSignatureProvider;

		@Inject
		private SARLCodeGenerator gen;;

		@Test
		public void getSARLFileExtension() {
			assertEquals("sarl", gen.getSARLFileExtension());
		}

		@Test
		public void getTypeReferences() {
			assertSame(this.typeReferences, gen.getTypeReferences());
		}

		@Test
		public void getTypesFactory() {
			assertSame(this.typesFactory, gen.getTypesFactory());
		}

		@Test
		public void getResourceFactory() {
			IResourceFactory factory = gen.getResourceFactory();
			assertNotNull(factory);
			assertNotSame(this.resourceFactory, factory);
		}

		@Test
		public void getActionSignatureProvider() {
			assertSame(this.actionSignatureProvider, gen.getActionSignatureProvider());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class DefaultTypeValue extends AbstractSarlTest {

		@Inject
		private ISerializer serializer;

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private EObject context;

		@Before
		public void setUp() {
			code = mock(GeneratedCode.class);
			context = mock(EObject.class);
		}

		private void assertSerialized(String expected, EObject actual) {
			String value = this.serializer.serialize(actual);
			assertNotNull(value);
			assertEquals(expected, value);
		}

		@Test
		public void getDefaultXExpressionForType_void() {
			assertNull(gen.getDefaultXExpressionForType(code, context, "void"));
		}

		@Test
		public void getDefaultXExpressionForType_Void() {
			assertNull(gen.getDefaultXExpressionForType(code, context, "java.lang.Void"));
		}

		@Test
		public void getDefaultXExpressionForType_boolean() {
			assertSerialized("false", gen.getDefaultXExpressionForType(code, context, "boolean"));
		}

		@Test
		public void getDefaultXExpressionForType_Boolean() {
			assertSerialized("false", gen.getDefaultXExpressionForType(code, context, "java.lang.Boolean"));
		}

		@Test
		public void getDefaultXExpressionForType_char() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "char"));
		}

		@Test
		public void getDefaultXExpressionForType_Character() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "java.lang.Character"));
		}

		@Test
		public void getDefaultXExpressionForType_byte() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "byte"));
		}

		@Test
		public void getDefaultXExpressionForType_Byte() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "java.lang.Byte"));
		}

		@Test
		public void getDefaultXExpressionForType_short() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "short"));
		}

		@Test
		public void getDefaultXExpressionForType_Short() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "java.lang.Short"));
		}

		@Test
		public void getDefaultXExpressionForType_int() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "int"));
		}

		@Test
		public void getDefaultXExpressionForType_Integer() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "java.lang.Integer"));
		}

		@Test
		public void getDefaultXExpressionForType_long() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "long"));
		}

		@Test
		public void getDefaultXExpressionForType_Long() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "java.lang.Long"));
		}

		@Test
		public void getDefaultXExpressionForType_float() {
			assertSerialized("0.0f", gen.getDefaultXExpressionForType(code, context, "float"));
		}

		@Test
		public void getDefaultXExpressionForType_Float() {
			assertSerialized("0.0f", gen.getDefaultXExpressionForType(code, context, "java.lang.Float"));
		}

		@Test
		public void getDefaultXExpressionForType_double() {
			assertSerialized("0.0", gen.getDefaultXExpressionForType(code, context, "double"));
		}

		@Test
		public void getDefaultXExpressionForType_Double() {
			assertSerialized("0.0", gen.getDefaultXExpressionForType(code, context, "java.lang.Double"));
		}

		@Test
		public void getDefaultXExpressionForType_String() {
			assertSerialized("null", gen.getDefaultXExpressionForType(code, context, "java.lang.String"));
		}

		@Test
		public void getDefaultXExpressionForType_AnyObject() {
			assertSerialized("null", gen.getDefaultXExpressionForType(code, context, "io.sarl.lang.tests.Dummy"));
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class References extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private EObject context;

		@Nullable
		private ImportManager importManager;;

		@Nullable
		private JvmType jvmType;

		@Before
		public void setUp() {
			importManager = new ImportManager();
			code = mock(GeneratedCode.class);
			context = mock(EObject.class);

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);
			jvmType = mock(JvmType.class);

			when(code.getImportManager()).thenReturn(importManager);
			when(typeProvider.findTypeByName(Matchers.anyString())).thenReturn(jvmType);
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(context.eResource()).thenReturn(eResource);
		}

		@Test
		public void newTypeRef_String() {
			when(jvmType.getIdentifier()).thenReturn("java.lang.String");
			when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn("java.lang.String");
			when(jvmType.getQualifiedName()).thenReturn("java.lang.String");
			when(jvmType.getSimpleName()).thenReturn("String");
			//
			JvmParameterizedTypeReference ref = gen.newTypeRef(code, "java.lang.String", context);
			//
			assertNotNull(ref);
			assertEquals("java.lang.String", ref.getIdentifier());
			assertContains(importManager.getImports());
		}

		@Test
		public void newTypeRef_Foo() {
			when(jvmType.getIdentifier()).thenReturn("foo.Foo");
			when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn("foo.Foo");
			when(jvmType.getQualifiedName()).thenReturn("foo.Foo");
			when(jvmType.getSimpleName()).thenReturn("Foo");
			//
			JvmParameterizedTypeReference ref = gen.newTypeRef(code, "foo.Foo", context);
			//
			assertNotNull(ref);
			assertEquals("foo.Foo", ref.getIdentifier());
			assertContains(importManager.getImports(), "foo.Foo");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class Comments extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private EObject context;

		@Nullable
		private EObject object;

		@Nullable
		private XBlockExpression block;;

		@Before
		public void setUp() {
			code = mock(GeneratedCode.class);
			context = mock(EObject.class);
			object = new EObjectImpl() {};
			block = new XBlockExpressionImpl() {};
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
			gen.attachComment(code, object, "my comment");
			DocumentationAdapter adapter = assertAdapter(DocumentationAdapter.class, object);
			assertEquals("my comment", adapter.getDocumentation());
		}

		@Test
		public void attachPostComment() {
			gen.attachPostComment(code, object, "my comment");
			PostDocumentationAdapter adapter = assertAdapter(PostDocumentationAdapter.class, object);
			assertEquals("my comment", adapter.getDocumentation());
		}

		@Test
		public void attachInnerComment() {
			gen.attachInnerComment(code, block, "my comment");
			BlockInnerDocumentationAdapter adapter = assertAdapter(BlockInnerDocumentationAdapter.class, block);
			assertEquals("my comment", adapter.getDocumentation());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class ScriptLevel extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private ResourceSet resourceSet;

		@Nullable
		private Resource resource;

		@Nullable
		private EList<EObject> content;

		@Nullable
		private GeneratedCode code;;

		@Before
		public void setUp() {
			resourceSet = mock(ResourceSet.class);
			content = mock(EList.class);
			resource = mock(Resource.class);
			when(resource.getContents()).thenReturn(content);
			when(resource.getResourceSet()).thenReturn(resourceSet);
			code = gen.createScript(resource, "io.sarl.lang.tests");
			assertNotNull(code);
		}

		@Test
		public void sarlScript()  {
			SarlScript script = code.getSarlScript();
			assertNotNull(script);
			assertEquals("io.sarl.lang.tests", script.getName());
			assertTrue(script.getElements().isEmpty());
			assertNull(script.getImportSection());
		}

		@Test
		public void codeGenerator()  {
			SARLCodeGenerator codeGenerator = code.getCodeGenerator();
			assertSame(gen, codeGenerator);
		}

		@Test
		public void importManager()  {
			ImportManager importManager = code.getImportManager();
			assertNotNull(importManager);
			assertTrue(importManager.getImports().isEmpty());
		}

		@Test
		public void resourceSet()  {
			assertSame(resourceSet, code.getResourceSet());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class AgentTopElement extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private SarlScript context;

		@Nullable
		private ImportManager importManager;;

		@Nullable
		private JvmType jvmType;

		@Nullable
		private EList<TopElement> elements;

		@Before
		public void setUp() {
			elements = new BasicEList<TopElement>();
			importManager = new ImportManager();
			code = mock(GeneratedCode.class);
			context = mock(SarlScript.class);

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);
			jvmType = mock(JvmType.class);

			when(code.getImportManager()).thenReturn(importManager);
			when(code.getSarlScript()).thenReturn(context);
			when(typeProvider.findTypeByName(Matchers.anyString())).thenReturn(jvmType);
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(context.eResource()).thenReturn(eResource);
			when(context.getElements()).thenReturn(elements);
		}

		@Test	
		public void nullSuperClass()  {
			Agent agent = gen.createAgent(code, "MyAgent", null);
			assertNotNull(agent);
			assertEquals("MyAgent", agent.getName());
			assertTrue(agent.getSuperTypes().isEmpty());
			assertTrue(agent.getFeatures().isEmpty());
		}

		@Test	
		public void agentSuperClass()  {
			Agent agent = gen.createAgent(code, "MyAgent", "io.sarl.lang.core.Agent");
			assertNotNull(agent);
			assertEquals("MyAgent", agent.getName());
			assertTrue(agent.getSuperTypes().isEmpty());
			assertTrue(agent.getFeatures().isEmpty());
		}

		@Test	
		public void subagentSuperClass()  {
			when(jvmType.getIdentifier()).thenReturn("foo.ecore.SubAgent");
			when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn("foo.ecore.SubAgent");
			when(jvmType.getQualifiedName()).thenReturn("foo.ecore.SubAgent");
			when(jvmType.getSimpleName()).thenReturn("SubAgent");
			//
			Agent agent = gen.createAgent(code, "MyAgent", "foo.ecore.SubAgent");
			//
			assertNotNull(agent);
			assertEquals("MyAgent", agent.getName());
			assertTypeReferenceIdentifiers(agent.getSuperTypes(), "foo.ecore.SubAgent");
			assertTrue(agent.getFeatures().isEmpty());
		}

		@Test	
		public void otherSuperClass()  {
			when(jvmType.getIdentifier()).thenReturn("foo.Foo");
			when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn("foo.Foo");
			when(jvmType.getQualifiedName()).thenReturn("foo.Foo");
			when(jvmType.getSimpleName()).thenReturn("Foo");
			//
			Agent agent = gen.createAgent(code, "MyAgent", "foo.Foo");
			//
			assertNotNull(agent);
			assertEquals("MyAgent", agent.getName());
			assertTypeReferenceIdentifiers(agent.getSuperTypes(), "foo.Foo");
			assertTrue(agent.getFeatures().isEmpty());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class BehaviorTopElement extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private SarlScript context;

		@Nullable
		private ImportManager importManager;;

		@Nullable
		private JvmType jvmType;

		@Nullable
		private EList<TopElement> elements;

		@Before
		public void setUp() {
			elements = new BasicEList();
			importManager = new ImportManager();
			code = mock(GeneratedCode.class);
			context = mock(SarlScript.class);

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);
			jvmType = mock(JvmType.class);

			when(code.getImportManager()).thenReturn(importManager);
			when(code.getSarlScript()).thenReturn(context);
			when(typeProvider.findTypeByName(Matchers.anyString())).thenReturn(jvmType);
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(context.eResource()).thenReturn(eResource);
			when(context.getElements()).thenReturn(elements);
		}

		@Test	
		public void nullSuperClass()  {
			Behavior behavior = gen.createBehavior(code, "MyBehavior", null);
			assertNotNull(behavior);
			assertEquals("MyBehavior", behavior.getName());
			assertTrue(behavior.getSuperTypes().isEmpty());
			assertTrue(behavior.getFeatures().isEmpty());
		}

		@Test	
		public void behaviorSuperClass()  {
			Behavior behavior = gen.createBehavior(code, "MyBehavior", "io.sarl.lang.core.Behavior");
			assertNotNull(behavior);
			assertEquals("MyBehavior", behavior.getName());
			assertTrue(behavior.getSuperTypes().isEmpty());
			assertTrue(behavior.getFeatures().isEmpty());
		}

		@Test	
		public void subbehaviorSuperClass()  {
			when(jvmType.getIdentifier()).thenReturn("foo.ecore.SubBehavior");
			when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn("foo.ecore.SubBehavior");
			when(jvmType.getQualifiedName()).thenReturn("foo.ecore.SubBehavior");
			when(jvmType.getSimpleName()).thenReturn("SubBehavior");
			//
			Behavior behavior = gen.createBehavior(code, "MyBehavior", "foo.ecore.SubBehavior");
			//
			assertNotNull(behavior);
			assertEquals("MyBehavior", behavior.getName());
			assertTypeReferenceIdentifiers(behavior.getSuperTypes(), "foo.ecore.SubBehavior");
			assertTrue(behavior.getFeatures().isEmpty());
		}

		@Test	
		public void otherSuperClass()  {
			when(jvmType.getIdentifier()).thenReturn("foo.Foo");
			when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn("foo.Foo");
			when(jvmType.getQualifiedName()).thenReturn("foo.Foo");
			when(jvmType.getSimpleName()).thenReturn("Foo");
			//
			Behavior behavior = gen.createBehavior(code, "MyBehavior", "foo.Foo");
			//
			assertNotNull(behavior);
			assertEquals("MyBehavior", behavior.getName());
			assertTypeReferenceIdentifiers(behavior.getSuperTypes(), "foo.Foo");
			assertTrue(behavior.getFeatures().isEmpty());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class CapacityTopElement extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private SarlScript context;

		@Nullable
		private ImportManager importManager;;

		@Nullable
		private JvmType jvmType;

		@Nullable
		private EList<TopElement> elements;

		@Before
		public void setUp() {
			elements = new BasicEList();
			importManager = new ImportManager();
			code = mock(GeneratedCode.class);
			context = mock(SarlScript.class);

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);
			jvmType = mock(JvmType.class);

			when(code.getImportManager()).thenReturn(importManager);
			when(code.getSarlScript()).thenReturn(context);
			when(typeProvider.findTypeByName(Matchers.anyString())).thenReturn(jvmType);
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(context.eResource()).thenReturn(eResource);
			when(context.getElements()).thenReturn(elements);
		}

		@Test	
		public void nullSuperClass()  {
			Capacity capacity = gen.createCapacity(code, "MyCapacity", null);
			assertNotNull(capacity);
			assertEquals("MyCapacity", capacity.getName());
			assertTrue(capacity.getSuperTypes().isEmpty());
			assertTrue(capacity.getFeatures().isEmpty());
		}

		@Test	
		public void behaviorSuperClass()  {
			Capacity capacity = gen.createCapacity(code, "MyCapacity", "io.sarl.lang.core.Capacity");
			assertNotNull(capacity);
			assertEquals("MyCapacity", capacity.getName());
			assertTrue(capacity.getSuperTypes().isEmpty());
			assertTrue(capacity.getFeatures().isEmpty());
		}

		@Test	
		public void subbehaviorSuperClass()  {
			when(jvmType.getIdentifier()).thenReturn("foo.ecore.SubCapacity");
			when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn("foo.ecore.SubCapacity");
			when(jvmType.getQualifiedName()).thenReturn("foo.ecore.SubCapacity");
			when(jvmType.getSimpleName()).thenReturn("SubCapacity");
			//
			Capacity capacity = gen.createCapacity(code, "MyCapacity", "foo.ecore.SubCapacity");
			//
			assertNotNull(capacity);
			assertEquals("MyCapacity", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getSuperTypes(), "foo.ecore.SubCapacity");
			assertTrue(capacity.getFeatures().isEmpty());
		}

		@Test	
		public void otherSuperClass()  {
			when(jvmType.getIdentifier()).thenReturn("foo.Foo");
			when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn("foo.Foo");
			when(jvmType.getQualifiedName()).thenReturn("foo.Foo");
			when(jvmType.getSimpleName()).thenReturn("Foo");
			//
			Capacity capacity = gen.createCapacity(code, "MyCapacity", "foo.Foo");
			//
			assertNotNull(capacity);
			assertEquals("MyCapacity", capacity.getName());
			assertTypeReferenceIdentifiers(capacity.getSuperTypes(), "foo.Foo");
			assertTrue(capacity.getFeatures().isEmpty());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class EventTopElement extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private SarlScript context;

		@Nullable
		private ImportManager importManager;;

		@Nullable
		private JvmType jvmType;

		@Nullable
		private EList<TopElement> elements;

		@Before
		public void setUp() {
			elements = new BasicEList();
			importManager = new ImportManager();
			code = mock(GeneratedCode.class);
			context = mock(SarlScript.class);

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);
			jvmType = mock(JvmType.class);

			when(code.getImportManager()).thenReturn(importManager);
			when(code.getSarlScript()).thenReturn(context);
			when(typeProvider.findTypeByName(Matchers.anyString())).thenReturn(jvmType);
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(context.eResource()).thenReturn(eResource);
			when(context.getElements()).thenReturn(elements);
		}

		@Test	
		public void nullSuperClass()  {
			Event event = gen.createEvent(code, "MyEvent", null);
			assertNotNull(event);
			assertEquals("MyEvent", event.getName());
			assertTrue(event.getSuperTypes().isEmpty());
			assertTrue(event.getFeatures().isEmpty());
		}

		@Test	
		public void behaviorSuperClass()  {
			Event event = gen.createEvent(code, "MyEvent", "io.sarl.lang.core.Event");
			assertNotNull(event);
			assertEquals("MyEvent", event.getName());
			assertTrue(event.getSuperTypes().isEmpty());
			assertTrue(event.getFeatures().isEmpty());
		}

		@Test	
		public void subbehaviorSuperClass()  {
			when(jvmType.getIdentifier()).thenReturn("foo.ecore.SubEvent");
			when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn("foo.ecore.SubEvent");
			when(jvmType.getQualifiedName()).thenReturn("foo.ecore.SubEvent");
			when(jvmType.getSimpleName()).thenReturn("SubEvent");
			//
			Event event = gen.createEvent(code, "MyEvent", "foo.ecore.SubEvent");
			//
			assertNotNull(event);
			assertEquals("MyEvent", event.getName());
			assertTypeReferenceIdentifiers(event.getSuperTypes(), "foo.ecore.SubEvent");
			assertTrue(event.getFeatures().isEmpty());
		}

		@Test	
		public void otherSuperClass()  {
			when(jvmType.getIdentifier()).thenReturn("foo.Foo");
			when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn("foo.Foo");
			when(jvmType.getQualifiedName()).thenReturn("foo.Foo");
			when(jvmType.getSimpleName()).thenReturn("Foo");
			//
			Event event = gen.createEvent(code, "MyEvent", "foo.Foo");
			//
			assertNotNull(event);
			assertEquals("MyEvent", event.getName());
			assertTypeReferenceIdentifiers(event.getSuperTypes(), "foo.Foo");
			assertTrue(event.getFeatures().isEmpty());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class SkillTopElement extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private SarlScript context;

		@Nullable
		private ImportManager importManager;;

		@Nullable
		private EList<TopElement> elements;

		@Before
		public void setUp() {
			elements = new BasicEList();
			importManager = new ImportManager();
			code = mock(GeneratedCode.class);
			context = mock(SarlScript.class);

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);

			when(code.getImportManager()).thenReturn(importManager);
			when(code.getSarlScript()).thenReturn(context);
			when(typeProvider.findTypeByName(Matchers.anyString())).thenAnswer(new Answer<JvmType>() {
				@Override
				public JvmType answer(InvocationOnMock it)
						throws Throwable {
					String typeName = (String) it.getArguments()[0];
					int idx = typeName.lastIndexOf(".");
					String simpleName;
					if (idx >= 0) {
						simpleName = typeName.substring(idx + 1);
					} else {
						simpleName = typeName;
					}
					JvmType jvmType = mock(JvmType.class);
					when(jvmType.getIdentifier()).thenReturn(typeName);
					when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn(typeName);
					when(jvmType.getQualifiedName()).thenReturn(typeName);
					when(jvmType.getSimpleName()).thenReturn(simpleName);
					return jvmType;
				}
			});

			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(context.eResource()).thenReturn(eResource);
			when(context.getElements()).thenReturn(elements);
		}

		@Test	
		public void nullSuperClass_noSuperInterfaces()  {
			Skill skill = gen.createSkill(code, "MySkill", null, Collections.<String>emptyList());
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTrue(skill.getSuperTypes().isEmpty());
			assertTrue(skill.getImplementedTypes().isEmpty());
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void behaviorSuperClass_noSuperInterfaces()  {
			Skill skill = gen.createSkill(code, "MySkill", "io.sarl.lang.core.Skill", Collections.<String>emptyList());
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTrue(skill.getSuperTypes().isEmpty());
			assertTrue(skill.getImplementedTypes().isEmpty());
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void subbehaviorSuperClass_noSuperInterface()  {
			Skill skill = gen.createSkill(code, "MySkill", "foo.ecore.SubSkill", Collections.<String>emptyList());
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes(), "foo.ecore.SubSkill");
			assertTrue(skill.getImplementedTypes().isEmpty());
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void otherSuperClass_noSuperInterface()  {
			Skill skill = gen.createSkill(code, "MySkill", "foo.Foo", Collections.<String>emptyList());
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes(), "foo.Foo");
			assertTrue(skill.getImplementedTypes().isEmpty());
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void nullSuperClass_oneSuperInterfaces()  {
			Skill skill = gen.createSkill(code, "MySkill", null, Collections.singleton("foo.ecore.SubCapacity"));
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTrue(skill.getSuperTypes().isEmpty());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "foo.ecore.SubCapacity");
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void behaviorSuperClass_oneSuperInterfaces()  {
			Skill skill = gen.createSkill(code, "MySkill", "io.sarl.lang.core.Skill", Collections.singleton("foo.ecore.SubCapacity"));
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTrue(skill.getSuperTypes().isEmpty());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "foo.ecore.SubCapacity");
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void subbehaviorSuperClass_oneSuperInterface()  {
			Skill skill = gen.createSkill(code, "MySkill", "foo.ecore.SubSkill", Collections.singleton("foo.ecore.SubCapacity"));
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes(), "foo.ecore.SubSkill");
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "foo.ecore.SubCapacity");
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void otherSuperClass_oneSuperInterface()  {
			Skill skill = gen.createSkill(code, "MySkill", "foo.Foo", Collections.singleton("foo.ecore.SubCapacity"));
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes(), "foo.Foo");
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "foo.ecore.SubCapacity");
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void nullSuperClass_twoSuperInterfaces()  {
			Skill skill = gen.createSkill(code, "MySkill", null, Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"));
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTrue(skill.getSuperTypes().isEmpty());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2");
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void behaviorSuperClass_twoSuperInterfaces()  {
			Skill skill = gen.createSkill(code, "MySkill", "io.sarl.lang.core.Skill", Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"));
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTrue(skill.getSuperTypes().isEmpty());
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2");
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void subbehaviorSuperClass_twoSuperInterface()  {
			Skill skill = gen.createSkill(code, "MySkill", "foo.ecore.SubSkill", Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"));
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes(), "foo.ecore.SubSkill");
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2");
			assertTrue(skill.getFeatures().isEmpty());
		}

		@Test	
		public void otherSuperClass_twoSuperInterface()  {
			Skill skill = gen.createSkill(code, "MySkill", "foo.Foo", Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"));
			//
			assertNotNull(skill);
			assertEquals("MySkill", skill.getName());
			assertTypeReferenceIdentifiers(skill.getSuperTypes(), "foo.Foo");
			assertTypeReferenceIdentifiers(skill.getImplementedTypes(), "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2");
			assertTrue(skill.getFeatures().isEmpty());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class AgentFeatures extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private EList<EObject> features;;

		@Nullable
		private XBlockExpression block;;

		@Nullable
		private Agent agent;

		@Nullable
		private ImportManager importManager;;

		@Before
		public void setUp() {
			importManager = new ImportManager();
			features = new BasicEList();
			code = mock(GeneratedCode.class);
			agent = mock(Agent.class);
			block = XbaseFactory.eINSTANCE.createXBlockExpression();

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);

			when(typeProvider.findTypeByName(Matchers.anyString())).thenAnswer(new Answer<JvmType>() {
				@Override
				public JvmType answer(InvocationOnMock it) throws Throwable {
					String typeName = (String) it.getArguments()[0];
					int idx = typeName.lastIndexOf(".");
					String simpleName;
					if (idx >= 0) {
						simpleName = typeName.substring(idx + 1);
					} else {
						simpleName = typeName;
					}
					JvmType jvmType = mock(JvmType.class);
					when(jvmType.getIdentifier()).thenReturn(typeName);
					when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn(typeName);
					when(jvmType.getQualifiedName()).thenReturn(typeName);
					when(jvmType.getSimpleName()).thenReturn(simpleName);
					return jvmType;
				}
			});

			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(agent.eResource()).thenReturn(eResource);
			when(agent.getFeatures()).thenReturn(features);
			when(code.getImportManager()).thenReturn(importManager);
		}

		@Test
		public void returnNull()  {
			Action action = gen.createAction(code, agent, "myFct", null, block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertNull(action.getType());
			assertTrue(action.getParams().isEmpty());
			assertSame(block, action.getBody());
		}

		@Test
		public void returnBoolean()  {
			Action action = gen.createAction(code, agent, "myFct", "boolean", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "boolean");
			assertTrue(action.getParams().isEmpty());
			assertSame(block, action.getBody());
		}

		@Test
		public void returnObject()  {
			Action action = gen.createAction(code, agent, "myFct", "java.lang.String", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			assertTrue(action.getParams().isEmpty());
			assertSame(block, action.getBody());
		}

		@Test
		public void createConstructor()  {
			Constructor constructor = gen.createConstructor(code, agent, block);
			//
			assertNotNull(constructor);
			assertTrue(constructor.getParams().isEmpty());
			assertSame(block, constructor.getBody());
		}

		@Test
		public void createBehaviorUnit_noGuard()  {
			BehaviorUnit unit = gen.createBehaviorUnit(code, agent, "foo.ecore.SubEvent", null, block);
			//
			assertNotNull(unit);
			assertTypeReferenceIdentifier(unit.getName(), "foo.ecore.SubEvent");
			assertNull(unit.getGuard());
			assertSame(block, unit.getBody());
		}

		@Test
		public void createBehaviorUnit_aGuard()  {
			XBlockExpression guard = XbaseFactory.eINSTANCE.createXBlockExpression();
			//
			BehaviorUnit unit = gen.createBehaviorUnit(code, agent, "foo.ecore.SubEvent", guard, block);
			//
			assertNotNull(unit);
			assertTypeReferenceIdentifier(unit.getName(), "foo.ecore.SubEvent");
			assertSame(guard, unit.getGuard());
			assertSame(block, unit.getBody());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createVariable(code, agent, "myVar", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createVariable(code, agent, "myVar", (XExpression) null);
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringString()  {
			Attribute variable = gen.createVariable(code, agent, "myVar", "java.lang.String");
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertTypeReferenceIdentifier(variable.getType(), "java.lang.String");
			assertNull(variable.getInitialValue());
			assertTrue(variable.isWriteable());
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			Attribute variable = gen.createVariable(code, agent, "myVar", numberLiteral);
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertNull(variable.getType());
			assertSame(numberLiteral, variable.getInitialValue());
			assertTrue(variable.isWriteable());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createValue(code, agent, "myConst", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createValue(code, agent, "myConst", (XExpression) null);
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringString()  {
			Attribute value = gen.createValue(code, agent, "myConst", "java.lang.String");
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertTypeReferenceIdentifier(value.getType(), "java.lang.String");
			assertNull(value.getInitialValue());
			assertFalse(value.isWriteable());
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral  numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			Attribute value = gen.createValue(code, agent, "myConst", numberLiteral);
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertNull(value.getType());
			assertSame(numberLiteral, value.getInitialValue());
			assertFalse(value.isWriteable());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class BehaviorFeatures extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private EList<EObject> features;;

		@Nullable
		private XBlockExpression block;;

		@Nullable
		private Behavior behavior;

		@Nullable
		private ImportManager importManager;;

		@Before
		public void setUp() {
			importManager = new ImportManager();
			features = new BasicEList();
			code = mock(GeneratedCode.class);
			behavior = mock(Behavior.class);
			block = XbaseFactory.eINSTANCE.createXBlockExpression();

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);

			when(typeProvider.findTypeByName(Matchers.anyString())).thenAnswer(new Answer<JvmType>() {
				@Override
				public JvmType answer(InvocationOnMock it) throws Throwable {
					String typeName = (String) it.getArguments()[0];
					int idx = typeName.lastIndexOf(".");
					String simpleName;
					if (idx >= 0) {
						simpleName = typeName.substring(idx + 1);
					} else {
						simpleName = typeName;
					}
					JvmType jvmType = mock(JvmType.class);
					when(jvmType.getIdentifier()).thenReturn(typeName);
					when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn(typeName);
					when(jvmType.getQualifiedName()).thenReturn(typeName);
					when(jvmType.getSimpleName()).thenReturn(simpleName);
					return jvmType;
				}
			});
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(behavior.eResource()).thenReturn(eResource);
			when(behavior.getFeatures()).thenReturn(features);
			when(code.getImportManager()).thenReturn(importManager);
		}

		@Test
		public void returnNull()  {
			Action action = gen.createAction(code, behavior, "myFct", null, block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertNull(action.getType());
			assertTrue(action.getParams().isEmpty());
			assertSame(block, action.getBody());
		}

		@Test
		public void returnBoolean()  {
			Action action = gen.createAction(code, behavior, "myFct", "boolean", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "boolean");
			assertTrue(action.getParams().isEmpty());
			assertSame(block, action.getBody());
		}

		@Test
		public void returnObject()  {
			Action action = gen.createAction(code, behavior, "myFct", "java.lang.String", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			assertTrue(action.getParams().isEmpty());
			assertSame(block, action.getBody());
		}

		@Test
		public void createConstructor()  {
			Constructor constructor = gen.createConstructor(code, behavior, block);
			//
			assertNotNull(constructor);
			assertTrue(constructor.getParams().isEmpty());
			assertSame(block, constructor.getBody());
		}

		@Test
		public void createBehaviorUnit_noGuard()  {
			BehaviorUnit unit = gen.createBehaviorUnit(code, behavior, "foo.ecore.SubEvent", null, block);
			//
			assertNotNull(unit);
			assertTypeReferenceIdentifier(unit.getName(), "foo.ecore.SubEvent");
			assertNull(unit.getGuard());
			assertSame(block, unit.getBody());
		}

		@Test
		public void createBehaviorUnit_aGuard()  {
			XBlockExpression guard = XbaseFactory.eINSTANCE.createXBlockExpression();
			//
			BehaviorUnit unit = gen.createBehaviorUnit(code, behavior, "foo.ecore.SubEvent", guard, block);
			//
			assertNotNull(unit);
			assertTypeReferenceIdentifier(unit.getName(), "foo.ecore.SubEvent");
			assertSame(guard, unit.getGuard());
			assertSame(block, unit.getBody());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createVariable(code, behavior, "myVar", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createVariable(code, behavior, "myVar", (XExpression) null);
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringString()  {
			Attribute variable = gen.createVariable(code, behavior, "myVar", "java.lang.String");
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertTypeReferenceIdentifier(variable.getType(), "java.lang.String");
			assertNull(variable.getInitialValue());
			assertTrue(variable.isWriteable());
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			Attribute variable = gen.createVariable(code, behavior, "myVar", numberLiteral);
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertNull(variable.getType());
			assertSame(numberLiteral, variable.getInitialValue());
			assertTrue(variable.isWriteable());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createValue(code, behavior, "myConst", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createValue(code, behavior, "myConst", (XExpression) null);
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringString()  {
			Attribute value = gen.createValue(code, behavior, "myConst", "java.lang.String");
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertTypeReferenceIdentifier(value.getType(), "java.lang.String");
			assertNull(value.getInitialValue());
			assertFalse(value.isWriteable());
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			Attribute value = gen.createValue(code, behavior, "myConst", numberLiteral);
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertNull(value.getType());
			assertSame(numberLiteral, value.getInitialValue());
			assertFalse(value.isWriteable());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class CapacityFeatures extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;;

		@Nullable
		private GeneratedCode code;;

		@Nullable
		private EList<EObject> features;;

		@Nullable
		private Capacity capacity;

		@Nullable
		private ImportManager importManager;;

		@Before
		public void setUp() {
			importManager = new ImportManager();
			features = new BasicEList();
			code = mock(GeneratedCode.class);
			capacity = mock(Capacity.class);

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);

			when(typeProvider.findTypeByName(Matchers.anyString())).thenAnswer(new Answer<JvmType>(){
				@Override
				public JvmType answer(InvocationOnMock it) throws Throwable {
					String typeName = (String) it.getArguments()[0];
					int idx = typeName.lastIndexOf(".");
					String simpleName;
					if (idx >= 0) {
						simpleName = typeName.substring(idx + 1);
					} else {
						simpleName = typeName;
					}
					JvmType jvmType = mock(JvmType.class);
					when(jvmType.getIdentifier()).thenReturn(typeName);
					when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn(typeName);
					when(jvmType.getQualifiedName()).thenReturn(typeName);
					when(jvmType.getSimpleName()).thenReturn(simpleName);
					return jvmType;
				}
			}); 
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(capacity.eResource()).thenReturn(eResource);
			when(capacity.getFeatures()).thenReturn(features);
			when(code.getImportManager()).thenReturn(importManager);
		}

		@Test
		public void returnNull()  {
			ActionSignature action = gen.createActionSignature(code, capacity, "myFct", null);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertNull(action.getType());
			assertTrue(action.getParams().isEmpty());
		}

		@Test
		public void returnBoolean()  {
			ActionSignature action = gen.createActionSignature(code, capacity, "myFct", "boolean");
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "boolean");
			assertTrue(action.getParams().isEmpty());
		}

		@Test
		public void returnObject()  {
			ActionSignature action = gen.createActionSignature(code, capacity, "myFct", "java.lang.String");
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			assertTrue(action.getParams().isEmpty());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class EventFeatures extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;

		@Nullable
		private GeneratedCode code;

		@Nullable
		private EList<EObject> features;

		@Nullable
		private XBlockExpression block;

		@Nullable
		private Event event;

		@Nullable
		private ImportManager importManager;

		@Before
		public void setUp() {
			importManager = new ImportManager();
			features = new BasicEList();
			code = mock(GeneratedCode.class);
			event = mock(Event.class);
			block = XbaseFactory.eINSTANCE.createXBlockExpression();

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);

			when(typeProvider.findTypeByName(Matchers.anyString())).thenAnswer(new Answer<JvmType>() {
				@Override
				public JvmType answer(InvocationOnMock it) throws Throwable {
					String typeName = (String) it.getArguments()[0];
					int idx = typeName.lastIndexOf(".");
					String simpleName;
					if (idx >= 0) {
						simpleName = typeName.substring(idx + 1);
					} else {
						simpleName = typeName;
					}
					JvmType jvmType = mock(JvmType.class);
					when(jvmType.getIdentifier()).thenReturn(typeName);
					when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn(typeName);
					when(jvmType.getQualifiedName()).thenReturn(typeName);
					when(jvmType.getSimpleName()).thenReturn(simpleName);
					return jvmType;
				}
			});
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(event.eResource()).thenReturn(eResource);
			when(event.getFeatures()).thenReturn(features);
			when(code.getImportManager()).thenReturn(importManager);
		}

		@Test
		public void createConstructor()  {
			Constructor constructor = gen.createConstructor(code, event, block);
			//
			assertNotNull(constructor);
			assertTrue(constructor.getParams().isEmpty());
			assertSame(block, constructor.getBody());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createVariable(code, event, "myVar", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createVariable(code, event, "myVar", (XExpression) null);
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringString()  {
			Attribute variable = gen.createVariable(code, event, "myVar", "java.lang.String");
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertTypeReferenceIdentifier(variable.getType(), "java.lang.String");
			assertNull(variable.getInitialValue());
			assertTrue(variable.isWriteable());
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			Attribute variable = gen.createVariable(code, event, "myVar", numberLiteral);
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertNull(variable.getType());
			assertSame(numberLiteral, variable.getInitialValue());
			assertTrue(variable.isWriteable());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createValue(code, event, "myConst", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createValue(code, event, "myConst", (XExpression) null);
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringString()  {
			Attribute value = gen.createValue(code, event, "myConst", "java.lang.String");
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertTypeReferenceIdentifier(value.getType(), "java.lang.String");
			assertNull(value.getInitialValue());
			assertFalse(value.isWriteable());
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			Attribute value = gen.createValue(code, event, "myConst", numberLiteral);
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertNull(value.getType());
			assertSame(numberLiteral, value.getInitialValue());
			assertFalse(value.isWriteable());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class SkillFeatures extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;

		@Nullable
		private GeneratedCode code;

		@Nullable
		private EList<EObject> features;

		@Nullable
		private XBlockExpression block;

		@Nullable
		private Skill skill;

		@Nullable
		private ImportManager importManager;

		@Before
		public void setUp() {
			importManager = new ImportManager();
			features = new BasicEList();
			code = mock(GeneratedCode.class);
			skill = mock(Skill.class);
			block = XbaseFactory.eINSTANCE.createXBlockExpression();

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);

			when(typeProvider.findTypeByName(Matchers.anyString())).thenAnswer(new Answer<JvmType>() {
				@Override
				public JvmType answer(InvocationOnMock it) throws Throwable {
					String typeName = (String) it.getArguments()[0];
					int idx = typeName.lastIndexOf(".");
					String simpleName;
					if (idx >= 0) {
						simpleName = typeName.substring(idx + 1);
					} else {
						simpleName = typeName;
					}
					JvmType jvmType = mock(JvmType.class);
					when(jvmType.getIdentifier()).thenReturn(typeName);
					when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn(typeName);
					when(jvmType.getQualifiedName()).thenReturn(typeName);
					when(jvmType.getSimpleName()).thenReturn(simpleName);
					return jvmType;

				}
			});
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(skill.eResource()).thenReturn(eResource);
			when(skill.getFeatures()).thenReturn(features);
			when(code.getImportManager()).thenReturn(importManager);
		}

		@Test
		public void returnNull()  {
			Action action = gen.createAction(code, skill, "myFct", null, block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertNull(action.getType());
			assertTrue(action.getParams().isEmpty());
			assertSame(block, action.getBody());
		}

		@Test
		public void returnBoolean()  {
			Action action = gen.createAction(code, skill, "myFct", "boolean", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "boolean");
			assertTrue(action.getParams().isEmpty());
			assertSame(block, action.getBody());
		}

		@Test
		public void returnObject()  {
			Action action = gen.createAction(code, skill, "myFct", "java.lang.String", block);
			//
			assertNotNull(action);
			assertEquals("myFct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			assertTrue(action.getParams().isEmpty());
			assertSame(block, action.getBody());
		}

		@Test
		public void createConstructor()  {
			Constructor constructor = gen.createConstructor(code, skill, block);
			//
			assertNotNull(constructor);
			assertTrue(constructor.getParams().isEmpty());
			assertSame(block, constructor.getBody());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createVariable(code, skill, "myVar", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createVariable(code, skill, "myVar", (XExpression) null);
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringString()  {
			Attribute variable = gen.createVariable(code, skill, "myVar", "java.lang.String");
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertTypeReferenceIdentifier(variable.getType(), "java.lang.String");
			assertNull(variable.getInitialValue());
			assertTrue(variable.isWriteable());
		}

		@Test
		public void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			Attribute variable = gen.createVariable(code, skill, "myVar", numberLiteral);
			//
			assertNotNull(variable);
			assertEquals("myVar", variable.getName());
			assertNull(variable.getType());
			assertSame(numberLiteral, variable.getInitialValue());
			assertTrue(variable.isWriteable());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createValue(code, skill, "myConst", (String) null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createValue(code, skill, "myConst", (XExpression) null);
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringString()  {
			Attribute value = gen.createValue(code, skill, "myConst", "java.lang.String");
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertTypeReferenceIdentifier(value.getType(), "java.lang.String");
			assertNull(value.getInitialValue());
			assertFalse(value.isWriteable());
		}

		@Test
		public void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			XNumberLiteral numberLiteral = XbaseFactory.eINSTANCE.createXNumberLiteral();
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			Attribute value = gen.createValue(code, skill, "myConst", numberLiteral);
			//
			assertNotNull(value);
			assertEquals("myConst", value.getName());
			assertNull(value.getType());
			assertSame(numberLiteral, value.getInitialValue());
			assertFalse(value.isWriteable());
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class Expressions extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;

		@Nullable
		private GeneratedCode code;

		@Nullable
		private ResourceSet eResourceSet;

		@Before
		public void setUp() {
			code = mock(GeneratedCode.class);
			eResourceSet = mock(ResourceSet.class);

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);

			when(typeProvider.findTypeByName(Matchers.anyString())).thenAnswer(new Answer<JvmType>() {
				@Override
				public JvmType answer(InvocationOnMock it) throws Throwable {
					String typeName = (String) it.getArguments()[0];
					int idx = typeName.lastIndexOf(".");
					String simpleName;
					if (idx >= 0) {
						simpleName = typeName.substring(idx + 1);
					} else {
						simpleName = typeName;
					}
					JvmType jvmType = mock(JvmType.class);
					when(jvmType.getIdentifier()).thenReturn(typeName);
					when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn(typeName);
					when(jvmType.getQualifiedName()).thenReturn(typeName);
					when(jvmType.getSimpleName()).thenReturn(simpleName);
					return jvmType;
				}
			});
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
		}

		@Test
		public void createXExpression_null()  {
			XExpression expr = gen.createXExpression(null, eResourceSet);
			//
			assertNull(expr);
		}

		@Test
		public void createXExpression_empty()  {
			XExpression expr = gen.createXExpression("", eResourceSet);
			//
			assertNull(expr);
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class FormalParameters extends AbstractSarlTest {

		@Inject
		private SARLCodeGenerator gen;

		@Nullable
		private GeneratedCode code;

		@Nullable
		private EObject context;

		@Nullable
		private ParameterizedFeature container;

		@Nullable
		private ImportManager importManager;

		@Nullable
		private EList<Resource> resources;

		@Before
		public void setUp() {
			importManager = new ImportManager();
			code = mock(GeneratedCode.class);
			context = mock(EObject.class);
			container = mock(ParameterizedFeature.class);

			Resource eResource = mock(Resource.class);
			ResourceSet eResourceSet = mock(ResourceSet.class);
			Resource.Factory.Registry registry = mock(Resource.Factory.Registry.class);
			Map factoryMap = mock(Map.class);
			IJvmTypeProvider typeProvider = mock(IJvmTypeProvider.class);

			when(typeProvider.findTypeByName(Matchers.anyString())).thenAnswer(new Answer<JvmType>() {
				@Override
				public JvmType answer(InvocationOnMock it) throws Throwable {
					String typeName = (String) it.getArguments()[0];
					int idx = typeName.lastIndexOf(".");
					String simpleName;
					if (idx >= 0) {
						simpleName = typeName.substring(idx + 1);
					} else {
						simpleName = typeName;
					}
					JvmType jvmType = mock(JvmType.class);
					when(jvmType.getIdentifier()).thenReturn(typeName);
					when(jvmType.getQualifiedName(Matchers.anyChar())).thenReturn(typeName);
					when(jvmType.getQualifiedName()).thenReturn(typeName);
					when(jvmType.getSimpleName()).thenReturn(simpleName);
					return jvmType;
				}
			});
			when(factoryMap.get(Matchers.any())).thenReturn(typeProvider);
			when(registry.getProtocolToFactoryMap()).thenReturn(factoryMap);
			when(eResourceSet.getResourceFactoryRegistry()).thenReturn(registry);
			when(eResourceSet.getResource(Matchers.any(URI.class), Matchers.anyBoolean())).thenReturn(null);
			when(eResourceSet.getResources()).thenReturn(resources);
			when(eResource.getResourceSet()).thenReturn(eResourceSet);
			when(container.eResource()).thenReturn(eResource);
			when(container.getParams()).thenReturn(new BasicEList());
			when(code.getImportManager()).thenReturn(importManager);
			when(code.getResourceSet()).thenReturn(eResourceSet);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createVarArgs_nullType()  {
			gen.createVarArgs(code, container, "myParam", null);
		}

		@Test
		public void createVarArgs()  {
			FormalParameter param = gen.createVarArgs(code, container, "myParam", "boolean");
			//
			assertNotNull(param);
			assertEquals("myParam", param.getName());
			assertNull(param.getDefaultValue());
			assertTypeReferenceIdentifier(param.getParameterType(), "boolean");
			//
			assertParameterNames(container.getParams(), "myParam");
			//
			ArgumentCaptor<Boolean> arg = ArgumentCaptor.forClass(boolean.class);
			verify(container).setVarargs(arg.capture());
			assertTrue(arg.getValue());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringStringResourceSet_nullType_noDefaultValue()  {
			gen.createFormalParameter(code, container, "myParam", null, null, code.getResourceSet());
		}

		@Test(expected = IllegalArgumentException.class)
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringStringResourceSet_nullType_defaultValue()  {
			gen.createFormalParameter(code, container, "myParam", null, "true", code.getResourceSet());
		}

		@Test
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringStringResourceSet_noDefaultValue()  {
			FormalParameter param = gen.createFormalParameter(code, container, "myParam", "java.lang.String", null, code.getResourceSet());
			//
			assertNotNull(param);
			assertEquals("myParam", param.getName());
			assertNull(param.getDefaultValue());
			assertTypeReferenceIdentifier(param.getParameterType(), "java.lang.String");
			//
			assertParameterNames(container.getParams(), "myParam");
		}

		@Test(expected = IllegalArgumentException.class)
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_nullType_noDefaultValue()  {
			gen.createFormalParameter(code, container, "myParam", null, null);
		}

		@Test(expected = IllegalArgumentException.class)
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_nullType_defaultValue()  {
			XStringLiteral expr = XbaseFactory.eINSTANCE.createXStringLiteral();
			expr.setValue("abc");
			gen.createFormalParameter(code, container, "myParam", null, expr);
		}

		@Test
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_noDefaultValue()  {
			FormalParameter param = gen.createFormalParameter(code, container, "myParam", "java.lang.String", null);
			//
			assertNotNull(param);
			assertEquals("myParam", param.getName());
			assertNull(param.getDefaultValue());
			assertTypeReferenceIdentifier(param.getParameterType(), "java.lang.String");
			//
			assertParameterNames(container.getParams(), "myParam");
		}

		@Test
		public void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_defaultValue()  {
			XStringLiteral expr = XbaseFactory.eINSTANCE.createXStringLiteral();
			expr.setValue("abc");
			FormalParameter param = gen.createFormalParameter(code, container, "myParam", "java.lang.String", expr);
			//
			assertNotNull(param);
			assertEquals("myParam", param.getName());
			assertSame(expr, param.getDefaultValue());
			assertTypeReferenceIdentifier(param.getParameterType(), "java.lang.String");
			//
			assertParameterNames(container.getParams(), "myParam");
		}

	}

	@InjectWith(SARLInjectorProvider.class)
	public static class CreateActionFromJvmElement extends AbstractSarlUiTest {

		@Inject
		private SARLCodeGenerator generator;

		/** Associator of the JVM elements and the SARL elements.
		 */
		@Inject
		protected JvmModelAssociator jvmModelAssociator;

		private JvmOperation createJvmFeature(String... sarlCode) throws Exception {
			String sarlFilename = generateFilename();
			SarlScript sarlScript = this.helper.createSARLScript(sarlFilename, multilineString(sarlCode));
			this.helper.waitForAutoBuild();
			EObject feature = ((FeatureContainer) sarlScript.getElements().get(0)).getFeatures().get(0);
			EObject jvmElement = this.jvmModelAssociator.getPrimaryJvmElement(feature);
			return (JvmOperation) jvmElement;
		}

		@Test
		public void noParam_noReturn() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"agent A1 {",
					"	def fct {}",
					"}");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(0, action.getParams().size());
		}

		@Test
		public void stdParam_noReturn() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int) {}",
					"}");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void variadicParam_noReturn() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int*) {}",
					"}");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertTrue(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void defaultValue_noReturn() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int=4, c : char) {}",
					"}");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void variadicParam_defaultValue_noReturn() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int=4, c : char*) {}",
					"}");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertTrue(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void noParam_returnValue() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"agent A1 {",
					"	def fct : String { null }",
					"}");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(0, action.getParams().size());
		}

		@Test
		public void stdParam_returnValue() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int) : String { null }",
					"}");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void variadicParam_returnValue() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int*) : String { null }",
					"}");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertTrue(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void defaultValue_returnValue() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int=4, c : char) : String { null }",
					"}");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void variadicParam_defaultValue_returnValue() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int=4, c : char*) : String { null }",
					"}");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertTrue(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void noParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"agent A1 {",
					"	def fct fires MyEvent {}",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(0, action.getParams().size());
		}

		@Test
		public void stdParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int) fires MyEvent {}",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void variadicParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int*) fires MyEvent {}",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertTrue(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void defaultValue_noReturn_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int=4, c : char) fires MyEvent {}",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void variadicParam_defaultValue_noReturn_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int=4, c : char*) fires MyEvent {}",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertTrue(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void noParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"agent A1 {",
					"	def fct : String fires MyEvent { null }",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(0, action.getParams().size());
		}

		@Test
		public void stdParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int) : String fires MyEvent { null }",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void variadicParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int*) : String fires MyEvent { null }",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertTrue(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void defaultValue_returnValue_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int=4, c : char) : String fires MyEvent { null }",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void variadicParam_defaultValue_returnValue_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"agent A1 {",
					"	def fct(a : URL, b : int=4, c : char*) : String fires MyEvent { null }",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createAction(operation, false);
			//
			assertNotNull(feature);
			Action action = (Action) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertTrue(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

	}

	@InjectWith(SARLInjectorProvider.class)
	public static class CreateConstructorFromJvmElement extends AbstractSarlUiTest {

		@Inject
		private SARLCodeGenerator generator;

		/** Associator of the JVM elements and the SARL elements.
		 */
		@Inject
		protected JvmModelAssociator jvmModelAssociator;

		private JvmConstructor createJvmFeature(String... sarlCode) throws Exception {
			String sarlFilename = generateFilename();
			SarlScript sarlScript = this.helper.createSARLScript(sarlFilename, multilineString(sarlCode));
			this.helper.waitForAutoBuild();
			EObject feature = ((FeatureContainer) sarlScript.getElements().get(0)).getFeatures().get(0);
			EObject jvmElement = this.jvmModelAssociator.getPrimaryJvmElement(feature);
			return (JvmConstructor) jvmElement;
		}

		@Test
		public void noParam() throws Exception {
			JvmConstructor cons = createJvmFeature(
					// Code
					"event E1 {",
					"	new() {}",
					"}");
			//
			ParameterizedFeature feature = this.generator.createConstructor(cons, false);
			//
			assertNotNull(feature);
			Constructor action = (Constructor) feature;
			//
			assertFalse(action.isVarargs());
			assertEquals(0, action.getParams().size());
		}

		@Test
		public void stdParam() throws Exception {
			JvmConstructor cons = createJvmFeature(
					// Code
					"import java.net.URL",
					"event E1 {",
					"	new (a : URL, b : int) {}",
					"}");
			//
			ParameterizedFeature feature = this.generator.createConstructor(cons, false);
			//
			assertNotNull(feature);
			Constructor action = (Constructor) feature;
			//
			assertFalse(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void variadicParam() throws Exception {
			JvmConstructor cons = createJvmFeature(
					// Code
					"import java.net.URL",
					"event E1 {",
					"	new (a : URL, b : int*) {}",
					"}");
			//
			ParameterizedFeature feature = this.generator.createConstructor(cons, false);
			//
			assertNotNull(feature);
			Constructor action = (Constructor) feature;
			//
			assertTrue(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void defaultValue() throws Exception {
			JvmConstructor cons = createJvmFeature(
					// Code
					"import java.net.URL",
					"event E1 {",
					"	new (a : URL, b : int=4, c : char) {}",
					"}");
			//
			ParameterizedFeature feature = this.generator.createConstructor(cons, false);
			//
			assertNotNull(feature);
			Constructor action = (Constructor) feature;
			//
			assertFalse(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void variadicParam_defaultValue() throws Exception {
			JvmConstructor cons = createJvmFeature(
					// Code
					"import java.net.URL",
					"event E1 {",
					"	new (a : URL, b : int=4, c : char*) {}",
					"}");
			//
			ParameterizedFeature feature = this.generator.createConstructor(cons, false);
			//
			assertNotNull(feature);
			Constructor action = (Constructor) feature;
			//
			assertTrue(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

	}

	@InjectWith(SARLInjectorProvider.class)
	public static class CreateActionSignatureFromJvmElement extends AbstractSarlUiTest {

		@Inject
		private SARLCodeGenerator generator;

		/** Associator of the JVM elements and the SARL elements.
		 */
		@Inject
		protected JvmModelAssociator jvmModelAssociator;

		private JvmOperation createJvmFeature(String... sarlCode) throws Exception {
			String sarlFilename = generateFilename();
			SarlScript sarlScript = this.helper.createSARLScript(sarlFilename, multilineString(sarlCode));
			this.helper.waitForAutoBuild();
			EObject feature = ((FeatureContainer) sarlScript.getElements().get(0)).getFeatures().get(0);
			EObject jvmElement = this.jvmModelAssociator.getPrimaryJvmElement(feature);
			return (JvmOperation) jvmElement;
		}

		@Test
		public void noParam_noReturn() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"capacity C1 {",
					"	def fct",
					"}");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(0, action.getParams().size());
		}

		@Test
		public void stdParam_noReturn() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int)",
					"}");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void variadicParam_noReturn() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int*)",
					"}");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertTrue(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void defaultValue_noReturn() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int=4, c : char)",
					"}");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void variadicParam_defaultValue_noReturn() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int=4, c : char*)",
					"}");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertTrue(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void noParam_returnValue() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"capacity C1 {",
					"	def fct : String",
					"}");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(0, action.getParams().size());
		}

		@Test
		public void stdParam_returnValue() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int) : String",
					"}");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void variadicParam_returnValue() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int*) : String",
					"}");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertTrue(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void defaultValue_returnValue() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int=4, c : char) : String",
					"}");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertFalse(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void variadicParam_defaultValue_returnValue() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int=4, c : char*) : String",
					"}");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(0, action.getFiredEvents().size());
			//
			assertTrue(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void noParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"capacity C1 {",
					"	def fct fires MyEvent",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(0, action.getParams().size());
		}

		@Test
		public void stdParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int) fires MyEvent",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void variadicParam_noReturn_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int*) fires MyEvent",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertTrue(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void defaultValue_noReturn_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int=4, c : char) fires MyEvent",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void variadicParam_defaultValue_noReturn_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int=4, c : char*) fires MyEvent",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "void");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertTrue(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void noParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"capacity C1 {",
					"	def fct : String fires MyEvent",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(0, action.getParams().size());
		}

		@Test
		public void stdParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int) : String fires MyEvent",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void variadicParam_returnValue_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int*) : String fires MyEvent",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertTrue(action.isVarargs());
			assertEquals(2, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b");
			assertParameterTypes(action.getParams(), "java.net.URL", "int");
			assertParameterDefaultValues(action.getParams(),
					null,
					null);
		}

		@Test
		public void defaultValue_returnValue_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int=4, c : char) : String fires MyEvent",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertFalse(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

		@Test
		public void variadicParam_defaultValue_returnValue_fireEvents() throws Exception {
			JvmOperation operation = createJvmFeature(
					// Code
					"import java.net.URL",
					"capacity C1 {",
					"	def fct(a : URL, b : int=4, c : char*) : String fires MyEvent",
					"}",
					"event MyEvent");
			//
			ParameterizedFeature feature = this.generator.createActionSignature(operation, false);
			//
			assertNotNull(feature);
			ActionSignature action = (ActionSignature) feature;
			assertEquals("fct", action.getName());
			assertTypeReferenceIdentifier(action.getType(), "java.lang.String");
			//
			assertEquals(1, action.getFiredEvents().size());
			assertTypeReferenceIdentifiers(
					action.getFiredEvents(),
					"MyEvent");
			//
			assertTrue(action.isVarargs());
			assertEquals(3, action.getParams().size());
			assertParameterNames(action.getParams(), "a", "b", "c");
			assertParameterTypes(action.getParams(), "java.net.URL", "int", "char");
			assertParameterDefaultValues(action.getParams(),
					null,
					XNumberLiteral.class, "4",
					null);
		}

	}

}
