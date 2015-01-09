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
package io.sarl.lang.tests.genmodel

import com.google.common.collect.Iterables
import com.google.inject.Inject
import io.sarl.lang.SARLInjectorProvider
import io.sarl.lang.genmodel.SARLCodeGenerator
import io.sarl.lang.genmodel.SARLCodeGenerator.BlockInnerDocumentationAdapter
import io.sarl.lang.genmodel.SARLCodeGenerator.GeneratedCode
import io.sarl.lang.genmodel.SARLCodeGenerator.PostDocumentationAdapter
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.Behavior
import io.sarl.lang.sarl.Capacity
import io.sarl.lang.sarl.Event
import io.sarl.lang.sarl.FormalParameter
import io.sarl.lang.sarl.ParameterizedFeature
import io.sarl.lang.sarl.SarlScript
import io.sarl.lang.sarl.Skill
import io.sarl.lang.sarl.TopElement
import io.sarl.lang.signature.ActionSignatureProvider
import io.sarl.tests.api.AbstractSarlTest
import io.sarl.tests.api.Nullable
import java.util.Arrays
import java.util.Collections
import java.util.Map
import org.eclipse.emf.common.notify.Adapter
import org.eclipse.emf.common.util.BasicEList
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.impl.EObjectImpl
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.ResourceSet
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.TypesFactory
import org.eclipse.xtext.common.types.access.IJvmTypeProvider
import org.eclipse.xtext.common.types.util.TypeReferences
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.resource.IResourceFactory
import org.eclipse.xtext.serializer.ISerializer
import org.eclipse.xtext.xbase.XBlockExpression
import org.eclipse.xtext.xbase.XExpression
import org.eclipse.xtext.xbase.XbaseFactory
import org.eclipse.xtext.xbase.compiler.DocumentationAdapter
import org.eclipse.xtext.xbase.compiler.ImportManager
import org.eclipse.xtext.xbase.impl.XBlockExpressionImpl
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses
import org.mockito.ArgumentCaptor
import org.mockito.Matchers

import static io.sarl.lang.tests.genmodel.SARLCodeGeneratorTest.*
import static org.junit.Assert.*
import static org.mockito.Mockito.*

/**
 * @author $Author: srodriguez$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite)
@SuiteClasses(#[
	SARLCodeGeneratorTest.InjectedAttributes,
	SARLCodeGeneratorTest.DefaultTypeValue,
	SARLCodeGeneratorTest.References,
	SARLCodeGeneratorTest.Comments,
	SARLCodeGeneratorTest.ScriptLevel,
	SARLCodeGeneratorTest.AgentTopElement,
	SARLCodeGeneratorTest.BehaviorTopElement,
	SARLCodeGeneratorTest.CapacityTopElement,
	SARLCodeGeneratorTest.EventTopElement,
	SARLCodeGeneratorTest.SkillTopElement,
	SARLCodeGeneratorTest.AgentFeatures,
	SARLCodeGeneratorTest.BehaviorFeatures,
	SARLCodeGeneratorTest.CapacityFeatures,
	SARLCodeGeneratorTest.EventFeatures,
	SARLCodeGeneratorTest.SkillFeatures,
	SARLCodeGeneratorTest.Expressions,
	SARLCodeGeneratorTest.FormalParameters
])
class SARLCodeGeneratorTest {
	
	def static void assertTypeReferenceIdentifiers(Iterable<? extends JvmTypeReference> elements, String... identifiers) {
		var i = 0
		for (JvmTypeReference reference : elements) {
			assertTypeReferenceIdentifier(reference, identifiers.get(i))
			i = i + 1
		}
		if (i < identifiers.length) {
			fail("Not enough identifiers. Expected: " + Arrays::toString(identifiers)
				+ "Actual: " + Iterables::toString(elements))
		}
	}

	def static void assertTypeReferenceIdentifier(JvmTypeReference element, String identifier) {
		assertEquals("Unexpected type reference: " + element.identifier + ". Expected: " + identifier,
			identifier, element.identifier)
	}

	def static void assertParameters(Iterable<? extends FormalParameter> params, String... identifiers) {
		var i = 0
		for (FormalParameter parameter : params) {
			assertEquals("Unexpected parameter: " + parameter + ". Expected: " + identifiers.get(i),
				parameter.name, identifiers.get(i))
			i = i + 1
		}
		if (i < identifiers.length) {
			fail("Not enough identifiers. Expected: " + Arrays::toString(identifiers)
				+ "Actual: " + Iterables::toString(params))
		}
	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class InjectedAttributes extends AbstractSarlTest {
		
		@Inject
		var TypeReferences typeReferences
		
		@Inject
		var TypesFactory typesFactory
		
		@Inject
		var IResourceFactory resourceFactory
	
		@Inject
		var ActionSignatureProvider actionSignatureProvider
		
		@Inject
		var SARLCodeGenerator gen
	
		@Test
		def void getSARLFileExtension() {
			assertEquals("sarl", gen.SARLFileExtension)
		}
	
		@Test
		def void getTypeReferences() {
			assertSame(this.typeReferences, gen.typeReferences)
		}
	
		@Test
		def void getTypesFactory() {
			assertSame(this.typesFactory, gen.typesFactory)
		}
	
		@Test
		def void getResourceFactory() {
			var factory = gen.resourceFactory;
			assertNotNull(factory)
			assertNotSame(this.resourceFactory, factory)
		}
	
		@Test
		def void getActionSignatureProvider() {
			assertSame(this.actionSignatureProvider, gen.actionSignatureProvider)
		}
		
	}
		
	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class DefaultTypeValue extends AbstractSarlTest {
	
		@Inject
		var ISerializer serializer
	
		@Inject
		var SARLCodeGenerator gen
		
		@Nullable
		var GeneratedCode code
		
		@Nullable
		var EObject context

		@Before
		def void setUp() {
			code = mock(typeof(GeneratedCode))
			context = mock(typeof(EObject))
		}

		def void assertSerialized(String expected, EObject actual) {
			var value = this.serializer.serialize(actual)
			assertNotNull(value)
			assertEquals(expected, value)
		}
	
		@Test
		def void getDefaultXExpressionForType_void() {
			assertNull(gen.getDefaultXExpressionForType(code, context, "void"))
		}
	
		@Test
		def void getDefaultXExpressionForType_Void() {
			assertNull(gen.getDefaultXExpressionForType(code, context, "java.lang.Void"))
		}
	
		@Test
		def void getDefaultXExpressionForType_boolean() {
			assertSerialized("false", gen.getDefaultXExpressionForType(code, context, "boolean"))
		}
	
		@Test
		def void getDefaultXExpressionForType_Boolean() {
			assertSerialized("false", gen.getDefaultXExpressionForType(code, context, "java.lang.Boolean"))
		}
	
		@Test
		def void getDefaultXExpressionForType_char() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "char"))
		}
	
		@Test
		def void getDefaultXExpressionForType_Character() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "java.lang.Character"))
		}
	
		@Test
		def void getDefaultXExpressionForType_byte() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "byte"))
		}
	
		@Test
		def void getDefaultXExpressionForType_Byte() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "java.lang.Byte"))
		}
	
		@Test
		def void getDefaultXExpressionForType_short() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "short"))
		}
	
		@Test
		def void getDefaultXExpressionForType_Short() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "java.lang.Short"))
		}
	
		@Test
		def void getDefaultXExpressionForType_int() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "int"))
		}
	
		@Test
		def void getDefaultXExpressionForType_Integer() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "java.lang.Integer"))
		}
	
		@Test
		def void getDefaultXExpressionForType_long() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "long"))
		}
	
		@Test
		def void getDefaultXExpressionForType_Long() {
			assertSerialized("0", gen.getDefaultXExpressionForType(code, context, "java.lang.Long"))
		}
	
		@Test
		def void getDefaultXExpressionForType_float() {
			assertSerialized("0.0f", gen.getDefaultXExpressionForType(code, context, "float"))
		}
	
		@Test
		def void getDefaultXExpressionForType_Float() {
			assertSerialized("0.0f", gen.getDefaultXExpressionForType(code, context, "java.lang.Float"))
		}
	
		@Test
		def void getDefaultXExpressionForType_double() {
			assertSerialized("0.0", gen.getDefaultXExpressionForType(code, context, "double"))
		}
	
		@Test
		def void getDefaultXExpressionForType_Double() {
			assertSerialized("0.0", gen.getDefaultXExpressionForType(code, context, "java.lang.Double"))
		}
	
		@Test
		def void getDefaultXExpressionForType_String() {
			assertSerialized("null", gen.getDefaultXExpressionForType(code, context, "java.lang.String"))
		}
	
		@Test
		def void getDefaultXExpressionForType_AnyObject() {
			assertSerialized("null", gen.getDefaultXExpressionForType(code, context, "io.sarl.lang.tests.Dummy"))
		}
	
	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class References extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var EObject context
		
		@Nullable
		var ImportManager importManager
		
		@Nullable
		var JvmType jvmType

		@Before
		def void setUp() {
			importManager = new ImportManager
			code = mock(typeof(GeneratedCode))
			context = mock(typeof(EObject))
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))
			jvmType = mock(typeof(JvmType))

			when(code.importManager).thenReturn(importManager)
			when(typeProvider.findTypeByName(Matchers::anyString())).thenReturn(jvmType)
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(context.eResource()).thenReturn(eResource)
		}

		@Test
		def void newTypeRef_String() {
			when(jvmType.identifier).thenReturn("java.lang.String")
			when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn("java.lang.String")
			when(jvmType.qualifiedName).thenReturn("java.lang.String")
			when(jvmType.simpleName).thenReturn("String")
			//
			var ref = gen.newTypeRef(code, "java.lang.String", context)
			//
			assertNotNull(ref)
			assertEquals("java.lang.String", ref.identifier)
			assertContains(importManager.imports)
		}

		@Test
		def void newTypeRef_Foo() {
			when(jvmType.identifier).thenReturn("foo.Foo")
			when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn("foo.Foo")
			when(jvmType.qualifiedName).thenReturn("foo.Foo")
			when(jvmType.simpleName).thenReturn("Foo")
			//
			var ref = gen.newTypeRef(code, "foo.Foo", context)
			//
			assertNotNull(ref)
			assertEquals("foo.Foo", ref.identifier)
			assertContains(importManager.imports, "foo.Foo")
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class Comments extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var EObject context

		@Nullable
		var EObject object

		@Nullable
		var XBlockExpression block

		@Before
		def void setUp() {
			code = mock(typeof(GeneratedCode))
			context = mock(typeof(EObject))
			object = new EObjectImpl() {}
			block = new XBlockExpressionImpl() {}
		}
		
		private def <T extends Adapter> T assertAdapter(Class<T> expectedType, EObject actual) {
			for(adapter : actual.eAdapters) {
				if (expectedType.isInstance(adapter)) {
					return expectedType.cast(adapter)
				}
			}
			fail("Expecting the adapter of type: " + expectedType.name)
			return null
		}
		
		@Test
		def void attachComment() {
			gen.attachComment(code, object, "my comment")
			var adapter = assertAdapter(typeof(DocumentationAdapter), object);
			assertEquals("my comment", adapter.documentation);
		}
	
		@Test
		def void attachPostComment() {
			gen.attachPostComment(code, object, "my comment")
			var adapter = assertAdapter(typeof(PostDocumentationAdapter), object);
			assertEquals("my comment", adapter.documentation);
		}
	
		@Test
		def void attachInnerComment() {
			gen.attachInnerComment(code, block, "my comment")
			var adapter = assertAdapter(typeof(BlockInnerDocumentationAdapter), block);
			assertEquals("my comment", adapter.documentation);
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class ScriptLevel extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var ResourceSet resourceSet
	
		@Nullable
		var Resource resource
		
		@Nullable
		var EList<EObject> content
		
		@Nullable
		var GeneratedCode code

		@Before
		def void setUp() {
			resourceSet = mock(typeof(ResourceSet))
			content = mock(typeof(EList))
			resource = mock(typeof(Resource))
			when(resource.contents).thenReturn(content)
			when(resource.resourceSet).thenReturn(resourceSet)
			code = gen.createScript(resource, "io.sarl.lang.tests")
			assertNotNull(code)
		}

		@Test
		def void sarlScript()  {
			var script = code.sarlScript
			assertNotNull(script)
			assertEquals("io.sarl.lang.tests", script.name)
			assertTrue(script.elements.empty)
			assertNull(script.importSection)
		}
	
		@Test
		def void codeGenerator()  {
			var codeGenerator = code.codeGenerator
			assertSame(gen, codeGenerator)
		}

		@Test
		def void importManager()  {
			var importManager = code.importManager
			assertNotNull(importManager)
			assertTrue(importManager.imports.empty)
		}

		@Test
		def void resourceSet()  {
			assertSame(resourceSet, code.resourceSet)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class AgentTopElement extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var SarlScript context
		
		@Nullable
		var ImportManager importManager
		
		@Nullable
		var JvmType jvmType
		
		@Nullable
		var EList<TopElement> elements

		@Before
		def void setUp() {
			elements = new BasicEList
			importManager = new ImportManager
			code = mock(typeof(GeneratedCode))
			context = mock(typeof(SarlScript))
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))
			jvmType = mock(typeof(JvmType))

			when(code.importManager).thenReturn(importManager)
			when(code.sarlScript).thenReturn(context)
			when(typeProvider.findTypeByName(Matchers::anyString())).thenReturn(jvmType)
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(context.eResource()).thenReturn(eResource)
			when(context.elements).thenReturn(elements)
		}

		@Test	
		def void nullSuperClass()  {
			var agent = gen.createAgent(code, "MyAgent", null)
			assertNotNull(agent)
			assertEquals("MyAgent", agent.name)
			assertTrue(agent.superTypes.empty)
			assertTrue(agent.features.empty)
		}
	
		@Test	
		def void agentSuperClass()  {
			var agent = gen.createAgent(code, "MyAgent", "io.sarl.lang.core.Agent")
			assertNotNull(agent)
			assertEquals("MyAgent", agent.name)
			assertTrue(agent.superTypes.empty)
			assertTrue(agent.features.empty)
		}

		@Test	
		def void subagentSuperClass()  {
			when(jvmType.identifier).thenReturn("foo.ecore.SubAgent")
			when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn("foo.ecore.SubAgent")
			when(jvmType.qualifiedName).thenReturn("foo.ecore.SubAgent")
			when(jvmType.simpleName).thenReturn("SubAgent")
			//
			var agent = gen.createAgent(code, "MyAgent", "foo.ecore.SubAgent")
			//
			assertNotNull(agent)
			assertEquals("MyAgent", agent.name)
			assertTypeReferenceIdentifiers(agent.superTypes, "foo.ecore.SubAgent")
			assertTrue(agent.features.empty)
		}

		@Test	
		def void otherSuperClass()  {
			when(jvmType.identifier).thenReturn("foo.Foo")
			when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn("foo.Foo")
			when(jvmType.qualifiedName).thenReturn("foo.Foo")
			when(jvmType.simpleName).thenReturn("Foo")
			//
			var agent = gen.createAgent(code, "MyAgent", "foo.Foo")
			//
			assertNotNull(agent)
			assertEquals("MyAgent", agent.name)
			assertTypeReferenceIdentifiers(agent.superTypes, "foo.Foo")
			assertTrue(agent.features.empty)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class BehaviorTopElement extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var SarlScript context
		
		@Nullable
		var ImportManager importManager
		
		@Nullable
		var JvmType jvmType
		
		@Nullable
		var EList<TopElement> elements

		@Before
		def void setUp() {
			elements = new BasicEList
			importManager = new ImportManager
			code = mock(typeof(GeneratedCode))
			context = mock(typeof(SarlScript))
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))
			jvmType = mock(typeof(JvmType))

			when(code.importManager).thenReturn(importManager)
			when(code.sarlScript).thenReturn(context)
			when(typeProvider.findTypeByName(Matchers::anyString())).thenReturn(jvmType)
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(context.eResource()).thenReturn(eResource)
			when(context.elements).thenReturn(elements)
		}

		@Test	
		def void nullSuperClass()  {
			var behavior = gen.createBehavior(code, "MyBehavior", null)
			assertNotNull(behavior)
			assertEquals("MyBehavior", behavior.name)
			assertTrue(behavior.superTypes.empty)
			assertTrue(behavior.features.empty)
		}
	
		@Test	
		def void behaviorSuperClass()  {
			var behavior = gen.createBehavior(code, "MyBehavior", "io.sarl.lang.core.Behavior")
			assertNotNull(behavior)
			assertEquals("MyBehavior", behavior.name)
			assertTrue(behavior.superTypes.empty)
			assertTrue(behavior.features.empty)
		}

		@Test	
		def void subbehaviorSuperClass()  {
			when(jvmType.identifier).thenReturn("foo.ecore.SubBehavior")
			when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn("foo.ecore.SubBehavior")
			when(jvmType.qualifiedName).thenReturn("foo.ecore.SubBehavior")
			when(jvmType.simpleName).thenReturn("SubBehavior")
			//
			var behavior = gen.createBehavior(code, "MyBehavior", "foo.ecore.SubBehavior")
			//
			assertNotNull(behavior)
			assertEquals("MyBehavior", behavior.name)
			assertTypeReferenceIdentifiers(behavior.superTypes, "foo.ecore.SubBehavior")
			assertTrue(behavior.features.empty)
		}

		@Test	
		def void otherSuperClass()  {
			when(jvmType.identifier).thenReturn("foo.Foo")
			when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn("foo.Foo")
			when(jvmType.qualifiedName).thenReturn("foo.Foo")
			when(jvmType.simpleName).thenReturn("Foo")
			//
			var behavior = gen.createBehavior(code, "MyBehavior", "foo.Foo")
			//
			assertNotNull(behavior)
			assertEquals("MyBehavior", behavior.name)
			assertTypeReferenceIdentifiers(behavior.superTypes, "foo.Foo")
			assertTrue(behavior.features.empty)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class CapacityTopElement extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var SarlScript context
		
		@Nullable
		var ImportManager importManager
		
		@Nullable
		var JvmType jvmType
		
		@Nullable
		var EList<TopElement> elements

		@Before
		def void setUp() {
			elements = new BasicEList
			importManager = new ImportManager
			code = mock(typeof(GeneratedCode))
			context = mock(typeof(SarlScript))
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))
			jvmType = mock(typeof(JvmType))

			when(code.importManager).thenReturn(importManager)
			when(code.sarlScript).thenReturn(context)
			when(typeProvider.findTypeByName(Matchers::anyString())).thenReturn(jvmType)
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(context.eResource()).thenReturn(eResource)
			when(context.elements).thenReturn(elements)
		}

		@Test	
		def void nullSuperClass()  {
			var capacity = gen.createCapacity(code, "MyCapacity", null)
			assertNotNull(capacity)
			assertEquals("MyCapacity", capacity.name)
			assertTrue(capacity.superTypes.empty)
			assertTrue(capacity.features.empty)
		}
	
		@Test	
		def void behaviorSuperClass()  {
			var capacity = gen.createCapacity(code, "MyCapacity", "io.sarl.lang.core.Capacity")
			assertNotNull(capacity)
			assertEquals("MyCapacity", capacity.name)
			assertTrue(capacity.superTypes.empty)
			assertTrue(capacity.features.empty)
		}

		@Test	
		def void subbehaviorSuperClass()  {
			when(jvmType.identifier).thenReturn("foo.ecore.SubCapacity")
			when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn("foo.ecore.SubCapacity")
			when(jvmType.qualifiedName).thenReturn("foo.ecore.SubCapacity")
			when(jvmType.simpleName).thenReturn("SubCapacity")
			//
			var capacity = gen.createCapacity(code, "MyCapacity", "foo.ecore.SubCapacity")
			//
			assertNotNull(capacity)
			assertEquals("MyCapacity", capacity.name)
			assertTypeReferenceIdentifiers(capacity.superTypes, "foo.ecore.SubCapacity")
			assertTrue(capacity.features.empty)
		}

		@Test	
		def void otherSuperClass()  {
			when(jvmType.identifier).thenReturn("foo.Foo")
			when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn("foo.Foo")
			when(jvmType.qualifiedName).thenReturn("foo.Foo")
			when(jvmType.simpleName).thenReturn("Foo")
			//
			var capacity = gen.createCapacity(code, "MyCapacity", "foo.Foo")
			//
			assertNotNull(capacity)
			assertEquals("MyCapacity", capacity.name)
			assertTypeReferenceIdentifiers(capacity.superTypes, "foo.Foo")
			assertTrue(capacity.features.empty)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class EventTopElement extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var SarlScript context
		
		@Nullable
		var ImportManager importManager
		
		@Nullable
		var JvmType jvmType
		
		@Nullable
		var EList<TopElement> elements

		@Before
		def void setUp() {
			elements = new BasicEList
			importManager = new ImportManager
			code = mock(typeof(GeneratedCode))
			context = mock(typeof(SarlScript))
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))
			jvmType = mock(typeof(JvmType))

			when(code.importManager).thenReturn(importManager)
			when(code.sarlScript).thenReturn(context)
			when(typeProvider.findTypeByName(Matchers::anyString())).thenReturn(jvmType)
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(context.eResource()).thenReturn(eResource)
			when(context.elements).thenReturn(elements)
		}

		@Test	
		def void nullSuperClass()  {
			var event = gen.createEvent(code, "MyEvent", null)
			assertNotNull(event)
			assertEquals("MyEvent", event.name)
			assertTrue(event.superTypes.empty)
			assertTrue(event.features.empty)
		}
	
		@Test	
		def void behaviorSuperClass()  {
			var event = gen.createEvent(code, "MyEvent", "io.sarl.lang.core.Event")
			assertNotNull(event)
			assertEquals("MyEvent", event.name)
			assertTrue(event.superTypes.empty)
			assertTrue(event.features.empty)
		}

		@Test	
		def void subbehaviorSuperClass()  {
			when(jvmType.identifier).thenReturn("foo.ecore.SubEvent")
			when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn("foo.ecore.SubEvent")
			when(jvmType.qualifiedName).thenReturn("foo.ecore.SubEvent")
			when(jvmType.simpleName).thenReturn("SubEvent")
			//
			var event = gen.createEvent(code, "MyEvent", "foo.ecore.SubEvent")
			//
			assertNotNull(event)
			assertEquals("MyEvent", event.name)
			assertTypeReferenceIdentifiers(event.superTypes, "foo.ecore.SubEvent")
			assertTrue(event.features.empty)
		}

		@Test	
		def void otherSuperClass()  {
			when(jvmType.identifier).thenReturn("foo.Foo")
			when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn("foo.Foo")
			when(jvmType.qualifiedName).thenReturn("foo.Foo")
			when(jvmType.simpleName).thenReturn("Foo")
			//
			var event = gen.createEvent(code, "MyEvent", "foo.Foo")
			//
			assertNotNull(event)
			assertEquals("MyEvent", event.name)
			assertTypeReferenceIdentifiers(event.superTypes, "foo.Foo")
			assertTrue(event.features.empty)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class SkillTopElement extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var SarlScript context
		
		@Nullable
		var ImportManager importManager
		
		@Nullable
		var EList<TopElement> elements

		@Before
		def void setUp() {
			elements = new BasicEList
			importManager = new ImportManager
			code = mock(typeof(GeneratedCode))
			context = mock(typeof(SarlScript))
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))

			when(code.importManager).thenReturn(importManager)
			when(code.sarlScript).thenReturn(context)
			when(typeProvider.findTypeByName(Matchers::anyString())).thenAnswer [
				var typeName = it.arguments.get(0) as String
				var idx = typeName.lastIndexOf(".")
				var String simpleName
				if (idx >= 0) {
					simpleName = typeName.substring(idx + 1)
				} else {
					simpleName = typeName
				}
				var jvmType = mock(JvmType)
				when(jvmType.identifier).thenReturn(typeName)
				when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn(typeName)
				when(jvmType.qualifiedName).thenReturn(typeName)
				when(jvmType.simpleName).thenReturn(simpleName)
				return jvmType
			]
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(context.eResource()).thenReturn(eResource)
			when(context.elements).thenReturn(elements)
		}

		@Test	
		def void nullSuperClass_noSuperInterfaces()  {
			var skill = gen.createSkill(code, "MySkill", null, Collections.emptyList)
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTrue(skill.superTypes.empty)
			assertTrue(skill.implementedTypes.empty)
			assertTrue(skill.features.empty)
		}
	
		@Test	
		def void behaviorSuperClass_noSuperInterfaces()  {
			var skill = gen.createSkill(code, "MySkill", "io.sarl.lang.core.Skill", Collections.emptyList)
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTrue(skill.superTypes.empty)
			assertTrue(skill.implementedTypes.empty)
			assertTrue(skill.features.empty)
		}

		@Test	
		def void subbehaviorSuperClass_noSuperInterface()  {
			var skill = gen.createSkill(code, "MySkill", "foo.ecore.SubSkill", Collections.emptyList)
			//
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTypeReferenceIdentifiers(skill.superTypes, "foo.ecore.SubSkill")
			assertTrue(skill.implementedTypes.empty)
			assertTrue(skill.features.empty)
		}

		@Test	
		def void otherSuperClass_noSuperInterface()  {
			var skill = gen.createSkill(code, "MySkill", "foo.Foo", Collections.emptyList)
			//
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTypeReferenceIdentifiers(skill.superTypes, "foo.Foo")
			assertTrue(skill.implementedTypes.empty)
			assertTrue(skill.features.empty)
		}

		@Test	
		def void nullSuperClass_oneSuperInterfaces()  {
			var skill = gen.createSkill(code, "MySkill", null, Collections.singleton("foo.ecore.SubCapacity"))
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTrue(skill.superTypes.empty)
			assertTypeReferenceIdentifiers(skill.implementedTypes, "foo.ecore.SubCapacity")
			assertTrue(skill.features.empty)
		}
	
		@Test	
		def void behaviorSuperClass_oneSuperInterfaces()  {
			var skill = gen.createSkill(code, "MySkill", "io.sarl.lang.core.Skill", Collections.singleton("foo.ecore.SubCapacity"))
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTrue(skill.superTypes.empty)
			assertTypeReferenceIdentifiers(skill.implementedTypes, "foo.ecore.SubCapacity")
			assertTrue(skill.features.empty)
		}

		@Test	
		def void subbehaviorSuperClass_oneSuperInterface()  {
			var skill = gen.createSkill(code, "MySkill", "foo.ecore.SubSkill", Collections.singleton("foo.ecore.SubCapacity"))
			//
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTypeReferenceIdentifiers(skill.superTypes, "foo.ecore.SubSkill")
			assertTypeReferenceIdentifiers(skill.implementedTypes, "foo.ecore.SubCapacity")
			assertTrue(skill.features.empty)
		}

		@Test	
		def void otherSuperClass_oneSuperInterface()  {
			var skill = gen.createSkill(code, "MySkill", "foo.Foo", Collections.singleton("foo.ecore.SubCapacity"))
			//
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTypeReferenceIdentifiers(skill.superTypes, "foo.Foo")
			assertTypeReferenceIdentifiers(skill.implementedTypes, "foo.ecore.SubCapacity")
			assertTrue(skill.features.empty)
		}
		
		@Test	
		def void nullSuperClass_twoSuperInterfaces()  {
			var skill = gen.createSkill(code, "MySkill", null, Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"))
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTrue(skill.superTypes.empty)
			assertTypeReferenceIdentifiers(skill.implementedTypes, "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2")
			assertTrue(skill.features.empty)
		}
	
		@Test	
		def void behaviorSuperClass_twoSuperInterfaces()  {
			var skill = gen.createSkill(code, "MySkill", "io.sarl.lang.core.Skill", Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"))
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTrue(skill.superTypes.empty)
			assertTypeReferenceIdentifiers(skill.implementedTypes, "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2")
			assertTrue(skill.features.empty)
		}

		@Test	
		def void subbehaviorSuperClass_twoSuperInterface()  {
			var skill = gen.createSkill(code, "MySkill", "foo.ecore.SubSkill", Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"))
			//
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTypeReferenceIdentifiers(skill.superTypes, "foo.ecore.SubSkill")
			assertTypeReferenceIdentifiers(skill.implementedTypes, "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2")
			assertTrue(skill.features.empty)
		}

		@Test	
		def void otherSuperClass_twoSuperInterface()  {
			var skill = gen.createSkill(code, "MySkill", "foo.Foo", Arrays.asList("foo.ecore.SubCapacity", "foo.ecore.SubCapacity2"))
			//
			assertNotNull(skill)
			assertEquals("MySkill", skill.name)
			assertTypeReferenceIdentifiers(skill.superTypes, "foo.Foo")
			assertTypeReferenceIdentifiers(skill.implementedTypes, "foo.ecore.SubCapacity", "foo.ecore.SubCapacity2")
			assertTrue(skill.features.empty)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class AgentFeatures extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var EList<EObject> features

		@Nullable
		var XBlockExpression block

		@Nullable
		var Agent agent

		@Nullable
		var ImportManager importManager

		@Before
		def void setUp() {
			importManager = new ImportManager
			features = new BasicEList
			code = mock(typeof(GeneratedCode))
			agent = mock(typeof(Agent))
			block = XbaseFactory::eINSTANCE.createXBlockExpression
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))

			when(typeProvider.findTypeByName(Matchers::anyString())).thenAnswer [
				var typeName = it.arguments.get(0) as String
				var idx = typeName.lastIndexOf(".")
				var String simpleName
				if (idx >= 0) {
					simpleName = typeName.substring(idx + 1)
				} else {
					simpleName = typeName
				}
				var jvmType = mock(JvmType)
				when(jvmType.identifier).thenReturn(typeName)
				when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn(typeName)
				when(jvmType.qualifiedName).thenReturn(typeName)
				when(jvmType.simpleName).thenReturn(simpleName)
				return jvmType
			]
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(agent.eResource).thenReturn(eResource)
			when(agent.features).thenReturn(features)
			when(code.importManager).thenReturn(importManager)
		}

		@Test
		def void createAction_returnNull()  {
			var action = gen.createAction(code, agent, "myFct", null, block)
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertNull(action.type)
			assertTrue(action.params.empty)
			assertSame(block, action.body)
		}

		@Test
		def void createAction_returnBoolean()  {
			var action = gen.createAction(code, agent, "myFct", "boolean", block)
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertTypeReferenceIdentifier(action.type, "boolean")
			assertTrue(action.params.empty)
			assertSame(block, action.body)
		}
	
		@Test
		def void createAction_returnObject()  {
			var action = gen.createAction(code, agent, "myFct", "java.lang.String", block)
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertTypeReferenceIdentifier(action.type, "java.lang.String")
			assertTrue(action.params.empty)
			assertSame(block, action.body)
		}

		@Test
		def void createConstructor()  {
			var constructor = gen.createConstructor(code, agent, block)
			//
			assertNotNull(constructor)
			assertTrue(constructor.params.empty)
			assertSame(block, constructor.body)
		}

		@Test
		def void createBehaviorUnit_noGuard()  {
			var unit = gen.createBehaviorUnit(code, agent, "foo.ecore.SubEvent", null, block)
			//
			assertNotNull(unit)
			assertTypeReferenceIdentifier(unit.name, "foo.ecore.SubEvent")
			assertNull(unit.guard)
			assertSame(block, unit.body)
		}

		@Test
		def void createBehaviorUnit_aGuard()  {
			var guard = XbaseFactory::eINSTANCE.createXBlockExpression
			//
			var unit = gen.createBehaviorUnit(code, agent, "foo.ecore.SubEvent", guard, block)
			//
			assertNotNull(unit)
			assertTypeReferenceIdentifier(unit.name, "foo.ecore.SubEvent")
			assertSame(guard, unit.guard)
			assertSame(block, unit.body)
		}

		@Test(expected = typeof(IllegalArgumentException))
		def void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createVariable(code, agent, "myVar", null as String)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createVariable(code, agent, "myVar", null as XExpression)
		}

		@Test
		def void createVariableGeneratedCodeFeatureContainerStringString()  {
			var variable = gen.createVariable(code, agent, "myVar", "java.lang.String")
			//
			assertNotNull(variable)
			assertEquals("myVar", variable.name)
			assertTypeReferenceIdentifier(variable.type, "java.lang.String")
			assertNull(variable.initialValue)
			assertTrue(variable.writeable)
		}

		@Test
		def void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			var numberLiteral = XbaseFactory::eINSTANCE.createXNumberLiteral
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			var variable = gen.createVariable(code, agent, "myVar", numberLiteral)
			//
			assertNotNull(variable)
			assertEquals("myVar", variable.name)
			assertNull(variable.type)
			assertSame(numberLiteral, variable.initialValue)
			assertTrue(variable.writeable)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createValue(code, agent, "myConst", null as String)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createValue(code, agent, "myConst", null as XExpression)
		}

		@Test
		def void createValueGeneratedCodeFeatureContainerStringString()  {
			var value = gen.createValue(code, agent, "myConst", "java.lang.String")
			//
			assertNotNull(value)
			assertEquals("myConst", value.name)
			assertTypeReferenceIdentifier(value.type, "java.lang.String")
			assertNull(value.initialValue)
			assertFalse(value.writeable)
		}

		@Test
		def void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			var numberLiteral = XbaseFactory::eINSTANCE.createXNumberLiteral
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			var value = gen.createValue(code, agent, "myConst", numberLiteral)
			//
			assertNotNull(value)
			assertEquals("myConst", value.name)
			assertNull(value.type)
			assertSame(numberLiteral, value.initialValue)
			assertFalse(value.writeable)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class BehaviorFeatures extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var EList<EObject> features

		@Nullable
		var XBlockExpression block

		@Nullable
		var Behavior behavior

		@Nullable
		var ImportManager importManager

		@Before
		def void setUp() {
			importManager = new ImportManager
			features = new BasicEList
			code = mock(typeof(GeneratedCode))
			behavior = mock(typeof(Behavior))
			block = XbaseFactory::eINSTANCE.createXBlockExpression
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))

			when(typeProvider.findTypeByName(Matchers::anyString())).thenAnswer [
				var typeName = it.arguments.get(0) as String
				var idx = typeName.lastIndexOf(".")
				var String simpleName
				if (idx >= 0) {
					simpleName = typeName.substring(idx + 1)
				} else {
					simpleName = typeName
				}
				var jvmType = mock(JvmType)
				when(jvmType.identifier).thenReturn(typeName)
				when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn(typeName)
				when(jvmType.qualifiedName).thenReturn(typeName)
				when(jvmType.simpleName).thenReturn(simpleName)
				return jvmType
			]
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(behavior.eResource).thenReturn(eResource)
			when(behavior.features).thenReturn(features)
			when(code.importManager).thenReturn(importManager)
		}

		@Test
		def void createAction_returnNull()  {
			var action = gen.createAction(code, behavior, "myFct", null, block)
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertNull(action.type)
			assertTrue(action.params.empty)
			assertSame(block, action.body)
		}

		@Test
		def void createAction_returnBoolean()  {
			var action = gen.createAction(code, behavior, "myFct", "boolean", block)
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertTypeReferenceIdentifier(action.type, "boolean")
			assertTrue(action.params.empty)
			assertSame(block, action.body)
		}
	
		@Test
		def void createAction_returnObject()  {
			var action = gen.createAction(code, behavior, "myFct", "java.lang.String", block)
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertTypeReferenceIdentifier(action.type, "java.lang.String")
			assertTrue(action.params.empty)
			assertSame(block, action.body)
		}

		@Test
		def void createConstructor()  {
			var constructor = gen.createConstructor(code, behavior, block)
			//
			assertNotNull(constructor)
			assertTrue(constructor.params.empty)
			assertSame(block, constructor.body)
		}

		@Test
		def void createBehaviorUnit_noGuard()  {
			var unit = gen.createBehaviorUnit(code, behavior, "foo.ecore.SubEvent", null, block)
			//
			assertNotNull(unit)
			assertTypeReferenceIdentifier(unit.name, "foo.ecore.SubEvent")
			assertNull(unit.guard)
			assertSame(block, unit.body)
		}

		@Test
		def void createBehaviorUnit_aGuard()  {
			var guard = XbaseFactory::eINSTANCE.createXBlockExpression
			//
			var unit = gen.createBehaviorUnit(code, behavior, "foo.ecore.SubEvent", guard, block)
			//
			assertNotNull(unit)
			assertTypeReferenceIdentifier(unit.name, "foo.ecore.SubEvent")
			assertSame(guard, unit.guard)
			assertSame(block, unit.body)
		}

		@Test(expected = typeof(IllegalArgumentException))
		def void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createVariable(code, behavior, "myVar", null as String)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createVariable(code, behavior, "myVar", null as XExpression)
		}

		@Test
		def void createVariableGeneratedCodeFeatureContainerStringString()  {
			var variable = gen.createVariable(code, behavior, "myVar", "java.lang.String")
			//
			assertNotNull(variable)
			assertEquals("myVar", variable.name)
			assertTypeReferenceIdentifier(variable.type, "java.lang.String")
			assertNull(variable.initialValue)
			assertTrue(variable.writeable)
		}

		@Test
		def void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			var numberLiteral = XbaseFactory::eINSTANCE.createXNumberLiteral
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			var variable = gen.createVariable(code, behavior, "myVar", numberLiteral)
			//
			assertNotNull(variable)
			assertEquals("myVar", variable.name)
			assertNull(variable.type)
			assertSame(numberLiteral, variable.initialValue)
			assertTrue(variable.writeable)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createValue(code, behavior, "myConst", null as String)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createValue(code, behavior, "myConst", null as XExpression)
		}

		@Test
		def void createValueGeneratedCodeFeatureContainerStringString()  {
			var value = gen.createValue(code, behavior, "myConst", "java.lang.String")
			//
			assertNotNull(value)
			assertEquals("myConst", value.name)
			assertTypeReferenceIdentifier(value.type, "java.lang.String")
			assertNull(value.initialValue)
			assertFalse(value.writeable)
		}

		@Test
		def void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			var numberLiteral = XbaseFactory::eINSTANCE.createXNumberLiteral
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			var value = gen.createValue(code, behavior, "myConst", numberLiteral)
			//
			assertNotNull(value)
			assertEquals("myConst", value.name)
			assertNull(value.type)
			assertSame(numberLiteral, value.initialValue)
			assertFalse(value.writeable)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class CapacityFeatures extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var EList<EObject> features

		@Nullable
		var Capacity capacity

		@Nullable
		var ImportManager importManager

		@Before
		def void setUp() {
			importManager = new ImportManager
			features = new BasicEList
			code = mock(typeof(GeneratedCode))
			capacity = mock(typeof(Capacity))
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))

			when(typeProvider.findTypeByName(Matchers::anyString())).thenAnswer [
				var typeName = it.arguments.get(0) as String
				var idx = typeName.lastIndexOf(".")
				var String simpleName
				if (idx >= 0) {
					simpleName = typeName.substring(idx + 1)
				} else {
					simpleName = typeName
				}
				var jvmType = mock(JvmType)
				when(jvmType.identifier).thenReturn(typeName)
				when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn(typeName)
				when(jvmType.qualifiedName).thenReturn(typeName)
				when(jvmType.simpleName).thenReturn(simpleName)
				return jvmType
			]
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(capacity.eResource).thenReturn(eResource)
			when(capacity.features).thenReturn(features)
			when(code.importManager).thenReturn(importManager)
		}

		@Test
		def void createActionSignature_returnNull()  {
			var action = gen.createActionSignature(code, capacity, "myFct", null)
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertNull(action.type)
			assertTrue(action.params.empty)
		}

		@Test
		def void createActionSignature_returnBoolean()  {
			var action = gen.createActionSignature(code, capacity, "myFct", "boolean")
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertTypeReferenceIdentifier(action.type, "boolean")
			assertTrue(action.params.empty)
		}
	
		@Test
		def void createActionSignature_returnObject()  {
			var action = gen.createActionSignature(code, capacity, "myFct", "java.lang.String")
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertTypeReferenceIdentifier(action.type, "java.lang.String")
			assertTrue(action.params.empty)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class EventFeatures extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var EList<EObject> features

		@Nullable
		var XBlockExpression block

		@Nullable
		var Event event

		@Nullable
		var ImportManager importManager

		@Before
		def void setUp() {
			importManager = new ImportManager
			features = new BasicEList
			code = mock(typeof(GeneratedCode))
			event = mock(typeof(Event))
			block = XbaseFactory::eINSTANCE.createXBlockExpression
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))

			when(typeProvider.findTypeByName(Matchers::anyString())).thenAnswer [
				var typeName = it.arguments.get(0) as String
				var idx = typeName.lastIndexOf(".")
				var String simpleName
				if (idx >= 0) {
					simpleName = typeName.substring(idx + 1)
				} else {
					simpleName = typeName
				}
				var jvmType = mock(JvmType)
				when(jvmType.identifier).thenReturn(typeName)
				when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn(typeName)
				when(jvmType.qualifiedName).thenReturn(typeName)
				when(jvmType.simpleName).thenReturn(simpleName)
				return jvmType
			]
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(event.eResource).thenReturn(eResource)
			when(event.features).thenReturn(features)
			when(code.importManager).thenReturn(importManager)
		}

		@Test
		def void createConstructor()  {
			var constructor = gen.createConstructor(code, event, block)
			//
			assertNotNull(constructor)
			assertTrue(constructor.params.empty)
			assertSame(block, constructor.body)
		}

		@Test(expected = typeof(IllegalArgumentException))
		def void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createVariable(code, event, "myVar", null as String)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createVariable(code, event, "myVar", null as XExpression)
		}

		@Test
		def void createVariableGeneratedCodeFeatureContainerStringString()  {
			var variable = gen.createVariable(code, event, "myVar", "java.lang.String")
			//
			assertNotNull(variable)
			assertEquals("myVar", variable.name)
			assertTypeReferenceIdentifier(variable.type, "java.lang.String")
			assertNull(variable.initialValue)
			assertTrue(variable.writeable)
		}

		@Test
		def void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			var numberLiteral = XbaseFactory::eINSTANCE.createXNumberLiteral
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			var variable = gen.createVariable(code, event, "myVar", numberLiteral)
			//
			assertNotNull(variable)
			assertEquals("myVar", variable.name)
			assertNull(variable.type)
			assertSame(numberLiteral, variable.initialValue)
			assertTrue(variable.writeable)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createValue(code, event, "myConst", null as String)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createValue(code, event, "myConst", null as XExpression)
		}

		@Test
		def void createValueGeneratedCodeFeatureContainerStringString()  {
			var value = gen.createValue(code, event, "myConst", "java.lang.String")
			//
			assertNotNull(value)
			assertEquals("myConst", value.name)
			assertTypeReferenceIdentifier(value.type, "java.lang.String")
			assertNull(value.initialValue)
			assertFalse(value.writeable)
		}

		@Test
		def void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			var numberLiteral = XbaseFactory::eINSTANCE.createXNumberLiteral
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			var value = gen.createValue(code, event, "myConst", numberLiteral)
			//
			assertNotNull(value)
			assertEquals("myConst", value.name)
			assertNull(value.type)
			assertSame(numberLiteral, value.initialValue)
			assertFalse(value.writeable)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class SkillFeatures extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var EList<EObject> features

		@Nullable
		var XBlockExpression block

		@Nullable
		var Skill skill

		@Nullable
		var ImportManager importManager

		@Before
		def void setUp() {
			importManager = new ImportManager
			features = new BasicEList
			code = mock(typeof(GeneratedCode))
			skill = mock(typeof(Skill))
			block = XbaseFactory::eINSTANCE.createXBlockExpression
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))

			when(typeProvider.findTypeByName(Matchers::anyString())).thenAnswer [
				var typeName = it.arguments.get(0) as String
				var idx = typeName.lastIndexOf(".")
				var String simpleName
				if (idx >= 0) {
					simpleName = typeName.substring(idx + 1)
				} else {
					simpleName = typeName
				}
				var jvmType = mock(JvmType)
				when(jvmType.identifier).thenReturn(typeName)
				when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn(typeName)
				when(jvmType.qualifiedName).thenReturn(typeName)
				when(jvmType.simpleName).thenReturn(simpleName)
				return jvmType
			]
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(skill.eResource).thenReturn(eResource)
			when(skill.features).thenReturn(features)
			when(code.importManager).thenReturn(importManager)
		}

		@Test
		def void createAction_returnNull()  {
			var action = gen.createAction(code, skill, "myFct", null, block)
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertNull(action.type)
			assertTrue(action.params.empty)
			assertSame(block, action.body)
		}

		@Test
		def void createAction_returnBoolean()  {
			var action = gen.createAction(code, skill, "myFct", "boolean", block)
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertTypeReferenceIdentifier(action.type, "boolean")
			assertTrue(action.params.empty)
			assertSame(block, action.body)
		}
	
		@Test
		def void createAction_returnObject()  {
			var action = gen.createAction(code, skill, "myFct", "java.lang.String", block)
			//
			assertNotNull(action)
			assertEquals("myFct", action.name)
			assertTypeReferenceIdentifier(action.type, "java.lang.String")
			assertTrue(action.params.empty)
			assertSame(block, action.body)
		}

		@Test
		def void createConstructor()  {
			var constructor = gen.createConstructor(code, skill, block)
			//
			assertNotNull(constructor)
			assertTrue(constructor.params.empty)
			assertSame(block, constructor.body)
		}

		@Test(expected = typeof(IllegalArgumentException))
		def void createVariableGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createVariable(code, skill, "myVar", null as String)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createVariableGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createVariable(code, skill, "myVar", null as XExpression)
		}

		@Test
		def void createVariableGeneratedCodeFeatureContainerStringString()  {
			var variable = gen.createVariable(code, skill, "myVar", "java.lang.String")
			//
			assertNotNull(variable)
			assertEquals("myVar", variable.name)
			assertTypeReferenceIdentifier(variable.type, "java.lang.String")
			assertNull(variable.initialValue)
			assertTrue(variable.writeable)
		}

		@Test
		def void createVariableGeneratedCodeFeatureContainerStringXExpression()  {
			var numberLiteral = XbaseFactory::eINSTANCE.createXNumberLiteral
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			var variable = gen.createVariable(code, skill, "myVar", numberLiteral)
			//
			assertNotNull(variable)
			assertEquals("myVar", variable.name)
			assertNull(variable.type)
			assertSame(numberLiteral, variable.initialValue)
			assertTrue(variable.writeable)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createValueGeneratedCodeFeatureContainerStringString_nullType()  {
			gen.createValue(code, skill, "myConst", null as String)
		}
	
		@Test(expected = typeof(IllegalArgumentException))
		def void createValueGeneratedCodeFeatureContainerStringString_nullExpression()  {
			gen.createValue(code, skill, "myConst", null as XExpression)
		}

		@Test
		def void createValueGeneratedCodeFeatureContainerStringString()  {
			var value = gen.createValue(code, skill, "myConst", "java.lang.String")
			//
			assertNotNull(value)
			assertEquals("myConst", value.name)
			assertTypeReferenceIdentifier(value.type, "java.lang.String")
			assertNull(value.initialValue)
			assertFalse(value.writeable)
		}

		@Test
		def void createValueGeneratedCodeFeatureContainerStringXExpression()  {
			var numberLiteral = XbaseFactory::eINSTANCE.createXNumberLiteral
			numberLiteral.setValue("2.3f"); //$NON-NLS-1$
			//
			var value = gen.createValue(code, skill, "myConst", numberLiteral)
			//
			assertNotNull(value)
			assertEquals("myConst", value.name)
			assertNull(value.type)
			assertSame(numberLiteral, value.initialValue)
			assertFalse(value.writeable)
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class Expressions extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var ResourceSet eResourceSet

		@Before
		def void setUp() {
			code = mock(typeof(GeneratedCode))
			eResourceSet = mock(typeof(ResourceSet))
			
			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))

			when(typeProvider.findTypeByName(Matchers::anyString())).thenAnswer [
				var typeName = it.arguments.get(0) as String
				var idx = typeName.lastIndexOf(".")
				var String simpleName
				if (idx >= 0) {
					simpleName = typeName.substring(idx + 1)
				} else {
					simpleName = typeName
				}
				var jvmType = mock(JvmType)
				when(jvmType.identifier).thenReturn(typeName)
				when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn(typeName)
				when(jvmType.qualifiedName).thenReturn(typeName)
				when(jvmType.simpleName).thenReturn(simpleName)
				return jvmType
			]
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResource.resourceSet).thenReturn(eResourceSet)
		}

		@Test
		def void createXExpression_null()  {
			var expr = gen.createXExpression(code, null, eResourceSet)
			//
			assertNull(expr);
		}

		@Test
		def void createXExpression_empty()  {
			var expr = gen.createXExpression(code, "", eResourceSet)
			//
			assertNull(expr);
		}

	}

	@RunWith(XtextRunner)
	@InjectWith(SARLInjectorProvider)
	static class FormalParameters extends AbstractSarlTest {
	
		@Inject
		var SARLCodeGenerator gen

		@Nullable
		var GeneratedCode code
		
		@Nullable
		var EObject context
		
		@Nullable
		var ParameterizedFeature container

		@Nullable
		var ImportManager importManager

		@Nullable
		var EList<Resource> resources

		@Before
		def void setUp() {
			importManager = new ImportManager
			code = mock(typeof(GeneratedCode))
			context = mock(typeof(EObject))
			container = mock(typeof(ParameterizedFeature))

			var eResource = mock(typeof(Resource))
			var eResourceSet = mock(typeof(ResourceSet))
			var registry = mock(typeof(Resource.Factory.Registry))
			var factoryMap = mock(typeof(Map))
			var typeProvider = mock(typeof(IJvmTypeProvider))

			when(typeProvider.findTypeByName(Matchers::anyString())).thenAnswer [
				var typeName = it.arguments.get(0) as String
				var idx = typeName.lastIndexOf(".")
				var String simpleName
				if (idx >= 0) {
					simpleName = typeName.substring(idx + 1)
				} else {
					simpleName = typeName
				}
				var jvmType = mock(JvmType)
				when(jvmType.identifier).thenReturn(typeName)
				when(jvmType.getQualifiedName(Matchers::anyChar)).thenReturn(typeName)
				when(jvmType.qualifiedName).thenReturn(typeName)
				when(jvmType.simpleName).thenReturn(simpleName)
				return jvmType
			]
			when(factoryMap.get(Matchers::any())).thenReturn(typeProvider)
			when(registry.protocolToFactoryMap).thenReturn(factoryMap)
			when(eResourceSet.resourceFactoryRegistry).thenReturn(registry)
			when(eResourceSet.getResource(Matchers::any(URI), Matchers::anyBoolean())).thenReturn(null)
			when(eResourceSet.resources).thenReturn(resources)
			when(eResource.resourceSet).thenReturn(eResourceSet)
			when(container.eResource).thenReturn(eResource)
			when(container.params).thenReturn(new BasicEList)
			when(code.importManager).thenReturn(importManager)
			when(code.resourceSet).thenReturn(eResourceSet)
		}

		@Test(expected = typeof(IllegalArgumentException))
		def void createVarArgs_nullType()  {
			gen.createVarArgs(code, container, "myParam", null)
		}

		@Test
		def void createVarArgs()  {
			var param = gen.createVarArgs(code, container, "myParam", "boolean")
			//
			assertNotNull(param)
			assertEquals("myParam", param.name)
			assertNull(param.defaultValue)
			assertTypeReferenceIdentifier(param.parameterType, "boolean")
			//
			assertParameters(container.params, "myParam")
			//
			var arg = ArgumentCaptor::forClass(typeof(boolean))
			verify(container).setVarargs(arg.capture())
			assertTrue(arg.value)
		}

		@Test(expected = typeof(IllegalArgumentException))
		def void createFormalParameterGeneratedCodeParameterizedFeatureStringStringStringResourceSet_nullType_noDefaultValue()  {
			gen.createFormalParameter(code, container, "myParam", null, null, code.resourceSet)
		}

		@Test(expected = typeof(IllegalArgumentException))
		def void createFormalParameterGeneratedCodeParameterizedFeatureStringStringStringResourceSet_nullType_defaultValue()  {
			gen.createFormalParameter(code, container, "myParam", null, "true", code.resourceSet)
		}

		@Test
		def void createFormalParameterGeneratedCodeParameterizedFeatureStringStringStringResourceSet_noDefaultValue()  {
			var param = gen.createFormalParameter(code, container, "myParam", "java.lang.String", null, code.resourceSet)
			//
			assertNotNull(param)
			assertEquals("myParam", param.name)
			assertNull(param.defaultValue)
			assertTypeReferenceIdentifier(param.parameterType, "java.lang.String")
			//
			assertParameters(container.params, "myParam")
		}

		@Test(expected = typeof(IllegalArgumentException))
		def void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_nullType_noDefaultValue()  {
			gen.createFormalParameter(code, container, "myParam", null, null)
		}

		@Test(expected = typeof(IllegalArgumentException))
		def void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_nullType_defaultValue()  {
			var expr = XbaseFactory::eINSTANCE.createXStringLiteral
			expr.value = "abc"
			gen.createFormalParameter(code, container, "myParam", null, expr)
		}

		@Test
		def void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_noDefaultValue()  {
			var param = gen.createFormalParameter(code, container, "myParam", "java.lang.String", null)
			//
			assertNotNull(param)
			assertEquals("myParam", param.name)
			assertNull(param.defaultValue)
			assertTypeReferenceIdentifier(param.parameterType, "java.lang.String")
			//
			assertParameters(container.params, "myParam")
		}

		@Test
		def void createFormalParameterGeneratedCodeParameterizedFeatureStringStringXExpression_defaultValue()  {
			var expr = XbaseFactory::eINSTANCE.createXStringLiteral
			expr.value = "abc"
			var param = gen.createFormalParameter(code, container, "myParam", "java.lang.String", expr)
			//
			assertNotNull(param)
			assertEquals("myParam", param.name)
			assertSame(expr, param.defaultValue)
			assertTypeReferenceIdentifier(param.parameterType, "java.lang.String")
			//
			assertParameters(container.params, "myParam")
		}

	}

}
