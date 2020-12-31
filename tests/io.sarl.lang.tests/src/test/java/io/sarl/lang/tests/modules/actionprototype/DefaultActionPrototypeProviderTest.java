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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.modules.actionprototype;

import static io.sarl.tests.api.tools.TestAssertions.assertContainsStrings;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestEObjects.getType;
import static io.sarl.tests.api.tools.TestMockito.mock;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.google.inject.Inject;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.XExpression;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.opentest4j.AssertionFailedError;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.lang.sarl.actionprototype.DefaultActionPrototypeProvider;
import io.sarl.lang.sarl.actionprototype.FormalParameterProvider;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeContext;
import io.sarl.lang.sarl.actionprototype.InferredPrototype;
import io.sarl.lang.sarl.actionprototype.InferredStandardParameter;
import io.sarl.lang.sarl.actionprototype.InferredValuedParameter;
import io.sarl.lang.sarl.actionprototype.QualifiedActionName;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({"javadoc", "nls", "incomplete-switch"})
@DisplayName("DefaultActionPrototypeProvider")
@Tag("core")
@Tag("unit")
public class DefaultActionPrototypeProviderTest extends AbstractSarlTest {

	static int index;

	static JvmIdentifiableElement createJvmIdentifiableElementStub() {
		++index;
		Resource resource = mock(Resource.class);
		when(resource.getURI()).thenReturn(URI.createFileURI("/path/to/io/sarl/tests/Stub" + index + ".sarl"));
		JvmIdentifiableElement container = mock(JvmIdentifiableElement.class);
		when(container.eResource()).thenReturn(resource);
		when(container.getQualifiedName()).thenReturn("io.sarl.tests.Stub" + index);
		return container;
	}

	static void assertSameFormalParameters(List<? extends XtendParameter> expected, FormalParameterProvider actual) {
		assertEquals(expected.size(), actual.getFormalParameterCount());
		for (int i = 0; i < expected.size(); ++i) {
			assertEquals(expected.get(i).getName(), actual.getFormalParameterName(i));
			assertEquals(expected.get(i).getParameterType().getQualifiedName(),
					actual.getFormalParameterType(i, false));
			if (expected.get(i) instanceof SarlFormalParameter) {
				assertSame(((SarlFormalParameter)  expected.get(i)).getDefaultValue(),
						actual.getFormalParameterDefaultValue(i));
			} else {
				assertNull(actual.getFormalParameterDefaultValue(i));
			}
		}
	}

	static void assertSameJvmFormalParameters(List<? extends JvmFormalParameter> expected, FormalParameterProvider actual) {
		assertEquals(expected.size(), actual.getFormalParameterCount());
		for (int i = 0; i < expected.size(); ++i) {
			assertEquals(expected.get(i).getName(), actual.getFormalParameterName(i));
			assertEquals(expected.get(i).getParameterType().getQualifiedName(),
					actual.getFormalParameterType(i, false));
		}
	}

	private static boolean matchPrototype(List<InferredStandardParameter> parameters, Object[] expected) {
		int i = 0;
		for (InferredStandardParameter parameter : parameters) {
			if (i >= expected.length || !((Class<?>) expected[i]).isInstance(parameter)) {
				return false;
			}
			++i;
			if (i >= expected.length || !parameter.getType().getIdentifier().equals(expected[i])) {
				return false;
			}
			++i;
			if (i >= expected.length || !parameter.getName().equals(expected[i])) {
				return false;
			}
			++i;
			if (parameter instanceof InferredValuedParameter) {
				String arg = ((InferredValuedParameter) parameter).getCallingArgument();
				if (i >= expected.length || !arg.equals(expected[i])) {
					return false;
				}
				++i;
			}
		}
		return true;
	}

	private static void assertPrototypes(
			List<InferredStandardParameter> parameters,
			Collection<Object[]> expected,
			Object[][] originalExpected) {
		Iterator<Object[]> iterator = expected.iterator();
		while (iterator.hasNext()) {
			if (matchPrototype(parameters, iterator.next())) {
				iterator.remove();
				return;
			}
		}
		throw new AssertionFailedError("Not same parameter prototype.",
				parameters.toString(), toString(originalExpected));
	}

	private static String toString(Object[][] array) {
		StringBuilder b = new StringBuilder();
		b.append("[\n");
		boolean addcoma = false;
		for (Object[] d : array) {
			if (addcoma) {
				b.append(",\n");
			} else {
				addcoma = true;
			}
			b.append(toString(d));
		}
		b.append("]");
		return b.toString();
	}

	private static String toString(Object[] array) {
		StringBuilder b = new StringBuilder();
		b.append("  [ ");
		boolean addcoma = false;
		for (Object o : array) {
			if (addcoma) {
				b.append(",\n    ");
			} else {
				addcoma = true;
			}
			b.append(o==null ? "null" : o.toString());
		}
		b.append("  ]");
		return b.toString();
	}

	static void assertPrototypes(
			Map<ActionParameterTypes, List<InferredStandardParameter>> elements,
			Object[]... expected) {
		Collection<Object[]> expectedElements = new ArrayList<>();
		for (int i = 0; i < expected.length; ++i) {
			expectedElements.add(expected[i]);
		}
		for (List<InferredStandardParameter> parameters : elements.values()) {
			assertPrototypes(parameters, expectedElements, expected);
		}
		if (!expectedElements.isEmpty()) {
			throw new AssertionFailedError(
					"Not same prototypes", expectedElements.toString(), elements.toString());
		}
	}

	static void assertPrototypes(
			List<InferredStandardParameter> elements,
			Object... expected) {
		if (!matchPrototype(elements, expected)) {
			fail("Expected elements: " + toString(expected)
			+ "; but is: " + elements.toString());
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("DefaultActionPrototypeProvider without default values")
	public class NoDefaultValues extends AbstractSarlTest {

		@Inject
		private DefaultActionPrototypeProvider provider;

		@Nullable
		private IActionPrototypeContext context;

		@Nullable
		private FormalParameterProvider parameterProvider;

		@Nullable
		private EList<SarlFormalParameter> sarlParameters;

		@Nullable
		private EList<JvmFormalParameter> jvmParameters;

		@BeforeEach
		public void setUp() throws Exception {
			this.context = this.provider.createContext();
			this.parameterProvider = mock(FormalParameterProvider.class);
			when(this.parameterProvider.getFormalParameterCount()).thenReturn(3);
			when(this.parameterProvider.getFormalParameterName(ArgumentMatchers.anyInt())).thenAnswer(new Answer<String>() {
				@Override
				public String answer(InvocationOnMock invocation) throws Throwable {
					switch(((Integer) invocation.getArguments()[0]).intValue()) {
					case 0:
						return "firstarg";
					case 1:
						return "secondarg";
					case 2:
						return "thirdarg";
					}
					return null;
				}
			});
			when(this.parameterProvider.getFormalParameterType(ArgumentMatchers.anyInt(),
					ArgumentMatchers.anyBoolean())).thenAnswer(new Answer<String>() {
						@Override
						public String answer(InvocationOnMock invocation) throws Throwable {
							switch(((Integer) invocation.getArguments()[0]).intValue()) {
							case 0:
								return "java.lang.String";
							case 1:
								return "float";
							case 2:
								return "java.lang.Object[]";
							}
							return null;
						}
					});
			//
			SarlFormalParameter p;
			this.sarlParameters = new BasicEList<>();
			p = mock(SarlFormalParameter.class);
			when(p.getName()).thenReturn("firstarg");
			JvmTypeReference ref = getType(getParseHelper(), "java.lang.String");
			when(p.getParameterType()).thenReturn(ref);
			this.sarlParameters.add(p);
			p = mock(SarlFormalParameter.class);
			when(p.getName()).thenReturn("secondarg");
			ref = getType(getParseHelper(), "float");
			when(p.getParameterType()).thenReturn(ref);
			this.sarlParameters.add(p);
			p = mock(SarlFormalParameter.class);
			when(p.getName()).thenReturn("thirdarg");
			ref = getType(getParseHelper(), "java.lang.Object");
			when(p.getParameterType()).thenReturn(ref);
			this.sarlParameters.add(p);
			//
			JvmFormalParameter jp;
			this.jvmParameters = new BasicEList<>();
			jp = mock(JvmFormalParameter.class);
			when(jp.getName()).thenReturn("firstarg");
			ref = getType(getParseHelper(), "java.lang.String");
			when(jp.getParameterType()).thenReturn(ref);
			when(jp.getAnnotations()).thenReturn(new BasicEList<JvmAnnotationReference>());
			this.jvmParameters.add(jp);
			jp = mock(JvmFormalParameter.class);
			when(jp.getName()).thenReturn("secondarg");
			ref = getType(getParseHelper(), "float");
			when(jp.getParameterType()).thenReturn(ref);
			when(jp.getAnnotations()).thenReturn(new BasicEList<JvmAnnotationReference>());
			this.jvmParameters.add(jp);
			jp = mock(JvmFormalParameter.class);
			when(jp.getName()).thenReturn("thirdarg");
			ref = getType(getParseHelper(), "java.lang.Object[]");
			when(jp.getParameterType()).thenReturn(ref);
			when(jp.getAnnotations()).thenReturn(new BasicEList<JvmAnnotationReference>());
			this.jvmParameters.add(jp);
		}

		@Test
		public void validateTypeOfVarArgInSarl() throws Exception {
			SarlScript s = file(getParseHelper(), "agent Foo { def fooFct(a : float, b : Object*) {} }");
			SarlFormalParameter param = (SarlFormalParameter) ((SarlAction) ((SarlAgent) s.getXtendTypes().get(0))
					.getMembers().get(0)).getParameters().get(1);
			assertNotNull(param);
			assertEquals("java.lang.Object", param.getParameterType().getIdentifier());
		}

		@Test
		public void createQualifiedActionName() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			final String xtextResourceID = container.eResource().getURI().toString();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			assertNotNull(qn);
			assertEquals("myfct", qn.getActionName());
			// xtextResourceID == "file:/path/to/io/sarl/tests/Stub"+index+".sarl" 
			assertEquals(xtextResourceID + "/io.sarl.tests.Stub"+index, qn.getContainerID());
			assertEquals(xtextResourceID + "/io.sarl.tests.Stub"+index+"#myfct", qn.toString());
		}

		@Test
		public void createConstructorQualifiedName() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			final String xtextResourceID = container.eResource().getURI().toString();
			QualifiedActionName qn = this.provider.createConstructorQualifiedName(container);
			assertNotNull(qn);
			assertEquals("new", qn.getActionName());
			// xtextResourceID == "file:/path/to/io/sarl/tests/Stub"+index+".sarl"
			assertEquals(xtextResourceID + "/io.sarl.tests.Stub"+index, qn.getContainerID());
			assertEquals(xtextResourceID + "/io.sarl.tests.Stub"+index+"#new", qn.toString());
		}

		@Test
		public void createParameterTypesForVoid() {
			ActionParameterTypes key = this.provider.createParameterTypesForVoid();
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertEquals("", key.toString());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypes_varArgs() {
			ActionParameterTypes key = this.provider.createParameterTypes(true, this.parameterProvider);
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object*", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object[]", key.get(2));
		}

		@Test
		public void createParameterTypes_noVarArgs() {
			ActionParameterTypes key = this.provider.createParameterTypes(false, this.parameterProvider);
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object[]", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object[]", key.get(2));
		}

		@Test
		public void createParameterTypesFromString_empty() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertEquals("", key.toString());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypesFromString_void() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("void");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertEquals("", key.toString());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypesFromString_Void() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.Void");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertEquals("", key.toString());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypesFromString_String_noVargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String", key.toString());
			assertEquals(1, key.size());
			assertEquals("java.lang.String", key.get(0));
		}

		@Test
		public void createParameterTypesFromString_StringFloat_noVargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float", key.toString());
			assertEquals(2, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
		}

		@Test
		public void createParameterTypesFromString_StringFloatObject_noVargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float,java.lang.Object");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object", key.get(2));
		}

		@Test
		public void createParameterTypesFromString_String_vargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String*");
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String*", key.toString());
			assertEquals(1, key.size());
			assertEquals("java.lang.String[]", key.get(0));
		}

		@Test
		public void createParameterTypesFromString_StringFloat_vargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float*");
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float*", key.toString());
			assertEquals(2, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float[]", key.get(1));
		}

		@Test
		public void createParameterTypesFromString_StringFloatObject_vargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float,java.lang.Object*");
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object*", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object[]", key.get(2));
		}

		@Test
		public void createParameterTypesFromString_String_noVargArg_array() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String[]");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String[]", key.toString());
			assertEquals(1, key.size());
			assertEquals("java.lang.String[]", key.get(0));
		}

		@Test
		public void createParameterTypesFromString_StringFloat_noVargArg_array() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float[]");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float[]", key.toString());
			assertEquals(2, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float[]", key.get(1));
		}

		@Test
		public void createParameterTypesFromString_StringFloatObject_noVargArg_array() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float,java.lang.Object[]");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object[]", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object[]", key.get(2));
		}

		@Test
		public void createParameterTypesFromSarlModell_void() {
			ActionParameterTypes key = this.provider.createParameterTypesFromSarlModel(false,
					new BasicEList<SarlFormalParameter>());
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypesFromSarlModel_noVarArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromSarlModel(false, this.sarlParameters);
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object", key.get(2));
		}

		@Test
		public void createParameterTypesFromSarlModel_varArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromSarlModel(true, this.sarlParameters);
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object*", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object[]", key.get(2));
		}

		@Test
		public void createParameterTypesFromJvmModel_void() {
			ActionParameterTypes key = this.provider.createParameterTypesFromJvmModel(false, new BasicEList<JvmFormalParameter>());
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypesFromJvmModel_noVarArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromJvmModel(false, this.jvmParameters);
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object[]", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object[]", key.get(2));
		}

		@Test
		public void createParameterTypesFromJvmModel_varArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromJvmModel(true, this.jvmParameters);
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object*", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object[]", key.get(2));
		}

		@Test
		public void createActionPrototype_void() {
			ActionParameterTypes params = this.provider.createParameterTypesFromJvmModel(true, new BasicEList<JvmFormalParameter>());
			ActionPrototype prototype = this.provider.createActionPrototype("myfct", params);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName());
			assertSame(params, prototype.getParametersTypes());
		}

		@Test
		public void createActionPrototype_noVarArg() {
			ActionParameterTypes params = this.provider.createParameterTypesFromJvmModel(false, this.jvmParameters);
			ActionPrototype prototype = this.provider.createActionPrototype("myfct", params);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName());
			assertSame(params, prototype.getParametersTypes());
		}

		@Test
		public void createActionPrototype_varArg() {
			ActionParameterTypes params = this.provider.createParameterTypesFromJvmModel(true, this.jvmParameters);
			ActionPrototype prototype = this.provider.createActionPrototype("myfct", params);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName());
			assertSame(params, prototype.getParametersTypes());
		}

		@Test
		public void createPrototypeFromSarlModel_void() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			EList<SarlFormalParameter> params = new BasicEList<>();
			//
			InferredPrototype prototype = this.provider.createPrototypeFromSarlModel(this.context, qn, false, params);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameFormalParameters(params, prototype.getFormalParameters());
			assertEquals("", prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"");
			assertTrue(prototype.getInferredParameterTypes().isEmpty());
		}

		@Test
		public void createPrototypeFromSarlModel_noVarArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			//
			InferredPrototype prototype = this.provider.createPrototypeFromSarlModel(this.context, qn, false, this.sarlParameters);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameFormalParameters(this.sarlParameters, prototype.getFormalParameters());
			assertEquals(
					"java.lang.String,float,java.lang.Object",
					prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"java.lang.String,float,java.lang.Object");
			assertTrue(prototype.getInferredParameterTypes().isEmpty());
		}

		@Test
		public void createPrototypeFromSarlModel_varArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			//
			InferredPrototype prototype = this.provider.createPrototypeFromSarlModel(this.context, qn, true, this.sarlParameters);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameFormalParameters(this.sarlParameters, prototype.getFormalParameters());
			assertEquals("java.lang.String,float,java.lang.Object*", prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"java.lang.String,float,java.lang.Object*");
			assertTrue(prototype.getInferredParameterTypes().isEmpty());
		}

		@Test
		public void createPrototypeFromJvmModel_void() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			EList<JvmFormalParameter> params = new BasicEList<>();
			//
			InferredPrototype prototype = this.provider.createPrototypeFromJvmModel(this.context, qn, false, params);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameJvmFormalParameters(params, prototype.getFormalParameters());
			assertEquals("", prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"");
			assertTrue(prototype.getInferredParameterTypes().isEmpty());
		}

		@Test
		public void createPrototypeFromJvmModel_noVarArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			//
			InferredPrototype prototype = this.provider.createPrototypeFromJvmModel(this.context, qn, false, this.jvmParameters);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameJvmFormalParameters(this.jvmParameters, prototype.getFormalParameters());
			assertEquals(
					"java.lang.String,float,java.lang.Object[]",
					prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"java.lang.String,float,java.lang.Object[]");
			assertTrue(prototype.getInferredParameterTypes().isEmpty());
		}

		@Test
		public void createPrototypeFromJvmModel_varArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			//
			InferredPrototype prototype = this.provider.createPrototypeFromJvmModel(this.context, qn, true, this.jvmParameters);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameJvmFormalParameters(this.jvmParameters, prototype.getFormalParameters());
			assertEquals("java.lang.String,float,java.lang.Object*", prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"java.lang.String,float,java.lang.Object*");
			assertTrue(prototype.getInferredParameterTypes().isEmpty());
		}

		@Test
		public void getPrototypesQualifiedActionName_noCreatedPrototype() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			//
			Iterable<InferredPrototype> iterable = this.provider.getPrototypes(this.context, qn);
			assertNotNull(iterable);
			assertFalse(iterable.iterator().hasNext());
		}

		@Test
		public void getPrototypesQualifiedActionName_createdPrototype_noVarArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			InferredPrototype expected = this.provider.createPrototypeFromSarlModel(this.context, qn, false, this.sarlParameters);
			//
			Iterable<InferredPrototype> iterable = this.provider.getPrototypes(this.context, qn);
			assertNotNull(iterable);
			Iterator<InferredPrototype> iterator = iterable.iterator();
			assertTrue(iterator.hasNext());
			InferredPrototype prototype = iterator.next();
			assertSame(expected, prototype);
			assertFalse(iterator.hasNext());
		}

		@Test
		public void getPrototypesQualifiedActionName_createdPrototype_varArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			InferredPrototype expected = this.provider.createPrototypeFromSarlModel(this.context, qn, true, this.sarlParameters);
			//
			Iterable<InferredPrototype> iterable = this.provider.getPrototypes(this.context, qn);
			assertNotNull(iterable);
			Iterator<InferredPrototype> iterator = iterable.iterator();
			assertTrue(iterator.hasNext());
			InferredPrototype prototype = iterator.next();
			assertSame(expected, prototype);
			assertFalse(iterator.hasNext());
		}

		@Test
		public void getPrototypesQualifiedActionNameActionParameterTypes_noCreatedPrototype() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(false, this.sarlParameters);
			//
			InferredPrototype prototype = this.provider.getPrototypes(this.context, qn, types);
			assertNull(prototype);
		}

		@Test
		public void getPrototypesQualifiedActionNameActionParameterTypes_createdPrototype_noVarArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(false, this.sarlParameters);
			InferredPrototype expected = this.provider.createPrototypeFromSarlModel(this.context, qn, false, this.sarlParameters);
			//
			InferredPrototype prototype = this.provider.getPrototypes(this.context, qn, types);
			assertNotNull(prototype);
			assertSame(expected, prototype);
		}

		@Test
		public void getPrototypesQualifiedActionNameActionParameterTypes_createdPrototype_varArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(true, this.sarlParameters);
			InferredPrototype expected = this.provider.createPrototypeFromSarlModel(this.context, qn, true, this.sarlParameters);
			//
			InferredPrototype prototype = this.provider.getPrototypes(this.context, qn, types);
			assertNotNull(prototype);
			assertSame(expected, prototype);
		}

		@Test
		public void resetPrototypes_noCreatedProtype() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(false, this.sarlParameters);
			assertNotNull(types);
			//
			assertFalse(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
			this.context.release();
			assertFalse(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
		}

		@Test
		public void resetPrototypes_createdProtype_noVarArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(false, this.sarlParameters);
			assertNotNull(types);
			InferredPrototype prototype = this.provider.createPrototypeFromSarlModel(this.context, qn, false, this.sarlParameters);
			assertNotNull(prototype);
			//
			assertTrue(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
			this.context.release();
			assertFalse(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
		}

		@Test
		public void resetPrototypes_createdProtype_varArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(true, this.sarlParameters);
			assertNotNull(types);
			InferredPrototype prototype = this.provider.createPrototypeFromSarlModel(this.context, qn, true, this.sarlParameters);
			assertNotNull(prototype);
			//
			assertTrue(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
			this.context.release();
			assertFalse(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("DefaultActionPrototypeProvider with default values")
	public class DefaultValues extends AbstractSarlTest {

		@Inject
		private DefaultActionPrototypeProvider provider;

		@Nullable
		private IActionPrototypeContext context;

		@Nullable
		private FormalParameterProvider parameterProvider;

		@Nullable
		private EList<SarlFormalParameter> sarlParameters;

		@Nullable
		private EList<JvmFormalParameter> jvmParameters;

		@BeforeEach
		public void setUp() throws Exception {
			this.context = this.provider.createContext();
			this.parameterProvider = mock(FormalParameterProvider.class);
			when(this.parameterProvider.getFormalParameterCount()).thenReturn(4);
			when(this.parameterProvider.getFormalParameterName(ArgumentMatchers.anyInt())).thenAnswer((invocation) -> {
				switch(((Integer) invocation.getArguments()[0]).intValue()) {
				case 0:
					return "firstarg";
				case 1:
					return "secondarg";
				case 2:
					return "thirdarg";
				case 3:
					return "fourtharg";
				}
				return null;
			});
			when(this.parameterProvider.getFormalParameterType(ArgumentMatchers.anyInt(),
					ArgumentMatchers.anyBoolean())).thenAnswer((invocation) -> {
						switch(((Integer) invocation.getArguments()[0]).intValue()) {
						case 0:
							return "java.lang.String";
						case 1:
							return "int";
						case 2:
							return "float";
						case 3:
							return "java.lang.Object[]";
						}
						return null;
					});
			when(this.parameterProvider.hasFormalParameterDefaultValue(ArgumentMatchers.anyInt())).thenAnswer((invocation) -> {
				switch(((Integer) invocation.getArguments()[0]).intValue()) {
				case 0:
				case 2:
					return Boolean.TRUE;
				}
				return Boolean.FALSE;
			});
			//
			SarlFormalParameter p;
			this.sarlParameters = new BasicEList<>();

			p = mock(SarlFormalParameter.class);
			when(p.getName()).thenReturn("firstarg");
			JvmTypeReference ref = getType(getParseHelper(), "java.lang.String");
			when(p.getParameterType()).thenReturn(ref);
			when(p.getDefaultValue()).thenReturn(mock(XExpression.class));
			this.sarlParameters.add(p);

			p = mock(SarlFormalParameter.class);
			when(p.getName()).thenReturn("secondarg");
			ref = getType(getParseHelper(), "int");
			when(p.getParameterType()).thenReturn(ref);
			this.sarlParameters.add(p);

			p = mock(SarlFormalParameter.class);
			when(p.getName()).thenReturn("thirdarg");
			ref = getType(getParseHelper(), "float");
			when(p.getParameterType()).thenReturn(ref);
			when(p.getDefaultValue()).thenReturn(mock(XExpression.class));
			this.sarlParameters.add(p);

			p = mock(SarlFormalParameter.class);
			when(p.getName()).thenReturn("fourtharg");
			ref = getType(getParseHelper(), "java.lang.Object");
			when(p.getParameterType()).thenReturn(ref);
			this.sarlParameters.add(p);
			//
			JvmFormalParameter jp;
			this.jvmParameters = new BasicEList<>();

			jp = mock(JvmFormalParameter.class);
			when(jp.getName()).thenReturn("firstarg");
			ref = getType(getParseHelper(), "java.lang.String");
			when(jp.getParameterType()).thenReturn(ref);
			JvmAnnotationType annotationType = mock(JvmAnnotationType.class);
			when(annotationType.getQualifiedName()).thenReturn(DefaultValue.class.getName());
			JvmAnnotationReference annotationRef = mock(JvmAnnotationReference.class);
			when(annotationRef.getAnnotation()).thenReturn(annotationType);
			when(jp.getAnnotations()).thenReturn(ECollections.singletonEList(annotationRef));
			when(jp.eIsSet(ArgumentMatchers.any())).thenAnswer((invocation) -> {
				if (Objects.equals(TypesPackage.Literals.JVM_ANNOTATION_TARGET__ANNOTATIONS,
						invocation.getArguments()[0])) {
					return Boolean.TRUE;
				}
				return Boolean.FALSE;
			});
			this.jvmParameters.add(jp);

			jp = mock(JvmFormalParameter.class);
			when(jp.getName()).thenReturn("secondarg");
			ref = getType(getParseHelper(), "int");
			when(jp.getParameterType()).thenReturn(ref);
			when(jp.getAnnotations()).thenReturn(ECollections.<JvmAnnotationReference>emptyEList());
			this.jvmParameters.add(jp);

			jp = mock(JvmFormalParameter.class);
			when(jp.getName()).thenReturn("thirdarg");
			ref = getType(getParseHelper(), "float");
			when(jp.getParameterType()).thenReturn(ref);
			annotationType = mock(JvmAnnotationType.class);
			when(annotationType.getQualifiedName()).thenReturn(DefaultValue.class.getName());
			annotationRef = mock(JvmAnnotationReference.class);
			when(annotationRef.getAnnotation()).thenReturn(annotationType);
			when(jp.getAnnotations()).thenReturn(ECollections.singletonEList(annotationRef));
			when(jp.eIsSet(ArgumentMatchers.any())).thenAnswer((invocation) -> {
				if (Objects.equals(TypesPackage.Literals.JVM_ANNOTATION_TARGET__ANNOTATIONS,
						invocation.getArguments()[0])) {
					return Boolean.TRUE;
				}
				return Boolean.FALSE;
			});
			this.jvmParameters.add(jp);

			jp = mock(JvmFormalParameter.class);
			when(jp.getName()).thenReturn("fourtharg");
			ref = getType(getParseHelper(), "java.lang.Object[]");
			when(jp.getParameterType()).thenReturn(ref);
			when(jp.getAnnotations()).thenReturn(ECollections.<JvmAnnotationReference>emptyEList());
			this.jvmParameters.add(jp);
		}

		@Test
		public void validateTypeOfVarArgInSarl() throws Exception {
			SarlScript s = file(getParseHelper(), "agent Foo { def fooFct(a : float, b : Object*) {} }");
			SarlFormalParameter param = (SarlFormalParameter) ((SarlAction) ((SarlAgent) s.getXtendTypes().get(0))
					.getMembers().get(0)).getParameters().get(1);
			assertNotNull(param);
			assertEquals("java.lang.Object", param.getParameterType().getIdentifier());
		}

		@Test
		public void createQualifiedActionName() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			final String xtextResourceID = container.eResource().getURI().toString();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			assertNotNull(qn);
			assertEquals("myfct", qn.getActionName());
			//xtextResourceID == "file:/path/to/io/sarl/tests/Stub"+index+".sarl"
			assertEquals(xtextResourceID + "/io.sarl.tests.Stub"+index, qn.getContainerID());
			assertEquals(xtextResourceID + "/io.sarl.tests.Stub"+index+"#myfct", qn.toString());
		}

		@Test
		public void createConstructorQualifiedName() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			final String xtextResourceID = container.eResource().getURI().toString();
			QualifiedActionName qn = this.provider.createConstructorQualifiedName(container);
			assertNotNull(qn);
			assertEquals("new", qn.getActionName());
			//xtextResourceID == "file:/path/to/io/sarl/tests/Stub"+index+".sarl"
			assertEquals(xtextResourceID + "/io.sarl.tests.Stub"+index, qn.getContainerID());
			assertEquals(xtextResourceID + "/io.sarl.tests.Stub"+index+"#new", qn.toString());
		}

		@Test
		public void createParameterTypesForVoid() {
			ActionParameterTypes key = this.provider.createParameterTypesForVoid();
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertEquals("", key.toString());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypes_varArgs() {
			ActionParameterTypes key = this.provider.createParameterTypes(true, this.parameterProvider);
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,int,float,java.lang.Object*", key.toString());
			assertEquals(4, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("int", key.get(1));
			assertEquals("float", key.get(2));
			assertEquals("java.lang.Object[]", key.get(3));
		}

		@Test
		public void createParameterTypes_noVarArgs() {
			ActionParameterTypes key = this.provider.createParameterTypes(false, this.parameterProvider);
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,int,float,java.lang.Object[]", key.toString());
			assertEquals(4, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("int", key.get(1));
			assertEquals("float", key.get(2));
			assertEquals("java.lang.Object[]", key.get(3));
		}

		@Test
		public void createParameterTypesFromString_empty() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertEquals("", key.toString());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypesFromString_void() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("void");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertEquals("", key.toString());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypesFromString_Void() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.Void");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertEquals("", key.toString());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypesFromString_String_noVargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String", key.toString());
			assertEquals(1, key.size());
			assertEquals("java.lang.String", key.get(0));
		}

		@Test
		public void createParameterTypesFromString_StringFloat_noVargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float", key.toString());
			assertEquals(2, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
		}

		@Test
		public void createParameterTypesFromString_StringFloatObject_noVargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float,java.lang.Object");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object", key.get(2));
		}

		@Test
		public void createParameterTypesFromString_String_vargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String*");
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String*", key.toString());
			assertEquals(1, key.size());
			assertEquals("java.lang.String[]", key.get(0));
		}

		@Test
		public void createParameterTypesFromString_StringFloat_vargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float*");
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float*", key.toString());
			assertEquals(2, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float[]", key.get(1));
		}

		@Test
		public void createParameterTypesFromString_StringFloatObject_vargArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float,java.lang.Object*");
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object*", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object[]", key.get(2));
		}

		@Test
		public void createParameterTypesFromString_String_noVargArg_array() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String[]");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String[]", key.toString());
			assertEquals(1, key.size());
			assertEquals("java.lang.String[]", key.get(0));
		}

		@Test
		public void createParameterTypesFromString_StringFloat_noVargArg_array() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float[]");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float[]", key.toString());
			assertEquals(2, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float[]", key.get(1));
		}

		@Test
		public void createParameterTypesFromString_StringFloatObject_noVargArg_array() {
			ActionParameterTypes key = this.provider.createParameterTypesFromString("java.lang.String,float,java.lang.Object[]");
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,float,java.lang.Object[]", key.toString());
			assertEquals(3, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("float", key.get(1));
			assertEquals("java.lang.Object[]", key.get(2));
		}

		@Test
		public void createParameterTypesFromSarlModell_void() {
			ActionParameterTypes key = this.provider.createParameterTypesFromSarlModel(false,
					new BasicEList<SarlFormalParameter>());
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypesFromSarlModel_noVarArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromSarlModel(false, this.sarlParameters);
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,int,float,java.lang.Object", key.toString());
			assertEquals(4, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("int", key.get(1));
			assertEquals("float", key.get(2));
			assertEquals("java.lang.Object", key.get(3));
		}

		@Test
		public void createParameterTypesFromSarlModel_varArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromSarlModel(true, this.sarlParameters);
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,int,float,java.lang.Object*", key.toString());
			assertEquals(4, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("int", key.get(1));
			assertEquals("float", key.get(2));
			assertEquals("java.lang.Object[]", key.get(3));
		}

		@Test
		public void createParameterTypesFromJvmModel_void() {
			ActionParameterTypes key = this.provider.createParameterTypesFromJvmModel(false, new BasicEList<JvmFormalParameter>());
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertTrue(key.isVoid());
			assertTrue(key.isEmpty());
		}

		@Test
		public void createParameterTypesFromJvmModel_noVarArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromJvmModel(false, this.jvmParameters);
			assertNotNull(key);
			assertFalse(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,int,float,java.lang.Object[]", key.toString());
			assertEquals(4, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("int", key.get(1));
			assertEquals("float", key.get(2));
			assertEquals("java.lang.Object[]", key.get(3));
		}

		@Test
		public void createParameterTypesFromJvmModel_varArg() {
			ActionParameterTypes key = this.provider.createParameterTypesFromJvmModel(true, this.jvmParameters);
			assertNotNull(key);
			assertTrue(key.isVarArg());
			assertFalse(key.isVoid());
			assertEquals("java.lang.String,int,float,java.lang.Object*", key.toString());
			assertEquals(4, key.size());
			assertEquals("java.lang.String", key.get(0));
			assertEquals("int", key.get(1));
			assertEquals("float", key.get(2));
			assertEquals("java.lang.Object[]", key.get(3));
		}

		@Test
		public void createActionPrototype_void() {
			ActionParameterTypes params = this.provider.createParameterTypesFromJvmModel(true, new BasicEList<JvmFormalParameter>());
			ActionPrototype prototype = this.provider.createActionPrototype("myfct", params);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName());
			assertSame(params, prototype.getParametersTypes());
		}

		@Test
		public void createActionPrototype_noVarArg() {
			ActionParameterTypes params = this.provider.createParameterTypesFromJvmModel(false, this.jvmParameters);
			ActionPrototype prototype = this.provider.createActionPrototype("myfct", params);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName());
			assertSame(params, prototype.getParametersTypes());
		}

		@Test
		public void createActionPrototype_varArg() {
			ActionParameterTypes params = this.provider.createParameterTypesFromJvmModel(true, this.jvmParameters);
			ActionPrototype prototype = this.provider.createActionPrototype("myfct", params);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName());
			assertSame(params, prototype.getParametersTypes());
		}

		@Test
		public void createPrototypeFromSarlModel_void() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			EList<SarlFormalParameter> params = new BasicEList<>();
			//
			InferredPrototype prototype = this.provider.createPrototypeFromSarlModel(this.context, qn, false, params);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameFormalParameters(params, prototype.getFormalParameters());
			assertEquals("", prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"");
			assertTrue(prototype.getInferredParameterTypes().isEmpty());
		}

		@Test
		public void createPrototypeFromSarlModel_noVarArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			//
			InferredPrototype prototype = this.provider.createPrototypeFromSarlModel(this.context, qn, false, this.sarlParameters);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameFormalParameters(this.sarlParameters, prototype.getFormalParameters());
			assertEquals(
					"java.lang.String,int,float,java.lang.Object",
					prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"java.lang.String,int,float,java.lang.Object",
					"java.lang.String,int,java.lang.Object",
					"int,float,java.lang.Object",
					"int,java.lang.Object");
			assertPrototypes(prototype.getOriginalParameterTypes(),
					InferredStandardParameter.class,
					"java.lang.String",
					"firstarg",
					InferredStandardParameter.class,
					"int",
					"secondarg",
					InferredStandardParameter.class,
					"float",
					"thirdarg",
					InferredStandardParameter.class,
					"java.lang.Object",
					"fourtharg");
			assertPrototypes(prototype.getInferredParameterTypes(),
					new Object[] {
							InferredStandardParameter.class,
							"java.lang.String",
							"firstarg",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredValuedParameter.class,
							"float",
							"thirdarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_1",
							InferredStandardParameter.class,
							"java.lang.Object",
							"fourtharg",
			},
					new Object[] {
							InferredValuedParameter.class,
							"java.lang.String",
							"firstarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_0",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredStandardParameter.class,
							"float",
							"thirdarg",
							InferredStandardParameter.class,
							"java.lang.Object",
							"fourtharg",
			},
					new Object[] {
							InferredValuedParameter.class,
							"java.lang.String",
							"firstarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_0",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredValuedParameter.class,
							"float",
							"thirdarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_1",
							InferredStandardParameter.class,
							"java.lang.Object",
							"fourtharg",
			});
		}

		@Test
		public void createPrototypeFromSarlModel_varArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			//
			InferredPrototype prototype = this.provider.createPrototypeFromSarlModel(this.context, qn, true, this.sarlParameters);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameFormalParameters(this.sarlParameters, prototype.getFormalParameters());
			assertEquals("java.lang.String,int,float,java.lang.Object*", prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"java.lang.String,int,float,java.lang.Object*",
					"java.lang.String,int,java.lang.Object*",
					"int,float,java.lang.Object*",
					"int,java.lang.Object*");
			assertPrototypes(prototype.getOriginalParameterTypes(),
					InferredStandardParameter.class,
					"java.lang.String",
					"firstarg",
					InferredStandardParameter.class,
					"int",
					"secondarg",
					InferredStandardParameter.class,
					"float",
					"thirdarg",
					InferredStandardParameter.class,
					"java.lang.Object[]",
					"fourtharg");
			assertPrototypes(prototype.getInferredParameterTypes(),
					new Object[] {
							InferredStandardParameter.class,
							"java.lang.String",
							"firstarg",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredValuedParameter.class,
							"float",
							"thirdarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_1",
							InferredStandardParameter.class,
							"java.lang.Object[]",
							"fourtharg",
			},
					new Object[] {
							InferredValuedParameter.class,
							"java.lang.String",
							"firstarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_0",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredStandardParameter.class,
							"float",
							"thirdarg",
							InferredStandardParameter.class,
							"java.lang.Object[]",
							"fourtharg",
			},
					new Object[] {
							InferredValuedParameter.class,
							"java.lang.String",
							"firstarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_0",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredValuedParameter.class,
							"float",
							"thirdarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_1",
							InferredStandardParameter.class,
							"java.lang.Object[]",
							"fourtharg",
			});
		}

		@Test
		public void createPrototypeFromJvmModel_void() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			EList<JvmFormalParameter> params = new BasicEList<>();
			//
			InferredPrototype prototype = this.provider.createPrototypeFromJvmModel(this.context, qn, false, params);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameJvmFormalParameters(params, prototype.getFormalParameters());
			assertEquals("", prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"");
			assertTrue(prototype.getInferredParameterTypes().isEmpty());
		}

		@Test
		public void createPrototypeFromJvmModel_noVarArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			//
			InferredPrototype prototype = this.provider.createPrototypeFromJvmModel(this.context, qn, false, this.jvmParameters);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameJvmFormalParameters(this.jvmParameters, prototype.getFormalParameters());
			assertEquals(
					"java.lang.String,int,float,java.lang.Object[]",
					prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"java.lang.String,int,float,java.lang.Object[]",
					"java.lang.String,int,java.lang.Object[]",
					"int,float,java.lang.Object[]",
					"int,java.lang.Object[]");
			assertPrototypes(prototype.getOriginalParameterTypes(),
					InferredStandardParameter.class,
					"java.lang.String",
					"firstarg",
					InferredStandardParameter.class,
					"int",
					"secondarg",
					InferredStandardParameter.class,
					"float",
					"thirdarg",
					InferredStandardParameter.class,
					"java.lang.Object[]",
					"fourtharg");
			assertPrototypes(prototype.getInferredParameterTypes(),
					new Object[] {
							InferredStandardParameter.class,
							"java.lang.String",
							"firstarg",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredValuedParameter.class,
							"float",
							"thirdarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_1",
							InferredStandardParameter.class,
							"java.lang.Object[]",
							"fourtharg",
			},
					new Object[] {
							InferredValuedParameter.class,
							"java.lang.String",
							"firstarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_0",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredStandardParameter.class,
							"float",
							"thirdarg",
							InferredStandardParameter.class,
							"java.lang.Object[]",
							"fourtharg",
			},
					new Object[] {
							InferredValuedParameter.class,
							"java.lang.String",
							"firstarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_0",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredValuedParameter.class,
							"float",
							"thirdarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_1",
							InferredStandardParameter.class,
							"java.lang.Object[]",
							"fourtharg",
			});
		}

		@Test
		public void createPrototypeFromJvmModel_varArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			//
			InferredPrototype prototype = this.provider.createPrototypeFromJvmModel(this.context, qn, true, this.jvmParameters);
			assertNotNull(prototype);
			assertEquals("myfct", prototype.getActionName().getActionName());
			assertSameJvmFormalParameters(this.jvmParameters, prototype.getFormalParameters());
			assertEquals(
					"java.lang.String,int,float,java.lang.Object*",
					prototype.getFormalParameterTypes().toString());
			assertContainsStrings(
					prototype.getParameterTypeAlternatives(),
					"java.lang.String,int,float,java.lang.Object*",
					"java.lang.String,int,java.lang.Object*",
					"int,float,java.lang.Object*",
					"int,java.lang.Object*");
			assertPrototypes(prototype.getOriginalParameterTypes(),
					InferredStandardParameter.class,
					"java.lang.String",
					"firstarg",
					InferredStandardParameter.class,
					"int",
					"secondarg",
					InferredStandardParameter.class,
					"float",
					"thirdarg",
					InferredStandardParameter.class,
					"java.lang.Object[]",
					"fourtharg");
			assertPrototypes(prototype.getInferredParameterTypes(),
					new Object[] {
							InferredStandardParameter.class,
							"java.lang.String",
							"firstarg",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredValuedParameter.class,
							"float",
							"thirdarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_1",
							InferredStandardParameter.class,
							"java.lang.Object[]",
							"fourtharg",
			},
					new Object[] {
							InferredValuedParameter.class,
							"java.lang.String",
							"firstarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_0",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredStandardParameter.class,
							"float",
							"thirdarg",
							InferredStandardParameter.class,
							"java.lang.Object[]",
							"fourtharg",
			},
					new Object[] {
							InferredValuedParameter.class,
							"java.lang.String",
							"firstarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_0",
							InferredStandardParameter.class,
							"int",
							"secondarg",
							InferredValuedParameter.class,
							"float",
							"thirdarg",
							"io.sarl.tests.Stub" + index + "#MYFCT_1",
							InferredStandardParameter.class,
							"java.lang.Object[]",
							"fourtharg",
			});
		}

		@Test
		public void getPrototypesQualifiedActionName_noCreatedPrototype() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			//
			Iterable<InferredPrototype> iterable = this.provider.getPrototypes(this.context, qn);
			assertNotNull(iterable);
			assertFalse(iterable.iterator().hasNext());
		}

		@Test
		public void getPrototypesQualifiedActionName_createdPrototype_noVarArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			InferredPrototype expected = this.provider.createPrototypeFromSarlModel(this.context, qn, false, this.sarlParameters);
			//
			Iterable<InferredPrototype> iterable = this.provider.getPrototypes(this.context, qn);
			assertNotNull(iterable);
			Iterator<InferredPrototype> iterator = iterable.iterator();
			assertTrue(iterator.hasNext());
			InferredPrototype prototype = iterator.next();
			assertSame(expected, prototype);
			assertFalse(iterator.hasNext());
		}

		@Test
		public void getPrototypesQualifiedActionName_createdPrototype_varArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			InferredPrototype expected = this.provider.createPrototypeFromSarlModel(this.context, qn, true, this.sarlParameters);
			//
			Iterable<InferredPrototype> iterable = this.provider.getPrototypes(this.context, qn);
			assertNotNull(iterable);
			Iterator<InferredPrototype> iterator = iterable.iterator();
			assertTrue(iterator.hasNext());
			InferredPrototype prototype = iterator.next();
			assertSame(expected, prototype);
			assertFalse(iterator.hasNext());
		}

		@Test
		public void getPrototypesQualifiedActionNameActionParameterTypes_noCreatedPrototype() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(false, this.sarlParameters);
			//
			InferredPrototype prototype = this.provider.getPrototypes(this.context, qn, types);
			assertNull(prototype);
		}

		@Test
		public void getPrototypesQualifiedActionNameActionParameterTypes_createdPrototype_noVarArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(false, this.sarlParameters);
			InferredPrototype expected = this.provider.createPrototypeFromSarlModel(this.context, qn, false, this.sarlParameters);
			//
			InferredPrototype prototype = this.provider.getPrototypes(this.context, qn, types);
			assertNotNull(prototype);
			assertSame(expected, prototype);
		}

		@Test
		public void getPrototypesQualifiedActionNameActionParameterTypes_createdPrototype_varArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(true, this.sarlParameters);
			InferredPrototype expected = this.provider.createPrototypeFromSarlModel(this.context, qn, true, this.sarlParameters);
			//
			InferredPrototype prototype = this.provider.getPrototypes(this.context, qn, types);
			assertNotNull(prototype);
			assertSame(expected, prototype);
		}

		@Test
		public void resetPrototypes_noCreatedProtype() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(false, this.sarlParameters);
			assertNotNull(types);
			//
			assertFalse(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
			this.context.release();
			assertFalse(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
		}

		@Test
		public void resetPrototypes_createdProtype_noVarArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(false, this.sarlParameters);
			assertNotNull(types);
			InferredPrototype prototype = this.provider.createPrototypeFromSarlModel(this.context, qn, false, this.sarlParameters);
			assertNotNull(prototype);
			//
			assertTrue(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
			this.context.release();
			assertFalse(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
		}

		@Test
		public void resetPrototypes_createdProtype_varArg() {
			JvmIdentifiableElement container = createJvmIdentifiableElementStub();
			QualifiedActionName qn = this.provider.createQualifiedActionName(container, "myfct");
			ActionParameterTypes types = this.provider.createParameterTypesFromSarlModel(true, this.sarlParameters);
			assertNotNull(types);
			InferredPrototype prototype = this.provider.createPrototypeFromSarlModel(this.context, qn, true, this.sarlParameters);
			assertNotNull(prototype);
			//
			assertTrue(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
			this.context.release();
			assertFalse(this.provider.getPrototypes(this.context, qn).iterator().hasNext());
		}

	}

}