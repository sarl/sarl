/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.tests.util;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import io.sarl.eclipse.util.Jdt2Ecore;
import io.sarl.eclipse.util.Jdt2Ecore.TypeFinder;
import io.sarl.lang.genmodel.SARLCodeGenerator;
import io.sarl.lang.genmodel.SARLCodeGenerator.GeneratedCode;
import io.sarl.lang.sarl.ParameterizedFeature;
import io.sarl.lang.signature.ActionKey;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.ActionSignatureProvider.FormalParameterProvider;
import io.sarl.lang.signature.SignatureKey;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.Nullable;
import io.sarl.tests.api.SARLNatureNeededForTest;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.mockito.Matchers;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	Jdt2EcoreTest.IsGeneratedOperation.class,
	Jdt2EcoreTest.GetAnnotation.class,
	Jdt2EcoreTest.FindType.class,
	Jdt2EcoreTest.IsVisible.class,
	Jdt2EcoreTest.FormalParameter.class,
	Jdt2EcoreTest.PopulateInheritanceContext.class,
})
@SuppressWarnings("all")
public class Jdt2EcoreTest {

	protected static String getResourceText(String id) {
		ResourceBundle bundle = ResourceBundle.getBundle(Jdt2EcoreTest.class.getName().replace(".", "/"));
		assertNotNull(bundle);
		String value = bundle.getString(id);
		assertFalse(Strings.isNullOrEmpty(value));
		return value;
	}

	/** Replies a mock of a IType.
	 *
	 * @param fullyQualifiedName - fully qualified name of the type.
	 * @param superClass - fully qualified name of the super class.
	 * @throws JavaModelException 
	 */
	protected static IType createITypeMock(String fullyQualifiedName, String superClass) throws JavaModelException {
		IType type = mock(IType.class);
		when(type.getFullyQualifiedName()).thenReturn(fullyQualifiedName);
		IPackageFragment packageFragment = mock(IPackageFragment.class);
		int idx = fullyQualifiedName.lastIndexOf('.');
		if (idx >= 0) {
			when(packageFragment.getElementName()).thenReturn(fullyQualifiedName.substring(0, idx));
			when(packageFragment.getElementType()).thenReturn(IJavaElement.PACKAGE_FRAGMENT);
			when(packageFragment.isDefaultPackage()).thenReturn(false);
			when(type.getElementName()).thenReturn(fullyQualifiedName.substring(idx + 1));
		} else {
			when(packageFragment.getElementName()).thenReturn("");
			when(packageFragment.getElementType()).thenReturn(IJavaElement.PACKAGE_FRAGMENT);
			when(packageFragment.isDefaultPackage()).thenReturn(true);
			when(type.getElementName()).thenReturn(fullyQualifiedName);
		}
		when(type.getPackageFragment()).thenReturn(packageFragment);
		when(type.getElementType()).thenReturn(IJavaElement.TYPE);
		if (!Strings.isNullOrEmpty(superClass)) {
			when(type.getSuperclassName()).thenReturn(superClass);
		}
		return type;
	}

	/** Replies a mock of a IMethod.
	 *
	 * @param type - the enclosing type of the method.
	 * @param methodName - the name of the method.
	 * @param returnType - the type of the returned value.
	 * @throws JavaModelException 
	 */
	protected static IMethod createIMethodMock(IType type, String methodName, String returnType,
			String[] parameterNames, String[] parameterTypes, int flags) throws JavaModelException {
		return createIMethodMock(type, methodName, returnType, parameterNames, parameterTypes,
				new IAnnotation[parameterNames.length], new IAnnotation[0], flags);
	}

	/** Replies a mock of a IMethod.
	 *
	 * @param type - the enclosing type of the method.
	 * @param methodName - the name of the method.
	 * @param returnType - the type of the returned value.
	 * @throws JavaModelException 
	 */
	protected static IMethod createIMethodMock(IType type, String methodName, String returnType,
			String[] parameterNames, String[] parameterTypes,
			IAnnotation[] parameterAnnotations, IAnnotation[] methodAnnotations, 
			int flags) throws JavaModelException {
		IMethod method = mock(IMethod.class);
		when(method.getDeclaringType()).thenReturn(type);
		when(method.getElementName()).thenReturn(methodName);
		IAnnotation[] ma = methodAnnotations;
		if (ma == null) {
			ma = new IAnnotation[0];
		}
		when(method.getAnnotations()).thenReturn(ma);
		if (Strings.isNullOrEmpty(returnType)) {
			when(method.getReturnType()).thenReturn("void");
		} else {
			when(method.getReturnType()).thenReturn(returnType);
		}
		when(method.getParameterNames()).thenReturn(parameterNames);
		when(method.getParameterTypes()).thenReturn(parameterTypes);
		ILocalVariable[] parameters = new ILocalVariable[parameterNames.length];
		for (int i = 0; i < parameterNames.length; ++i) {
			ILocalVariable var = mock(ILocalVariable.class);
			when(var.getElementName()).thenReturn(parameterNames[i]);
			when(var.getTypeSignature()).thenReturn(parameterTypes[i]);
			when(var.getDeclaringMember()).thenReturn(method);
			IAnnotation a = parameterAnnotations[i];
			if (a == null) {
				when(var.getAnnotations()).thenReturn(new IAnnotation[0]);
			} else {
				when(var.getAnnotations()).thenReturn(new IAnnotation[] { a });
			}
			parameters[i] = var;
		}
		when(method.getParameters()).thenReturn(parameters);
		when(method.getFlags()).thenReturn(flags);
		return method;
	}

	/** Replies a mock of a IJavaProject.
	 *
	 * @return the mock.
	 * @throws JavaModelException
	 */
	protected static IJavaProject createIJavaProjectMock() throws JavaModelException {
		final IJavaProject project = mock(IJavaProject.class);
		when(project.findType(Matchers.anyString())).thenAnswer(new Answer<IType>() {
			@Override
			public IType answer(InvocationOnMock invocation)
					throws Throwable {
				String fqn = (String) invocation.getArguments()[0];
				IType type = mock(IType.class);
				when(type.getJavaProject()).thenReturn(project);
				when(type.getFullyQualifiedName()).thenReturn(fqn);
				when(type.getElementName()).thenReturn(fqn);
				return type;
			}
		});
		return project;
	}

	/** Assert the action keys. The order of the key is not important.
	 * 
	 * @param actualKeys
	 * @param expectedKeys
	 */
	protected static void assertActionKeys(Set<? extends ActionKey> actualKeys, String... expectedKeys) {
		Set<String> expected = Sets.newHashSet(expectedKeys);
		for (ActionKey actualKey : actualKeys) {
			Iterator<String> iterator = expected.iterator();
			boolean cont = true;
			while (cont && iterator.hasNext()) {
				String actual = iterator.next();
				if (actual.equals(actualKey.toString())) {
					iterator.remove();
					cont = false;
				}
			}
			if (cont) {
				fail("Action key not expected: " + actualKey.toString());
			}
		}
		if (!expected.isEmpty()) {
			fail("Not enough action keys. Expected: " + Arrays.toString(expectedKeys)
					+ ". Actual: " + Iterables.toString(actualKeys));
		}
	}

	/** Assert the signature keys. The order of the key is not important.
	 * 
	 * @param actualKeys
	 * @param expectedKeys
	 */
	protected static void assertSignatureKeys(Set<? extends SignatureKey> actualKeys, String... expectedKeys) {
		Set<String> expected = Sets.newHashSet(expectedKeys);
		for (SignatureKey actualKey : actualKeys) {
			Iterator<String> iterator = expected.iterator();
			boolean cont = true;
			while (cont && iterator.hasNext()) {
				String actual = iterator.next();
				if (actual.equals(actualKey.toString())) {
					iterator.remove();
					cont = false;
				}
			}
			if (cont) {
				fail("Signature key not expected: " + actualKey.toString());
			}
		}
		if (!expected.isEmpty()) {
			fail("Not enough signature keys. Expected: " + Arrays.toString(expectedKeys)
					+ ". Actual: " + Iterables.toString(actualKeys));
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class IsGeneratedOperation extends AbstractSarlUiTest {	

		@Test
		public void isGeneratedOperation_noAnnotation() throws JavaModelException {
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
			});
			assertFalse(Jdt2Ecore.isGeneratedOperation(m));
		}

		@Test
		public void isGeneratedOperation_generatedAnnotation() throws JavaModelException {
			IAnnotation annot = mock(IAnnotation.class);
			when(annot.getElementName()).thenReturn("io.sarl.lang.annotation.Generated");
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
					annot
			});
			assertTrue(Jdt2Ecore.isGeneratedOperation(m));
		}

		@Test
		public void isGeneratedOperation_otherAnnotation() throws JavaModelException {
			IAnnotation annot = mock(IAnnotation.class);
			when(annot.getElementName()).thenReturn("javax.annotation.Generated");
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
					annot
			});
			assertFalse(Jdt2Ecore.isGeneratedOperation(m));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class GetAnnotation extends AbstractSarlUiTest {	

		@Test
		public void getAnnotation_noAnnotation() throws JavaModelException {
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
			});
			assertNull(Jdt2Ecore.getAnnotation(m, "io.sarl.lang.annotation.Generated"));
		}

		@Test
		public void getAnnotation_generatedAnnotation() throws JavaModelException {
			IAnnotation annot = mock(IAnnotation.class);
			when(annot.getElementName()).thenReturn("io.sarl.lang.annotation.Generated");
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
					annot
			});
			IAnnotation a = Jdt2Ecore.getAnnotation(m, "io.sarl.lang.annotation.Generated");
			assertNotNull(a);
			assertEquals("io.sarl.lang.annotation.Generated", a.getElementName());
		}

		@Test
		public void getAnnotation_otherAnnotation() throws JavaModelException {
			IAnnotation annot = mock(IAnnotation.class);
			when(annot.getElementName()).thenReturn("javax.annotation.Generated");
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
					annot
			});
			assertNull(Jdt2Ecore.getAnnotation(m, "io.sarl.lang.annotation.Generated"));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class FindType extends AbstractSarlUiTest {	

		@Nullable
		private IJavaProject project;

		@Before
		public void setUp() throws JavaModelException {
			this.project = createIJavaProjectMock();
		}

		@Test
		public void javaLangString() throws JavaModelException {
			IType type = this.project.findType("java.lang.String");
			assertNotNull(type);
			assertEquals("java.lang.String", type.getFullyQualifiedName());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@RunWith(Suite.class)
	@SuiteClasses({
		Jdt2EcoreTest.IsVisible.DefaultpackageDefaultpackageNosubtype.class,
		Jdt2EcoreTest.IsVisible.DefaultpackageDefaultpackageSubtype.class,
		Jdt2EcoreTest.IsVisible.DefaultpackagePackage1Nosubtype.class,
		Jdt2EcoreTest.IsVisible.DefaultpackagePackage1Subtype.class,
		Jdt2EcoreTest.IsVisible.Package1Package1Nosubtype.class,
		Jdt2EcoreTest.IsVisible.Package1Package1Subtype.class,
		Jdt2EcoreTest.IsVisible.Package1Package2Nosubtype.class,
		Jdt2EcoreTest.IsVisible.Package1Package2Subtype.class,
	})
	public static class IsVisible {	

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class DefaultpackageDefaultpackageNosubtype extends AbstractSarlUiTest {

			@Nullable
			private IJavaProject project;

			@Before
			public void setUp() throws JavaModelException {
				this.project = createIJavaProjectMock();
			}

			@Test
			public void publicMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock("Type2", null);

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("Type2", null);

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("Type2", null);

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("Type2", null);

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class DefaultpackageDefaultpackageSubtype extends AbstractSarlUiTest {

			@Nullable
			private IJavaProject project;

			@Before
			public void setUp() throws JavaModelException {
				this.project = createIJavaProjectMock();
			}

			@Test
			public void publicMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock("Type2", "Type1");

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("Type2", "Type1");

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("Type2", "Type1");

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("Type2", "Type1");

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class DefaultpackagePackage1Nosubtype extends AbstractSarlUiTest {

			@Nullable
			private IJavaProject project;

			@Before
			public void setUp() throws JavaModelException {
				this.project = createIJavaProjectMock();
			}

			@Test
			public void publicMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class DefaultpackagePackage1Subtype extends AbstractSarlUiTest {

			@Nullable
			private IJavaProject project;

			@Before
			public void setUp() throws JavaModelException {
				this.project = createIJavaProjectMock();
			}

			@Test
			public void publicMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "Type1");

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "Type1");

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "Type1");

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "Type1");

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class Package1Package1Nosubtype extends AbstractSarlUiTest {

			@Nullable
			private IJavaProject project;

			@Before
			public void setUp() throws JavaModelException {
				this.project = createIJavaProjectMock();
			}

			@Test
			public void publicMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class Package1Package1Subtype extends AbstractSarlUiTest {

			@Nullable
			private IJavaProject project;

			@Before
			public void setUp() throws JavaModelException {
				this.project = createIJavaProjectMock();
			}

			@Test
			public void publicMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class Package1Package2Nosubtype extends AbstractSarlUiTest {

			@Nullable
			private IJavaProject project;

			@Before
			public void setUp() throws JavaModelException {
				this.project = createIJavaProjectMock();
			}

			@Test
			public void publicMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", null);

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", null);

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", null);

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", null);

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class Package1Package2Subtype extends AbstractSarlUiTest {

			@Nullable
			private IJavaProject project;

			@Before
			public void setUp() throws JavaModelException {
				this.project = createIJavaProjectMock();
			}

			@Test
			public void publicMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertFalse(Jdt2Ecore.isVisible(Jdt2Ecore.toTypeFinder(this.project), type, method));
			}

		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class FormalParameter extends AbstractSarlUiTest {	

		@Test
		public void getFormalParameterProvider_noVarargs() throws JavaModelException {
			IType declaringType = createITypeMock("io.sarl.eclipse.tests.p1.Type1", null);
			IMethod method = createIMethodMock(
					declaringType, "myFct", null,
					new String[] { "param1", "param2", "param3" },
					new String[] { "Ljava.lang.String;", "I", "Z" },
					0);
			FormalParameterProvider provider = Jdt2Ecore.getFormalParameterProvider(method);
			assertNotNull(provider);
			assertEquals(3, provider.getFormalParameterCount());
			assertEquals("param1", provider.getFormalParameterName(0));
			assertEquals("java.lang.String", provider.getFormalParameterType(0, false));
			assertEquals("param2", provider.getFormalParameterName(1));
			assertEquals("int", provider.getFormalParameterType(1, false));
			assertEquals("param3", provider.getFormalParameterName(2));
			assertEquals("boolean", provider.getFormalParameterType(2, false));
		}

		@Test
		public void getFormalParameterProvider_noVarargs_lastIsArray() throws JavaModelException {
			IType declaringType = createITypeMock("io.sarl.eclipse.tests.p1.Type1", null);
			IMethod method = createIMethodMock(
					declaringType, "myFct", null,
					new String[] { "param1", "param2", "param3" },
					new String[] { "Ljava.lang.String;", "I", "[Z" },
					0);
			FormalParameterProvider provider = Jdt2Ecore.getFormalParameterProvider(method);
			assertNotNull(provider);
			assertEquals(3, provider.getFormalParameterCount());
			assertEquals("param1", provider.getFormalParameterName(0));
			assertEquals("java.lang.String", provider.getFormalParameterType(0, false));
			assertEquals("param2", provider.getFormalParameterName(1));
			assertEquals("int", provider.getFormalParameterType(1, false));
			assertEquals("param3", provider.getFormalParameterName(2));
			assertEquals("boolean[]", provider.getFormalParameterType(2, false));
		}

		@Test
		public void getFormalParameterProvider_varargs() throws JavaModelException {
			IType declaringType = createITypeMock("io.sarl.eclipse.tests.p1.Type1", null);
			IMethod method = createIMethodMock(
					declaringType, "myFct", null,
					new String[] { "param1", "param2", "param3" },
					new String[] { "Ljava.lang.String;", "I", "[Z" },
					0);
			FormalParameterProvider provider = Jdt2Ecore.getFormalParameterProvider(method);
			assertNotNull(provider);
			assertEquals(3, provider.getFormalParameterCount());
			assertEquals("param1", provider.getFormalParameterName(0));
			assertEquals("java.lang.String", provider.getFormalParameterType(0, false));
			assertEquals("param2", provider.getFormalParameterName(1));
			assertEquals("int", provider.getFormalParameterType(1, false));
			assertEquals("param3", provider.getFormalParameterName(2));
			assertEquals("boolean[]", provider.getFormalParameterType(2, true));
		}

		@Test
		public void createFormalParameters_noDefault_noVarargs() throws JavaModelException, IllegalArgumentException {
			ResourceSet resourceSet = mock(ResourceSet.class);
			SARLCodeGenerator generator = mock(SARLCodeGenerator.class);
			GeneratedCode code = mock(GeneratedCode.class);
			when(code.getCodeGenerator()).thenReturn(generator);
			when(code.getResourceSet()).thenReturn(resourceSet);
			IType declaringType = createITypeMock("io.sarl.eclipse.tests.p1.Type1", null);
			IMethod method = createIMethodMock(
					declaringType, "myFct", null,
					new String[] { "param1", "param2", "param3" },
					new String[] { "Ljava.lang.String;", "I", "[Z" },
					0);
			ParameterizedFeature container = mock(ParameterizedFeature.class);
			//
			Jdt2Ecore.createFormalParameters(code, method, container);
			//
			ArgumentCaptor<GeneratedCode> arg0 = ArgumentCaptor.forClass(GeneratedCode.class);
			ArgumentCaptor<ParameterizedFeature> arg1 = ArgumentCaptor.forClass(ParameterizedFeature.class);
			ArgumentCaptor<String> arg2 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg3 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg4 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<ResourceSet> arg5 = ArgumentCaptor.forClass(ResourceSet.class);
			verify(generator, times(3)).createFormalParameter(
					arg0.capture(),
					arg1.capture(),
					arg2.capture(),
					arg3.capture(),
					arg4.capture(),
					arg5.capture());
			assertContains(arg0.getAllValues(), code, code, code);
			assertContains(arg1.getAllValues(), container, container, container);
			assertContains(arg2.getAllValues(), "param1", "param2", "param3");
			assertContains(arg3.getAllValues(), "java.lang.String", "int", "boolean[]");
			assertContains(arg4.getAllValues(), null, null, null);
			assertContains(arg5.getAllValues(), resourceSet, resourceSet, resourceSet);
		}

		@Test
		public void createFormalParameters_noDefault_varargs() throws JavaModelException, IllegalArgumentException {
			ResourceSet resourceSet = mock(ResourceSet.class);
			SARLCodeGenerator generator = mock(SARLCodeGenerator.class);
			GeneratedCode code = mock(GeneratedCode.class);
			when(code.getCodeGenerator()).thenReturn(generator);
			when(code.getResourceSet()).thenReturn(resourceSet);
			IType declaringType = createITypeMock("io.sarl.eclipse.tests.p1.Type1", null);
			IMethod method = createIMethodMock(
					declaringType, "myFct", null,
					new String[] { "param1", "param2", "param3" },
					new String[] { "Ljava.lang.String;", "I", "[Z" },
					Flags.AccVarargs);
			ParameterizedFeature container = mock(ParameterizedFeature.class);
			//
			Jdt2Ecore.createFormalParameters(code, method, container);
			//
			ArgumentCaptor<GeneratedCode> arg0 = ArgumentCaptor.forClass(GeneratedCode.class);
			ArgumentCaptor<ParameterizedFeature> arg1 = ArgumentCaptor.forClass(ParameterizedFeature.class);
			ArgumentCaptor<String> arg2 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg3 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg4 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<ResourceSet> arg5 = ArgumentCaptor.forClass(ResourceSet.class);
			verify(generator, times(3)).createFormalParameter(
					arg0.capture(),
					arg1.capture(),
					arg2.capture(),
					arg3.capture(),
					arg4.capture(),
					arg5.capture());
			assertContains(arg0.getAllValues(), code, code, code);
			assertContains(arg1.getAllValues(), container, container, container);
			assertContains(arg2.getAllValues(), "param1", "param2", "param3");
			assertContains(arg3.getAllValues(), "java.lang.String", "int", "boolean");
			assertContains(arg4.getAllValues(), null, null, null);
			assertContains(arg5.getAllValues(), resourceSet, resourceSet, resourceSet);
		}

		@Test
		public void createFormalParameters_default_noVarargs() throws JavaModelException, IllegalArgumentException {
			ResourceSet resourceSet = mock(ResourceSet.class);
			SARLCodeGenerator generator = mock(SARLCodeGenerator.class);
			GeneratedCode code = mock(GeneratedCode.class);
			when(code.getCodeGenerator()).thenReturn(generator);
			when(code.getResourceSet()).thenReturn(resourceSet);
			IType declaringType = createITypeMock("io.sarl.eclipse.tests.p1.Type1", null);
			when(declaringType.getField(Matchers.anyString())).thenAnswer(new Answer<IField>() {
				@Override
				public IField answer(InvocationOnMock invocation) throws Throwable {
					String fieldName = (String) invocation.getArguments()[0];
					IAnnotation annotation = mock(IAnnotation.class);
					when(annotation.getElementName()).thenReturn("io.sarl.lang.annotation.Generated");
					IMemberValuePair pair = mock(IMemberValuePair.class);
					when(pair.getValue()).thenReturn("1+2");
					when(annotation.getMemberValuePairs()).thenReturn(new IMemberValuePair[] { pair });
					IField field = mock(IField.class);
					when(field.getElementName()).thenReturn(fieldName);
					when(field.getAnnotations()).thenReturn(new IAnnotation[] { annotation });
					return field;
				}
			});
			IMemberValuePair pair = mock(IMemberValuePair.class);
			when(pair.getValue()).thenReturn("0_1");
			IAnnotation annotation1 = mock(IAnnotation.class);
			when(annotation1.getElementName()).thenReturn("io.sarl.lang.annotation.DefaultValue");
			when(annotation1.getMemberValuePairs()).thenReturn(new IMemberValuePair[] { pair });
			IAnnotation annotation2 = mock(IAnnotation.class);
			when(annotation2.getElementName()).thenReturn("");
			IMethod method = createIMethodMock(
					declaringType, "myFct", null,
					new String[] { "param1", "param2", "param3" },
					new String[] { "Ljava.lang.String;", "I", "[Z" },
					new IAnnotation[] { null, annotation1, null },
					new IAnnotation[] { annotation2 },
					0);
			ParameterizedFeature container = mock(ParameterizedFeature.class);
			//
			Jdt2Ecore.createFormalParameters(code, method, container);
			//
			ArgumentCaptor<GeneratedCode> arg0 = ArgumentCaptor.forClass(GeneratedCode.class);
			ArgumentCaptor<ParameterizedFeature> arg1 = ArgumentCaptor.forClass(ParameterizedFeature.class);
			ArgumentCaptor<String> arg2 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg3 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg4 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<ResourceSet> arg5 = ArgumentCaptor.forClass(ResourceSet.class);
			verify(generator, times(3)).createFormalParameter(
					arg0.capture(),
					arg1.capture(),
					arg2.capture(),
					arg3.capture(),
					arg4.capture(),
					arg5.capture());
			assertContains(arg0.getAllValues(), code, code, code);
			assertContains(arg1.getAllValues(), container, container, container);
			assertContains(arg2.getAllValues(), "param1", "param2", "param3");
			assertContains(arg3.getAllValues(), "java.lang.String", "int", "boolean[]");
			assertContains(arg4.getAllValues(), null, "1+2", null);
			assertContains(arg5.getAllValues(), resourceSet, resourceSet, resourceSet);
		}

		@Test
		public void createFormalParameters_default_varargs() throws JavaModelException, IllegalArgumentException {
			ResourceSet resourceSet = mock(ResourceSet.class);
			SARLCodeGenerator generator = mock(SARLCodeGenerator.class);
			GeneratedCode code = mock(GeneratedCode.class);
			when(code.getCodeGenerator()).thenReturn(generator);
			when(code.getResourceSet()).thenReturn(resourceSet);
			IType declaringType = createITypeMock("io.sarl.eclipse.tests.p1.Type1", null);
			when(declaringType.getField(Matchers.anyString())).thenAnswer(new Answer<IField>() {
				@Override
				public IField answer(InvocationOnMock invocation) throws Throwable {
					String fieldName = (String) invocation.getArguments()[0];
					IAnnotation annotation = mock(IAnnotation.class);
					when(annotation.getElementName()).thenReturn("io.sarl.lang.annotation.Generated");
					IMemberValuePair pair = mock(IMemberValuePair.class);
					when(pair.getValue()).thenReturn("1+2");
					when(annotation.getMemberValuePairs()).thenReturn(new IMemberValuePair[] { pair });
					IField field = mock(IField.class);
					when(field.getElementName()).thenReturn(fieldName);
					when(field.getAnnotations()).thenReturn(new IAnnotation[] { annotation });
					return field;
				}
			});
			IMemberValuePair pair = mock(IMemberValuePair.class);
			when(pair.getValue()).thenReturn("0_1");
			IAnnotation annotation1 = mock(IAnnotation.class);
			when(annotation1.getElementName()).thenReturn("io.sarl.lang.annotation.DefaultValue");
			when(annotation1.getMemberValuePairs()).thenReturn(new IMemberValuePair[] { pair });
			IAnnotation annotation2 = mock(IAnnotation.class);
			when(annotation2.getElementName()).thenReturn("");
			IMethod method = createIMethodMock(
					declaringType, "myFct", null,
					new String[] { "param1", "param2", "param3" },
					new String[] { "Ljava.lang.String;", "I", "[Z" },
					new IAnnotation[] { null, annotation1, null },
					new IAnnotation[] { annotation2 },
					Flags.AccVarargs);
			ParameterizedFeature container = mock(ParameterizedFeature.class);
			//
			Jdt2Ecore.createFormalParameters(code, method, container);
			//
			ArgumentCaptor<GeneratedCode> arg0 = ArgumentCaptor.forClass(GeneratedCode.class);
			ArgumentCaptor<ParameterizedFeature> arg1 = ArgumentCaptor.forClass(ParameterizedFeature.class);
			ArgumentCaptor<String> arg2 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg3 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg4 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<ResourceSet> arg5 = ArgumentCaptor.forClass(ResourceSet.class);
			verify(generator, times(3)).createFormalParameter(
					arg0.capture(),
					arg1.capture(),
					arg2.capture(),
					arg3.capture(),
					arg4.capture(),
					arg5.capture());
			assertContains(arg0.getAllValues(), code, code, code);
			assertContains(arg1.getAllValues(), container, container, container);
			assertContains(arg2.getAllValues(), "param1", "param2", "param3");
			assertContains(arg3.getAllValues(), "java.lang.String", "int", "boolean");
			assertContains(arg4.getAllValues(), null, "1+2", null);
			assertContains(arg5.getAllValues(), resourceSet, resourceSet, resourceSet);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class PopulateInheritanceContext extends AbstractSarlUiTest {	

		@Inject
		private ActionSignatureProvider sarlSignatureProvider;

		@Nullable
		private Map<ActionKey, IMethod> finalOperations;

		@Nullable
		private Map<ActionKey, IMethod> overridableOperations;

		@Nullable
		private Map<String, IField> inheritedFields;

		@Nullable
		private Map<ActionKey, IMethod> operationsToImplement;

		@Nullable
		private Map<SignatureKey, IMethod> superConstructors;

		@Nullable
		private IJavaProject project;

		@Before
		public void setUp() throws Exception {
			this.finalOperations = Maps.newHashMap();
			this.overridableOperations = Maps.newHashMap();
			this.inheritedFields = Maps.newHashMap();
			this.operationsToImplement = Maps.newHashMap();
			this.superConstructors = Maps.newHashMap();
			this.project = JavaCore.create(this.helper.getProject());
		}
		
		protected void loadSARLCode(int codeIndex) throws Exception {
			this.helper.createSARLScript(
					this.project.getProject(),
					"PopulateInherithanceContextTest" + codeIndex,
					getResourceText("unit_test_code" + codeIndex));
		}
		
		@Test
		@SARLNatureNeededForTest
		public void populateInheritanceContext_0() throws Exception {
			// Create the SARL scripts
			loadSARLCode(0);
			//
			IStatus s = Jdt2Ecore.populateInheritanceContext(
					new UnitTestTypeFinder(this.project),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					this.sarlSignatureProvider,
					"io.sarl.eclipse.tests.p0.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p0.Capacity3", "io.sarl.eclipse.tests.p0.Capacity2"));
			//
			assertTrue(this.finalOperations.isEmpty());
			assertActionKeys(this.overridableOperations.keySet(),
					"skilFct(char)", "myFct1()");
			assertContains(this.inheritedFields.keySet(), "attr1");
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()");
			assertSignatureKeys(this.superConstructors.keySet(), "int");
			//
			assertNotNull(s);
			assertTrue(s.toString(), s.isOK());
		}

		@Test
		@SARLNatureNeededForTest
		public void populateInheritanceContext_1() throws Exception {
			// Create the SARL scripts
			loadSARLCode(1);
			//
			IStatus s = Jdt2Ecore.populateInheritanceContext(
					new UnitTestTypeFinder(this.project),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					this.sarlSignatureProvider,
					"io.sarl.eclipse.tests.p1.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p1.Capacity3", "io.sarl.eclipse.tests.p1.Capacity2"));
			//
			assertActionKeys(this.finalOperations.keySet(),
					"myFct1()");
			assertActionKeys(this.overridableOperations.keySet(),
					"skilFct(char)", "myFct1(int)");
			assertContains(this.inheritedFields.keySet(), "attr1");
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()");
			assertSignatureKeys(this.superConstructors.keySet(), "int");
			//
			assertNotNull(s);
			assertTrue(s.toString(), s.isOK());
		}

		@Test
		public void populateInheritanceContext_2() throws Exception {
			// Create the SARL scripts
			loadSARLCode(2);
			//
			IStatus s = Jdt2Ecore.populateInheritanceContext(
					new UnitTestTypeFinder(this.project),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					this.sarlSignatureProvider,
					"io.sarl.eclipse.tests.p2.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p2.Capacity3", "io.sarl.eclipse.tests.p2.Capacity2"));
			//
			assertTrue(this.finalOperations.isEmpty());
			assertActionKeys(this.overridableOperations.keySet(),
					"skilFct(char)", "myFct1(int*)");
			assertContains(this.inheritedFields.keySet(), "attr1");
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()");
			assertSignatureKeys(this.superConstructors.keySet(), "int");
			//
			assertNotNull(s);
			assertTrue(s.toString(), s.isOK());
		}

		@Test
		@SARLNatureNeededForTest
		public void populateInheritanceContext_3() throws Exception {
			// Create the SARL scripts
			loadSARLCode(3);
			//
			IStatus s = Jdt2Ecore.populateInheritanceContext(
					new UnitTestTypeFinder(this.project),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					this.sarlSignatureProvider,
					"io.sarl.eclipse.tests.p3.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p3.Capacity3", "io.sarl.eclipse.tests.p3.Capacity2"));
			//
			assertTrue(this.finalOperations.isEmpty());
			assertTrue(this.overridableOperations.isEmpty());
			assertTrue(this.inheritedFields.isEmpty());
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()", "myFct1(int*)");
			assertTrue(this.superConstructors.isEmpty());
			//
			assertNotNull(s);
			assertFalse(s.toString(), s.isOK());
			assertFalse(s.toString(), s.isMultiStatus());
			assertEquals("io.sarl.eclipse.tests.p3.Skill1", s.getMessage());
		}
		
		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private static class UnitTestTypeFinder implements TypeFinder {

			private final IJavaProject project;
			
			public UnitTestTypeFinder(IJavaProject project) {
				this.project = project;
			}
			
			private IType createType(String fullyQualifiedName, String simpleName) throws JavaModelException {
				IType type = mock(IType.class);
				when(type.getFullyQualifiedName()).thenReturn(fullyQualifiedName);
				when(type.getJavaProject()).thenReturn(this.project);
				when(type.getElementName()).thenReturn(simpleName);
				when(type.getSuperclassName()).thenReturn("java.lang.Object");
				when(type.getSuperclassTypeSignature()).thenReturn("Ljava.lang.Object;");
				doReturn(new IField[0]).when(type).getFields();
				doReturn(new IMethod[0]).when(type).getMethods();
				doReturn(new String[0]).when(type).getSuperInterfaceNames();
				doReturn(new String[0]).when(type).getSuperInterfaceTypeSignatures();
				return type;
			}

			@Override
			public IType findType(String typeName) throws JavaModelException {
				if ("io.sarl.lang.core.Capacity".equals(typeName)) {
					return createType(typeName, "Capacity");
				} else if ("io.sarl.lang.core.Skill".equals(typeName)) {
					return createType(typeName, "Skill");
				}
				return this.project.findType(typeName);
			}
			
		}
		
	}

}