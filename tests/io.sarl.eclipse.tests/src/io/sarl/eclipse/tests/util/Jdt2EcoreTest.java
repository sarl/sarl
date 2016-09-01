/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Set;

import javax.annotation.Generated;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.google.inject.Inject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.Jdt2Ecore;
import io.sarl.eclipse.util.Jdt2Ecore.TypeFinder;
import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.actionprototype.FormalParameterProvider;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlUiTest;

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
public class Jdt2EcoreTest extends AbstractSarlTest {

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
		when(project.findType(anyString())).thenAnswer((invocation) -> {
				String fqn = (String) invocation.getArguments()[0];
				IType type = mock(IType.class);
				when(type.getJavaProject()).thenReturn(project);
				when(type.getFullyQualifiedName()).thenReturn(fqn);
				when(type.getElementName()).thenReturn(fqn);
				return type;
			});
		return project;
	}

	/** Assert the action keys. The order of the key is not important.
	 *
	 * @param actualKeys
	 * @param expectedKeys
	 */
	protected static void assertActionKeys(Set<? extends ActionPrototype> actualKeys, String... expectedKeys) {
		Set<String> expected = Sets.newHashSet(expectedKeys);
		for (ActionPrototype actualKey : actualKeys) {
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
	protected static void assertSignatureKeys(Set<? extends ActionParameterTypes> actualKeys, String... expectedKeys) {
		Set<String> expected = Sets.newHashSet(expectedKeys);
		for (ActionParameterTypes actualKey : actualKeys) {
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

		@Inject
		private Jdt2Ecore jdt2ecore;
		
		@Test
		public void isGeneratedOperation_noAnnotation() throws JavaModelException {
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
			});
			assertFalse(this.jdt2ecore.isGeneratedOperation(m));
		}

		@Test
		public void isGeneratedOperation_generatedAnnotation() throws JavaModelException {
			IAnnotation annot = mock(IAnnotation.class);
			when(annot.getElementName()).thenReturn("javax.annotation.Generated");
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
					annot
			});
			assertTrue(this.jdt2ecore.isGeneratedOperation(m));
		}

		@Test
		public void isGeneratedOperation_otherAnnotation() throws JavaModelException {
			IAnnotation annot = mock(IAnnotation.class);
			when(annot.getElementName()).thenReturn("io.sarl.lang.annotation.Generated");
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
					annot
			});
			assertFalse(this.jdt2ecore.isGeneratedOperation(m));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class GetAnnotation extends AbstractSarlUiTest {

		@Inject
		private Jdt2Ecore jdt2ecore;
		
		@Test
		public void getAnnotation_noAnnotation() throws JavaModelException {
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
			});
			assertNull(this.jdt2ecore.getAnnotation(m, Generated.class.getName()));
		}

		@Test
		public void getAnnotation_generatedAnnotation() throws JavaModelException {
			IAnnotation annot = mock(IAnnotation.class);
			when(annot.getElementName()).thenReturn(Generated.class.getName());
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
					annot
			});
			IAnnotation a = this.jdt2ecore.getAnnotation(m, Generated.class.getName());
			assertNotNull(a);
			assertEquals(Generated.class.getName(), a.getElementName());
		}

		@Test
		public void getAnnotation_otherAnnotation() throws JavaModelException {
			IAnnotation annot = mock(IAnnotation.class);
			when(annot.getElementName()).thenReturn("io.sarl.lang.annotation.Generated");
			IMethod m = mock(IMethod.class);
			when(m.getAnnotations()).thenReturn(new IAnnotation[] {
					annot
			});
			assertNull(this.jdt2ecore.getAnnotation(m, Generated.class.getName()));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class FindType extends AbstractSarlUiTest {

		@NonNullByDefault
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

			@NonNullByDefault
			private IJavaProject project;

			@Inject
			private Jdt2Ecore jdt2ecore;

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

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class DefaultpackageDefaultpackageSubtype extends AbstractSarlUiTest {

			@NonNullByDefault
			private IJavaProject project;

			@Inject
			private Jdt2Ecore jdt2ecore;

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

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("Type2", "Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("Type2", "Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("Type2", "Type1");

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class DefaultpackagePackage1Nosubtype extends AbstractSarlUiTest {

			@NonNullByDefault
			private IJavaProject project;

			@Inject
			private Jdt2Ecore jdt2ecore;

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

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class DefaultpackagePackage1Subtype extends AbstractSarlUiTest {

			@NonNullByDefault
			private IJavaProject project;

			@Inject
			private Jdt2Ecore jdt2ecore;

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

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "Type1");

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "Type1");

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class Package1Package1Nosubtype extends AbstractSarlUiTest {

			@NonNullByDefault
			private IJavaProject project;

			@Inject
			private Jdt2Ecore jdt2ecore;

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

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class Package1Package1Subtype extends AbstractSarlUiTest {

			@NonNullByDefault
			private IJavaProject project;

			@Inject
			private Jdt2Ecore jdt2ecore;

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

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class Package1Package2Nosubtype extends AbstractSarlUiTest {

			@NonNullByDefault
			private IJavaProject project;

			@Inject
			private Jdt2Ecore jdt2ecore;

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

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class Package1Package2Subtype extends AbstractSarlUiTest {

			@NonNullByDefault
			private IJavaProject project;

			@Inject
			private Jdt2Ecore jdt2ecore;

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

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock("io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock("io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
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

		@Inject
		private Jdt2Ecore jdt2ecore;

		@Test
		public void getFormalParameterProvider_noVarargs() throws JavaModelException {
			IType declaringType = createITypeMock("io.sarl.eclipse.tests.p1.Type1", null);
			IMethod method = createIMethodMock(
					declaringType, "myFct", null,
					new String[] { "param1", "param2", "param3" },
					new String[] { "Ljava.lang.String;", "I", "Z" },
					0);
			FormalParameterProvider provider = this.jdt2ecore.getFormalParameterProvider(method);
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
			FormalParameterProvider provider = this.jdt2ecore.getFormalParameterProvider(method);
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
			FormalParameterProvider provider = this.jdt2ecore.getFormalParameterProvider(method);
			assertNotNull(provider);
			assertEquals(3, provider.getFormalParameterCount());
			assertEquals("param1", provider.getFormalParameterName(0));
			assertEquals("java.lang.String", provider.getFormalParameterType(0, false));
			assertEquals("param2", provider.getFormalParameterName(1));
			assertEquals("int", provider.getFormalParameterType(1, false));
			assertEquals("param3", provider.getFormalParameterName(2));
			assertEquals("boolean[]", provider.getFormalParameterType(2, true));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class PopulateInheritanceContext extends AbstractSarlUiTest {

		@NonNullByDefault
		private SARLEclipsePlugin plugin;
		
		@Inject
		private Jdt2Ecore jdt2ecore;

		@NonNullByDefault
		private Map<ActionPrototype, IMethod> finalOperations;

		@NonNullByDefault
		private Map<ActionPrototype, IMethod> overridableOperations;

		@NonNullByDefault
		private Map<String, IField> inheritedFields;

		@NonNullByDefault
		private Map<ActionPrototype, IMethod> operationsToImplement;

		@NonNullByDefault
		private Map<ActionParameterTypes, IMethod> superConstructors;

		private void ensureEclipsePlugin() {
			this.plugin = SARLEclipsePlugin.getDefault();
			if (this.plugin == null) {
				this.plugin = spy(new SARLEclipsePlugin());
				SARLEclipsePlugin.setDefault(this.plugin);
			}
		}
		
		@Before
		public void setUp() throws Exception {
			this.finalOperations = Maps.newHashMap();
			this.overridableOperations = Maps.newHashMap();
			this.inheritedFields = Maps.newHashMap();
			this.operationsToImplement = Maps.newHashMap();
			this.superConstructors = Maps.newHashMap();
			ensureEclipsePlugin();
		}

		@Test
		public void populateInheritanceContext_0() throws Exception {
			// Create the SARL scripts
			helper().sarlScript("populateInheritanceContext0.sarl", multilineString(
						"package io.sarl.eclipse.tests.p0",
						"capacity Capacity1 {",
						"    def myFct1 : boolean",
						"}",
						"capacity Capacity2 extends Capacity1 {",
						"    def myFct2 : boolean",
						"}",
						"capacity Capacity3 {",
						"    def myFct3",
						"}",
						"skill Skill1 implements Capacity1 {",
						"	protected var attr1 : int",
						"	new(attr : int) {",
						"		this.attr1 = attr",
						"	}",
						"	def myFct1 : boolean {",
						"		return true",
						"	}",
						"	def skilFct(a : char) {",
						"		System.out.println(a)",
						"	}",
						"}"));
			helper().fullBuild();
			helper().awaitAutoBuild();
			//
			/*
			TypeFinder typeFinder,
			Map<ActionPrototype, IMethod> finalOperations,
			Map<ActionPrototype, IMethod> overridableOperations,
			Map<String, IField> inheritedFields,
			Map<ActionPrototype, IMethod> operationsToImplement,
			Map<ActionParameterTypes, IMethod> superConstructors,
			String superClass,
			List<String> superInterfaces
			 */
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					new UnitTestTypeFinder(helper().getJavaProject()),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					"io.sarl.eclipse.tests.p0.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p0.Capacity3", "io.sarl.eclipse.tests.p0.Capacity2"));
			//
			assertNotNull(s);
			assertTrue(s.toString(), s.isOK());
			//
			assertTrue(this.finalOperations.isEmpty());
			assertActionKeys(this.overridableOperations.keySet(),
					"skilFct(char)", "myFct1()");
			assertContains(this.inheritedFields.keySet(), "attr1");
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()");
			assertSignatureKeys(this.superConstructors.keySet(), "int");
		}

		@Test
		public void populateInheritanceContext_1() throws Exception {
			// Create the SARL scripts
			helper().sarlScript("populateInheritanceContext0.sarl", multilineString(
						"package io.sarl.eclipse.tests.p0",
						"capacity Capacity1 {",
						"    def myFct1 : boolean",
						"}",
						"capacity Capacity2 extends Capacity1 {",
						"    def myFct2 : boolean",
						"}",
						"capacity Capacity3 {",
						"    def myFct3",
						"}",
						"skill Skill1 implements Capacity1 {",
						"	var attr1 : int",
						"	new(attr : int) {",
						"		this.attr1 = attr",
						"	}",
						"	def myFct1 : boolean {",
						"		return true",
						"	}",
						"	def skilFct(a : char) {",
						"		System.out.println(a)",
						"	}",
						"}"));
			helper().fullBuild();
			helper().awaitAutoBuild();
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					new UnitTestTypeFinder(helper().getJavaProject()),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					"io.sarl.eclipse.tests.p0.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p0.Capacity3", "io.sarl.eclipse.tests.p0.Capacity2"));
			//
			assertNotNull(s);
			assertTrue(s.toString(), s.isOK());
			//
			assertTrue(this.finalOperations.isEmpty());
			assertActionKeys(this.overridableOperations.keySet(),
					"skilFct(char)", "myFct1()");
			assertContains(this.inheritedFields.keySet(), "attr1");
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()");
			assertSignatureKeys(this.superConstructors.keySet(), "int");
		}

		@Test
		public void populateInheritanceContext_2() throws Exception {
			// Create the SARL scripts
			helper().sarlScript("populateInheritanceContext1.sarl", multilineString(
				"package io.sarl.eclipse.tests.p1",
				"capacity Capacity1 {",
				"    def myFct1(a : int = 4) : boolean",
				"}",
				"capacity Capacity2 extends Capacity1 {",
				"    def myFct2 : boolean",
				"}",
				"capacity Capacity3 {",
				"    def myFct3",
				"}",
				"skill Skill1 implements Capacity1 {",
				"	protected var attr1 : int",
				"	new(attr : int) {",
				"		this.attr1 = attr",
				"	}",
				"	def myFct1(a : int = 4) : boolean {",
				"		return true",
				"	}",
				"	def skilFct(a : char) {",
				"		System.out.println(a)",
				"	}",
				"}"));
			helper().fullBuild();
			helper().awaitAutoBuild();
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					new UnitTestTypeFinder(helper().getJavaProject()),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					"io.sarl.eclipse.tests.p1.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p1.Capacity3", "io.sarl.eclipse.tests.p1.Capacity2"));
			//
			assertNotNull(s);
			assertTrue(s.toString(), s.isOK());
			//
			assertActionKeys(this.finalOperations.keySet(),
					"myFct1()");
			assertActionKeys(this.overridableOperations.keySet(),
					"skilFct(char)", "myFct1(int)");
			assertContains(this.inheritedFields.keySet(), "attr1");
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()");
			assertSignatureKeys(this.superConstructors.keySet(), "int");
		}

		@Test
		public void populateInheritanceContext_3() throws Exception {
			// Create the SARL scripts
			helper().sarlScript("populateInheritanceContext1.sarl", multilineString(
				"package io.sarl.eclipse.tests.p1",
				"capacity Capacity1 {",
				"    def myFct1(a : int = 4) : boolean",
				"}",
				"capacity Capacity2 extends Capacity1 {",
				"    def myFct2 : boolean",
				"}",
				"capacity Capacity3 {",
				"    def myFct3",
				"}",
				"skill Skill1 implements Capacity1 {",
				"	var attr1 : int",
				"	new(attr : int) {",
				"		this.attr1 = attr",
				"	}",
				"	def myFct1(a : int = 4) : boolean {",
				"		return true",
				"	}",
				"	def skilFct(a : char) {",
				"		System.out.println(a)",
				"	}",
				"}"));
			helper().fullBuild();
			helper().awaitAutoBuild();
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					new UnitTestTypeFinder(helper().getJavaProject()),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					"io.sarl.eclipse.tests.p1.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p1.Capacity3", "io.sarl.eclipse.tests.p1.Capacity2"));
			//
			assertNotNull(s);
			assertTrue(s.toString(), s.isOK());
			//
			assertActionKeys(this.finalOperations.keySet(),
					"myFct1()");
			assertActionKeys(this.overridableOperations.keySet(),
					"skilFct(char)", "myFct1(int)");
			assertContains(this.inheritedFields.keySet(), "attr1");
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()");
			assertSignatureKeys(this.superConstructors.keySet(), "int");
		}

		@Test
		public void populateInheritanceContext_4() throws Exception {
			// Create the SARL scripts
			helper().sarlScript("populateInheritanceContext1.sarl", multilineString(
					"package io.sarl.eclipse.tests.p2",
					"capacity Capacity1 {",
					"    def myFct1(a : int*) : boolean",
					"}",
					"capacity Capacity2 extends Capacity1 {",
					"    def myFct2 : boolean",
					"}",
					"capacity Capacity3 {",
					"    def myFct3",
					"}",
					"skill Skill1 implements Capacity1 {",
					"	protected var attr1 : int",
					"	new(attr : int) {",
					"		this.attr1 = attr",
					"	}",
					"	def myFct1(a : int*) : boolean {",
					"		return true",
					"	}",
					"	def skilFct(a : char) {",
					"		System.out.println(a)",
					"	}",
					"}"));
			helper().fullBuild();
			helper().awaitAutoBuild();
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					new UnitTestTypeFinder(helper().getJavaProject()),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					"io.sarl.eclipse.tests.p2.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p2.Capacity3", "io.sarl.eclipse.tests.p2.Capacity2"));
			//
			assertNotNull(s);
			assertTrue(s.toString(), s.isOK());
			//
			assertTrue(this.finalOperations.isEmpty());
			assertActionKeys(this.overridableOperations.keySet(),
					"skilFct(char)", "myFct1(int*)");
			assertContains(this.inheritedFields.keySet(), "attr1");
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()");
			assertSignatureKeys(this.superConstructors.keySet(), "int");
		}

		@Test
		public void populateInheritanceContext_5() throws Exception {
			// Create the SARL scripts
			helper().sarlScript("populateInheritanceContext1.sarl", multilineString(
					"package io.sarl.eclipse.tests.p2",
					"capacity Capacity1 {",
					"    def myFct1(a : int*) : boolean",
					"}",
					"capacity Capacity2 extends Capacity1 {",
					"    def myFct2 : boolean",
					"}",
					"capacity Capacity3 {",
					"    def myFct3",
					"}",
					"skill Skill1 implements Capacity1 {",
					"	var attr1 : int",
					"	new(attr : int) {",
					"		this.attr1 = attr",
					"	}",
					"	def myFct1(a : int*) : boolean {",
					"		return true",
					"	}",
					"	def skilFct(a : char) {",
					"		System.out.println(a)",
					"	}",
					"}"));
			helper().fullBuild();
			helper().awaitAutoBuild();
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					new UnitTestTypeFinder(helper().getJavaProject()),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					"io.sarl.eclipse.tests.p2.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p2.Capacity3", "io.sarl.eclipse.tests.p2.Capacity2"));
			//
			assertNotNull(s);
			assertTrue(s.toString(), s.isOK());
			//
			assertTrue(this.finalOperations.isEmpty());
			assertActionKeys(this.overridableOperations.keySet(),
					"skilFct(char)", "myFct1(int*)");
			assertContains(this.inheritedFields.keySet(), "attr1");
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()");
			assertSignatureKeys(this.superConstructors.keySet(), "int");
		}

		@Test
		public void populateInheritanceContext_6() throws Exception {
			// Create the SARL scripts
			helper().sarlScript("populateInheritanceContext1.sarl", multilineString(
					"package io.sarl.eclipse.tests.p3",
					"capacity Capacity1 {",
					"    def myFct1(a : int*) : boolean",
					"}",
					"capacity Capacity2 extends Capacity1 {",
					"    def myFct2 : boolean",
					"}",
					"capacity Capacity3 {",
					"    def myFct3",
					"}"));
			helper().fullBuild();
			helper().awaitAutoBuild();
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					new UnitTestTypeFinder(helper().getJavaProject()),
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					"io.sarl.eclipse.tests.p3.Skill1",
					Arrays.asList("io.sarl.eclipse.tests.p3.Capacity3", "io.sarl.eclipse.tests.p3.Capacity2"));
			//
			assertNotNull(s);
			assertFalse(s.toString(), s.isOK());
			assertFalse(s.toString(), s.isMultiStatus());
			assertEquals("io.sarl.eclipse.tests.p3.Skill1", s.getMessage());
			//
			assertTrue(this.finalOperations.isEmpty());
			assertTrue(this.overridableOperations.isEmpty());
			assertTrue(this.inheritedFields.isEmpty());
			assertActionKeys(this.operationsToImplement.keySet(), "myFct2()", "myFct3()", "myFct1(int*)");
			assertTrue(this.superConstructors.isEmpty());
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
