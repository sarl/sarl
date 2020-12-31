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
package io.sarl.eclipse.tests.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import javax.annotation.Generated;
import javax.naming.InitialContext;

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
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;
import org.junit.Before;
import org.junit.ComparisonFailure;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentMatcher;
import org.mockito.ArgumentMatchers;

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
	 * <table>
	 * <tr><td>Z</td><td>boolean</td></tr>
	 * <tr><td>B</td><td>byte</td></tr>
	 * <tr><td>C</td><td>char</td></tr>
	 * <tr><td>S</td><td>short</td></tr>
	 * <tr><td>I</td><td>int</td></tr>
	 * <tr><td>J</td><td>long</td></tr>
	 * <tr><td>F</td><td>float</td></tr>
	 * <tr><td>D</td><td>double</td></tr>
	 * <tr><td>V</td><td>void</td></tr>
	 * <tr><td>L fully-qualified-class ;</td><td>fully-qualified-class</td></tr>
	 * <tr><td>[ type</td><td>type[]</td></tr>
	 * </table>
	 */
	protected static IType createITypeMock(boolean isInterface, String fullyQualifiedName, String superClass) {
		return createITypeMock(isInterface, fullyQualifiedName, superClass, null, null, null, null);
	}

	/** Replies a mock of a IType.
	 *
	 * <table>
	 * <tr><td>Z</td><td>boolean</td></tr>
	 * <tr><td>B</td><td>byte</td></tr>
	 * <tr><td>C</td><td>char</td></tr>
	 * <tr><td>S</td><td>short</td></tr>
	 * <tr><td>I</td><td>int</td></tr>
	 * <tr><td>J</td><td>long</td></tr>
	 * <tr><td>F</td><td>float</td></tr>
	 * <tr><td>D</td><td>double</td></tr>
	 * <tr><td>V</td><td>void</td></tr>
	 * <tr><td>L fully-qualified-class ;</td><td>fully-qualified-class</td></tr>
	 * <tr><td>[ type</td><td>type[]</td></tr>
	 * </table>
	 */
	protected static IType createITypeMock(
			boolean isInterface,
			String fullyQualifiedName,
			String superType,
			String[] superInterfaces,
			Procedure2<IType, List<IField>> fieldInitializer,
			Procedure2<IType, List<IMethod>> methodInitializer,
			TypeFinder typeFinder) {
		try {
			IType type = AbstractSarlTest.mock(IType.class);
			when(type.getFullyQualifiedName()).thenReturn(fullyQualifiedName);
			IPackageFragment packageFragment = AbstractSarlTest.mock(IPackageFragment.class);
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
			if (isInterface) {
				final List<String> superNames = new ArrayList<>();
				final List<String> superSignatures = new ArrayList<>();
				if (!Strings.isNullOrEmpty(superType)) {
					superNames.add(superType);
					superSignatures.add("L" + superType + ";");
				}
				if (superInterfaces != null) {
					for (String superInterface : superInterfaces) {
						superNames.add(superInterface);
						superSignatures.add("L" + superInterface + ";");
					}
				}
				String[] array = new String[superNames.size()];
				superNames.toArray(array);
				doReturn(array).when(type).getSuperInterfaceNames();
				array = new String[superSignatures.size()];
				superSignatures.toArray(array);
				doReturn(array).when(type).getSuperInterfaceTypeSignatures();
			} else {
				if (Strings.isNullOrEmpty(superType)) {
					when(type.getSuperclassName()).thenReturn("java.lang.Object");
					when(type.getSuperclassTypeSignature()).thenReturn("Ljava.lang.Object;");
				} else {
					when(type.getSuperclassName()).thenReturn(superType);
					when(type.getSuperclassTypeSignature()).thenReturn("L" + superType + ";");
				}
			}
			List<IField> fields = new ArrayList<>();
			if (fieldInitializer != null) {
				fieldInitializer.apply(type, fields);
			}
			IField[] fieldArray = new IField[fields.size()];
			fields.toArray(fieldArray);
			doReturn(fieldArray).when(type).getFields();
			List<IMethod> methods = new ArrayList<>();
			if (methodInitializer != null) {
				methodInitializer.apply(type, methods);
			}
			IMethod[] methodArray = new IMethod[methods.size()];
			methods.toArray(methodArray);
			doReturn(methodArray).when(type).getMethods();
			if (!isInterface) {
				if (superInterfaces != null && superInterfaces.length > 0) {
					String[] interSigs = new String[superInterfaces.length];
					for (int i = 0; i < superInterfaces.length; ++i) {
						interSigs[i] = "L" + superInterfaces[i] + ";";
					}
					doReturn(superInterfaces).when(type).getSuperInterfaceNames();
					doReturn(interSigs).when(type).getSuperInterfaceTypeSignatures();
				} else {
					doReturn(new String[0]).when(type).getSuperInterfaceNames();
					doReturn(new String[0]).when(type).getSuperInterfaceTypeSignatures();
				}
			}
			if (typeFinder != null) {
				when(type.resolveType(ArgumentMatchers.anyString())).thenAnswer((it) -> {
					final IType itype = typeFinder.findType(it.getArgument(0));
					if (itype != null) {
						return new String[][] {
							new String[] {
								itype.getPackageFragment().getElementName(),
								itype.getElementName()
							},
						};
					}
					return null;
				});
			}
			return type;
		} catch (JavaModelException exception) {
			throw new RuntimeException(exception);
		}
	}

	/** Replies a mock of a IMethod.
	 *
	 * <table>
	 * <tr><td>Z</td><td>boolean</td></tr>
	 * <tr><td>B</td><td>byte</td></tr>
	 * <tr><td>C</td><td>char</td></tr>
	 * <tr><td>S</td><td>short</td></tr>
	 * <tr><td>I</td><td>int</td></tr>
	 * <tr><td>J</td><td>long</td></tr>
	 * <tr><td>F</td><td>float</td></tr>
	 * <tr><td>D</td><td>double</td></tr>
	 * <tr><td>V</td><td>void</td></tr>
	 * <tr><td>L fully-qualified-class ;</td><td>fully-qualified-class</td></tr>
	 * <tr><td>[ type</td><td>type[]</td></tr>
	 * </table>
	 *
	 * @param type the enclosing type of the method.
	 * @param methodName the name of the method.
	 * @param returnType the type of the returned value.
	 * @throws JavaModelException
	 */
	protected static IMethod createIMethodMock(IType type, String methodName, String returnType,
			String[] parameterNames, String[] parameterTypes, int flags) {
		return createIMethodMock(type, methodName, returnType, parameterNames, parameterTypes,
				new IAnnotation[parameterNames.length], new IAnnotation[0], flags);
	}

	/** Replies a mock of a IMethod.
	 *
	 * <table>
	 * <tr><td>Z</td><td>boolean</td></tr>
	 * <tr><td>B</td><td>byte</td></tr>
	 * <tr><td>C</td><td>char</td></tr>
	 * <tr><td>S</td><td>short</td></tr>
	 * <tr><td>I</td><td>int</td></tr>
	 * <tr><td>J</td><td>long</td></tr>
	 * <tr><td>F</td><td>float</td></tr>
	 * <tr><td>D</td><td>double</td></tr>
	 * <tr><td>V</td><td>void</td></tr>
	 * <tr><td>L fully-qualified-class ;</td><td>fully-qualified-class</td></tr>
	 * <tr><td>[ type</td><td>type[]</td></tr>
	 * </table>
	 *
	 * @param type the enclosing type of the method.
	 * @param methodName the name of the method.
	 * @param returnType the type of the returned value.
	 * @throws JavaModelException
	 */
	protected static IMethod createIMethodMock(IType type, String methodName, String returnType,
			int flags) {
		return createIMethodMock(type, methodName, returnType,
				new String[0], new String[0], flags);
	}

	/** Replies a mock of a IMethod method.
	 *
	 * <table>
	 * <tr><td>Z</td><td>boolean</td></tr>
	 * <tr><td>B</td><td>byte</td></tr>
	 * <tr><td>C</td><td>char</td></tr>
	 * <tr><td>S</td><td>short</td></tr>
	 * <tr><td>I</td><td>int</td></tr>
	 * <tr><td>J</td><td>long</td></tr>
	 * <tr><td>F</td><td>float</td></tr>
	 * <tr><td>D</td><td>double</td></tr>
	 * <tr><td>V</td><td>void</td></tr>
	 * <tr><td>L fully-qualified-class ;</td><td>fully-qualified-class</td></tr>
	 * <tr><td>[ type</td><td>type[]</td></tr>
	 * </table>
	 */
	protected static IMethod createIMethodMock(IType type, String methodName, String returnType,
			String[] parameterNames, String[] parameterTypes,
			IAnnotation[] parameterAnnotations, IAnnotation[] methodAnnotations,
			int flags) {
		return createIMethodMock(false, type, methodName, returnType, parameterNames,
				parameterTypes, parameterAnnotations, methodAnnotations, flags);
	}

	/** Replies a mock of a IMethod constructor.
	 *
	 * <table>
	 * <tr><td>Z</td><td>boolean</td></tr>
	 * <tr><td>B</td><td>byte</td></tr>
	 * <tr><td>C</td><td>char</td></tr>
	 * <tr><td>S</td><td>short</td></tr>
	 * <tr><td>I</td><td>int</td></tr>
	 * <tr><td>J</td><td>long</td></tr>
	 * <tr><td>F</td><td>float</td></tr>
	 * <tr><td>D</td><td>double</td></tr>
	 * <tr><td>V</td><td>void</td></tr>
	 * <tr><td>L fully-qualified-class ;</td><td>fully-qualified-class</td></tr>
	 * <tr><td>[ type</td><td>type[]</td></tr>
	 * </table>
	 */
	protected static IMethod createIConstructorMock(IType type, String[] parameterNames, String[] parameterTypes,
			int flags) {
		return createIMethodMock(true, type,
				type.getElementName(), "L" + type.getFullyQualifiedName() + ";", parameterNames,
				parameterTypes, new IAnnotation[parameterNames.length], new IAnnotation[0], flags);
	}

	/** Replies a mock of a IMethod (method or constructor).
	 *
	 * <table>
	 * <tr><td>Z</td><td>boolean</td></tr>
	 * <tr><td>B</td><td>byte</td></tr>
	 * <tr><td>C</td><td>char</td></tr>
	 * <tr><td>S</td><td>short</td></tr>
	 * <tr><td>I</td><td>int</td></tr>
	 * <tr><td>J</td><td>long</td></tr>
	 * <tr><td>F</td><td>float</td></tr>
	 * <tr><td>D</td><td>double</td></tr>
	 * <tr><td>V</td><td>void</td></tr>
	 * <tr><td>L fully-qualified-class ;</td><td>fully-qualified-class</td></tr>
	 * <tr><td>[ type</td><td>type[]</td></tr>
	 * </table>
	 */
	private static IMethod createIMethodMock(boolean isConstructor, IType type, String methodName, String returnType,
			String[] parameterNames, String[] parameterTypes,
			IAnnotation[] parameterAnnotations, IAnnotation[] methodAnnotations,
			int flags) {
		try {
			IMethod method = AbstractSarlTest.mock(IMethod.class);
			when(method.getDeclaringType()).thenReturn(type);
			when(method.getElementName()).thenReturn(methodName);
			when(method.getElementType()).thenReturn(IJavaElement.METHOD);
			when(method.isConstructor()).thenReturn(isConstructor);
			IAnnotation[] ma = methodAnnotations;
			if (ma == null) {
				ma = new IAnnotation[0];
			}
			when(method.getAnnotations()).thenReturn(ma);
			if (Strings.isNullOrEmpty(returnType)) {
				when(method.getReturnType()).thenReturn("V");
			} else {
				when(method.getReturnType()).thenReturn(returnType);
			}
			when(method.getNumberOfParameters()).thenReturn(parameterNames.length);
			when(method.getParameterNames()).thenReturn(parameterNames);
			when(method.getParameterTypes()).thenReturn(parameterTypes);
			ILocalVariable[] parameters = new ILocalVariable[parameterNames.length];
			for (int i = 0; i < parameterNames.length; ++i) {
				ILocalVariable var = AbstractSarlTest.mock(ILocalVariable.class);
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
		} catch (JavaModelException exception) {
			throw new RuntimeException(exception);
		}
	}

	/** Replies a mock of a IField.
	 *
	 * <table>
	 * <tr><td>Z</td><td>boolean</td></tr>
	 * <tr><td>B</td><td>byte</td></tr>
	 * <tr><td>C</td><td>char</td></tr>
	 * <tr><td>S</td><td>short</td></tr>
	 * <tr><td>I</td><td>int</td></tr>
	 * <tr><td>J</td><td>long</td></tr>
	 * <tr><td>F</td><td>float</td></tr>
	 * <tr><td>D</td><td>double</td></tr>
	 * <tr><td>V</td><td>void</td></tr>
	 * <tr><td>L fully-qualified-class ;</td><td>fully-qualified-class</td></tr>
	 * <tr><td>[ type</td><td>type[]</td></tr>
	 * </table>
	 */
	protected static IField createIFieldMock(IType type, String fieldName, String typeSignature,
			int flags) {
		try {
			IField field = AbstractSarlTest.mock(IField.class);
			when(field.getDeclaringType()).thenReturn(type);
			when(field.getElementName()).thenReturn(fieldName);
			when(field.getElementName()).thenReturn(fieldName);
			when(field.getElementType()).thenReturn(IJavaElement.FIELD);
			if (Strings.isNullOrEmpty(typeSignature)) {
				when(field.getTypeSignature()).thenReturn("V");
			} else {
				when(field.getTypeSignature()).thenReturn(typeSignature);
			}
			when(field.getFlags()).thenReturn(flags);
			return field;
		} catch (JavaModelException exception) {
			throw new RuntimeException(exception);
		}
	}

	/** Replies a mock of a IJavaProject.
	 *
	 * @return the mock.
	 * @throws JavaModelException
	 */
	protected static IJavaProject createIJavaProjectMock() throws JavaModelException {
		final IJavaProject project = AbstractSarlTest.mock(IJavaProject.class);
		when(project.findType(anyString())).thenAnswer((invocation) -> {
			String fqn = (String) invocation.getArguments()[0];
			IType type = AbstractSarlTest.mock(IType.class);
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
				final SortedSet<String> expectedSetToOutput = new TreeSet<>(Arrays.asList(expectedKeys));
				final SortedSet<String> actualSetToOutput = new TreeSet<>();
				for (ActionPrototype proto : actualKeys) {
					actualSetToOutput.add(proto.toString());
				}
				throw new ComparisonFailure("Action key not expected: " + actualKey.toString(),
						expectedSetToOutput.toString(), actualSetToOutput.toString());
			}
		}
		if (!expected.isEmpty()) {
			final SortedSet<String> expectedSetToOutput = new TreeSet<>(Arrays.asList(expectedKeys));
			final SortedSet<String> actualSetToOutput = new TreeSet<>();
			for (ActionPrototype proto : actualKeys) {
				actualSetToOutput.add(proto.toString());
			}
			throw new ComparisonFailure("Not enough action keys.",
					expectedSetToOutput.toString(), actualSetToOutput.toString());
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
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock(false, "Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock(false, "Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock(false, "Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock(false, "Type2", null);

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
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock(false, "Type2", "Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock(false, "Type2", "Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock(false, "Type2", "Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock(false, "Type2", "Type1");

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
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", null);

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
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", "Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", "Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", "Type1");

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", "Type1");

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
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", null);

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
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type2", "io.sarl.eclipse.tests.package1.Type1");

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
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package2.Type2", null);

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package2.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package2.Type2", null);

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package2.Type2", null);

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
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPublic);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void protectedMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccProtected);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertTrue(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void packageMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						0);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

				assertFalse(this.jdt2ecore.isVisible(this.jdt2ecore.toTypeFinder(this.project), type, method));
			}

			@Test
			public void privateMethod() throws JavaModelException {
				IType parentType = createITypeMock(false, "io.sarl.eclipse.tests.package1.Type1", null);
				IMethod method = createIMethodMock(parentType, "myFct", "boolean",
						new String[0], new String[0],
						Flags.AccPrivate);

				IType type = createITypeMock(false, "io.sarl.eclipse.tests.package2.Type2", "io.sarl.eclipse.tests.package1.Type1");

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
			IType declaringType = createITypeMock(false, "io.sarl.eclipse.tests.p1.Type1", null);
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
			IType declaringType = createITypeMock(false, "io.sarl.eclipse.tests.p1.Type1", null);
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
			IType declaringType = createITypeMock(false, "io.sarl.eclipse.tests.p1.Type1", null);
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
	public static class PopulateInheritanceContext extends AbstractSarlTest {

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
			IType capacity1 = createITypeMock(true, "io.sarl.eclipse.tests.p0.Capacity1", "io.sarl.lang.core.Capacity",
					null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct1", "Z", Flags.AccPublic));
					},
					null);
			IType capacity2 = createITypeMock(true, "io.sarl.eclipse.tests.p0.Capacity2", "io.sarl.eclipse.tests.p0.Capacity1",
					null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct2", "Z", Flags.AccPublic));
					},
					null);
			IType capacity3 = createITypeMock(true, "io.sarl.eclipse.tests.p0.Capacity3", "io.sarl.lang.core.Capacity",
					null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct3", "V", Flags.AccPublic));
					},
					null);
			IType skill1 = createITypeMock(false, "io.sarl.eclipse.tests.p0.Skill1",
					"io.sarl.lang.core.Skill",
					new String[] {"io.sarl.eclipse.tests.p0.Capacity1"},
					(type, it) -> {
						it.add(createIFieldMock(type, "attr1", "I", Flags.AccProtected));
					},
					(type, it) -> {
						it.add(createIConstructorMock(type, new String[] {"attr"}, 
								new String[] {"I"}, Flags.AccPublic));
						it.add(createIMethodMock(type, "myFct1", "Z", Flags.AccPublic));
						it.add(createIMethodMock(type, "skilFct", "B", new String[] {"a"}, 
								new String[] {"C"}, Flags.AccPublic));
					},
					null);
			UnitTestTypeFinder finder = new UnitTestTypeFinder(capacity1, capacity2, capacity3, skill1);
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					finder,
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
			IType capacity1 = createITypeMock(true, "io.sarl.eclipse.tests.p1.Capacity1", "io.sarl.lang.core.Capacity", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct1", "Z", Flags.AccPublic));
						it.add(createIMethodMock(type, "myFct1", "Z",
								new String[] {"a"}, new String[]{"I"}, Flags.AccPublic));
					},
					null);
			IType capacity2 = createITypeMock(true, "io.sarl.eclipse.tests.p1.Capacity2", "io.sarl.eclipse.tests.p1.Capacity1", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct2", "Z", Flags.AccPublic));
					},
					null);
			IType capacity3 = createITypeMock(true, "io.sarl.eclipse.tests.p1.Capacity3", "io.sarl.lang.core.Capacity", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct3", "V", Flags.AccPublic));
					},
					null);
			IType skill1 = createITypeMock(false, "io.sarl.eclipse.tests.p1.Skill1",
					"io.sarl.lang.core.Skill",
					new String[] {"io.sarl.eclipse.tests.p1.Capacity1"},
					(type, it) -> {
						it.add(createIFieldMock(type, "attr1", "I", Flags.AccProtected));
					},
					(type, it) -> {
						it.add(createIConstructorMock(type, new String[] {"attr"}, 
								new String[] {"I"}, Flags.AccPublic));
						it.add(createIMethodMock(type, "myFct1", "Z", Flags.AccPublic | Flags.AccFinal));
						it.add(createIMethodMock(type, "myFct1", "Z", new String[] {"a"}, new String[]{"I"}, Flags.AccPublic));
						it.add(createIMethodMock(type, "skilFct", "B", new String[] {"a"}, 
								new String[] {"C"}, Flags.AccPublic));
					},
					null);
			UnitTestTypeFinder finder = new UnitTestTypeFinder(capacity1, capacity2, capacity3, skill1);
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					finder,
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
		public void populateInheritanceContext_2() throws Exception {
			IType capacity1 = createITypeMock(true, "io.sarl.eclipse.tests.p2.Capacity1", "io.sarl.lang.core.Capacity", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct1", "Z",
								new String[] {"a"}, new String[]{"[I"}, Flags.AccPublic | Flags.AccVarargs));
					},
					null);
			IType capacity2 = createITypeMock(true, "io.sarl.eclipse.tests.p2.Capacity2", "io.sarl.eclipse.tests.p2.Capacity1", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct2", "Z", Flags.AccPublic));
					},
					null);
			IType capacity3 = createITypeMock(true, "io.sarl.eclipse.tests.p2.Capacity3", "io.sarl.lang.core.Capacity", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct3", "V", Flags.AccPublic));
					},
					null);
			IType skill1 = createITypeMock(false, "io.sarl.eclipse.tests.p2.Skill1",
					"io.sarl.lang.core.Skill",
					new String[] {"io.sarl.eclipse.tests.p2.Capacity1"},
					(type, it) -> {
						it.add(createIFieldMock(type, "attr1", "I", Flags.AccProtected));
					},
					(type, it) -> {
						it.add(createIConstructorMock(type, new String[] {"attr"}, 
								new String[] {"I"}, Flags.AccPublic));
						it.add(createIMethodMock(type, "myFct1", "Z", new String[] {"a"}, new String[]{"[I"},
								Flags.AccPublic | Flags.AccVarargs));
						it.add(createIMethodMock(type, "skilFct", "B", new String[] {"a"}, 
								new String[] {"C"}, Flags.AccPublic));
					},
					null);
			UnitTestTypeFinder finder = new UnitTestTypeFinder(capacity1, capacity2, capacity3, skill1);
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					finder,
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
		public void populateInheritanceContext_3() throws Exception {
			IType capacity1 = createITypeMock(true, "io.sarl.eclipse.tests.p3.Capacity1", "io.sarl.lang.core.Capacity", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct1", "Z",
								new String[] {"a"}, new String[]{"[I"}, Flags.AccPublic | Flags.AccVarargs));
					},
					null);
			IType capacity2 = createITypeMock(true, "io.sarl.eclipse.tests.p3.Capacity2", "io.sarl.eclipse.tests.p3.Capacity1", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct2", "Z", Flags.AccPublic));
					},
					null);
			IType capacity3 = createITypeMock(true, "io.sarl.eclipse.tests.p3.Capacity3", "io.sarl.lang.core.Capacity", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct3", "V", Flags.AccPublic));
					},
					null);
			UnitTestTypeFinder finder = new UnitTestTypeFinder(capacity1, capacity2, capacity3);
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					finder,
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
		}

		@Test
		public void populateInheritanceContext_4() throws Exception {
			UnitTestTypeFinder finder = new UnitTestTypeFinder();
			IType capacity1 = createITypeMock(true, "io.sarl.eclipse.tests.p4.Capacity1", "io.sarl.lang.core.Capacity", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct1", "Z",
								new String[] {"a"}, new String[]{"[I"}, Flags.AccPublic | Flags.AccVarargs));
					},
					finder);
			IType capacity2 = createITypeMock(true, "io.sarl.eclipse.tests.p4.Capacity2", "io.sarl.eclipse.tests.p4.Capacity1", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct2", "Z", Flags.AccPublic));
					},
					finder);
			IType capacity3 = createITypeMock(true, "io.sarl.eclipse.tests.p4.Capacity3", "io.sarl.lang.core.Capacity", null, null,
					(type, it) -> {
						it.add(createIMethodMock(type, "myFct3", "V", Flags.AccPublic));
					},
					finder);
			finder.setTypes(capacity1, capacity2, capacity3);
			//
			IStatus s = this.jdt2ecore.populateInheritanceContext(
					finder,
					finalOperations,
					overridableOperations,
					inheritedFields,
					operationsToImplement,
					superConstructors,
					"io.sarl.lang.core.Skill",
					Arrays.asList("io.sarl.eclipse.tests.p4.Capacity3", "io.sarl.eclipse.tests.p4.Capacity2"));
			//
			assertNotNull(s);
			assertTrue(s.toString(), s.isOK());
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

			private Collection<IType> types;

			public UnitTestTypeFinder(IType... types) {
				this.types = Arrays.asList(types);
			}

			public void setTypes(IType... types) {
				this.types = Arrays.asList(types);
			}

			@Override
			public IType findType(String typeName) throws JavaModelException {
				if ("io.sarl.lang.core.Capacity".equals(typeName)) {
					return createITypeMock(true, typeName, null, null, null, null, null);
				} else if ("io.sarl.lang.core.Skill".equals(typeName)) {
					return createITypeMock(false, typeName, null, null, null, null, null);
				}
				for (IType type : this.types) {
					if (Objects.equals(typeName, type.getFullyQualifiedName())) {
						return type;
					}
				}
				return null;
			}

		}

	}

}
