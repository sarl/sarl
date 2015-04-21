/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.tests.api;

import static com.google.common.collect.Iterables.filter;
import static com.google.common.collect.Iterables.isEmpty;
import static com.google.common.collect.Lists.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlSkill;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.TemporaryFolder;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.junit.Assume;
import org.junit.Rule;
import org.junit.internal.AssumptionViolatedException;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runner.RunWith;
import org.junit.runners.model.Statement;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.google.common.base.Joiner;
import com.google.common.base.Objects;
import com.google.common.base.Predicate;
import com.google.common.base.Strings;
import com.google.common.collect.Collections2;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.inject.Inject;
import com.google.inject.Provider;

/** Abstract class that is providing useful tools for unit tests.
 *
 * This class provides assertion functions, clear any property
 * related to Sarl, and reset the attributes of the unit test that
 * are marked <code>@Mock</code>, <code>@InjectMocks</code> or
 * <code>@Nullable</code>.
 *
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
public abstract class AbstractSarlTest {

	@Inject
	private ValidationTestHelper validationHelper;
	
	@Inject
	private ParseHelper<XtendFile> parser;
	
	@Inject
	private SarlJvmModelAssociations associations;
	
	/** This rule permits to clean automatically the fields
	 * at the end of the test.
	 */
	@Rule
	public TestWatcher rootSarlWatchter = new TestWatcher() {
		private boolean isMockable() {
			Class<?> type = AbstractSarlTest.this.getClass();
			while (type != null && !Object.class.equals(type)) {
				for (Field field : type.getDeclaredFields()) {
					if (field.getAnnotation(Mock.class) != null
						|| field.getAnnotation(InjectMocks.class) != null) {
						return true;
					}
				}
				type = type.getSuperclass();
			}
			return false;
		}
		@Override
		protected void starting(Description description) {
			if (isMockable()) {
				MockitoAnnotations.initMocks(AbstractSarlTest.this);
			}
		}
		@Override
		public Statement apply(Statement base, Description description) {
			// This test is working only in Eclipse, or Maven/Tycho.
			TestScope scope = description.getAnnotation(TestScope.class);
			if (scope == null) {
				Class<?> enclosingType = description.getTestClass();
				while (scope == null && enclosingType != null) {
					scope = enclosingType.getAnnotation(TestScope.class);
					enclosingType = enclosingType.getEnclosingClass();
				}
			}
			if (scope != null) {
				if (!scope.tycho() && !scope.eclipse()) {
					throw new AssumptionViolatedException("not running on the current framework");
				} else if (scope.tycho() || scope.eclipse()) {
					boolean isEclipse = System.getProperty("sun.java.command", "").startsWith("org.eclipse.jdt.internal.junit.");
					if (scope.tycho()) {
						Assume.assumeFalse(isEclipse);
					} else {
						Assume.assumeTrue(isEclipse);
					}
				}
			}
			return super.apply(base, description);
		}
		@Override
		protected void finished(Description description) {
			// Clear the references to the mock objects or the injected objects
			Class<?> type = AbstractSarlTest.this.getClass();
			while (type != null && !Object.class.equals(type)) {
				for (Field field : type.getDeclaredFields()) {
					if ((field.getAnnotation(Mock.class) != null
							|| field.getAnnotation(InjectMocks.class) != null
							|| field.getAnnotation(Nullable.class) != null)
							&& (field.getModifiers() & (Modifier.FINAL | Modifier.STATIC)) == 0) {
						boolean isAcc = field.isAccessible();
						try {
							field.setAccessible(true);
							field.set(AbstractSarlTest.this, null);
						} catch (Exception e) {
							throw new Error(e);
						} finally {
							field.setAccessible(isAcc);
						}
					}
				}
				type = type.getSuperclass();
			}
		}
	};

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual - the collection to test.
	 * @param expected - the expected objects.
	 */
	public static void assertContains(Iterable<?> actual, Object... expected) {
		assertContainsCollection(actual, Arrays.asList(expected));
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual - the collection to test.
	 * @param expected - the expected objects.
	 */
	public static void assertContainsCollection(Iterable<?> actual, Iterable<?> expected) {
		assertNotNull(actual);
		Collection<Object> la = new ArrayList<>();
		Iterables.addAll(la, actual);
		Collection<Object> le = new ArrayList<>();
		Iterables.addAll(le, expected);

		Iterator<?> it1 = la.iterator();
		while (it1.hasNext()) {
			Object ac = it1.next();
			it1.remove();
			if (!le.remove(ac)) {
				fail("Unexpecting element: " + ac);
				return;
			}
		}

		if (!le.isEmpty()) {
			fail("Expecting the following elements:\n" + le.toString() + "\nbut was:\n" +
					Iterables.toString(actual));
		}
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual - the collection to test.
	 * @param expected - the expected objects.
	 */
	public static void assertContainsStrings(Iterable<?> actual, String... expected) {
		assertContainsStringCollection(actual, Arrays.asList(expected));
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual - the collection to test.
	 * @param expected - the expected objects.
	 */
	public static void assertContainsStringCollection(Iterable<?> actual, Iterable<String> expected) {
		assertNotNull(actual);
		Collection<Object> la = new ArrayList<>();
		Iterables.addAll(la, actual);
		Collection<String> le = new ArrayList<>();
		Iterables.addAll(le, expected);

		Iterator<?> it1 = la.iterator();
		while (it1.hasNext()) {
			Object ac = it1.next();
			it1.remove();
			if (!le.remove(ac.toString())) {
				fail("Unexpecting element: " + ac);
				return;
			}
		}

		if (!le.isEmpty()) {
			fail("Expecting the following elements:\n" + le.toString() + "\nbut was:\n" +
					Iterables.toString(actual));
		}
	}

	/** Assert if the value is the string representation of
	 * the boolean vlaue <code>true</code>.
	 *
	 * @param actual - the value.
	 */
	public static void assertTrueStr(String actual) {
		assertTrueStr(null, actual);
	}

	/** Assert if the value is the string representation of
	 * the boolean vlaue <code>true</code>.
	 *
	 * @param message - the error message.
	 * @param actual - the value.
	 */
	public static void assertTrueStr(String message, String actual) {
		assertEquals(message, Boolean.TRUE.toString(), actual);
	}

	/** Assert if the value is the string representation of
	 * the boolean vlaue <code>false</code>.
	 *
	 * @param actual - the value.
	 */
	public static void assertFalseStr(String actual) {
		assertFalseStr(null, actual);
	}

	/** Assert if the value is the string representation of
	 * the boolean vlaue <code>false</code>.
	 *
	 * @param message - the error message.
	 * @param actual - the value.
	 */
	public static void assertFalseStr(String message, String actual) {
		assertEquals(message, Boolean.FALSE.toString(), actual);
	}

	/** Assert if the system property with the given name has
	 * the boolean value <code>true</code>.
	 *
	 * The property must be defined
	 *
	 * @param name - the name of the property.
	 */
	public static void assertTrueProperty(String name) {
		String v = System.getProperty(name);
		if (Strings.isNullOrEmpty(v)) {
			fail("The property '" + name + "' is not defined.");
		}
		assertTrueStr("The property '" + name + "' is expected to be true.", v);
	}

	/** Assert if the system property with the given name has
	 * the boolean value <code>false</code>.
	 *
	 * @param name - the name of the property.
	 */
	public static void assertFalseProperty(String name) {
		String v = System.getProperty(name);
		if (Strings.isNullOrEmpty(v)) {
			fail("The property '" + name + "' is not defined.");
		}
		assertFalseStr("The property '" + name + "' is expected to be true.", v);
	}

	/** Assert if the system property with the given name has
	 * the boolean value <code>false</code>.
	 *
	 * @param name - the name of the property.
	 */
	public static void assertNullProperty(String name) {
		String v = System.getProperty(name);
		if (!Strings.isNullOrEmpty(v)) {
			fail("The property '" + name + "' is expected to be undefined; but is has the value: " + v);
		}
	}

	/** Assert if the system property with the given name has
	 * the given value.
	 *
	 * @param name - the name of the property.
	 * @param value - the value of the property.
	 */
	public static void assertProperty(String name, String value) {
		if (Strings.isNullOrEmpty(value)) {
			assertNullProperty(name);
		} else {
			String v = System.getProperty(name);
			if (Strings.isNullOrEmpty(v)) {
				fail("The property '" + name + "' is expected to be defined." + v);
			}
			assertEquals(value, v);
		}
	}

	/** Assert that the given value is stricty positive.
	 *
	 * @param actual - the value to test.
	 */
	public static void assertStrictlyPositive(int actual) {
		if (actual <= 0) {
			fail("Expecting a strictly positive number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is stricty negative.
	 *
	 * @param actual - the value to test.
	 */
	public static void assertStrictlyNegative(int actual) {
		if (actual >= 0) {
			fail("Expecting a strictly negative number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is stricty positive.
	 *
	 * @param actual - the value to test.
	 */
	public static void assertPositiveOrZero(int actual) {
		if (actual < 0) {
			fail("Expecting a positive or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is negative or zero.
	 *
	 * @param actual - the value to test.
	 */
	public static void assertNegativeOrZero(int actual) {
		if (actual > 0) {
			fail("Expecting a negative or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is equal to zero.
	 *
	 * @param actual - the value to test.
	 */
	public static void assertZero(int actual) {
		assertZero(null, actual);
	}

	/** Assert that the given value is equal to zero.
	 *
	 * @param message - the error message.
	 * @param actual - the value to test.
	 */
	public static void assertZero(String message, int actual) {
		if (actual != 0) {
			String msg;
			if (!Strings.isNullOrEmpty(message)) {
				msg = message + ". "; //$NON-NLS-1$
			} else {
				msg = ""; //$NON-NLS-1$
			}
			fail(msg + "Expecting a zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the two given arrays contain the same values even
	 * they are not in the same order.
	 *
	 * @param expected - the expected values.
	 * @param actual - the actual values.
	 */
	public static <T> void assertArraySimilar(T[] expected, T[] actual) {
		List<T> expectedValues = new ArrayList<>(Arrays.asList(expected));
		List<T> actualValues = new ArrayList<>(Arrays.asList(actual));
		Iterator<T> iterator = expectedValues.iterator();
		while (iterator.hasNext()) {
			T v = iterator.next();
			iterator.remove();
			if (!actualValues.remove(v)) {
				fail("Expecting a value in the array: " + v); //$NON-NLS-1$
			}
		}
		if (!expectedValues.isEmpty()) {
			fail("Expecting a value(s) in the array: " + expectedValues); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is inside the array..
	 *
	 * @param expected - the array.
	 * @param actual - the value.
	 */
	public static <T> void assertArrayContains(T[] expected, T actual) {
		for (T value : expected) {
			if (Objects.equal(actual, value)) {
				return;
			}
		}
		fail("Unable to find the value in the array, value: " + actual //$NON-NLS-1$
				+ "\narray: " + Arrays.toString(expected)); //$NON-NLS-1$
	}

	/** Helper for writting a multiline string in unit tests.
	 *
	 * @param lines - the lines in the string.
	 * @return the complete multiline string.
	 */
	public static String multilineString(Object... lines) {
		return Joiner.on("\n").join(lines);
	}

	/** Assert that the given iterable object replies the expected identifiers.
	 *
	 * The order of the identifier is significant.
	 *
	 * @param actualReferences - the actual elements.
	 * @param expectedIdentifiers - the expected elements.
	 * @see JvmTypeReference#getIdentifier()
	 */
	public static void assertTypeReferenceIdentifiers(Iterable<? extends JvmTypeReference> actualReferences, String... expectedIdentifiers) {
		int i = 0;
		for (JvmTypeReference reference : actualReferences) {
			assertTypeReferenceIdentifier(reference, expectedIdentifiers[i]);
			++i;
		}
		if (i < expectedIdentifiers.length) {
			fail("Not enough identifiers. Expected: " + Arrays.toString(expectedIdentifiers)
					+ "Actual: " + Iterables.toString(actualReferences));
		}
	}

	/** Assert the the given type reference has the given identifier.
	 *
	 * @param actualReference - the actual type reference.
	 * @param expectedIdentifier - the expected identifier.
	 * @see JvmTypeReference#getIdentifier()
	 */
	public static void assertTypeReferenceIdentifier(JvmTypeReference actualReference, String expectedIdentifier) {
		if (actualReference == null) {
			assertEquals("void", expectedIdentifier);
			return;
		}
		assertEquals("Unexpected type reference", expectedIdentifier, actualReference.getIdentifier());
	}

	/** Assert that the given actual formal parameters have the expected names.
	 *
	 * The order of the parameters and the expected names is significant.
	 *
	 * @param actualFormalParameters - the list of the formal parameters.
	 * @param expectedParameterNames - the expected names for the formal parameters.
	 */
	public static void assertParameterNames(Iterable<? extends XtendParameter> actualFormalParameters, String... expectedParameterNames) {
		int i = 0;
		for (XtendParameter parameter : actualFormalParameters) {
			assertEquals("Unexpected parameter: " + parameter + ". Expected: " + expectedParameterNames[i],
					parameter.getName(), expectedParameterNames[i]);
			++i;
		}
		if (i < expectedParameterNames.length) {
			fail("Not enough identifiers. Expected: " + Arrays.toString(expectedParameterNames)
					+ "Actual: " + Iterables.toString(actualFormalParameters));
		}
	}

	/** Assert that the given actual formal parameters have the expected types.
	 *
	 * The order of the parameters and the expected types is significant.
	 *
	 * @param actualFormalParameters - the list of the formal parameters.
	 * @param expectedParameterTypes - the expected types for the formal parameters.
	 */
	public static void assertParameterTypes(Iterable<? extends XtendParameter> actualFormalParameters, String... expectedParameterTypes) {
		int i = 0;
		for (XtendParameter parameter : actualFormalParameters) {
			assertTypeReferenceIdentifier(
					parameter.getParameterType(), expectedParameterTypes[i]);
			++i;
		}
		if (i < expectedParameterTypes.length) {
			fail("Not enough identifiers. Expected: " + Arrays.toString(expectedParameterTypes)
					+ "Actual: " + Iterables.toString(actualFormalParameters));
		}
	}

	/** Assert that the given actual formal parameters have the expected default values.
	 *
	 * The order of the parameters and the expected types is significant.
	 *
	 * The parameter <code>expectedDefaultValues</code> is a sequence of pairs, where
	 * the first element is the type of the default value and the second element is
	 * the representation of the default value.
	 * If the first element of the pair is null (meaning no default value), then
	 * the second element must be missed.
	 *
	 * @param actualFormalParameters - the list of the formal parameters.
	 * @param expectedDefaultValues - the expected default values.
	 */
	public static void assertParameterDefaultValues(Iterable<? extends XtendParameter> actualFormalParameters, Object... expectedDefaultValues) {
		int i = 0;
		for (XtendParameter parameter : actualFormalParameters) {
			if (expectedDefaultValues[i] == null) {
				if (parameter instanceof SarlFormalParameter) {
					assertNull("No default value expected", ((SarlFormalParameter) parameter).getDefaultValue());
				}
			} else {
				assertTrue(parameter instanceof SarlFormalParameter);
				assertTrue("The #" + i + " in expectedDefaultValues is not a Class", expectedDefaultValues[i] instanceof Class);
				Class type = (Class) expectedDefaultValues[i];
				assertTrue("Unexpected type for the default value.", type.isInstance(((SarlFormalParameter) parameter).getDefaultValue()));
				if (XNumberLiteral.class.isAssignableFrom(type)) {
					++i;
					assertEquals(expectedDefaultValues[i], ((XNumberLiteral) ((SarlFormalParameter) parameter).getDefaultValue()).getValue());
				} else if (XStringLiteral.class.isAssignableFrom(type)) {
					++i;
					assertEquals(expectedDefaultValues[i], ((XStringLiteral) ((SarlFormalParameter) parameter).getDefaultValue()).getValue());
				} else if (XNullLiteral.class.isAssignableFrom(type)) {
					//
				} else {
					throw new RuntimeException("Unsupported type of literal for this assertion function");
				}
			}
			++i;
		}
		if (i < expectedDefaultValues.length) {
			fail("Not enough default values. Expected: " + Arrays.toString(expectedDefaultValues)
					+ "Actual: " + Iterables.toString(actualFormalParameters));
		}
	}
	
	/** Assert that the last parameter in the given actual formal parameters is variadic.
	 *
	 * @param actualFormalParameters
	 */
	public static void assertParameterVarArg(Iterable<? extends XtendParameter> actualFormalParameters) {
		Iterator<? extends XtendParameter> iterator = actualFormalParameters.iterator();
		XtendParameter lastParam = null;
		while (iterator.hasNext()) {
			lastParam = iterator.next();
		}
		if (lastParam == null || !lastParam.isVarArg()) {
			fail("The last parameter is expected to be a variadic parameter.");
		}
	}

	/** Assert that the last parameter in the given actual formal parameters is not variadic.
	 *
	 * @param actualFormalParameters
	 */
	public static void assertNoParameterVarArg(Iterable<? extends XtendParameter> actualFormalParameters) {
		Iterator<? extends XtendParameter> iterator = actualFormalParameters.iterator();
		XtendParameter lastParam = null;
		while (iterator.hasNext()) {
			lastParam = iterator.next();
		}
		if (lastParam != null && lastParam.isVarArg()) {
			fail("The last parameter is expected to be not a variadic parameter.");
		}
	}

	/** Assert the actual XExpression is of the given type and initialized with the given literal.
	 *
	 * @param actualExpression - the expression to test.
	 * @param expectedType - the expected type of expression.
	 * @param expectedValue - the expected value.
	 */
	public static void assertXExpression(XExpression actualExpression, Class<? extends XExpression> expectedType, String expectedValue) {
		assertTrue("Expecting type of expression: " + expectedType.getName(),
				expectedType.isInstance(actualExpression));
		if (XNumberLiteral.class.isAssignableFrom(expectedType)) {
			assertEquals("Invalid value.", expectedValue, ((XNumberLiteral) actualExpression).getValue());
		}
		else if (XStringLiteral.class.isAssignableFrom(expectedType)) {
			assertEquals("Invalid value.", expectedValue, ((XStringLiteral) actualExpression).getValue());
		}
		else if (XNullLiteral.class.isAssignableFrom(expectedType)) {
			//
		}
	}

	/** Create an instance of agent
	 */
	protected SarlAgent agent(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlAgent) decls.get(decls.size() - 1);
	}

	/** Create an instance of agent
	 */
	protected SarlAgent agent(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlAgent) decls.get(decls.size() - 1);
	}

	/** Create an instance of capacity.
	 */
	protected SarlCapacity capacity(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlCapacity) decls.get(decls.size() - 1);
	}

	/** Create an instance of capacity.
	 */
	protected SarlCapacity capacity(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlCapacity) decls.get(decls.size() - 1);
	}

	/** Create an instance of event.
	 */
	protected SarlEvent event(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlEvent) decls.get(decls.size() - 1);
	}

	/** Create an instance of event.
	 */
	protected SarlEvent event(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlEvent) decls.get(decls.size() - 1);
	}

	/** Create an instance of skill.
	 */
	protected SarlSkill skill(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlSkill) decls.get(decls.size() - 1);
	}

	/** Create an instance of skill.
	 */
	protected SarlSkill skill(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlSkill) decls.get(decls.size() - 1);
	}

	/** Create an instance of behavior.
	 */
	protected SarlBehavior behavior(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (SarlBehavior) decls.get(decls.size() - 1);
	}

	/** Create an instance of behavior.
	 */
	protected SarlBehavior behavior(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (SarlBehavior) decls.get(decls.size() - 1);
	}

	/** Create an instance of class.
	 */
	protected XtendClass clazz(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (XtendClass) decls.get(decls.size() - 1);
	}

	/** Create an instance of class.
	 */
	protected XtendClass clazz(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (XtendClass) decls.get(decls.size() - 1);
	}

	/** Create a SARL script.
	 */
	protected XtendFile file(String string) throws Exception {
		return file(string, false);
	}

	/** Create an instance of class.
	 */
	protected XtendFile file(String string, boolean validate) throws Exception {
		XtendFile script = this.parser.parse(string);
		if (validate) {
			Resource resource = script.eResource();
			ResourceSet resourceSet = resource.getResourceSet();
			if (resourceSet instanceof XtextResourceSet) {
				((XtextResourceSet) resourceSet).setClasspathURIContext(getClass());
			}
			assertEquals(resource.getErrors().toString(), 0, resource.getErrors().size());
			Collection<Issue> issues = Collections2.filter(issues(resource), new Predicate<Issue>() {
				@Override
				public boolean apply(Issue input) {
					return input.getSeverity() == Severity.ERROR;
				}
			});
			assertTrue("Resource contained errors : " + issues.toString(), issues.isEmpty());
		}
		return script;
	}
	
	/** Validate the given file and reply the issues.
	 */
	protected List<Issue> issues(XtendFile file) {
		return issues(file.eResource());
	}

	/** Validate the given resource and reply the issues.
	 */
	protected List<Issue> issues(Resource resource) {
		return this.validationHelper.validate(resource);
	}

	/** Validate the given file and reply the validator.
	 */
	protected Validator validate(XtendFile file) {
		return validate(file.eResource());
	}

	/** Validate the given resource and reply the validator.
	 */
	protected Validator validate(Resource resource) {
		return new Validator(resource);
	}

	/** Create an instance of annotation type.
	 */
	protected XtendAnnotationType annotationType(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (XtendAnnotationType) decls.get(decls.size() - 1);
	}

	/** Create an instance of annotation type.
	 */
	protected XtendAnnotationType annotationType(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (XtendAnnotationType) decls.get(decls.size() - 1);
	}

	/** Create an instance of interface.
	 */
	protected XtendInterface interfaze(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (XtendInterface) decls.get(decls.size() - 1);
	}

	/** Create an instance of interface.
	 */
	protected XtendInterface interfaze(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (XtendInterface) decls.get(decls.size() - 1);
	}

	/** Create an instance of enumeration.
	 */
	protected XtendEnum enumeration(String string) throws Exception {
		List<XtendTypeDeclaration> decls = file(string).getXtendTypes();
		return (XtendEnum) decls.get(decls.size() - 1);
	}

	/** Create an instance of enumeration.
	 */
	protected XtendEnum enumeration(String string, boolean validate) throws Exception {
		List<XtendTypeDeclaration> decls = file(string, validate).getXtendTypes();
		return (XtendEnum) decls.get(decls.size() - 1);
	}

	/** Create an instance of function.
	 */
	protected SarlAction function(String string, String... prefix) throws Exception {
		XtendClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\nclass Foo { " + string + "}");
		return (SarlAction) clazz.getMembers().get(0);
	}
	
	/** Create an instance of function.
	 */
	protected SarlAction function(String string, boolean validate, String... prefix) throws Exception {
		XtendClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\nclass Foo { " + string + "}");
		return (SarlAction) clazz.getMembers().get(0);
	}

	/** Create an instance of JVM function.
	 */
	protected JvmOperation jvmOperation(String string, String... prefix) throws Exception {
		SarlAction action = function(string, prefix);
		return (JvmOperation) this.associations.getPrimaryJvmElement(action);
	}

	/** Create an instance of JVM function.
	 */
	protected JvmOperation jvmOperation(String string, boolean validate, String... prefix) throws Exception {
		SarlAction action = function(string, validate, prefix);
		return (JvmOperation) this.associations.getPrimaryJvmElement(action);
	}

	/** Create an instance of function signature.
	 */
	protected SarlAction functionSignature(String string, String... prefix) throws Exception {
		XtendInterface interfaze = interfaze(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\ninterface Foo { " + string + "}");
		return (SarlAction) interfaze.getMembers().get(0);
	}
	
	/** Create an instance of function signature.
	 */
	protected SarlAction functionSignature(String string, boolean validate, String... prefix) throws Exception {
		XtendInterface interfaze = interfaze(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\ninterface Foo { " + string + "}", validate);
		return (SarlAction) interfaze.getMembers().get(0);
	}

	/** Create an instance of JVM function.
	 */
	protected JvmOperation jvmOperationSignature(String string, String... prefix) throws Exception {
		SarlAction action = functionSignature(string, prefix);
		return (JvmOperation) this.associations.getPrimaryJvmElement(action);
	}

	/** Create an instance of JVM function.
	 */
	protected JvmOperation jvmOperationSignature(String string, boolean validate, String... prefix) throws Exception {
		SarlAction action = functionSignature(string, validate, prefix);
		return (JvmOperation) this.associations.getPrimaryJvmElement(action);
	}

	/** Create an instance of constructor.
	 */
	protected XtendConstructor constructor(String string, String... prefix) throws Exception {
		XtendClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\nclass Foo { " + string + "}");
		return (XtendConstructor) clazz.getMembers().get(0);
	}

	/** Create an instance of constructor.
	 */
	protected XtendConstructor constructor(String string, boolean validate, String... prefix) throws Exception {
		XtendClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\nclass Foo { " + string + "}", validate);
		return (XtendConstructor) clazz.getMembers().get(0);
	}

	/** Create an instance of JVM constructor.
	 */
	protected JvmConstructor jvmConstructor(String string, String... prefix) throws Exception {
		XtendConstructor constructor = constructor(string, prefix);
		return (JvmConstructor) this.associations.getPrimaryJvmElement(constructor);
	}

	/** Create an instance of JVM constructor.
	 */
	protected JvmConstructor jvmConstructor(String string, boolean validate, String... prefix) throws Exception {
		XtendConstructor constructor = constructor(string, validate, prefix);
		return (JvmConstructor) this.associations.getPrimaryJvmElement(constructor);
	}

	/** Create an instance of field.
	 */
	protected XtendField field(String string, String... prefix) throws Exception {
		XtendClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\nclass Foo { " + string + "}");
		return (XtendField) clazz.getMembers().get(0);
	}

	/** Create an instance of field.
	 */
	protected XtendField field(String string, boolean validate, String... prefix) throws Exception {
		XtendClass clazz = clazz(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\nclass Foo { " + string + "}", validate);
		return (XtendField) clazz.getMembers().get(0);
	}

	/** Create an instance of behavior unit.
	 */
	protected SarlBehaviorUnit behaviorUnit(String string, String... prefix) throws Exception {
		SarlAgent agent = agent(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\nagent Foo { " + string + "}");
		return (SarlBehaviorUnit) agent.getMembers().get(0);
	}
	
	/** Create an instance of behavior unit.
	 */
	protected SarlBehaviorUnit behaviorUnit(String string, boolean validate, String... prefix) throws Exception {
		SarlAgent agent = agent(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\nagent Foo { " + string + "}", validate);
		return (SarlBehaviorUnit) agent.getMembers().get(0);
	}

	/** Create a type reference with the SARL parser.
	 */
	protected JvmTypeReference getType(String typeName, String... prefix) throws Exception {
		SarlAgent agent = agent(
				IterableExtensions.join(Arrays.asList(prefix), "\n")
				+ "\nagent Foo { var fooAttr : " + typeName + " }");
		return ((XtendField) agent.getMembers().get(0)).getType();
	}

	/** Wrapper for the validation helper on a specific resource.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public class Validator {

		private final Resource resource;
		
		/**
		 * @param resource - the resource to validate.
		 */
		private Validator(Resource resource) {
			this.resource = resource;
		}
		
		public List<Issue> validate() {
			return AbstractSarlTest.this.validationHelper.validate(this.resource);
		}
		
		public Validator assertNoIssues() {
			AbstractSarlTest.this.validationHelper.assertNoIssues(this.resource);
			return this;
		}
		
		public Validator assertNoErrors() {
			AbstractSarlTest.this.validationHelper.assertNoErrors(this.resource);
			return this;
		}

		public Validator assertNoError(String issuecode) {
			AbstractSarlTest.this.validationHelper.assertNoError(this.resource, issuecode);
			return this;
		}

		public Validator assertNoErrors(EClass objectType, String code, String... messageParts) {
			AbstractSarlTest.this.validationHelper.assertNoErrors(this.resource, objectType, code, messageParts);
			return this;
		}
		
		public Validator assertNoErrors(String code) {
			AbstractSarlTest.this.validationHelper.assertNoErrors(this.resource, code);
			return this;
		}
		
		public Validator assertNoIssues(EClass objectType) {
			AbstractSarlTest.this.validationHelper.assertNoIssues(this.resource, objectType);
			return this;
		}
		
		public Validator assertNoIssue(EClass objectType, String issuecode) {
			AbstractSarlTest.this.validationHelper.assertNoIssue(this.resource, objectType, issuecode);
			return this;
		}

		public Validator assertError(EClass objectType, String code, int offset, int length, String... messageParts) {
			AbstractSarlTest.this.validationHelper.assertError(this.resource, objectType, code, offset, length, messageParts);
			return this;
		}

		public Validator assertError(EClass objectType, String code, String... messageParts) {
			AbstractSarlTest.this.validationHelper.assertError(this.resource, objectType, code, messageParts);
			return this;
		}
		
		public Validator assertIssue(EClass objectType, String code, Severity severity, String... messageParts) {
			AbstractSarlTest.this.validationHelper.assertIssue(this.resource, objectType, code, severity, messageParts);
			return this;
		}
		
		public Validator assertIssue(EClass objectType, String code, int offset, int length,  Severity severity,
				String... messageParts) {
			AbstractSarlTest.this.validationHelper.assertIssue(this.resource, objectType, code, offset, length, severity,
					messageParts);
			return this;
		}
		
		public Validator assertNoIssues(EClass objectType, String code, Severity severity, String... messageParts) {
			AbstractSarlTest.this.validationHelper.assertNoIssues(this.resource, objectType, code, severity, messageParts);
			return this;
		}
		
		public Validator assertNoIssues(EClass objectType, String code, int offset, int length, Severity severity,
				String... messageParts) {
			AbstractSarlTest.this.validationHelper.assertNoIssues(this.resource, objectType, code, offset, length, severity,
					messageParts);
			return this;
		}

		public Validator assertWarning(EClass objectType, String code, String... messageParts) {
			AbstractSarlTest.this.validationHelper.assertWarning(this.resource, objectType, code, messageParts);
			return this;
		}

		public Validator assertNoWarnings(EClass objectType, String code, String... messageParts) {
			AbstractSarlTest.this.validationHelper.assertNoWarnings(this.resource, objectType, code, messageParts);
			return this;
		}

		public Validator assertWarning(EClass objectType, String code, int offset, int length, String... messageParts) {
			AbstractSarlTest.this.validationHelper.assertWarning(this.resource, objectType, code, offset,
					length, messageParts);
			return this;
		}

	}
	
}
