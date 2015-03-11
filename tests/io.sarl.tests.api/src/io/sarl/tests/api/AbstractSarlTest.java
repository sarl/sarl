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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import io.sarl.lang.sarl.SarlFormalParameter;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.google.common.base.Joiner;
import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;

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
public abstract class AbstractSarlTest {

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

}
