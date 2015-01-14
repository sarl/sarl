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
package io.sarl.tests.api;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import io.sarl.lang.sarl.SarlScript;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.mockito.InjectMocks;
import org.mockito.Mock;

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
		if (actual != 0) {
			fail("Expecting a zero number, actual: " + actual); //$NON-NLS-1$
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

}
