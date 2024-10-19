/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
package io.sarl.tests.api.tools;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TreeSet;
import java.util.function.Supplier;

import com.google.common.base.Objects;
import com.google.common.base.Predicate;
import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.common.collect.Iterables;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.junit.jupiter.api.AssertionFailureBuilder;
import org.opentest4j.AssertionFailedError;
import org.osgi.framework.Version;

/** Set of utility classes that provide additional assertion functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public final class TestAssertions {

	private TestAssertions() {
		//
	}
	
	/** Assert the values are equal.
	 *
	 * @param expected the expected value.
	 * @param actual the actual value.
	 * @param epsilon the precision.
	 */
	public static void assertEpsilonEquals(double expected, double actual) {
		if (!TestUtils.isEpsilonEquals(expected, actual, true)) {
			throw new AssertionFailedError("Values are not equal.", Double.toString(expected), Double.toString(actual)); //$NON-NLS-1$
		}
	}

	/**
	 * Test if the given exception has a cause of the given type.
	 *
	 * If the given exception has no cause, it is the cause.
	 *
	 * @param <T> - the type of the expected cause.
	 * @param expected the type of the expected cause.
	 * @param actual the exception to test.
	 * @return the cause.
	 */
	public static <T extends Throwable> T assertCause(Class<T> expected, Throwable actual) {
		var cause0 = actual;
		while (cause0 != null && cause0.getCause() != null && cause0.getCause() != cause0) {
			cause0 = cause0.getCause();
		}
		if (cause0 == null) {
			cause0 = actual;
		}
		final var cause = cause0;
		assertTrue(expected.isInstance(cause0),
				() -> "Unexpected type of exception's cause. Expected: " + expected.getName() + ". Actual: " //$NON-NLS-1$ //$NON-NLS-2$
						+ cause.getClass().getName());
		return expected.cast(cause0);
	}

	/** Check if the given value is {@code null} or empty.
	 *
	 * @param actual
	 */
	public static void assertNullOrEmpty(Iterable<?> actual) {
		if (actual != null) {
			assertFalse(actual.iterator().hasNext(), "Not null nor empty"); //$NON-NLS-1$
		}
	}

	/** Check if the given value is {@code null} or empty.
	 *
	 * @param actual
	 */
	public static void assertNullOrEmpty(String actual) {
		if (!Strings.isNullOrEmpty(actual)) {
			fail("Not null nor empty. Actual value: " + actual); //$NON-NLS-1$
		}
	}

	/** Check if the given value is not {@code null} nor empty.
	 *
	 * @param actual
	 */
	public static void assertNotNullOrEmpty(String actual) {
		if (Strings.isNullOrEmpty(actual)) {
			fail("Null or empty."); //$NON-NLS-1$
		}
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertContains(Iterable<?> actual, Object... expected) {
		assertContainsMsg(actual, null, expected);
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param message the error message.
	 * @param expected the expected objects.
	 */
	public static void assertContainsMsg(Iterable<?> actual, Supplier<String> message, Object... expected) {
		assertContainsCollection(actual, Arrays.asList(expected), message);
	}

	/** Test if the actual collection/iterable contains at least the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 * @since 0.11
	 */
	public static void assertContainsAtLeast(Iterable<?> actual, Object... expected) {
		assertContainsAtLeastMsg(actual, null, expected);
	}

	/** Test if the actual collection/iterable contains at least the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param message the error message.
	 * @param expected the expected objects.
	 * @since 0.11
	 */
	public static void assertContainsAtLeastMsg(Iterable<?> actual, Supplier<String> message, Object... expected) {
		assertContainsAtLeastCollection(actual, Arrays.asList(expected), message);
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertContainsCollection(Iterable<?> actual, Iterable<?> expected) {
		assertContainsCollection(actual, expected, null);
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 * @param message the error message.
	 */
	public static void assertContainsCollection(Iterable<?> actual, Iterable<?> expected, Supplier<String> message) {
		assertNotNull(actual);
		var la = new ArrayList<>();
		Iterables.addAll(la, actual);
		var le = new ArrayList<>();
		Iterables.addAll(le, expected);

		final var unexpectedElements = new TreeSet<String>();

		var it1 = la.iterator();
		while (it1.hasNext()) {
			var ac = it1.next();
			it1.remove();
			if (ac != null && !le.remove(ac)) {
				unexpectedElements.add(ac.toString());
			}
		}

		if (!unexpectedElements.isEmpty()) {
			if (!le.isEmpty()) {
				final var emsg = message != null ? "\n" + message.get() : ""; //$NON-NLS-1$ //$NON-NLS-2$
				throw new AssertionFailedError(
						"Unexpected elements:\n" + unexpectedElements.toString() //$NON-NLS-1$
						+ "\nExpecting the following elements:\n" + le.toString() + emsg, //$NON-NLS-1$
						toString(expected),
						toString(actual));
			}
			final var emsg = message != null ? "\n" + message.get() : ""; //$NON-NLS-1$ //$NON-NLS-2$
			throw new AssertionFailedError(
					"Unexpected elements:\n" + unexpectedElements.toString() + emsg, //$NON-NLS-1$
					toString(expected),
					toString(actual));
		} else if (!le.isEmpty()) {
			final var emsg = message != null ? "\n" + message.get() : ""; //$NON-NLS-1$ //$NON-NLS-2$
			throw new AssertionFailedError("Expecting the following elements:\n" + le.toString() + emsg, //$NON-NLS-1$
					toString(expected),
					toString(actual));
		}
	}

	/** Test if the actual collection/iterable contains at least the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 * @since 0.11
	 */
	public static void assertContainsAtLeastCollection(Iterable<?> actual, Iterable<?> expected) {
		assertContainsAtLeastCollection(actual, expected, null);
	}

	/** Test if the actual collection/iterable contains at least the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 * @param message the error message.
	 * @since 0.11
	 */
	public static void assertContainsAtLeastCollection(Iterable<?> actual, Iterable<?> expected, Supplier<String> message) {
		assertNotNull(actual);
		var la = new ArrayList<>();
		Iterables.addAll(la, actual);
		var le = new ArrayList<>();
		Iterables.addAll(le, expected);

		var it1 = la.iterator();
		while (it1.hasNext()) {
			var ac = it1.next();
			it1.remove();
			if (ac != null) {
				le.remove(ac);
			}
		}

		if (!le.isEmpty()) {
			final var emsg = message != null ? "\n" + message.get() : ""; //$NON-NLS-1$ //$NON-NLS-2$
			throw new AssertionFailedError("Expecting the following elements:\n" + le.toString() + emsg, //$NON-NLS-1$
					toString(expected),
					toString(actual));
		}
	}

	private static String toString(Iterable<?> iterable) {
		final var buf = new StringBuilder();
		if (iterable != null) {
			final var elements = new ArrayList<String>();
			for (final var obj : iterable) {
				if (obj == null) {
					elements.add("null"); //$NON-NLS-1$
				} else {
					elements.add("<" + obj.toString() + ">"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
			final var tab = new String[elements.size()];
			elements.toArray(tab);
			Arrays.sort(tab);
			for (final var obj : tab) {
				buf.append(obj).append("\n"); //$NON-NLS-1$
			}
		}
		return buf.toString();
	}

	/** Test if the actual collection/iterable contains at least all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertPartlyContains(Iterable<?> actual, Object... expected) {
		assertPartlyContainsCollection(actual, Arrays.asList(expected));
	}

	/** Test if the actual collection/iterable contains at least all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertPartlyContainsCollection(Iterable<?> actual, Iterable<?> expected) {
		assertNotNull(actual);
		var la = new ArrayList<>();
		Iterables.addAll(la, actual);
		var le = new ArrayList<>();
		Iterables.addAll(le, expected);

		var it1 = la.iterator();
		while (it1.hasNext()) {
			var ac = it1.next();
			it1.remove();
			le.remove(ac);
		}

		if (!le.isEmpty()) {
			fail("Expecting the following elements:\n" + le.toString() + "\nbut was:\n" + //$NON-NLS-1$ //$NON-NLS-2$
					Iterables.toString(actual));
		}
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertContainsStrings(Iterable<?> actual, String... expected) {
		assertContainsStringCollection(actual, Arrays.asList(expected));
	}

	/** Test if the actual collection/iterable contains all the expected objects.
	 *
	 * @param actual the collection to test.
	 * @param expected the expected objects.
	 */
	public static void assertContainsStringCollection(Iterable<?> actual, Iterable<String> expected) {
		assertNotNull(actual);
		var la = new ArrayList<>();
		Iterables.addAll(la, actual);
		var le = new ArrayList<>();
		Iterables.addAll(le, expected);

		var it1 = la.iterator();
		while (it1.hasNext()) {
			var ac = it1.next();
			it1.remove();
			if (!le.remove(ac.toString())) {
				fail("Unexpecting element: " + ac); //$NON-NLS-1$
				return;
			}
		}

		if (!le.isEmpty()) {
			fail("Expecting the following elements:\n" + le.toString() + "\nbut was:\n" + //$NON-NLS-1$ //$NON-NLS-2$
					Iterables.toString(actual));
		}
	}

	/** Test if the actual string contains all the expected substring.
	 *
	 * @param actual the actual value of the string.
	 * @param expected the expected substring.
	 * @since 0.13
	 */
	public static void assertContains(String expected, String actual) {
		assertNotNull(actual, "The value is null and cannot contain a substring"); //$NON-NLS-1$
		if (!actual.contains(expected)) {
			AssertionFailureBuilder.assertionFailure()
				.expected(expected)
				.actual(actual)
				.message("Value does not contain the substring: " + expected) //$NON-NLS-1$
				.buildAndThrow();
		}
	}

	/** Test if the actual string contains all the expected substring.
	 *
	 * @param actual the actual value of the string.
	 * @param expected the expected substring.
	 * @since 0.13
	 */
	public static void assertMultiContains(String actual, String... expected) {
		assertNotNull(actual, "The value is null and cannot contain a substring"); //$NON-NLS-1$
		for (final var exp : expected) {
			if (!actual.contains(exp)) {
				AssertionFailureBuilder.assertionFailure()
					.expected(exp)
					.actual(actual)
					.message("Value does not contain the substring: " + exp) //$NON-NLS-1$
					.buildAndThrow();
			}
		}
	}

	/** Assert if the value is the string representation of
	 * the boolean value {@code true}.
	 *
	 * @param actual the value.
	 */
	public static void assertTrueStr(String actual) {
		assertTrueStr(actual, null);
	}

	/** Assert if the value is the string representation of
	 * the boolean value {@code true}.
	 *
	 * @param actual the value.
	 * @param message the error message.
	 */
	public static void assertTrueStr(String actual, Supplier<String> message) {
		assertEquals(Boolean.TRUE.toString(), actual, message);
	}

	/** Assert if the value is the string representation of
	 * the boolean value {@code false}.
	 *
	 * @param actual the value.
	 */
	public static void assertFalseStr(String actual) {
		assertFalseStr(actual, null);
	}

	/** Assert if the value is the string representation of
	 * the boolean value {@code false}.
	 *
	 * @param actual the value.
	 * @param message the error message.
	 */
	public static void assertFalseStr(String actual, Supplier<String> message) {
		assertEquals(Boolean.FALSE.toString(), actual, message);
	}

	/** Assert if the system property with the given name has
	 * the boolean value {@code true}.
	 *
	 * The property must be defined
	 *
	 * @param name the name of the property.
	 */
	public static void assertTrueProperty(String name) {
		var v = System.getProperty(name);
		if (Strings.isNullOrEmpty(v)) {
			fail("The property '" + name + "' is not defined."); //$NON-NLS-1$ //$NON-NLS-2$
		}
		assertTrueStr(v, () -> "The property '" + name + "' is expected to be true."); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Assert if the system property with the given name has
	 * the boolean value {@code false}.
	 *
	 * @param name the name of the property.
	 */
	public static void assertFalseProperty(String name) {
		var v = System.getProperty(name);
		if (Strings.isNullOrEmpty(v)) {
			fail("The property '" + name + "' is not defined."); //$NON-NLS-1$ //$NON-NLS-2$
		}
		assertFalseStr(v, () -> "The property '" + name + "' is expected to be true."); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Assert if the system property with the given name has
	 * the boolean value {@code false}.
	 *
	 * @param name the name of the property.
	 */
	public static void assertNullProperty(String name) {
		var v = System.getProperty(name);
		if (!Strings.isNullOrEmpty(v)) {
			fail("The property '" + name + "' is expected to be undefined; but is has the value: " + v); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/** Assert if the system property with the given name has
	 * the given value.
	 *
	 * @param name the name of the property.
	 * @param value the value of the property.
	 */
	public static void assertProperty(String name, String value) {
		if (Strings.isNullOrEmpty(value)) {
			assertNullProperty(name);
		} else {
			var v = System.getProperty(name);
			if (Strings.isNullOrEmpty(v)) {
				fail("The property '" + name + "' is expected to be defined." + v); //$NON-NLS-1$ //$NON-NLS-2$
			}
			assertEquals(value, v);
		}
	}

	/** Assert that the given value is strictly positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyPositive(int actual) {
		if (actual <= 0) {
			fail("Expecting a strictly positive number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is strictly negative.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyNegative(int actual) {
		if (actual >= 0) {
			fail("Expecting a strictly negative number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is strictly positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertPositiveOrZero(int actual) {
		if (actual < 0) {
			fail("Expecting a positive or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is negative or zero.
	 *
	 * @param actual the value to test.
	 */
	public static void assertNegativeOrZero(int actual) {
		if (actual > 0) {
			fail("Expecting a negative or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is strictly positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyPositive(float actual) {
		if (actual <= 0f) {
			fail("Expecting a strictly positive number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is strictly negative.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyNegative(float actual) {
		if (actual >= 0f) {
			fail("Expecting a strictly negative number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is strictly positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertPositiveOrZero(float actual) {
		if (actual < 0f) {
			fail("Expecting a positive or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is negative or zero.
	 *
	 * @param actual the value to test.
	 */
	public static void assertNegativeOrZero(float actual) {
		if (actual > 0f) {
			fail("Expecting a negative or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is strictly positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyPositive(double actual) {
		if (actual <= 0.) {
			fail("Expecting a strictly positive number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is strictly negative.
	 *
	 * @param actual the value to test.
	 */
	public static void assertStrictlyNegative(double actual) {
		if (actual >= 0.) {
			fail("Expecting a strictly negative number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is strictly positive.
	 *
	 * @param actual the value to test.
	 */
	public static void assertPositiveOrZero(double actual) {
		if (actual < 0.) {
			fail("Expecting a positive or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is negative or zero.
	 *
	 * @param actual the value to test.
	 */
	public static void assertNegativeOrZero(double actual) {
		if (actual > 0.) {
			fail("Expecting a negative or zero number, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is equal to zero.
	 *
	 * @param actual the value to test.
	 */
	public static void assertZero(int actual) {
		assertZero(null, actual);
	}

	/** Assert that the given value is equal to zero.
	 *
	 * @param message the error message.
	 * @param actual the value to test.
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

	/** Assert that the given value is not equal to zero.
	 *
	 * @param actual the value to test.
	 * @since 0.11
	 */
	public static void assertNotZero(int actual) {
		assertNotZero(null, actual);
	}

	/** Assert that the given value is not equal to zero.
	 *
	 * @param message the error message.
	 * @param actual the value to test.
	 * @since 0.11
	 */
	public static void assertNotZero(String message, int actual) {
		if (actual == 0) {
			String msg;
			if (!Strings.isNullOrEmpty(message)) {
				msg = message + ". "; //$NON-NLS-1$
			} else {
				msg = ""; //$NON-NLS-1$
			}
			fail(msg + "Expecting a number different than zero"); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is NaN.
	 *
	 * @param actual the value to test.
	 */
	public static void assertNaN(float actual) {
		assertNaN(null, actual);
	}

	/** Assert that the given value is NaN.
	 *
	 * @param message the error message.
	 * @param actual the value to test.
	 */
	public static void assertNaN(String message, float actual) {
		if (!Float.isNaN(actual)) {
			String msg;
			if (!Strings.isNullOrEmpty(message)) {
				msg = message + ". "; //$NON-NLS-1$
			} else {
				msg = ""; //$NON-NLS-1$
			}
			fail(msg + "Expecting NaN, actual: " + actual); //$NON-NLS-1$
		}
	}

	/** Assert that the given value is NaN.
	 *
	 * @param actual the value to test.
	 */
	public static void assertNaN(double actual) {
		assertNaN(actual, null);
	}

	/** Assert that the given value is NaN.
	 *
	 * @param actual the value to test.
	 * @param message the error message.
	 */
	public static void assertNaN(double actual, Supplier<String> message) {
		if (!Double.isNaN(actual)) {
			final var bmsg = new StringBuilder();
			if (message != null) {
				bmsg.append(message.get());
			}
			if (bmsg.length() > 0) {
				bmsg.append(". "); //$NON-NLS-1$
			}
			bmsg.append("Expecting NaN, actual: ").append(actual); //$NON-NLS-1$
			fail(bmsg.toString());
		}
	}

	/** Assert that the two given arrays contain the same values even
	 * they are not in the same order.
	 *
	 * @param expected the expected values.
	 * @param actual the actual values.
	 */
	public static <T> void assertArraySimilar(T[] expected, T[] actual) {
		var expectedValues = new ArrayList<>(Arrays.asList(expected));
		var actualValues = new ArrayList<>(Arrays.asList(actual));
		var iterator = expectedValues.iterator();
		while (iterator.hasNext()) {
			var v = iterator.next();
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
	 * @param expected the array.
	 * @param actual the value.
	 */
	public static <T> void assertArrayContains(T[] expected, T actual) {
		for (var value : expected) {
			if (Objects.equal(actual, value)) {
				return;
			}
		}
		fail("Unable to find the value in the array, value: " + actual //$NON-NLS-1$
				+ "\narray: " + Arrays.toString(expected)); //$NON-NLS-1$
	}

	/** Assert that the given iterable object replies the expected identifiers.
	 *
	 * The order of the identifier is significant.
	 *
	 * @param actualReferences the actual elements.
	 * @param expectedIdentifiers the expected elements.
	 * @see JvmTypeReference#getIdentifier()
	 */
	public static void assertTypeReferenceIdentifiers(Iterable<? extends JvmTypeReference> actualReferences, String... expectedIdentifiers) {
		var i = 0;
		for (final var reference : actualReferences) {
			assertTypeReferenceIdentifier(reference, expectedIdentifiers[i]);
			++i;
		}
		if (i < expectedIdentifiers.length) {
			fail("Not enough identifiers. Expected: " + Arrays.toString(expectedIdentifiers) //$NON-NLS-1$
			+ "Actual: " + Iterables.toString(actualReferences)); //$NON-NLS-1$
		}
	}

	/** Assert the the given type reference has the given identifier.
	 *
	 * @param actualReference the actual type reference.
	 * @param expectedIdentifier the expected identifier.
	 * @see JvmTypeReference#getIdentifier()
	 */
	public static void assertTypeReferenceIdentifier(JvmTypeReference actualReference, String expectedIdentifier) {
		if (actualReference == null) {
			assertEquals("void", expectedIdentifier); //$NON-NLS-1$
			return;
		}
		assertEquals(expectedIdentifier, actualReference.getIdentifier(), () -> "Unexpected type reference"); //$NON-NLS-1$
	}

	/** Assert that the given actual formal parameters have the expected names.
	 *
	 * The order of the parameters and the expected names is significant.
	 *
	 * @param actualFormalParameters the list of the formal parameters.
	 * @param expectedParameterNames the expected names for the formal parameters.
	 */
	public static void assertParameterNames(Iterable<? extends XtendParameter> actualFormalParameters, String... expectedParameterNames) {
		var i = 0;
		for (final var parameter : actualFormalParameters) {
			final var ii = i;
			assertEquals(parameter.getName(), expectedParameterNames[i],
					() -> "Unexpected parameter: " + parameter + ". Expected: " + expectedParameterNames[ii]); //$NON-NLS-1$ //$NON-NLS-2$
			++i;
		}
		if (i < expectedParameterNames.length) {
			fail("Not enough identifiers. Expected: " + Arrays.toString(expectedParameterNames) //$NON-NLS-1$
			+ "Actual: " + Iterables.toString(actualFormalParameters)); //$NON-NLS-1$
		}
	}

	/** Assert that the given actual formal parameters have the expected types.
	 *
	 * The order of the parameters and the expected types is significant.
	 *
	 * @param actualFormalParameters the list of the formal parameters.
	 * @param expectedParameterTypes the expected types for the formal parameters.
	 */
	public static void assertParameterTypes(Iterable<? extends XtendParameter> actualFormalParameters, String... expectedParameterTypes) {
		var i = 0;
		for (var parameter : actualFormalParameters) {
			assertTypeReferenceIdentifier(
					parameter.getParameterType(), expectedParameterTypes[i]);
			++i;
		}
		if (i < expectedParameterTypes.length) {
			fail("Not enough identifiers. Expected: " + Arrays.toString(expectedParameterTypes) //$NON-NLS-1$
			+ "Actual: " + Iterables.toString(actualFormalParameters)); //$NON-NLS-1$
		}
	}

	/** Assert that the last parameter in the given actual formal parameters is variadic.
	 *
	 * @param actualFormalParameters
	 */
	public static void assertParameterVarArg(Iterable<? extends XtendParameter> actualFormalParameters) {
		var iterator = actualFormalParameters.iterator();
		XtendParameter lastParam = null;
		while (iterator.hasNext()) {
			lastParam = iterator.next();
		}
		if (lastParam == null || !lastParam.isVarArg()) {
			fail("The last parameter is expected to be a variadic parameter."); //$NON-NLS-1$
		}
	}

	/** Assert that the last parameter in the given actual formal parameters is not variadic.
	 *
	 * @param actualFormalParameters
	 */
	public static void assertNoParameterVarArg(Iterable<? extends XtendParameter> actualFormalParameters) {
		var iterator = actualFormalParameters.iterator();
		XtendParameter lastParam = null;
		while (iterator.hasNext()) {
			lastParam = iterator.next();
		}
		if (lastParam != null && lastParam.isVarArg()) {
			fail("The last parameter is expected to be not a variadic parameter."); //$NON-NLS-1$
		}
	}

	/** Assert the actual XExpression is of the given type and initialized with the given literal.
	 *
	 * @param actualExpression the expression to test.
	 * @param expectedType the expected type of expression.
	 * @param expectedValue the expected value.
	 */
	public static void assertXExpression(XExpression actualExpression, Class<? extends XExpression> expectedType, String expectedValue) {
		assertTrue(expectedType.isInstance(actualExpression), () -> "Expecting type of expression: " + expectedType.getName()); //$NON-NLS-1$
		if (XNumberLiteral.class.isAssignableFrom(expectedType)) {
			assertEquals(expectedValue, ((XNumberLiteral) actualExpression).getValue(), () -> "Invalid value."); //$NON-NLS-1$
		}
		else if (XStringLiteral.class.isAssignableFrom(expectedType)) {
			assertEquals(expectedValue, ((XStringLiteral) actualExpression).getValue(), () -> "Invalid value."); //$NON-NLS-1$
		}
		else if (XNullLiteral.class.isAssignableFrom(expectedType)) {
			//
		}
	}

	/** Assert the actual object is a not-null instance of the given type.
	 *
	 * @param expected the expected type.
	 * @param actual the instance.
	 */
	public static void assertInstanceOf(Class<?> expected, Object actual) {
		assertInstanceOf(expected, actual, null);
	}

	/** Assert the actual object is a not-null instance of the given type.
	 *
	 * @param expected the expected type.
	 * @param actual the instance.
	 * @param message the error message.
	 */
	public static void assertInstanceOf(Class<?> expected, Object actual, Supplier<String> message) {
		var m = (message == null) ? null : message.get();
		if (Strings.isNullOrEmpty(m)) {
			m = "Unexpected object type."; //$NON-NLS-1$
		}
		if (actual == null) {
			fail(m);
		} else if (!expected.isInstance(actual)) {
			throw new AssertionFailedError(
					m,
					expected.getName(),
					actual.getClass().getName());
		}
	}

	/** Assert if the two OSGI version are equal.
	 *
	 * @param expected the expected value.
	 * @param actual the actual value.
	 */
	public static void assertOsgiVersionEquals(Version expected, Version actual) {
		if (Objects.equal(expected, actual)) {
			return;
		}
		if (expected == null) {
			fail("Version not null"); //$NON-NLS-1$
		} else if (actual == null) {
			fail("Unexpected null value"); //$NON-NLS-1$
		} else if (expected.getMajor() == actual.getMajor()
				&& expected.getMinor() == actual.getMinor()
				&& expected.getMicro() == actual.getMicro()) {
			if (!Strings.isNullOrEmpty(expected.getQualifier())) {
				final var expectedQualifier = expected.getQualifier();
				if ("qualifier".equals(expectedQualifier)) { //$NON-NLS-1$
					if (!Strings.isNullOrEmpty(actual.getQualifier())) {
						return;
					}
				}
				if (Objects.equal(expected, actual.getQualifier())) {
					return;
				}
			} else {
				return;
			}
		} else {
			throw new AssertionFailedError("Not same versions", expected.toString(), actual.toString()); //$NON-NLS-1$
		}
	}

	/** Assert that the given issue is inside the list of issues.
	 *
	 * @param issues the list of issues.
	 * @param severity the expected severity.
	 * @param model the issued model.
	 * @param objectType the type of the object.
	 * @param code the issue code.
	 * @param messageParts the parts of the issue message that are expected.
	 */
	public static void assertIssue(List<Issue> issues, Severity severity, EObject model,
			EClass objectType, String code, String... messageParts) {
		var iterator = issues.iterator();
		while (iterator.hasNext()) {
			var issue = iterator.next();
			if (Objects.equal(issue.getCode(), code) && issue.getSeverity() == severity) {
				var object = model.eResource().getResourceSet().getEObject(issue.getUriToProblem(), true);
				if (objectType.isInstance(object)) {
					if (TestIssues.isIssueMessage(issue, messageParts)) {
						iterator.remove();
						return;
					}
				}
			}
		}
		var message = new StringBuilder("Expected "); //$NON-NLS-1$
		message.append(severity);
		message.append(" '"); //$NON-NLS-1$
		message.append(code);
		message.append("' on "); //$NON-NLS-1$
		message.append(objectType.getName());
		message.append(" but got\n"); //$NON-NLS-1$
		TestIssues.getIssuesAsString(model, issues, message);
		fail(message.toString());
	}

	/** Assert that the given object was compiled with at least one error.
	 *
	 * @param validationHelper the helper for running the validation process.
	 * @param source the compiled object from which errors should be retrieved.
	 * @param codes the list of the error codes to be ignored.
	 * @since 0.7
	 */
	public static void assertAnyError(ValidationTestHelper validationHelper, EObject source, String... codes) {
		assert validationHelper != null;
		final var codeSet = Arrays.asList(codes);
		final var validate = validationHelper.validate(source);
		if (!Iterables.any(validate, input -> Severity.ERROR == input.getSeverity() && !codeSet.contains(input.getCode()))) {
			fail("Expected an error, but got nothing"); //$NON-NLS-1$
		}
	}

	/** Assert that the given object was compiled without any error except the ones with the specified codes.
	 *
	 * @param validationHelper the helper for running the validation process.
	 * @param source the compiled object from which errors should be retrieved.
	 * @param codes the list of the error codes to be ignored.
	 * @since 0.7
	 */
	public static void assertNoErrorsExcept(ValidationTestHelper validationHelper, EObject source, String... codes) {
		assert validationHelper != null;
		final var codeSet = Arrays.asList(codes);
		final var validate = validationHelper.validate(source);
		final Predicate<Issue> pred = input -> Severity.ERROR == input.getSeverity() && !codeSet.contains(input.getCode());
		if (Iterables.any(validate, pred)) {
			fail("Expected no error, found: " + Iterables.filter(validate, pred)); //$NON-NLS-1$
		}
	}

	/** Assert that the given warning is inside the list of issues.
	 *
	 * @param issues the list of issues.
	 * @param severity the expected severity.
	 * @param model the issued model.
	 * @param objectType the type of the object.
	 * @param code the issue code.
	 * @param messageParts the parts of the issue message that are expected.
	 */
	public static void assertWarning(List<Issue> issues, EObject model, EClass objectType, String code,
			String... messageParts) {
		assertIssue(issues, Severity.WARNING, model, objectType, code, messageParts);
	}

	/**
	 * Assert there is no more issue in the list.
	 *
	 * @param issues the list of issues.
	 * @param model the checked model.
	 */
	public static void assertNoMoreIssues(List<Issue> issues, EObject model) {
		if (!issues.isEmpty()) {
			var message = new StringBuilder("Expecting no issue but got\n"); //$NON-NLS-1$
			TestIssues.getIssuesAsString(model, issues, message);
			fail(message.toString());
		}
	}

	/** Assert all the elements within the given collection are different.
	 *
	 * @param collection the collection to test.
	 */
	public static void assertAllDifferents(List<?> collection) {
		final var len = collection.size();
		final var penulvian = collection.size() - 1;
		for (var i = 0; i < penulvian; ++i) {
			final var obj1 = collection.get(i);
			for (var j = i + 1; j < len; ++j) {
				final var obj2 = collection.get(j);
				if (Objects.equal(obj1, obj2)) {
					fail("Same objects at positions " + i + " and " + j); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}
	}

	/** Assert that the given code generates an exception of the given type.
	 *
	 * @param <T> the type of the expected exception.
	 * @param expected the type of the expected exception.
	 * @param code the code to run.
	 * @return the exception if it is thrown.
	 * @throws Exception 
	 */
	public static <T extends Throwable> T assertException(Class<T> expected, Code code) throws Exception {
		try {
			code.run();
			fail("Expecting exception of type " + expected.getName()); //$NON-NLS-1$
			return null;
		} catch (AssertionFailedError innerException) {
			// See fail above
			return null;
		} catch (Throwable ex) {
			final var cause = Throwables.getRootCause(ex);
			if (!expected.isAssignableFrom(cause.getClass())) {
				fail("Expecting exception of type " + expected.getName() + ", but got " + cause.getClass().getName(), cause); //$NON-NLS-1$ //$NON-NLS-2$
				return null;
			}
			return expected.cast(cause);
		}
	}

	/** Assert that the given code generates an exception of the given type, and replies the checker if
	 * the exception occured.
	 *
	 * @param <T> the type of the exception.
	 * @param expected the type of the expected exception.
	 * @param code the code to run.
	 * @return the exception checker.
	 * @throws Exception 
	 */
	@SuppressWarnings("unchecked")
	public static <T extends Throwable> ExceptionChecker<T> whenException(Class<T> expected, Code code) throws Exception {
		try {
			code.run();
			fail("Expecting exception of type " + expected.getName() + ", but no exception is known"); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (Throwable ex) {
			final var cause = Throwables.getRootCause(ex);
			if (!expected.isAssignableFrom(cause.getClass())) {
				fail("Expecting exception of type " + expected.getName() + ", but get " + cause.getClass().getName(), cause); //$NON-NLS-1$ //$NON-NLS-2$
			} else {
				return new ExceptionChecker<>((T) cause);
			}
		}
		return null;
	}

	/** Assert that the two strings are equal except for the new-line characters.
	 *
	 * @param expected the expected value.
	 * @param actual the actual value.
	 * @param message the supplier of error message. 
	 * @since 0.12
	 */
	public static void assertEqualsExceptNewLines(String expected, String actual, Supplier<String> message) {
		var es = expected.replaceAll("(\\r)|(\r)", ""); //$NON-NLS-1$ //$NON-NLS-2$
		es = es.replaceAll("(\\n)|(\n)", "\n"); //$NON-NLS-1$ //$NON-NLS-2$
		var as = actual.replaceAll("(\\r)|(\r)", ""); //$NON-NLS-1$ //$NON-NLS-2$
		as = as.replaceAll("(\\n)|(\n)", "\n"); //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals(es, as, message);
	}

	/** Assert that the two strings are equal except for the new-line characters.
	 *
	 * @param expected the expected value.
	 * @param actual the actual value.
	 * @since 0.12
	 */
	public static void assertEqualsExceptNewLines(String expected, String actual) {
		assertEqualsExceptNewLines(expected, actual, () -> {
			final String diff = TestUtils.differences(expected, actual);
			return "Not equal. " + diff; //$NON-NLS-1$
		});
	}

	/** Code to be run.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.11
	 */
	@FunctionalInterface
	public interface Code {

		/** Code.
		 *
		 * @throws Exception any exception
		 */
		void run() throws Exception;

	}

	/** Verification code for exception.
	 *
	 * @param <T> the type of the exception.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.11
	 */
	@FunctionalInterface
	public interface ExceptionCode<T extends Throwable> {

		/** Code.
		 *
		 * @param it the exception to verify.
		 * @throws Exception any exception
		 */
		void run(T it) throws Exception;

	}

	/** Exception checker.
	 *
	 * @param <T> the type of the exception.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.11
	 */
	public static class ExceptionChecker<T extends Throwable> {

		private final T exception;

		/** Constructor.
		 *
		 * @param exception the exception to check.
		 */
		ExceptionChecker(T exception) {
			this.exception = exception;
		}
		
		/** Run the given check code.
		 *
		 * @param code the value to check.
		 * @throws Exception any exception
		 */
		public void verify(ExceptionCode<T> code) throws Exception {
			assertNotNull(code);
			code.run(this.exception);
		}

	}

}
