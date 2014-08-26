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
package io.sarl.docs.utils;

import static com.google.common.collect.Iterables.contains;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.net.URL;
import java.sql.Date;
import java.util.regex.Pattern;

import junit.framework.AssertionFailedError;

import org.arakhne.afc.vmutil.Caller;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.Resources;
import org.arakhne.afc.vmutil.URISchemeType;
import org.eclipse.xtext.util.Arrays;
import org.eclipse.xtext.xbase.lib.Functions;
import org.hamcrest.Matcher;

import com.google.common.io.Files;


/** Helper for tests.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SpecificationTools {

	private SpecificationTools() {
		//
	}

	/** Replies a path built from the given elements.
	 *
	 * @param element1 - first mandatory element.
	 * @param elements - the rest of the elements of the path.
	 * @return the path.
	 */
	public static String path(String element1, String... elements) {
		StringBuilder b = new StringBuilder();
		if (element1 != null && !element1.isEmpty()) {
			b.append(element1);
		}
		if (elements != null) {
			for (String element : elements) {
				if (element != null && !element.isEmpty()) {
					if (b.length() > 0) {
						b.append(File.separator);
					}
					b.append(element);
				}
			}
		}
		return b.toString();
	}

	/** Ensure that the given object is matching a predicate.
	 *
	 * @param <T> - type of the object.
	 * @param obj - the object to test.
	 * @param func - the predicate.
	 * @return obj
	 */
	public static <T> T mustBe(T obj, Functions.Function1<T, Boolean> func) {
		assertTrue(func.apply(obj));
		return obj;
	}

	private static boolean isArray(Object obj) {
		if (obj == null) {
			return false;
		}
		return obj.getClass().isArray();
	}

	/** Ensure that the given object is equal to another object.
	 *
	 * @param <T> - type of the object.
	 * @param actual - the object to test.
	 * @param expected - the expected object.
	 * @return actual
	 */
	public static <T> T mustBe(T actual, T expected) {
		if (isArray(actual) && isArray(expected)) {
			assertArrayEquals("not equal", (Object[]) expected, (Object[]) actual); //$NON-NLS-1$
		} else {
			assertEquals("not equal", expected, actual); //$NON-NLS-1$
		}
		return actual;
	}

	/** Ensure that the given type is equals another type.
	 *
	 * @param <T> - expected of the object.
	 * @param actual - the type to test.
	 * @param expectedType - the expected type.
	 * @return actual
	 */
	public static <T> Class<T> mustBe(Class<T> actual, Class<?> expectedType) {
		assertEquals("not equal", expectedType, actual); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given object is of the given type.
	 *
	 * @param <T> - expected type of the object.
	 * @param actual - the object to test.
	 * @param expectedType - the type.
	 * @return actual
	 */
	public static <T> T mustBe(Object actual, Class<T> expectedType) {
		String msg = "not equal, expected: " + expectedType.getName() + ", actual: "; //$NON-NLS-1$ //$NON-NLS-2$
		if (actual != null) {
			msg += actual.getClass().getName();
		} else {
			msg += actual;
		}
		assertTrue(msg, expectedType.isInstance(actual));
		return expectedType.cast(actual);
	}

	/** Ensure that the given object is matching the given predicate.
	 *
	 * @param <T> - type of the object.
	 * @param actual - the object to test.
	 * @param matcher - the predicate.
	 * @return actual
	 */
	public static <T> T mustBe(T actual, Matcher<? super T> matcher) {
		if (matcher == null) {
			assertNull("not equal", actual); //$NON-NLS-1$
		} else {
			assertTrue("not equal", matcher.matches(actual)); //$NON-NLS-1$
		}
		return actual;
	}

	/** Ensure that the given iterable object contains an element equals to the
	 * given value.
	 *
	 * @param <T> - type of the elements in the collection.
	 * @param <I> - type of the collection.
	 * @param actual - the collection to test.
	 * @param element - the element that must be inside the collection.
	 * @return actual
	 */
	public static <T, I extends Iterable<T>> I mustContain(I actual, T element) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		assertTrue(
				String.format("the following iterable must contain \"%s\": %s", //$NON-NLS-1$
						element, actual),
				contains(actual, element));
		return actual;
	}

	/** Ensure that the given iterable object contains an element that is
	 * matching the given predicate.
	 *
	 * @param <T> - type of the elements in the collection.
	 * @param <I> - type of the collection.
	 * @param collection - the collection to test.
	 * @param matcher - the predicate.
	 * @return collection
	 */
	public static <T, I extends Iterable<T>> I mustContain(I collection, Matcher<? super T> matcher) {
		assertNotNull("collection cannot be null", collection); //$NON-NLS-1$
		assertNotNull("matcher cannot be null", matcher); //$NON-NLS-1$
		for (T item : collection) {
			if (matcher.matches(item)) {
				return collection;
			}
		}
		throw new AssertionFailedError("the collection does not contains an element matching the given critera"); //$NON-NLS-1$
	}

	/** Ensure that the given array contains an element equals to the
	 * given value.
	 *
	 * @param <T> - type of the elements in the collection.
	 * @param actual - the collection to test.
	 * @param element - the element that must be inside the collection.
	 * @return actual
	 */
	public static <T> T[] mustContain(T[] actual, T element) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		assertTrue(Arrays.contains(actual, element));
		return actual;
	}

	/** Ensure that the given array contains an element equals to the
	 * given value.
	 *
	 * @param actual - the collection to test.
	 * @param element - the element that must be inside the collection.
	 * @return actual
	 */
	public static boolean[] mustContain(boolean[] actual, boolean element) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		for (boolean candidate : actual) {
			if (candidate == element) {
				return actual;
			}
		}
		fail("The element was not found in the array"); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given array contains an element equals to the
	 * given value.
	 *
	 * @param actual - the collection to test.
	 * @param element - the element that must be inside the collection.
	 * @return actual
	 */
	public static char[] mustContain(char[] actual, char element) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		for (char candidate : actual) {
			if (candidate == element) {
				return actual;
			}
		}
		fail("The element was not found in the array"); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given array contains an element equals to the
	 * given value.
	 *
	 * @param actual - the collection to test.
	 * @param element - the element that must be inside the collection.
	 * @return actual
	 */
	public static byte[] mustContain(byte[] actual, byte element) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		for (byte candidate : actual) {
			if (candidate == element) {
				return actual;
			}
		}
		fail("The element was not found in the array"); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given array contains an element equals to the
	 * given value.
	 *
	 * @param actual - the collection to test.
	 * @param element - the element that must be inside the collection.
	 * @return actual
	 */
	public static short[] mustContain(short[] actual, short element) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		for (short candidate : actual) {
			if (candidate == element) {
				return actual;
			}
		}
		fail("The element was not found in the array"); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given array contains an element equals to the
	 * given value.
	 *
	 * @param actual - the collection to test.
	 * @param element - the element that must be inside the collection.
	 * @return actual
	 */
	public static int[] mustContain(int[] actual, int element) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		for (int candidate : actual) {
			if (candidate == element) {
				return actual;
			}
		}
		fail("The element was not found in the array"); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given array contains an element equals to the
	 * given value.
	 *
	 * @param actual - the collection to test.
	 * @param element - the element that must be inside the collection.
	 * @return actual
	 */
	public static long[] mustContain(long[] actual, long element) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		for (long candidate : actual) {
			if (candidate == element) {
				return actual;
			}
		}
		fail("The element was not found in the array"); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given array contains an element equals to the
	 * given value.
	 *
	 * @param actual - the collection to test.
	 * @param element - the element that must be inside the collection.
	 * @return actual
	 */
	public static float[] mustContain(float[] actual, float element) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		for (float candidate : actual) {
			if (candidate == element) {
				return actual;
			}
		}
		fail("The element was not found in the array"); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given array contains an element equals to the
	 * given value.
	 *
	 * @param actual - the collection to test.
	 * @param element - the element that must be inside the collection.
	 * @return actual
	 */
	public static double[] mustContain(double[] actual, double element) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		for (double candidate : actual) {
			if (candidate == element) {
				return actual;
			}
		}
		fail("The element was not found in the array"); //$NON-NLS-1$
		return actual;
	}

	/** Ensure that the given string contains a substring.
	 *
	 * @param <T> - type of the string.
	 * @param actual - the string to test.
	 * @param substring - the expected substring.
	 * @return actual
	 */
	public static <T extends CharSequence> T mustContain(T actual, CharSequence substring) {
		assertNotNull("actual cannot be null", actual); //$NON-NLS-1$
		assertTrue(
				String.format("\"%s\" must contain \"%s\".", //$NON-NLS-1$
					actual.toString(), substring),
				actual.toString().contains(substring));
		return actual;
	}

	/** Ensure that the given object is a boolean object equals to the given value.
	 *
	 * @param <T> - type of the object.
	 * @param actual - the object to test.
	 * @param result - the expected value.
	 * @return actual
	 */
	public static <T> T mustBe(T actual, boolean result) {
		if (actual instanceof Boolean) {
			assertEquals(Boolean.valueOf(result), actual);
		} else {
			fail("actual must be a boolean"); //$NON-NLS-1$
		}
		return actual;
	}

	/** Ensure that the given string starts with the given substring.
	 *
	 * @param <T> - type of the string.
	 * @param s - the string to test.
	 * @param substring - the string to be at the beginning.
	 * @return s
	 */
	public static <T extends CharSequence> T mustStartWith(T s, String substring) {
		assertNotNull("s cannot be null", s); //$NON-NLS-1$
		assertTrue(
				String.format("\"%s\" must start with \"%s\".", //$NON-NLS-1$
					s.toString(), substring),
				s.toString().startsWith(substring));
		return s;
	}

	/** Ensure that the given string ends with the given substring.
	 *
	 * @param <T> - type of the string.
	 * @param s - the string to test.
	 * @param substring - the string to be at the end.
	 * @return s
	 */
	public static <T extends CharSequence> T mustEndWith(T s, String substring) {
		assertNotNull("s cannot be null", s); //$NON-NLS-1$
		assertTrue(
				String.format("\"%s\" must end with \"%s\".", //$NON-NLS-1$
					s.toString(), substring),
				s.toString().endsWith(substring));
		return s;
	}

	/** Ensure that the given string does not start with the given substring.
	 *
	 * @param <T> - type of the string.
	 * @param s - the string to test.
	 * @param substring - the string to be at the beginning.
	 * @return s
	 */
	public static <T extends CharSequence> T mustNotStartWith(T s, String substring) {
		assertNotNull("s cannot be null", s); //$NON-NLS-1$
		assertFalse(
				String.format("\"%s\" must not start with \"%s\".", //$NON-NLS-1$
					s.toString(), substring),
				s.toString().startsWith(substring));
		return s;
	}

	/** Ensure that the given string does not end with the given substring.
	 *
	 * @param <T> - type of the string.
	 * @param s - the string to test.
	 * @param substring - the string to be at the end.
	 * @return s
	 */
	public static <T extends CharSequence> T mustNotEndWith(T s, String substring) {
		assertNotNull("s cannot be null", s); //$NON-NLS-1$
		assertFalse(
				String.format("\"%s\" must not end with \"%s\".", //$NON-NLS-1$
					s.toString(), substring),
				s.toString().endsWith(substring));
		return s;
	}

	/** Ensure that the given caller specification has a valid link
	 * to another Jnario specification with the given name.
	 *
	 * @param referencedLink - URL
	 * @return refencedLink
	 */
	public static String mustBeJnarioLink(String referencedLink) {
		Class<?> callingSpecification = Caller.getCallerClass();
		assertNotNull("referencedLink cannot be null", referencedLink); //$NON-NLS-1$
		assertNotNull("callingSpecification cannot be null", callingSpecification); //$NON-NLS-1$
		//
		String ref = referencedLink;
		if (ref.contains(".html#")) { //$NON-NLS-1$
			String[] parts = ref.split(java.util.regex.Matcher.quoteReplacement(".html#")); //$NON-NLS-1$
			assertEquals(
					String.format("Invalid link format: %s", ref), //$NON-NLS-1$
					2, parts.length);
			String[] sections = parts[1].split(java.util.regex.Matcher.quoteReplacement("_") + "+"); //$NON-NLS-1$ //$NON-NLS-2$
			StringBuilder b = new StringBuilder();
			for (String s : sections) {
				if (s.length() > 1) {
					b.append(s.substring(0, 1).toUpperCase() + s.substring(1));
				} else {
					b.append(s.toUpperCase());
				}
			}
			if (parts[0].endsWith("Spec")) { //$NON-NLS-1$
				ref = parts[0].substring(0, parts[0].length() - 4)
						+ b.toString() + "Spec.html"; //$NON-NLS-1$
			} else {
				ref = parts[0] + b.toString() + "Spec.html"; //$NON-NLS-1$
			}
		}
		//
		assertTrue(
					String.format("\"%s\" must end with \".html\".", //$NON-NLS-1$
							ref.toString(), ".html"), //$NON-NLS-1$
					ref.endsWith(".html")); //$NON-NLS-1$
		//
		ref = ref.substring(0, ref.length() - 5);
		File caller = new File(callingSpecification.getName().replaceAll(
				"\\.", File.separator)).getParentFile(); //$NON-NLS-1$
		File resolved = new File(caller, ref.replaceAll("\\/", File.separator)); //$NON-NLS-1$
		String resolvedPath = Files.simplifyPath(resolved.getPath());
		resolvedPath = resolvedPath.replaceAll(java.util.regex.Matcher.quoteReplacement(File.separator), "."); //$NON-NLS-1$
		try {
			Class.forName(resolvedPath);
		} catch (Throwable _) {
			fail(String.format("The link \"%s\" is not linked to a Jnario specification", referencedLink)); //$NON-NLS-1$
		}
		return referencedLink;
	}

	/** Ensure that the given string is a valid hyper-link.
	 * The link must start with "http://" and not end with "/".
	 *
	 * @param referencedLink - the string to test.
	 * @return refencedLink
	 */
	public static String mustBeHttpLink(String referencedLink) {
		return mustNotEndWith(mustStartWith(referencedLink, "http://"), "/"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Ensure that the given string is a MAven snapshot version.
	 *
	 * @param version - the version string.
	 * @return version
	 */
	public static String mustBeSnapshotVersion(String version) {
		return mustEndWith(version, "-SNAPSHOT"); //$NON-NLS-1$
	}

	/** Ensure that the given string is not a MAven snapshot version.
	 *
	 * @param version - the version string.
	 * @return version
	 */
	public static String mustNotBeSnapshotVersion(String version) {
		return mustNotEndWith(version, "-SNAPSHOT"); //$NON-NLS-1$
	}

	/** Ensure that the given caller specification has a valid link
	 * to a picture with the given name.
	 *
	 * @param referencedLink - URL
	 * @return refencedLink
	 */
	public static String mustBePicture(String referencedLink) {
		Class<?> callingSpecification = Caller.getCallerClass();
		assertNotNull("referencedLink cannot be null", referencedLink); //$NON-NLS-1$
		assertNotNull("callingSpecification cannot be null", callingSpecification); //$NON-NLS-1$
		// Check if it is a URL of a file path
		URL fileURL = FileSystem.convertStringToURL(referencedLink, true, true);
		if (fileURL == null) {
			fail(String.format("The picture '%s' was nout found.", referencedLink)); //$NON-NLS-1$
		} else {
			if (URISchemeType.FILE.isURL(fileURL)) {
				// Get local resource
				URL u = Resources.getResource(callingSpecification, referencedLink);
				assertNotNull(String.format("The picture '%s' was nout found.", referencedLink), u); //$NON-NLS-1$
			}
		}
		//
		return referencedLink;
	}

	/** Ensure that the given string is a string representation of an integer,
	 * without sign symbols.
	 *
	 * @param str - the string.
	 * @return str
	 */
	public static String mustBeInteger(String str) {
		assertTrue(
				String.format("The string \"%s\" is not an integer number.", str), //$NON-NLS-1$
				Pattern.matches("^[0-9]+$", str)); //$NON-NLS-1$
		return str;
	}

	/** Ensure that the given string is a string representation of a date.
	 * A date has the format <code>yyyy-[m]m-[d]d</code>
	 *
	 * @param str - the string.
	 * @return str
	 */
	public static String mustBeDate(String str) {
		Date d = null;
		try {
			d = Date.valueOf(str);
		} catch (Throwable _) {
			d = null;
		}
		if (d == null) {
			fail(String.format("The date \"%s\" has an invalid format.", str)); //$NON-NLS-1$
		}
		return str;
	}

}
