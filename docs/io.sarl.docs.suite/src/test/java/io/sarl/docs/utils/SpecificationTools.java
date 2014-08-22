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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import junit.framework.AssertionFailedError;

import org.eclipse.xtext.util.Arrays;
import org.eclipse.xtext.xbase.lib.Functions;
import org.hamcrest.Matcher;


/** Helper for tests.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("static-method")
public class SpecificationTools {

	/** Replies a path built from the given elements.
	 * 
	 * @param element1 - first mandatory element.
	 * @param elements - the rest of the elements of the path.
	 * @return the path.
	 */
	public String path(String element1, String... elements) {
		StringBuilder b = new StringBuilder();
		if (element1 != null && !element1.isEmpty()) {
			b.append(element1);
		}
		if (elements != null) {
			for(String element : elements) {
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
	public <T> T mustBe(T obj, Functions.Function1<T, Boolean> func) {
		assertTrue(func.apply(obj));
		return obj;
	}

	private boolean isArray(Object obj) {
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
	public <T> T mustBe(T actual, T expected) {
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
	public <T> Class<T> mustBe(Class<T> actual, Class<?> expectedType) {
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
	public <T> T mustBe(Object actual, Class<T> expectedType) {
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
	public <T> T mustBe(T actual, Matcher<? super T> matcher) {
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
	public <T, I extends Iterable<T>> I mustContain(I actual, T element) {
		assertTrue(contains(actual, element));
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
	public <T, I extends Iterable<T>> I mustContain(I collection, Matcher<? super T> matcher) {
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
	public <T> T[] mustContain(T[] actual, T element) {
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
	public boolean[] mustContain(boolean[] actual, boolean element) {
		for(boolean candidate : actual) {
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
	public char[] mustContain(char[] actual, char element) {
		for(char candidate : actual) {
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
	public byte[] mustContain(byte[] actual, byte element) {
		for(byte candidate : actual) {
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
	public short[] mustContain(short[] actual, short element) {
		for(short candidate : actual) {
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
	public int[] mustContain(int[] actual, int element) {
		for(int candidate : actual) {
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
	public long[] mustContain(long[] actual, long element) {
		for(long candidate : actual) {
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
	public float[] mustContain(float[] actual, float element) {
		for(float candidate : actual) {
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
	public double[] mustContain(double[] actual, double element) {
		for(double candidate : actual) {
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
	public <T extends CharSequence> T mustContain(T actual, CharSequence substring) {
		assertTrue(actual.toString().contains(substring));
		return actual;
	}

	/** Ensure that the given object is a boolean object equals to the given value.
	 *
	 * @param <T> - type of the object.
	 * @param actual - the object to test.
	 * @param result - the expected value.
	 * @return actual
	 */
	public <T> T mustBe(T actual, boolean result) {
		if (actual instanceof Boolean) {
			assertEquals(Boolean.valueOf(result), actual);
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
	public <T extends CharSequence> T mustStartWith(T s, String substring) {
		assertTrue(s.toString().startsWith(substring));
		return s;
	}

	/** Ensure that the given string ends with the given substring.
	 *
	 * @param <T> - type of the string.
	 * @param s - the string to test.
	 * @param substring - the string to be at the end.
	 * @return s
	 */
	public <T extends CharSequence> T mustEndWith(T s, String substring) {
		assertTrue(s.toString().endsWith(substring));
		return s;
	}

}
