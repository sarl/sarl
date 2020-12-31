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

package io.sarl.maven.docs.testing;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.google.common.collect.Iterables;

/** Extended Functions for writing collection-based code within the documentation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class CollectionExtensions {

	private CollectionExtensions() {
		//
	}

	/** Sort an array according to their natural order.
	 * The objects within the given array must implement the {@link Comparable} interface.
	 *
	 * @param array the array to sort.
	 * @return the sorted array (a copy of the {@code array}).
	 * @throws Exception if the array cannot be sorted.
	 */
	public static <T extends Comparable<?>> T[] sort(T[] array) throws Exception {
		if (array == null) {
			return null;
		}
		final T[] copy = Arrays.copyOf(array, array.length);
		Arrays.sort(copy);
		return copy;
	}

	/** Sort an array of classes on their fully qualified names.
	 *
	 * @param array the array to sort.
	 * @return the sorted array (a copy of the {@code array}).
	 * @throws Exception if the array cannot be sorted.
	 * @see #sortOnSimpleName(Class[])
	 */
	public static Class<?>[] sort(Class<?>[] array) throws Exception {
		if (array == null) {
			return null;
		}
		final Class<?>[] copy = Arrays.copyOf(array, array.length);
		Arrays.sort(copy, (a, b) -> {
			if (a == b) {
				return 0;
			}
			if (a == null) {
				return -1;
			}
			if (b == null) {
				return 1;
			}
			return a.getName().compareTo(b.getName());
		});
		return copy;
	}

	/** Sort an array of classes on their simple names.
	 *
	 * @param array the array to sort.
	 * @return the sorted array (a copy of the {@code array}).
	 * @throws Exception if the array cannot be sorted.
	 * @see #sort(Class[])
	 */
	public static Class<?>[] sortOnSimpleName(Class<?>[] array) throws Exception {
		if (array == null) {
			return null;
		}
		final Class<?>[] copy = Arrays.copyOf(array, array.length);
		Arrays.sort(copy, (a, b) -> {
			if (a == b) {
				return 0;
			}
			if (a == null) {
				return -1;
			}
			if (b == null) {
				return 1;
			}
			return a.getSimpleName().compareTo(b.getSimpleName());
		});
		return copy;
	}

	/** Generate an horizontal (one row) matrix of strings for the given argument.
	 *
	 * @param value the value to translate.
	 * @return the matrix of strings
	 * @throws Exception if the matrix cannot be generated.
	 */
	@SuppressWarnings("unchecked")
	public static List<List<String>> toHorizontalStringMatrix(Object value) throws Exception {
		List<List<String>> list = null;
		if (value instanceof Class<?>[]) {
			list = _toHorizontalStringMatrix((Class<?>[]) value);
		} else if (value instanceof List) {
			final List<?> input = (List<?>) value;
			if (!input.isEmpty()) {
				final Object element = Iterables.find(input, it -> it != null);
				if (element instanceof Class) {
					list = _toHorizontalStringMatrix((List<Class<?>>) input);
				}
			}
		}
		if (list == null) {
			return Collections.emptyList();
		}
		return list;
	}

	/** Generate an horizontal (one row) matrix of strings for the given argument.
	 *
	 * @param value the value to translate.
	 * @return the matrix of strings
	 * @throws Exception if the matrix cannot be generated.
	 */
	protected static List<List<String>> _toHorizontalStringMatrix(List<Class<?>> value) throws Exception {
		final List<String> list = new ArrayList<>();
		for (final Class<?> type : value) {
			list.add(type.getName());
		}
		return Collections.singletonList(list);
	}

	/** Generate an horizontal (one row) matrix of strings for the given argument.
	 *
	 * @param value the value to translate.
	 * @return the matrix of strings
	 * @throws Exception if the matrix cannot be generated.
	 */
	protected static List<List<String>> _toHorizontalStringMatrix(Class<?>[] value) throws Exception {
		return _toHorizontalStringMatrix(Arrays.asList(value));
	}

	/** Generate a vertical (one column) matrix of strings for the given argument.
	 *
	 * @param value the value to translate.
	 * @return the matrix of strings
	 * @throws Exception if the matrix cannot be generated.
	 */
	@SuppressWarnings("unchecked")
	public static List<List<String>> toVerticalStringMatrix(Object value) throws Exception {
		List<List<String>> list = null;
		if (value instanceof Class<?>[]) {
			list = _toVerticalStringMatrix((Class<?>[]) value);
		} else if (value instanceof List) {
			final List<?> input = (List<?>) value;
			if (!input.isEmpty()) {
				final Object element = Iterables.find(input, it -> it != null);
				if (element instanceof Class) {
					list = _toVerticalStringMatrix((List<Class<?>>) input);
				}
			}
		}
		if (list == null) {
			return Collections.emptyList();
		}
		return list;
	}

	/** Generate a vertical (one column) matrix of strings for the given argument.
	 *
	 * @param value the value to translate.
	 * @return the matrix of strings
	 * @throws Exception if the matrix cannot be generated.
	 */
	protected static List<List<String>> _toVerticalStringMatrix(List<Class<?>> value) throws Exception {
		final List<List<String>> list = new ArrayList<>();
		for (final Class<?> type : value) {
			list.add(Collections.singletonList(type.getName()));
		}
		return list;
	}

	/** Generate a vertical (one column) matrix of strings for the given argument.
	 *
	 * @param value the value to translate.
	 * @return the matrix of strings
	 * @throws Exception if the matrix cannot be generated.
	 */
	protected static List<List<String>> _toVerticalStringMatrix(Class<?>[] value) throws Exception {
		return _toVerticalStringMatrix(Arrays.asList(value));
	}

}
