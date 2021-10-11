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

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;

import com.google.inject.ImplementedBy;
import org.eclipse.xtext.xbase.lib.Functions.Function3;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.typesystem.IPureOperationNameValidator;

/** Extended Functions for writing facts within the documentation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings({"checkstyle:methodname"})
public final class FactExtensions {

	private static IPureOperationNameValidator nameValidator;

	private FactExtensions() {
		//
	}

	private static IPureOperationNameValidator getOperationNameValidator() {
		synchronized (FactExtensions.class) {
			if (nameValidator == null) {
				ImplementedBy anno = IPureOperationNameValidator.class.getAnnotation(ImplementedBy.class);
				assert anno != null;
				try {
					nameValidator = (IPureOperationNameValidator) anno.value().getDeclaredConstructor().newInstance();
				} catch (InstantiationException | IllegalAccessException | IllegalArgumentException
						| InvocationTargetException | NoSuchMethodException | SecurityException e) {
					throw new Error(e);
				}
			}
			return nameValidator;
		}
	}

	/** Replies the first declared field matching with the given marcher.
	 *
	 * @param type the type to check.
	 * @param matcher the matcher.
	 * @return the validation status.
	 */
	public static Field findField(Class<?> type, Function3<Class<?>, Field, Boolean, Boolean> matcher) {
		if (matcher != null) {
			for (final Field field : type.getDeclaredFields()) {
				final Deprecated deprecatedAnnotation = field.getAnnotation(Deprecated.class);
				final boolean deprecated = deprecatedAnnotation != null;
				if (matcher.apply(type, field, deprecated)) {
					return field;
				}
			}
		}
		return null;
	}

	/** Success if each of the declared fields is matching with the given marcher.
	 *
	 * @param type the type to check.
	 * @param matcher the matcher.
	 * @return the validation status.
	 */
	public static boolean allFields(Class<?> type, Function3<Class<?>, Field, Boolean, Boolean> matcher) {
		if (matcher != null) {
			for (final Field field : type.getDeclaredFields()) {
				final Deprecated deprecatedAnnotation = field.getAnnotation(Deprecated.class);
				final boolean deprecated = deprecatedAnnotation != null;
				if (!matcher.apply(type, field, deprecated)) {
					return false;
				}
			}
		}
		return true;
	}

	/** Replies the two iterable objects are equal.
	 *
	 * @param iter1 the first object.
	 * @param iter2 the second object.
	 * @return {@code true} if the arguments are equal.
	 */
	public static boolean similarTo(Iterable<?> iter1, Iterable<?> iter2) {
		Iterator<?> iterator1 = iter1.iterator();
		Iterator<?> iterator2 = iter2.iterator();
		while (iterator1.hasNext() && iterator2.hasNext()) {
			final Object obj1 = iterator1.next();
			final Object obj2 = iterator2.next();
			if (!Objects.equals(obj1, obj2)) {
				return false;
			}
		}
		return iterator1.hasNext() == iterator2.hasNext();
	}

	/** Replies the two maps are equal.
	 *
	 * @param map1 the first map.
	 * @param map2 the second map.
	 * @return {@code true} if the arguments are equal.
	 */
	public static boolean similarTo(Map<?, ?> map1, Map<?, ?> map2) {
		final Map<?, ?> m1 = new HashMap<>(map1);
		final Map<?, ?> m2 = new HashMap<>(map2);
		final Iterator<?> iterator1 = m1.entrySet().iterator();
		while (iterator1.hasNext()) {
			final Entry<?,?> entry = (Entry<?,?>) iterator1.next();
			iterator1.remove();
			if (!m2.containsKey(entry.getKey())) {
				return false;
			}
			final Object v = m2.remove(entry.getKey());
			if (!Objects.equals(entry.getValue(), v)) {
				return false;
			}
		}
		return m1.isEmpty() == m2.isEmpty();
	}

	/** Replies if the given function is marked as pure or its names is considered as pure.
	 * This function does not test the purity of the associated code.
	 *
	 * @param type the type in which the function is declared.
	 * @param name the name of the function.
	 * @param parameters the type of the arguments.
	 * @return {@code true} if the function is pure.
	 * @throws SecurityException if the function declaration cannot be accessed.
	 * @throws NoSuchMethodException if the function cannot be found.
	 * @since 0.12
	 */
	public static boolean isPureFunctionPrototype(Class<?> type, String name, Class<?>... parameters) {
		Method method;
		try {
			method = type.getDeclaredMethod(name, parameters);
		} catch (NoSuchMethodException | SecurityException e) {
			return false;
		}
		if (method.getAnnotation(Pure.class) != null) {
			return true;
		}
		if (getOperationNameValidator().isNamePatternForNotPureOperation(method.getName())) {
			return false;
		}
		return getOperationNameValidator().isNamePatternForPureOperation(method.getName());
	}

}
