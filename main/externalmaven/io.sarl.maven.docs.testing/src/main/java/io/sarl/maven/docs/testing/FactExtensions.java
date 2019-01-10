/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;

import org.arakhne.afc.vmutil.ClasspathUtil;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.xbase.lib.Functions.Function3;

/** Extended Functions for writting facts within the documentation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings({"checkstyle:methodname"})
public final class FactExtensions {

	private FactExtensions() {
		//
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

	/** Replies the URL of the bundle's file.
	 *
	 * @param bundleName the name of the bundle, i.e. the name of the jar file.
	 * @param filename the name of the file.
	 * @return the URL, or <code>null</code>.
	 */
	public static URL getBundlePropertyURL(String bundleName, String filename) {
		try {
			final Iterator<URL> urls = ClasspathUtil.getClasspath();
			URL url;
			while (urls.hasNext()) {
				url = urls.next();
				final String resourceName = FileSystem.basename(url);
				if (resourceName != null && resourceName.startsWith(bundleName + "-")) { //$NON-NLS-1$
					return FileSystem.toJarURL(url, filename);
				}
			}
		} catch (Throwable exception) {
			//
		}
		return null;
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

}
