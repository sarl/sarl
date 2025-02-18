/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.core.util;

import java.util.Comparator;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentMap;

/** Represent a map of objects which is not thread-safe.
 *
 * @param <K> the type of the keys in the map.
 * @param <V> the type of the values in the map.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
class NoConcurrentTreeMap<K, V> extends TreeMap<K, V> implements ConcurrentMap<K, V> {

    private static final long serialVersionUID = 8594356534650651331L;

	/**
     * Constructs a new, empty set that orders its elements according to
     * their {@linkplain Comparable natural ordering}.
     */
    NoConcurrentTreeMap() {
        super();
    }

    /**
     * Constructs a new, empty map that orders its keys according to
     * the specified comparator.
     *
     * @param comparator the comparator that will be used to order this map.
     *        If {@code null}, the {@linkplain Comparable natural
     *        ordering} of the elements will be used.
     */
    NoConcurrentTreeMap(Comparator<? super K> comparator) {
        super(comparator);
    }

    /**
     * Constructs a new map containing the elements in the specified
     * collection, that orders its elements according to their
     * {@linkplain Comparable natural ordering}.
     *
     * @param source the elements that will comprise the new map
     * @throws ClassCastException if the elements in {@code c} are
     *         not {@link Comparable}, or are not mutually comparable
     * @throws NullPointerException if the specified collection or any
     *         of its elements are null
     */
    NoConcurrentTreeMap(Map<? extends K, ? extends V> source) {
        super(source);
    }

	@Override
	public boolean remove(Object key, Object value) {
		if (containsKey(key) && Objects.equals(get(key), value)) {
			remove(key);
			return true;
		}
		return false;
	}

}
