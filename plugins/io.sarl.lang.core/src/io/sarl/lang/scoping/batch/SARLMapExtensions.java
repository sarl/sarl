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

package io.sarl.lang.scoping.batch;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentMap;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Pure;

import com.google.common.annotations.GwtCompatible;
import com.google.common.collect.Maps;


/** This is an extension library for {@link Map maps} in the SARL language.
 * <p>
 * This extension is provided for the elements that are not (yet) provided
 * by the Xbase API.
 * The enhancement of Xbase is asked in the issue
 * {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=454319"}.
 *
 * FIXME: Remove if Xbase or Xtend are providing these functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@GwtCompatible
public final class SARLMapExtensions {

	private SARLMapExtensions() {
		//
	}

	@Pure
	private static <K, V> Map<K, V> cloneMap(Map<K, V> oldMap) {
		if (oldMap instanceof TreeMap<?, ?>) {
			return Maps.newTreeMap((TreeMap<K, V>) oldMap);
		}
		if (oldMap instanceof LinkedHashMap<?, ?>) {
			return Maps.newLinkedHashMap(oldMap);
		}
		if (oldMap instanceof ConcurrentMap<?, ?>) {
			Map<K, V> m = Maps.newConcurrentMap();
			m.putAll(oldMap);
			return m;
		}
		return Maps.newHashMap(oldMap);
	}

	/** Add the given pair into the map.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param entry - the entry (key, value) to add into the map.
	 * @return the value previously associated to the key, or <code>null</code>
	 * if the key was not present in the map before the addition.
	 */
	@Inline(value = "$1.put($2.getKey(), $2.getValue())", statementExpression = true)
	public static <K, V> V operator_add(Map<K, V> map, Pair<? extends K, ? extends V> entry) {
		return map.put(entry.getKey(), entry.getValue());
	}

	/** Add the given entries of the input map into the output map.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param outputMap - the map to update.
	 * @param inputMap - the entries to add.
	 * @return the outputMap
	 */
	public static <K, V> Map<K, V> operator_add(Map<K, V> outputMap, Map<? extends K, ? extends V> inputMap) {
		outputMap.putAll(inputMap);
		return outputMap;
	}

	/** Add the given pair to a given map for obtaining a new map.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to consider.
	 * @param entry - the entry (key, value) to add into the map.
	 * @return a map with the content of the map and with the given entry.
	 */
	@Pure
	public static <K, V> Map<K, V> operator_plus(Map<K, V> map, Pair<? extends K, ? extends V> entry) {
		Map<K, V> newMap = cloneMap(map);
		newMap.put(entry.getKey(), entry.getValue());
		return newMap;
	}

	/** Merge the given maps.
	 * If a key exists in the two maps, the value in the second map is retained.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the first map.
	 * @param map2 - the second map.
	 * @return a map with the merged contents from the two maps.
	 */
	@Pure
	public static <K, V> Map<K, V> operator_plus(Map<K, V> map, Map<? extends K, ? extends V> map2) {
		Map<K, V> newMap = cloneMap(map);
		newMap.putAll(map2);
		return newMap;
	}

	/** Remove a key from the given map.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param key - the key to remove.
	 * @return the removed value, or <code>null</code> if the key was not
	 * present in the map.
	 */
	@Inline(value = "$1.remove($2)", statementExpression = true)
	public static <K, V> V operator_remove(Map<K, V> map, K key) {
		return map.remove(key);
	}

	/** Create a copy of the given map without the given key.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param key - the key to remove.
	 * @return the removed value, or <code>null</code> if the key was not
	 * present in the map.
	 */
	@Pure
	public static <K, V> Map<K, V> operator_minus(Map<K, V> map, K key) {
		Map<K, V> newMap = cloneMap(map);
		newMap.remove(key);
		return newMap;
	}

}
