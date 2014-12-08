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

import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Pure;

import com.google.common.annotations.GwtCompatible;
import com.google.common.base.Objects;
import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;


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

	/** Add the given pair into the map.
	 *
	 * If the pair key already exists in the map, its value is replaced
	 * by the value in the pair, and the old value in the map is returned.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param entry - the entry (key, value) to add into the map.
	 * @return the value previously associated to the key, or <code>null</code>
	 * if the key was not present in the map before the addition.
	 * @since 2.8
	 */
	@Inline(value = "$1.put($2.getKey(), $2.getValue())", statementExpression=true)
	public static <K, V> V operator_add(Map<K, V> map, Pair<? extends K, ? extends V> entry) {
		return map.put(entry.getKey(), entry.getValue());
	}

	/** Add the given entries of the input map into the output map.
	 *
	 * If a key in the inputMap already exists in the outputMap, its value is
	 * replaced in the outputMap by the value from the inputMap.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param outputMap - the map to update.
	 * @param inputMap - the entries to add.
	 * @return the outputMap
	 * @since 2.8
	 */
	public static <K, V> Map<K, V> operator_add(Map<K, V> outputMap, Map<? extends K, ? extends V> inputMap) {
		outputMap.putAll(inputMap);
		return outputMap;
	}

	/** Add the given pair to a given map for obtaining a new map.
	 *
	 * If the pair key already exists in the map, the exception
	 * {@link IllegalArgumentException} is thrown.
	 * 
	 * The replied map is a view on the given map. It means that any change
	 * in the original map is reflected to the result of this operation.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param left - the map to consider.
	 * @param right - the entry (key, value) to add into the map.
	 * @return an immutable map with the content of the map and with the given entry.
	 * @since 2.8
	 * @throws IllegalArgumentException - when the right operand key exists in the left operand.
	 */
	@Pure
	@Inline(value = "(ImmutableMap.<K,V>builder().putAll(left).put(right.getKey(), right.getValue()).build())",
			imported=ImmutableMap.class)
	public static <K, V> Map<K, V> operator_plus(Map<K, V> left, Pair<? extends K, ? extends V> right) {
		return ImmutableMap.<K,V>builder().putAll(left).put(right.getKey(), right.getValue()).build();
	}

	/** Merge the given maps.
	 *
	 * Builder don't accept duplicate keys (throws {@link IllegalArgumentException} if duplicate keys were added).
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param left - the left map.
	 * @param right - the right map.
	 * @return a map with the merged contents from the two maps.
	 * @since 2.8
	 * @throws IllegalArgumentException - when a right operand key exists in the left operand.
	 */
	@Pure
	@Inline(value = "(ImmutableMap.<K,V>builder().putAll(left).putAll(right).build())", imported=ImmutableMap.class)
	public static <K, V> Map<K, V> operator_plus(Map<K, V> left, Map<? extends K, ? extends V> right) {
		return ImmutableMap.<K,V>builder().putAll(left).putAll(right).build();
	}

	/** Remove a key from the given map.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param key - the key to remove.
	 * @return the removed value, or <code>null</code> if the key was not
	 * present in the map.
	 * @since 2.8
	 */
	@Inline(value="$1.remove($2)", statementExpression=true)
	public static <K, V> V operator_remove(Map<K, V> map, K key) {
		return map.remove(key);
	}

	/** Remove pairs from the given map.
	 *
	 * The pair is removed only if the key and the value are associated in the map.
	 * If the key exists in the map, but it is not associated with the given value,
	 * then the map stays unchanged.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param pairsToRemove - the pairs to remove.
	 * @return the map.
	 * @since 2.8
	 */
	public static <K, V> Map<K, V> operator_remove(Map<K, V> map, Map<? super K, ? super V> pairsToRemove) {
		for (Entry<? super K, ? super V> pair : pairsToRemove.entrySet()) {
			V value = map.get(pair.getKey());
			if (Objects.equal(value, pair.getValue())) {
				map.remove(pair.getKey());
			}
		}
		return map;
	}

	/** Remove pairs with the given keys from the map.
	 *
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param keysToRemove - the keys of the pairs to remove.
	 * @return the map.
	 * @since 2.8
	 */
	public static <K, V> Map<K, V> operator_remove(Map<K, V> map, Iterable<? super K> keysToRemove) {
		for (Object key : keysToRemove) {
			map.remove(key);
		}
		return map;
	}

	/** Replies the elements of the given map except the pair with the given key.
	 *
	 * The replied map is a view on the given map. It means that any change
	 * in the original map is reflected to the result of this operation.
	 * 
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param key - the key to remove.
	 * @return the map with the content of the map except the key.
	 * @since 2.8
	 */
	@Pure
	public static <K, V> Map<K, V> operator_minus(Map<K, V> map, final K key) {
		return Maps.filterKeys(map, new Predicate<K>() {
			@Override
			public boolean apply(K input) {
				return !Objects.equal(input, key);
			}
		});
	}
	
	/** Replies the elements of the left map without the pairs in the right map.
	 *
	 * The difference is an immutable
	 * snapshot of the state of the maps at the time this method is called. It
	 * will never change, even if the maps change at a later time.
	 *
	 * Since this method uses {@code HashMap} instances internally, the keys of
	 * the supplied maps must be well-behaved with respect to
	 * {@link Object#equals} and {@link Object#hashCode}.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param left - the map to update.
	 * @param right - the pairs to remove.
	 * @return the map with the content of the left map except the pairs of the right map.
	 * @since 2.8
	 */
	@Pure
	@Inline(value = "(Maps.difference($1, $2).entriesOnlyOnLeft())", imported=Maps.class)
	public static <K, V> Map<K, V> operator_minus(Map<K, V> left, Map<? extends K, ? extends V> right) {
		return Maps.difference(left, right).entriesOnlyOnLeft();
	}

	/** Replies the elements of the given map except the pairs with the given keys.
	 *
	 * The replied map is a view on the given map. It means that any change
	 * in the original map is reflected to the result of this operation.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param keys - the keys of the pairs to remove.
	 * @return the map with the content of the map except the pairs.
	 * @since 2.8
	 */
	@Pure
	public static <K, V> Map<K, V> operator_minus(Map<K, V> map, final Iterable<? super K> keys) {
		return Maps.filterKeys(map, new Predicate<K>() {
			private Set<Object> keySet = Sets.newHashSet(keys);
			@Override
			public boolean apply(K input) {
				return !this.keySet.contains(input);
			}
		});
	}
	
	/** Replies the value associated to the given key in the map.
	 *
	 * @param map - the map to consider.
	 * @param key - the key of the value.
	 * @return the value associated to the key, or <code>null</code> if
	 * the key was not found.
	 */
	@Pure
	@Inline(value="($1.get($2))")
	public static <K, V> V operator_mappedTo(Map<K, V> map, K key) {
		return map.get(key);
	}

}
