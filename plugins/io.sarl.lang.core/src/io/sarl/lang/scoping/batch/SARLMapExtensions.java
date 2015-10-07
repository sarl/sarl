/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.GwtCompatible;
import com.google.common.base.Objects;
import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Iterators;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Pure;


/** This is an extension library for {@link Map maps} in the SARL language.
 *
 * <p>This extension is provided for the elements that are not (yet) provided
 * by the Xbase API.
 * The enhancement of Xbase is asked in <a href="https://bugs.eclipse.org/bugs/show_bug.cgi?id=454319>this issue</a>.
 *
 * <p>FIXME: Remove if Xbase or Xtend are providing these functions.
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
	 * <p>If the pair key already exists in the map, its value is replaced
	 * by the value in the pair, and the old value in the map is returned.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param entry - the entry (key, value) to add into the map.
	 * @return the value previously associated to the key, or <code>null</code>
	 *     if the key was not present in the map before the addition.
	 */
	@Inline(value = "$1.put($2.getKey(), $2.getValue())", statementExpression = true)
	public static <K, V> V operator_add(Map<K, V> map, Pair<? extends K, ? extends V> entry) {
		return map.put(entry.getKey(), entry.getValue());
	}

	/** Add the given entries of the input map into the output map.
	 *
	 * <p>If a key in the inputMap already exists in the outputMap, its value is
	 * replaced in the outputMap by the value from the inputMap.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param outputMap - the map to update.
	 * @param inputMap - the entries to add.
	 */
	@Inline(value = "$1.putAll($2)", statementExpression = true)
	public static <K, V> void operator_add(Map<K, V> outputMap, Map<? extends K, ? extends V> inputMap) {
		outputMap.putAll(inputMap);
	}

	/** Add the given pair to a given map for obtaining a new map.
	 *
	 * <p>The replied map is a view on the given map. It means that any change
	 * in the original map is reflected to the result of this operation.
	 *
	 * <p>Even if the key of the right operand exists in the left operand, the value in the right operand is preferred.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param left - the map to consider.
	 * @param right - the entry (key, value) to add into the map.
	 * @return an immutable map with the content of the map and with the given entry.
	 * @throws IllegalArgumentException - when the right operand key exists in the left operand.
	 */
	@Pure
	@Inline(value = "SARLMapExtensions.union($1, Collections.singletonMap($2.getKey(), $2.getValue()))",
			imported = { SARLMapExtensions.class, Collections.class })
	public static <K, V> Map<K, V> operator_plus(Map<K, V> left, final Pair<? extends K, ? extends V> right) {
		return union(left, Collections.singletonMap(right.getKey(), right.getValue()));
	}

	/** Merge the two maps.
	 *
	 * <p>The replied map is a view on the given map. It means that any change
	 * in the original map is reflected to the result of this operation.
	 *
	 * <p>If a key exists in the left and right operands, the value in the right operand is preferred.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param left - the left map.
	 * @param right - the right map.
	 * @return a map with the merged contents from the two maps.
	 * @throws IllegalArgumentException - when a right operand key exists in the left operand.
	 */
	@Pure
	@Inline(value = "SARLMapExtensions.union($1, $2)", imported = SARLMapExtensions.class)
	public static <K, V> Map<K, V> operator_plus(Map<K, V> left, Map<? extends K, ? extends V> right) {
		return union(left, right);
	}

	/** Remove a key from the given map.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param key - the key to remove.
	 * @return the removed value, or <code>null</code> if the key was not
	 *     present in the map.
	 * @since 2.8
	 */
	@Inline(value = "$1.remove($2)", statementExpression = true)
	public static <K, V> V operator_remove(Map<K, V> map, K key) {
		return map.remove(key);
	}

	/** Remove pairs with the given keys from the map.
	 *
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param keysToRemove - the keys of the pairs to remove.
	 */
	public static <K, V> void operator_remove(Map<K, V> map, Iterable<? super K> keysToRemove) {
		for (Object key : keysToRemove) {
			map.remove(key);
		}
	}

	/** Replies the elements of the given map except the pair with the given key.
	 *
	 * <p>The replied map is a view on the given map. It means that any change
	 * in the original map is reflected to the result of this operation.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param key - the key to remove.
	 * @return the map with the content of the map except the key.
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
	 * <p>The difference is an immutable
	 * snapshot of the state of the maps at the time this method is called. It
	 * will never change, even if the maps change at a later time.
	 *
	 * <p>Since this method uses {@code HashMap} instances internally, the keys of
	 * the supplied maps must be well-behaved with respect to
	 * {@link Object#equals} and {@link Object#hashCode}.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param left - the map to update.
	 * @param right - the pairs to remove.
	 * @return the map with the content of the left map except the pairs of the right map.
	 */
	@Pure
	@Inline(value = "SARLMapExtensions.operator_minus(left, right.keySet())", imported = SARLMapExtensions.class)
	public static <K, V> Map<K, V> operator_minus(Map<K, V> left, Map<? extends K, ? extends V> right) {
		return operator_minus(left, right.keySet());
	}

	/** Replies the elements of the given map except the pairs with the given keys.
	 *
	 * <p>The replied map is a view on the given map. It means that any change
	 * in the original map is reflected to the result of this operation.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to update.
	 * @param keys - the keys of the pairs to remove.
	 * @return the map with the content of the map except the pairs.
	 */
	@Pure
	public static <K, V> Map<K, V> operator_minus(Map<K, V> map, final Iterable<?> keys) {
		return Maps.filterKeys(map, new Predicate<K>() {
			@Override
			public boolean apply(K input) {
				return !Iterables.contains(keys, input);
			}
		});
	}

	/** Replies the value associated to the given key in the map.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param map - the map to consider.
	 * @param key - the key of the value.
	 * @return the value associated to the key, or <code>null</code> if
	 *     the key was not found.
	 */
	@Pure
	@Inline(value = "$1.get($2)")
	public static <K, V> V operator_mappedTo(Map<K, V> map, K key) {
		return map.get(key);
	}

	/** Merge the given maps.
	 *
	 * <p>The replied map is a view on the given two maps.
	 * If a key exsits in the two maps, the replied value is the value of the right operand.
	 *
	 * <p>The replied map is unmodifiable.
	 *
	 * @param <K> - type of the map keys.
	 * @param <V> - type of the map values.
	 * @param left - the left map.
	 * @param right - the right map.
	 * @return a map with the merged contents from the two maps.
	 */
	public static <K, V> Map<K, V> union(Map<? extends K, ? extends V> left, Map<? extends K, ? extends V> right) {
		return new MergingMap<>(left, right);
	}

	/** Map implementation that is merging two maps.
	 *
	 * @param <K> the type of the keys.
	 * @param <V> the type of the values in the maps.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class MergingMap<K, V> implements Map<K, V> {

		private final Map<? extends K, ? extends V> left;

		private final Map<? extends K, ? extends V> filteredLeft;

		private final Map<? extends K, ? extends V> right;

		/** Construct the wrapping map.
		 *
		 * @param left - the left operand to merge.
		 * @param right - the right operand to merge.
		 */
		MergingMap(Map<? extends K, ? extends V> left, Map<? extends K, ? extends V> right) {
			this.left = left;
			this.right = right;
			this.filteredLeft = Maps.filterKeys(this.left, new Predicate<K>() {
				@SuppressWarnings("synthetic-access")
				@Override
				public boolean apply(K input) {
					return !MergingMap.this.right.containsKey(input);
				}

			});
		}

		@Override
		public int size() {
			return this.right.size() + this.filteredLeft.size();
		}

		@Override
		public boolean isEmpty() {
			return this.right.isEmpty() && this.left.isEmpty();
		}

		@Override
		public boolean containsKey(Object key) {
			return this.right.containsKey(key) || this.left.containsKey(key);
		}

		@Override
		public boolean containsValue(Object value) {
			if (this.right.containsValue(value)) {
				return true;
			}
			return this.filteredLeft.containsValue(value);
		}

		@Override
		public V get(Object key) {
			if (this.right.containsKey(key)) {
				return this.right.get(key);
			}
			return this.left.get(key);
		}

		@Override
		public V put(K key, V value) {
			throw new UnsupportedOperationException();
		}

		@Override
		public V remove(Object key) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void putAll(Map<? extends K, ? extends V> map) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void clear() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Set<K> keySet() {
			return Sets.union(this.right.keySet(), this.filteredLeft.keySet());
		}

		@Override
		public Collection<V> values() {
			return new MergingCollection();
		}

		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public Set<Entry<K, V>> entrySet() {
			Set ks1 = this.right.entrySet();
			Set ks2 = this.filteredLeft.entrySet();
			return Sets.union(ks1, ks2);
		}

		/** Implementation of the collection of values in a map
		 * that is representing the merging of two maps.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@SuppressWarnings("synthetic-access")
		private class MergingCollection implements Collection<V> {

			MergingCollection() {
				//
			}

			@Override
			public int size() {
				return MergingMap.this.size();
			}

			@Override
			public boolean isEmpty() {
				return MergingMap.this.isEmpty();
			}

			@Override
			public boolean contains(Object object) {
				return MergingMap.this.containsValue(object);
			}

			@Override
			public Iterator<V> iterator() {
				return Iterators.concat(
						MergingMap.this.right.values().iterator(),
						MergingMap.this.filteredLeft.values().iterator());
			}

			@Override
			public Object[] toArray() {
				Object[] tab = new Object[MergingMap.this.size()];
				Iterator<?> iterator = iterator();
				for (int i = 0; i < tab.length; ++i) {
					tab[i] = iterator.next();
				}
				return tab;
			}

			@SuppressWarnings("unchecked")
			@Override
			public <T> T[] toArray(T[] output) {
				T[] tab = output;
				int size = MergingMap.this.size();
				if (tab == null || tab.length < size) {
					tab = (T[]) Array.newInstance(Object.class, size);
				}
				Iterator<?> iterator = iterator();
				for (int i = 0; i < tab.length; ++i) {
					tab[i] = (T) iterator.next();
				}
				return tab;
			}

			@Override
			public boolean containsAll(Collection<?> collection) {
				for (Object o : collection) {
					if (!MergingMap.this.containsValue(o)) {
						return false;
					}
				}
				return true;
			}

			@Override
			public boolean add(V element) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean remove(Object object) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean addAll(Collection<? extends V> collection) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean removeAll(Collection<?> collection) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean retainAll(Collection<?> collection) {
				throw new UnsupportedOperationException();
			}

			@Override
			public void clear() {
				throw new UnsupportedOperationException();
			}

		}

	}

}
