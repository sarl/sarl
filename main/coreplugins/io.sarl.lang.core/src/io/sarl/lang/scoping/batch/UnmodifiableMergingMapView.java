/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterators;

/** Map implementation that is merging two maps.
 *
 * <p>If a key exists within the two merged maps, then the retained value is the one of the right map.
 *
 * <p>FIXME: Xtext upgrade, No proposal within Xtext. https://github.com/eclipse/xtext-lib/pull/63.
 *
 * @param <K> the type of the keys.
 * @param <V> the type of the values in the maps.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class UnmodifiableMergingMapView<K, V> extends AbstractMap<K, V> {

	private final Map<? extends K, ? extends V> left;

	private final Map<? extends K, ? extends V> right;

	/** Construct the wrapping map.
	 *
	 * @param left the left operand to merge.
	 * @param right the right operand to merge.
	 */
	public UnmodifiableMergingMapView(Map<? extends K, ? extends V> left, Map<? extends K, ? extends V> right) {
		assert left != null : "left must not be null"; //$NON-NLS-1$
		assert right != null : "right must not be null"; //$NON-NLS-1$
		this.left = left;
		this.right = right;
	}

	@Override
	public Set<Entry<K, V>> entrySet() {
		// A call to "Sets.union(ks1, ks2)" does not work because of the equals() definition on Map.Entry.
		// This equality test breaks the unicity of the keys over the resulting Set.
		// In other words, "Sets.union(ks1, ks2)" replies all the entries that
		// are different on their keys or values.

		final Set<Entry<K, V>> diff =  difference(this.left, this.right);
		return new AbstractEarlyFailingSet<Entry<K, V>>() {
			@SuppressWarnings({ "unchecked", "rawtypes", "synthetic-access" })
			@Override
			public Iterator<Entry<K, V>> iterator() {
				return Iterators.unmodifiableIterator((Iterator) Iterators.concat(
						UnmodifiableMergingMapView.this.right.entrySet().iterator(), diff.iterator()));
			}

			@Override
			public int size() {
				return Iterators.size(iterator());
			}
		};
	}

	private static <K, V> Set<Entry<K, V>> difference(final Map<? extends K, ? extends V> left, final Map<? extends K, ? extends V> right) {
		final Predicate<Entry<? extends K, ? extends V>> notInSet = new Predicate<Map.Entry<? extends K, ? extends V>>() {
			@Override
			public boolean apply(Entry<? extends K, ? extends V> it) {
				if (it == null) {
					return false;
				}
				return !right.containsKey(it.getKey());
			}
		};

		return new AbstractEarlyFailingSet<Entry<K, V>>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public Iterator<Entry<K, V>> iterator() {
				return Iterators.unmodifiableIterator((Iterator) Iterators.filter(left.entrySet().iterator(), notInSet));
			}

			@Override
			public int size() {
				return Iterators.size(iterator());
			}
		};
	}

	@Override
	public void clear() {
		// Fail even if the set is empty.
		throw new UnsupportedOperationException();
	}

	@Override
	public V put(K key, V value) {
		// Fail even if the set is empty.
		throw new UnsupportedOperationException();
	}

	@Override
	public V remove(Object key) {
		// Fail even if the set is empty.
		throw new UnsupportedOperationException();
	}

	/**
	 * Abstract implements of a set that is failing as soon as possible
	 * when modifiers are called.
	 *
	 * @param <T> the type of the set elements.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private abstract static class AbstractEarlyFailingSet<T> extends AbstractSet<T> {

		AbstractEarlyFailingSet() {
			//
		}

		@Override
		public void clear() {
			// Fail even if the set is empty.
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean add(T entry) {
			// Fail even if the set is empty.
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean remove(Object element) {
			// Fail even if the set is empty.
			throw new UnsupportedOperationException();
		}

	}

}
