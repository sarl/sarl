/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.kernel.services.hazelcast;

import java.io.Serializable;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.common.base.Objects;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;
import com.google.common.collect.SetMultimap;
import com.hazelcast.core.MultiMap;
import io.janusproject.util.AbstractDMultiMapView;
import io.janusproject.util.DataViewDelegate;
import io.janusproject.util.DataViewDelegate.Delegator;
import io.janusproject.util.MultisetView;

/**
 * A view from the Hazelcast multimap to DMultiMap.
 *
 * @param <K> - type of the keys.
 * @param <V> - type of the values.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class HazelcastDMultiMapView<K, V> extends AbstractDMultiMapView<K, V> implements SetMultimap<K, V> {

	private static final long serialVersionUID = -6970650402150118406L;

	private final Multimap<K, V> map;

	/**
	 * @param map - the multimap.
	 */
	public HazelcastDMultiMapView(MultiMap<K, V> map) {
		super(map.getName());
		this.map = new Wrapper<>(map);
	}

	@Override
	public boolean isBackedCollection() {
		return false;
	}

	@Override
	public Multimap<K, V> getDelegatedObject() {
		return this.map;
	}

	@Override
	public boolean equals(Object obj) {
		return getDelegatedObject().equals(DataViewDelegate.undelegate(obj));
	}

	@Override
	public int hashCode() {
		return getDelegatedObject().hashCode();
	}

	@Override
	public String toString() {
		return getDelegatedObject().toString();
	}

	@Override
	public synchronized Set<V> removeAll(Object key) {
		return (Set<V>) super.removeAll(key);
	}

	@Override
	public Set<V> replaceValues(K key, Iterable<? extends V> values) {
		return (Set<V>) super.replaceValues(key, values);
	}

	@Override
	public Set<V> get(K key) {
		return (Set<V>) super.get(key);
	}

	@Override
	public Set<Entry<K, V>> entries() {
		return new EntryCollectionView(getDelegatedObject().entries());

	}

	/**
	 * Wrapper of a multimap.
	 *
	 * @param <K> the keys.
	 * @param <V> the values.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Wrapper<K, V> implements Multimap<K, V> {

		private final MultiMap<K, V> map;

		private Map<K, Collection<V>> mapView;

		Wrapper(MultiMap<K, V> map) {
			this.map = map;
		}

		@Override
		public boolean equals(Object obj) {
			return this.map.equals(obj);
		}

		@Override
		public int hashCode() {
			return this.map.hashCode();
		}

		@Override
		public String toString() {
			return this.map.toString();
		}

		@Override
		public int size() {
			return this.map.size();
		}

		@Override
		public boolean isEmpty() {
			return this.map.size() == 0;
		}

		@SuppressWarnings("unchecked")
		@Override
		public boolean containsKey(Object key) {
			try {
				return this.map.containsKey((K) key);
			} catch (ClassCastException exception) {
				return false;
			}
		}

		@Override
		public boolean containsValue(Object value) {
			return this.map.containsValue(value);
		}

		@SuppressWarnings("unchecked")
		@Override
		public boolean containsEntry(Object key, Object value) {
			try {
				return this.map.containsEntry((K) key, (V) value);
			} catch (ClassCastException exception) {
				return false;
			}
		}

		@Override
		public Collection<V> get(K key) {
			return this.map.get(key);
		}

		@Override
		public boolean put(K key, V value) {
			if (this.map.put(key, value)) {
				this.mapView = null;
				return true;
			}
			return false;
		}

		@Override
		public boolean putAll(K key, Iterable<? extends V> values) {
			boolean changed = false;
			for (V value : values) {
				if (this.map.put(key, value)) {
					changed = true;
				}
			}
			if (changed) {
				this.mapView = null;
			}
			return changed;
		}

		@Override
		public boolean putAll(Multimap<? extends K, ? extends V> multimap) {
			boolean changed = false;
			for (Entry<? extends K, ? extends V> entry : multimap.entries()) {
				if (this.map.put(entry.getKey(), entry.getValue())) {
					changed = true;
				}
			}
			if (changed) {
				this.mapView = null;
			}
			return changed;
		}

		@Override
		public boolean remove(Object key, Object value) {
			if (this.map.remove(key, value)) {
				this.mapView = null;
				return true;
			}
			return false;
		}

		@Override
		public Collection<V> removeAll(Object key) {
			this.mapView = null;
			return this.map.remove(key);
		}

		@Override
		public Collection<V> replaceValues(K key, Iterable<? extends V> values) {
			this.mapView = null;
			Collection<V> oldValue = this.map.remove(key);
			for (V value : values) {
				this.map.put(key, value);
			}
			return oldValue;
		}

		@Override
		public void clear() {
			this.map.clear();
		}

		@Override
		public Set<K> keySet() {
			return this.map.keySet();
		}

		@Override
		public Collection<V> values() {
			return this.map.values();
		}

		@Override
		public Collection<Entry<K, V>> entries() {
			return this.map.entrySet();
		}

		@Override
		public Multiset<K> keys() {
			return new MultisetView<>(this);
		}

		@Override
		public Map<K, Collection<V>> asMap() {
			if (this.mapView == null) {
				this.mapView = createMapView();
			}
			return this.mapView;
		}

		private Map<K, Collection<V>> createMapView() {
			Map<K, Collection<V>> map = Maps.newHashMap();
			K oldKey = null;
			Collection<V> oldValues = null;
			for (Entry<K, V> entry : this.map.entrySet()) {
				Collection<V> values;
				if (Objects.equal(entry.getKey(), oldKey)) {
					values = oldValues;
				} else {
					values = map.get(entry.getKey());
				}
				if (values == null) {
					values = Lists.newArrayList();
					map.put(entry.getKey(), values);
				}
				values.add(entry.getValue());
				oldKey = entry.getKey();
				oldValues = values;
			}
			return map;
		}

	}

	/**
	 * A view on the entries in a {@link AbstractDMultiMapView}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected class EntryCollectionView extends AbstractSet<Entry<K, V>>
			implements Serializable, Delegator<Collection<Entry<K, V>>> {

		private static final long serialVersionUID = 3746778947439539504L;

		private final Collection<Entry<K, V>> entries;

		/**
		 * @param entries - the entries in the multimap.
		 */
		public EntryCollectionView(Collection<Entry<K, V>> entries) {
			this.entries = entries;
		}

		@Override
		public Set<Entry<K, V>> getDelegatedObject() {
			return (Set<Entry<K, V>>) HazelcastDMultiMapView.this.getDelegatedObject().entries();
		}

		@Override
		public boolean equals(Object obj) {
			return getDelegatedObject().equals(DataViewDelegate.undelegate(obj));
		}

		@Override
		public int hashCode() {
			return getDelegatedObject().hashCode();
		}

		@Override
		public String toString() {
			return getDelegatedObject().toString();
		}

		@Override
		public int size() {
			return this.entries.size();
		}

		@Override
		public Iterator<Entry<K, V>> iterator() {
			return new EntryIterator(this.entries.iterator());
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public boolean add(Entry<K, V> entry) {
			if (this.entries.add(entry)) {
				fireEntryAdded(entry.getKey(), entry.getValue());
				return true;
			}
			return false;
		}

		/**
		 * Iterator on the multimap entries.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class EntryIterator implements Iterator<Entry<K, V>> {

			private final Iterator<Entry<K, V>> iterator;

			private Entry<K, V> entry;

			EntryIterator(Iterator<Entry<K, V>> iterator) {
				this.iterator = iterator;
			}

			@Override
			public boolean hasNext() {
				return this.iterator.hasNext();
			}

			@Override
			public Entry<K, V> next() {
				this.entry = this.iterator.next();
				return this.entry;
			}

			@SuppressWarnings("synthetic-access")
			@Override
			public void remove() {
				this.iterator.remove();
				fireEntryRemoved(this.entry.getKey(), this.entry.getValue());
			}
		}

	}

}
