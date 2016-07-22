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

package io.janusproject.util;

import java.io.Serializable;
import java.util.AbstractCollection;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DMapListener;
import io.janusproject.util.DataViewDelegate.Delegator;

/**
 * A view on a standard Map that provides the API for DMap.
 *
 * @param <K> - type of the keys.
 * @param <V> - type of the values.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractDMapView<K, V> extends AbstractMapView<K, V>
implements DMap<K, V>, Serializable, Delegator<Map<K, V>> {

	private static final long serialVersionUID = -2836872682477402816L;

	private final String name;

	/**
	 * @param name - the name of the map.
	 */
	public AbstractDMapView(String name) {
		this.name = name;
	}

	@Override
	public boolean isBackedCollection() {
		return true;
	}

	@Override
	public String getName() {
		return this.name;
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
	public boolean containsKey(Object key) {
		return getDelegatedObject().containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return getDelegatedObject().containsValue(value);
	}

	@Override
	public V get(Object key) {
		return getDelegatedObject().get(key);
	}

	@Override
	public boolean isEmpty() {
		return getDelegatedObject().isEmpty();
	}

	@Override
	public int size() {
		return getDelegatedObject().size();
	}

	@Override
	public Set<Entry<K, V>> entrySet() {
		return new EntryView(getDelegatedObject().entrySet());
	}

	@Override
	public Set<K> keySet() {
		return new KeyView(getDelegatedObject().entrySet());
	}

	@Override
	public Collection<V> values() {
		return new ValueView(getDelegatedObject().entrySet());
	}

	@Override
	public void clear() {
		if (!getDelegatedObject().isEmpty()) {
			getDelegatedObject().clear();
			fireCleared(false);
		}
	}

	@Override
	public V put(K key, V value) {
		V old = getDelegatedObject().put(key, value);
		if (old != null) {
			fireEntryUpdated(key, value);
		} else {
			fireEntryAdded(key, value);
		}
		return old;
	}

	@Override
	public void putAll(Map<? extends K, ? extends V> map) {
		for (Entry<? extends K, ? extends V> entry : map.entrySet()) {
			put(entry.getKey(), entry.getValue());
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public V remove(Object key) {
		V old = getDelegatedObject().remove(key);
		if (old != null) {
			fireEntryRemoved((K) key, old);
		}
		return old;
	}

	@Override
	public V putIfAbsent(K key, V value) {
		if (!getDelegatedObject().containsKey(key)) {
			V old = getDelegatedObject().put(key, value);
			fireEntryAdded(key, value);
			return old;
		}
		return getDelegatedObject().get(key);
	}

	/**
	 * A view on a Set of entries that is able to fire the events for the {@link DMapListener listeners}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected class EntryView extends AbstractSet<Entry<K, V>> implements Serializable, Delegator<Set<Entry<K, V>>> {

		private static final long serialVersionUID = 7929741824792802767L;

		private final Set<Entry<K, V>> entries;

		/**
		 * @param data - the set.
		 */
		public EntryView(Set<Entry<K, V>> data) {
			this.entries = data;
		}

		@Override
		public Set<Entry<K, V>> getDelegatedObject() {
			return this.entries;
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
		public Iterator<Entry<K, V>> iterator() {
			return new EntryIterator(this.entries.iterator());
		}

		@Override
		public int size() {
			return this.entries.size();
		}

		/**
		 * Iterator on DMap entries.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class EntryIterator implements Iterator<Entry<K, V>> {

			private final Iterator<Entry<K, V>> iterator;

			private K key;

			private V value;

			EntryIterator(Iterator<Entry<K, V>> iterator) {
				this.iterator = iterator;
			}

			@Override
			public boolean hasNext() {
				return this.iterator.hasNext();
			}

			@Override
			public Entry<K, V> next() {
				Entry<K, V> entry = this.iterator.next();
				this.key = entry.getKey();
				this.value = entry.getValue();
				return entry;
			}

			@Override
			public void remove() {
				this.iterator.remove();
				fireEntryRemoved(this.key, this.value);
			}
		}

	}

	/**
	 * A view on a Set of keys that is able to fire the events for the {@link DMapListener listeners}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected class KeyView extends AbstractSet<K> implements Serializable, Delegator<Set<K>> {

		private static final long serialVersionUID = -1182559049456023451L;

		private final Set<Entry<K, V>> entries;

		/**
		 * @param data - the set.
		 */
		public KeyView(Set<Entry<K, V>> data) {
			this.entries = data;
		}

		@Override
		public Set<K> getDelegatedObject() {
			return AbstractDMapView.this.getDelegatedObject().keySet();
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
		public Iterator<K> iterator() {
			return new KeyIterator(this.entries.iterator());
		}

		@Override
		public int size() {
			return this.entries.size();
		}

		/**
		 * Iterator on DMap keys.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class KeyIterator implements Iterator<K> {

			private final Iterator<Entry<K, V>> iterator;

			private K key;

			private V value;

			KeyIterator(Iterator<Entry<K, V>> iterator) {
				this.iterator = iterator;
			}

			@Override
			public boolean hasNext() {
				return this.iterator.hasNext();
			}

			@Override
			public K next() {
				Entry<K, V> entry = this.iterator.next();
				this.key = entry.getKey();
				this.value = entry.getValue();
				return this.key;
			}

			@Override
			public void remove() {
				this.iterator.remove();
				fireEntryRemoved(this.key, this.value);
			}
		}

	}

	/**
	 * A view on a Collection of keys that is able to fire the events for the {@link DMapListener listeners}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected class ValueView extends AbstractCollection<V> implements Serializable, Delegator<Collection<V>> {

		private static final long serialVersionUID = -5603710938557144864L;

		private final Set<Entry<K, V>> entries;

		/**
		 * @param data - the set.
		 */
		public ValueView(Set<Entry<K, V>> data) {
			this.entries = data;
		}

		@Override
		public Collection<V> getDelegatedObject() {
			return AbstractDMapView.this.getDelegatedObject().values();
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
		public Iterator<V> iterator() {
			return new ValueIterator(this.entries.iterator());
		}

		@Override
		public int size() {
			return this.entries.size();
		}

		/**
		 * Iterator on DMap values.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class ValueIterator implements Iterator<V> {

			private final Iterator<Entry<K, V>> iterator;

			private K key;

			private V value;

			ValueIterator(Iterator<Entry<K, V>> iterator) {
				this.iterator = iterator;
			}

			@Override
			public boolean hasNext() {
				return this.iterator.hasNext();
			}

			@Override
			public V next() {
				Entry<K, V> entry = this.iterator.next();
				this.key = entry.getKey();
				this.value = entry.getValue();
				return this.value;
			}

			@Override
			public void remove() {
				this.iterator.remove();
				fireEntryRemoved(this.key, this.value);
			}
		}

	}

}
