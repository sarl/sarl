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
import java.util.AbstractList;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;
import com.google.common.collect.Sets;
import io.janusproject.services.distributeddata.DMultiMap;
import io.janusproject.util.DataViewDelegate.Delegator;

/**
 * A view on a Map that provides the API for the DMultiMap.
 *
 * @param <K> - type of the keys.
 * @param <V> - type of the values.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractDMultiMapView<K, V> extends AbstractMapView<K, V>
		implements DMultiMap<K, V>, Serializable, Delegator<Multimap<K, V>> {

	private static final long serialVersionUID = -6970650402150118406L;

	private final String name;

	private transient Map<K, Collection<V>> map;

	/**
	 * @param name - the name of the map.
	 */
	public AbstractDMultiMapView(String name) {
		this.name = name;
	}

	@Override
	public boolean isBackedCollection() {
		return true;
	}

	/**
	 * Wrap the given values into a dedicated view.
	 *
	 * <p>
	 * The replies view may be a {@link SingleKeyValueListView} or a {@link SingleKeyValueSetView} according to the type of the
	 * given values' collection.
	 *
	 * @param key - the key of the values.
	 * @param values - the values.
	 * @return the wrapper.
	 */
	Collection<V> wrapValues(K key, Collection<V> values) {
		Object backEnd = DataViewDelegate.undelegate(values);
		if (backEnd instanceof List<?>) {
			return new SingleKeyValueListView(key, (List<V>) values);
		}
		if (backEnd instanceof Set<?>) {
			return new SingleKeyValueSetView(key, (Set<V>) values);
		}
		throw new IllegalStateException("Unsupported type of the backend multimap: " + values.getClass()); //$NON-NLS-1$
	}

	/**
	 * Copy the given values.
	 *
	 * <p>
	 * The replies collection may be a {@link List} or a {@link Set} according to the type of the given values' collection.
	 *
	 * @param values - the values.
	 * @return the copy.
	 */
	@SuppressWarnings({ "unchecked", "checkstyle:illegaltype" })
	Collection<V> copyValues(Collection<V> values) {
		Object backEnd = DataViewDelegate.undelegate(values);
		if (backEnd instanceof List<?>) {
			return Lists.newArrayList(values);
		}
		if (backEnd instanceof TreeSet<?>) {
			TreeSet<V> c = (TreeSet<V>) backEnd;
			c = Sets.newTreeSet(c.comparator());
			c.addAll(values);
			return c;
		}
		if (backEnd instanceof LinkedHashSet<?>) {
			return Sets.newLinkedHashSet(values);
		}
		if (backEnd instanceof Set<?>) {
			return Sets.newHashSet(values);
		}
		throw new IllegalStateException("Unsupported type of the backend multimap: " + values.getClass()); //$NON-NLS-1$
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
	public boolean containsEntry(Object key, Object value) {
		return getDelegatedObject().containsEntry(key, value);
	}

	@Override
	public int size() {
		return getDelegatedObject().size();
	}

	@Override
	public int valueCount(K key) {
		Collection<V> values = getDelegatedObject().get(key);
		if (values != null && !values.isEmpty()) {
			return values.size();
		}
		return 0;
	}

	@Override
	public boolean isEmpty() {
		return getDelegatedObject().isEmpty();
	}

	@Override
	public boolean put(K key, V value) {
		if (getDelegatedObject().put(key, value)) {
			fireEntryAdded(key, value);
			return true;
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean remove(Object key, Object value) {
		if (getDelegatedObject().remove(key, value)) {
			fireEntryRemoved((K) key, (V) value);
			return true;
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Collection<V> removeAll(Object key) {
		Collection<V> values = getDelegatedObject().removeAll(key);
		if (values != null && !values.isEmpty()) {
			K kkey = (K) key;
			for (V value : values) {
				fireEntryRemoved(kkey, value);
			}
		}
		return values;
	}

	@Override
	public boolean putAll(K key, Iterable<? extends V> values) {
		boolean changed = false;
		for (V value : values) {
			if (getDelegatedObject().put(key, value)) {
				changed = true;
				fireEntryAdded(key, value);
			}
		}
		return changed;
	}

	@Override
	public boolean putAll(Multimap<? extends K, ? extends V> multimap) {
		boolean changed = false;
		for (Entry<? extends K, ? extends V> entry : multimap.entries()) {
			if (getDelegatedObject().put(entry.getKey(), entry.getValue())) {
				changed = true;
				fireEntryAdded(entry.getKey(), entry.getValue());
			}
		}
		return changed;
	}

	@Override
	public Collection<V> replaceValues(K key, Iterable<? extends V> values) {
		return getDelegatedObject().replaceValues(key, values);
	}

	@Override
	public void clear() {
		if (!getDelegatedObject().isEmpty()) {
			getDelegatedObject().clear();
			fireCleared(false);
		}
	}

	@Override
	public Collection<V> get(K key) {
		return wrapValues(key, getDelegatedObject().get(key));
	}

	@Override
	public Set<K> keySet() {
		return new KeySetView(getDelegatedObject().keySet());
	}

	@Override
	public Collection<Entry<K, V>> entries() {
		return new EntryCollectionView(getDelegatedObject().entries());
	}

	@Override
	public Collection<V> values() {
		return new ValueCollectionView(getDelegatedObject().entries());
	}

	@Override
	public Multiset<K> keys() {
		return new MultisetView<>(this);
	}

	@Override
	public Map<K, Collection<V>> asMap() {
		if (this.map == null) {
			this.map = new MapView(getDelegatedObject().asMap());
		}
		return this.map;
	}

	/**
	 * A view on the values of specific key in a {@link AbstractDMultiMapView}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected class SingleKeyValueListView extends AbstractList<V> implements Serializable, Delegator<List<V>> {

		private static final long serialVersionUID = 4290615787745160981L;

		private final K key;

		private final List<V> values;

		/**
		 * @param key - the key of the values.
		 * @param values - the values.
		 */
		public SingleKeyValueListView(K key, List<V> values) {
			this.key = key;
			this.values = values;
		}

		@Override
		public List<V> getDelegatedObject() {
			return this.values;
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
			return this.values.size();
		}

		@Override
		public V get(int index) {
			return this.values.get(index);
		}

		@Override
		public Iterator<V> iterator() {
			return new ValueIterator(this.values.iterator());
		}

		@Override
		public boolean add(V element) {
			if (this.values.add(element)) {
				fireEntryAdded(this.key, element);
				return true;
			}
			return false;
		}

		@Override
		public void clear() {
			AbstractDMultiMapView.this.removeAll(this.key);
		}

		/**
		 * Iterator on the DMulitmap values.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class ValueIterator implements Iterator<V> {

			private final Iterator<V> iterator;

			private V value;

			ValueIterator(Iterator<V> iterator) {
				this.iterator = iterator;
			}

			@Override
			public boolean hasNext() {
				return this.iterator.hasNext();
			}

			@Override
			public V next() {
				this.value = this.iterator.next();
				return this.value;
			}

			@SuppressWarnings("synthetic-access")
			@Override
			public void remove() {
				this.iterator.remove();
				fireEntryRemoved(SingleKeyValueListView.this.key, this.value);
			}

		}

	}

	/**
	 * A view on the values of specific key in a {@link AbstractDMultiMapView}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected class SingleKeyValueSetView extends AbstractSet<V> implements Serializable, Delegator<Set<V>> {

		private static final long serialVersionUID = 4290615787745160981L;

		private final K key;

		private final Set<V> values;

		/**
		 * @param key - the key of the values.
		 * @param values - the values.
		 */
		public SingleKeyValueSetView(K key, Set<V> values) {
			this.key = key;
			this.values = values;
		}

		@Override
		public Set<V> getDelegatedObject() {
			return this.values;
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
			return this.values.size();
		}

		@Override
		public Iterator<V> iterator() {
			return new ValueIterator(this.values.iterator());
		}

		@Override
		public boolean add(V element) {
			if (this.values.add(element)) {
				fireEntryAdded(this.key, element);
				return true;
			}
			return false;
		}

		@Override
		public void clear() {
			AbstractDMultiMapView.this.removeAll(this.key);
		}

		/**
		 * Iterator on the DMultimap values.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class ValueIterator implements Iterator<V> {

			private final Iterator<V> iterator;

			private V value;

			ValueIterator(Iterator<V> iterator) {
				this.iterator = iterator;
			}

			@Override
			public boolean hasNext() {
				return this.iterator.hasNext();
			}

			@Override
			public V next() {
				this.value = this.iterator.next();
				return this.value;
			}

			@SuppressWarnings("synthetic-access")
			@Override
			public void remove() {
				this.iterator.remove();
				fireEntryRemoved(SingleKeyValueSetView.this.key, this.value);
			}

		}

	}

	/**
	 * A view on the keys in a {@link AbstractDMultiMapView}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected class KeySetView extends AbstractSet<K> implements Serializable, Delegator<Set<K>> {

		private static final long serialVersionUID = -5008029594376258921L;

		private final Set<K> keys;

		/**
		 * @param keys - the keys in the multimap.
		 */
		public KeySetView(Set<K> keys) {
			this.keys = keys;
		}

		@Override
		public Set<K> getDelegatedObject() {
			return this.keys;
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
			return this.keys.size();
		}

		@Override
		public Iterator<K> iterator() {
			return new KeyIterator(this.keys.iterator());
		}

		/**
		 * Iterator on the DMultiMap keys.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class KeyIterator implements Iterator<K> {

			private final Iterator<K> iterator;

			private K key;

			KeyIterator(Iterator<K> iterator) {
				this.iterator = iterator;
			}

			@Override
			public boolean hasNext() {
				return this.iterator.hasNext();
			}

			@Override
			public K next() {
				this.key = this.iterator.next();
				return this.key;
			}

			@SuppressWarnings("unchecked")
			@Override
			public void remove() {
				Collection<V> oldValues = get(this.key);
				V[] tab = (V[]) new Object[oldValues.size()];
				oldValues.toArray(tab);
				this.iterator.remove();
				for (V value : tab) {
					fireEntryRemoved(this.key, value);
				}
			}
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
	protected class EntryCollectionView extends AbstractCollection<Entry<K, V>>
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
		public Collection<Entry<K, V>> getDelegatedObject() {
			return AbstractDMultiMapView.this.getDelegatedObject().entries();
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

		@Override
		public boolean add(Entry<K, V> entry) {
			if (this.entries.add(entry)) {
				fireEntryAdded(entry.getKey(), entry.getValue());
				return true;
			}
			return false;
		}

		/**
		 * Iterator on DMultiMap entries.
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

			@Override
			public void remove() {
				this.iterator.remove();
				fireEntryRemoved(this.entry.getKey(), this.entry.getValue());
			}
		}

	}

	/**
	 * A view on all the values in a {@link AbstractDMultiMapView}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected class ValueCollectionView extends AbstractCollection<V> implements Serializable, Delegator<Collection<V>> {

		private static final long serialVersionUID = -1288962742301549934L;

		private final Collection<Entry<K, V>> entries;

		/**
		 * @param entries - the entries in the multimap.
		 */
		public ValueCollectionView(Collection<Entry<K, V>> entries) {
			this.entries = entries;
		}

		@Override
		public Collection<V> getDelegatedObject() {
			return AbstractDMultiMapView.this.getDelegatedObject().values();
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
		public void clear() {
			AbstractDMultiMapView.this.clear();
		}

		@Override
		public int size() {
			return this.entries.size();
		}

		@Override
		public Iterator<V> iterator() {
			return new ValueIterator(this.entries.iterator());
		}

		/**
		 * Iterator on the DMultiMap values.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class ValueIterator implements Iterator<V> {

			private final Iterator<Entry<K, V>> iterator;

			private Entry<K, V> entry;

			ValueIterator(Iterator<Entry<K, V>> iterator) {
				this.iterator = iterator;
			}

			@Override
			public boolean hasNext() {
				return this.iterator.hasNext();
			}

			@Override
			public V next() {
				this.entry = this.iterator.next();
				return this.entry.getValue();
			}

			@Override
			public void remove() {
				this.iterator.remove();
				fireEntryRemoved(this.entry.getKey(), this.entry.getValue());
			}

		}

	}

	/**
	 * Map view on a {@link AbstractDMultiMapView}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected class MapView extends AbstractMap<K, Collection<V>> implements Delegator<Map<K, Collection<V>>> {

		private final Map<K, Collection<V>> backedMap;

		/**
		 * @param backedMap - the backed map.
		 */
		public MapView(Map<K, Collection<V>> backedMap) {
			this.backedMap = backedMap;
		}

		@Override
		public Map<K, Collection<V>> getDelegatedObject() {
			return this.backedMap;
		}

		@Override
		public boolean equals(Object obj) {
			return this.backedMap.equals(obj);
		}

		@Override
		public int hashCode() {
			return this.backedMap.hashCode();
		}

		@Override
		public String toString() {
			return this.backedMap.toString();
		}

		@Override
		public Set<Entry<K, Collection<V>>> entrySet() {
			return new EntrySet(this.backedMap.entrySet());
		}

		@SuppressWarnings("unchecked")
		@Override
		public Collection<V> remove(Object key) {
			Collection<V> collection = this.backedMap.remove(key);
			if (collection == null) {
				return null;
			}
			Collection<V> output = copyValues(collection);
			collection.clear();
			for (V value : output) {
				fireEntryRemoved((K) key, value);
			}
			return output;
		}

		@SuppressWarnings("unchecked")
		@Override
		public Collection<V> get(Object key) {
			Collection<V> collection = this.backedMap.get(key);
			if (collection == null) {
				return null;
			}
			return wrapValues((K) key, collection);
		}

		@Override
		public void clear() {
			AbstractDMultiMapView.this.clear();
		}

		@Override
		public Set<K> keySet() {
			return AbstractDMultiMapView.this.keySet();
		}

		/**
		 * Set of the DMultiMap entries.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class EntrySet extends AbstractSet<Entry<K, Collection<V>>> {

			private final Set<Entry<K, Collection<V>>> backedSet;

			/**
			 * @param backedSet - the backed set.
			 */
			EntrySet(Set<Entry<K, Collection<V>>> backedSet) {
				this.backedSet = backedSet;
			}

			@Override
			public Iterator<java.util.Map.Entry<K, Collection<V>>> iterator() {
				return new EntryIterator(this.backedSet.iterator());
			}

			@Override
			public int size() {
				return this.backedSet.size();
			}

			/**
			 * Iterator on the DMultiMap entries.
			 *
			 * @author $Author: sgalland$
			 * @version $FullVersion$
			 * @mavengroupid $GroupId$
			 * @mavenartifactid $ArtifactId$
			 */
			private class EntryIterator implements Iterator<Entry<K, Collection<V>>> {

				private final Iterator<Entry<K, Collection<V>>> iterator;

				private Entry<K, Collection<V>> entry;

				/**
				 * @param iterator - the iterator.
				 */
				EntryIterator(Iterator<Entry<K, Collection<V>>> iterator) {
					this.iterator = iterator;
				}

				@Override
				public boolean hasNext() {
					return this.iterator.hasNext();
				}

				@Override
				public Entry<K, Collection<V>> next() {
					this.entry = this.iterator.next();
					Collection<V> wrappedValues = wrapValues(this.entry.getKey(), this.entry.getValue());
					return Maps.immutableEntry(this.entry.getKey(), wrappedValues);
				}

				@Override
				public void remove() {
					if (this.entry == null) {
						throw new IllegalStateException();
					}
					Collection<V> originalCollection = this.entry.getValue();
					Collection<V> oldValues = Lists.newArrayList(originalCollection);
					// The remove is clearing the backend collection
					this.iterator.remove();
					K key = this.entry.getKey();
					for (V value : oldValues) {
						fireEntryRemoved(key, value);
					}
					this.entry = null;
				}

			}

		}

	}

}
