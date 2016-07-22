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

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.common.base.Objects;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.collect.Multiset;
import com.google.common.util.concurrent.Service;
import com.google.inject.Inject;
import com.hazelcast.core.EntryEvent;
import com.hazelcast.core.EntryListener;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.core.IMap;
import com.hazelcast.core.MapEvent;
import com.hazelcast.core.MultiMap;
import com.hazelcast.map.listener.MapListener;

import io.janusproject.services.AbstractDependentService;
import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DMapListener;
import io.janusproject.services.distributeddata.DMultiMap;
import io.janusproject.services.distributeddata.DistributedDataStructureService;

/**
 * Service based on Hazelcast that permits to manage data structures that are shared over a network.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class HazelcastDistributedDataStructureService extends AbstractDependentService
		implements DistributedDataStructureService {

	@Inject
	private HazelcastInstance hazelcastInstance;

	@Override
	public final Class<? extends Service> getServiceType() {
		return DistributedDataStructureService.class;
	}

	/**
	 * Change the hazelcast instance used by this factory.
	 *
	 * @param hazelcastInstance - reference to the Hazelcast engine.
	 */
	void setHazelcastInstance(HazelcastInstance hazelcastInstance) {
		this.hazelcastInstance = hazelcastInstance;
	}

	@Override
	protected void doStart() {
		notifyStarted();
	}

	@Override
	protected void doStop() {
		notifyStopped();
	}

	@Override
	public <K, V> DMap<K, V> getMap(String name) {
		IMap<K, V> map = this.hazelcastInstance.getMap(name);
		if (map != null) {
			return new MapView<>(name, map);
		}
		return null;
	}

	@Override
	public <K, V> DMap<K, V> getMap(String name, Comparator<? super K> comparator) {
		IMap<K, V> map = this.hazelcastInstance.getMap(name);
		if (map != null) {
			return new MapView<>(name, map);
		}
		return null;
	}

	@Override
	public <K, V> DMultiMap<K, V> getMultiMap(String name) {
		MultiMap<K, V> map = this.hazelcastInstance.getMultiMap(name);
		if (map != null) {
			return new MultiMapView<>(name, map);
		}
		return null;
	}

	@Override
	public <K, V> DMultiMap<K, V> getMultiMap(String name, Comparator<? super K> comparator) {
		MultiMap<K, V> map = this.hazelcastInstance.getMultiMap(name);
		if (map != null) {
			return new MultiMapView<>(name, map);
		}
		return null;
	}

	/**
	 * @param <K> - type of the keys.
	 * @param <V> - type of the values.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static final class MapView<K, V> implements DMap<K, V> {

		private final String name;

		private final IMap<K, V> map;

		MapView(String name, IMap<K, V> map) {
			assert (map != null);
			this.name = name;
			this.map = map;
		}

		@Override
		public boolean isBackedCollection() {
			return false;
		}

		@Override
		public String getName() {
			return this.name;
		}

		@Override
		public void clear() {
			this.map.clear();
		}

		@Override
		public boolean containsKey(Object arg0) {
			return this.map.containsKey(arg0);
		}

		@Override
		public boolean containsValue(Object arg0) {
			return this.map.containsValue(arg0);
		}

		@Override
		public Set<java.util.Map.Entry<K, V>> entrySet() {
			return this.map.entrySet();
		}

		@Override
		public V get(Object arg0) {
			return this.map.get(arg0);
		}

		@Override
		public boolean isEmpty() {
			return this.map.isEmpty();
		}

		@Override
		public Set<K> keySet() {
			return this.map.keySet();
		}

		@Override
		public V put(K arg0, V arg1) {
			return this.map.put(arg0, arg1);
		}

		@Override
		public void putAll(Map<? extends K, ? extends V> arg0) {
			this.map.putAll(arg0);
		}

		@Override
		public V remove(Object arg0) {
			return this.map.remove(arg0);
		}

		@Override
		public int size() {
			return this.map.size();
		}

		@Override
		public Collection<V> values() {
			return this.map.values();
		}

		@Override
		public V putIfAbsent(K key, V value) {
			return this.map.putIfAbsent(key, value);
		}

		@Override
		public void addDMapListener(DMapListener<? super K, ? super V> listener) {
			EntryListenerWrapper<K, V> w = new EntryListenerWrapper<>(listener);
			String k = this.map.addEntryListener((MapListener) w, true);
			w.setHazelcastListener(k);
		}

		@Override
		public void removeDMapListener(DMapListener<? super K, ? super V> listener) {
			if (listener instanceof EntryListenerWrapper) {
				String k = ((EntryListenerWrapper<?, ?>) listener).getHazelcastListener();
				if (k != null) {
					this.map.removeEntryListener(k);
				}
			}
		}

	}

	/**
	 * @param <K> - type of the keys.
	 * @param <V> - type of the values.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SuppressWarnings("unchecked")
	private static final class MultiMapView<K, V> implements DMultiMap<K, V> {

		private final String name;

		private final MultiMap<K, V> map;

		MultiMapView(String name, MultiMap<K, V> map) {
			this.name = name;
			assert (map != null);
			this.map = map;
		}

		@Override
		public boolean isBackedCollection() {
			return false;
		}

		@Override
		public String getName() {
			return this.name;
		}

		@Override
		public boolean put(K key, V value) {
			return this.map.put(key, value);
		}

		@Override
		public Collection<V> get(K key) {
			return this.map.get(key);
		}

		@Override
		public boolean remove(Object key, Object value) {
			return this.map.remove(key, value);
		}

		@Override
		public Collection<V> removeAll(Object key) {
			return this.map.remove(key);
		}

		@Override
		public Set<K> keySet() {
			return this.map.keySet();
		}

		@Override
		public Multiset<K> keys() {
			return new SetMultiset();
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

		@Override
		public boolean containsEntry(Object key, Object value) {
			try {
				return this.map.containsEntry((K) key, (V) value);
			} catch (ClassCastException exception) {
				return false;
			}
		}

		@Override
		public int size() {
			return this.map.size();
		}

		@Override
		public void clear() {
			this.map.clear();
		}

		@Override
		public int valueCount(K key) {
			return this.map.valueCount(key);
		}

		@Override
		public void addDMapListener(DMapListener<? super K, ? super V> listener) {
			EntryListenerWrapper<K, V> w = new EntryListenerWrapper<>(listener);
			String k = this.map.addEntryListener(w, true);
			w.setHazelcastListener(k);
		}

		@Override
		public void removeDMapListener(DMapListener<? super K, ? super V> listener) {
			if (listener instanceof EntryListenerWrapper) {
				String k = ((EntryListenerWrapper<?, ?>) listener).getHazelcastListener();
				if (k != null) {
					this.map.removeEntryListener(k);
				}
			}
		}

		@Override
		public boolean isEmpty() {
			return this.map.size() == 0;
		}

		@Override
		public Collection<V> replaceValues(K key, Iterable<? extends V> values) {
			Collection<V> oldValues = this.map.remove(key);
			for (V value : values) {
				this.map.put(key, value);
			}
			return oldValues;
		}

		@Override
		public boolean putAll(Multimap<? extends K, ? extends V> multimap) {
			boolean changed = false;
			for (Entry<? extends K, ? extends V> value : multimap.entries()) {
				changed = this.map.put(value.getKey(), value.getValue()) && changed;
			}
			return changed;
		}

		@Override
		public boolean putAll(K key, Iterable<? extends V> values) {
			boolean changed = false;
			for (V value : values) {
				changed = this.map.put(key, value) && changed;
			}
			return changed;
		}

		@Override
		public Map<K, Collection<V>> asMap() {
			return Multimaps.asMap(this);
		}

		/**
		 * Internal implementation of a multiset.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private class SetMultiset implements Multiset<K> {

			/**
			 * Construct.
			 */
			SetMultiset() {
				//
			}

			@Override
			public int size() {
				return MultiMapView.this.size();
			}

			@Override
			public boolean isEmpty() {
				return MultiMapView.this.isEmpty();
			}

			@Override
			public Object[] toArray() {
				Object[] tab = new Object[MultiMapView.this.size()];
				int i = 0;
				for (Map.Entry<K, ?> e : MultiMapView.this.entries()) {
					tab[i] = e.getKey();
					++i;
				}
				return tab;
			}

			@Override
			public <T> T[] toArray(T[] array) {
				T[] tab = array;
				if (tab == null || tab.length < MultiMapView.this.size()) {
					tab = (T[]) new Object[MultiMapView.this.size()];
					int i = 0;
					for (Map.Entry<K, ?> e : MultiMapView.this.entries()) {
						tab[i] = (T) e.getKey();
						++i;
					}
				}
				return tab;
			}

			@Override
			public void clear() {
				MultiMapView.this.clear();
			}

			@Override
			public int count(Object element) {
				int c = 0;
				for (Map.Entry<K, ?> e : MultiMapView.this.entries()) {
					if (Objects.equal(element, e.getKey())) {
						++c;
					}
				}
				return c;
			}

			@Override
			public int add(K element, int occurrences) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean add(K element) {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean remove(Object element) {
				Collection<?> values = MultiMapView.this.removeAll(element);
				return values != null && !values.isEmpty();
			}

			@Override
			public int remove(Object element, int occurrences) {
				if (occurrences < 0) {
					throw new IllegalArgumentException();
				}
				try {
					Collection<?> values = MultiMapView.this.get((K) element);
					int old = values.size();
					Iterator<?> iterator = values.iterator();
					for (int i = 0; i < occurrences && iterator.hasNext(); ++i) {
						iterator.next();
						iterator.remove();
					}
					return old;
				} catch (ClassCastException exception) {
					return 0;
				}
			}

			@Override
			public int setCount(K element, int count) {
				if (count < 0) {
					throw new IllegalArgumentException();
				}
				Collection<?> values = MultiMapView.this.get(element);
				int old = values.size();
				if (count > old) {
					throw new UnsupportedOperationException();
				}
				try {
					Iterator<?> iterator = values.iterator();
					int toRemove = old - count;
					for (int i = 0; i < toRemove && iterator.hasNext(); ++i) {
						iterator.next();
						iterator.remove();
					}
					return old;
				} catch (ClassCastException exception) {
					return 0;
				}
			}

			@Override
			public boolean setCount(K element, int oldCount, int newCount) {
				if (oldCount < 0 || newCount < 0) {
					throw new IllegalArgumentException();
				}
				Collection<?> values = MultiMapView.this.get(element);
				int old = values.size();
				if (oldCount == old) {
					if (newCount > old) {
						throw new UnsupportedOperationException();
					}
					try {
						Iterator<?> iterator = values.iterator();
						int toRemove = old - newCount;
						if (toRemove > 0) {
							for (int i = 0; i < toRemove && iterator.hasNext(); ++i) {
								iterator.next();
								iterator.remove();
							}
							return true;
						}
					} catch (ClassCastException exception) {
						//
					}
				}
				return false;
			}

			@Override
			public boolean addAll(Collection<? extends K> collection) {
				throw new UnsupportedOperationException();
			}

			@Override
			public Set<K> elementSet() {
				return MultiMapView.this.keySet();
			}

			@Override
			public Set<com.google.common.collect.Multiset.Entry<K>> entrySet() {
				throw new UnsupportedOperationException();
			}

			@Override
			public Iterator<K> iterator() {
				final Iterator<Map.Entry<K, V>> entries = MultiMapView.this.entries().iterator();
				return new Iterator<K>() {
					@Override
					public boolean hasNext() {
						return entries.hasNext();
					}

					@Override
					public K next() {
						return entries.next().getKey();
					}

					@Override
					public void remove() {
						entries.remove();
					}

				};
			}

			@Override
			public boolean contains(Object element) {
				return MultiMapView.this.containsKey(element);
			}

			@Override
			public boolean containsAll(Collection<?> elements) {
				return MultiMapView.this.keySet().containsAll(elements);
			}

			@Override
			public boolean removeAll(Collection<?> collection) {
				return MultiMapView.this.keySet().removeAll(collection);
			}

			@Override
			public boolean retainAll(Collection<?> collection) {
				return MultiMapView.this.keySet().retainAll(collection);
			}

		}

	}

	/**
	 * Listener on the Hazelcast map events.
	 *
	 * @param <K> type of the keys.
	 * @param <V> type of the values.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class EntryListenerWrapper<K, V> implements EntryListener<K, V> {

		private final DMapListener<? super K, ? super V> dmapListener;

		private String key;

		EntryListenerWrapper(DMapListener<? super K, ? super V> listener) {
			this.dmapListener = listener;
		}

		/**
		 * Replies the Hazelcast listener associated to this object.
		 *
		 * @return the hazelcast listener.
		 */
		public String getHazelcastListener() {
			return this.key;
		}

		/**
		 * Replies the Hazelcast listener associated to this object.
		 *
		 * @param hazelcastListener - the hazelcast listener.
		 */
		public void setHazelcastListener(String hazelcastListener) {
			this.key = hazelcastListener;
		}

		@Override
		public void entryAdded(EntryEvent<K, V> event) {
			this.dmapListener.entryAdded(event.getKey(), event.getValue());
		}

		@Override
		public void entryEvicted(EntryEvent<K, V> event) {
			if (event.getValue() != null) {
				this.dmapListener.entryRemoved(event.getKey(), event.getValue());
			} else {
				this.dmapListener.entryRemoved(event.getKey(), event.getOldValue());
			}
		}

		@Override
		public void entryRemoved(EntryEvent<K, V> event) {
			if (event.getValue() != null) {
				this.dmapListener.entryRemoved(event.getKey(), event.getValue());
			} else {
				this.dmapListener.entryRemoved(event.getKey(), event.getOldValue());
			}
		}

		@Override
		public void entryUpdated(EntryEvent<K, V> event) {
			this.dmapListener.entryUpdated(event.getKey(), event.getValue());
		}

		@Override
		public void mapCleared(MapEvent event) {
			this.dmapListener.mapCleared(true);
		}

		@Override
		public void mapEvicted(MapEvent event) {
			this.dmapListener.mapCleared(false);
		}

	}

}
