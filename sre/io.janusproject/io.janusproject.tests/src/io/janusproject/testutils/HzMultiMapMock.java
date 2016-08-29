/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.janusproject.testutils;

import java.util.AbstractSet;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.google.common.base.Supplier;
import com.google.common.collect.Multimaps;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.Sets;
import com.hazelcast.core.EntryListener;
import com.hazelcast.core.IMap;
import com.hazelcast.core.MultiMap;
import com.hazelcast.mapreduce.JobTracker;
import com.hazelcast.mapreduce.aggregation.Aggregation;
import com.hazelcast.monitor.LocalMultiMapStats;

/**
 * A mock of {@link IMap}.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @param <K>
 * @param <V>
 */
@SuppressWarnings("all")
public class HzMultiMapMock<K, V> implements MultiMap<K, V> {

	private final String name;
	private final SetMultimap<K, V> map;

	/**
	 * @param name
	 */
	public HzMultiMapMock(String name) {
		this.name = name;
		Supplier<Set<V>> supplier = new Supplier<Set<V>>() {
			@Override
			public Set<V> get() {
				return Sets.newHashSet();
			}
		};
		this.map = Multimaps.newSetMultimap(new HashMap<K, Collection<V>>(), supplier);
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
	public String getPartitionKey() {
		throw new UnsupportedOperationException();
	}

	@Override
	public String getServiceName() {
		throw new UnsupportedOperationException();
	}

	@Override
	public void destroy() {
		throw new UnsupportedOperationException();
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
	public Collection<V> remove(Object key) {
		return this.map.removeAll(key);
	}

	@Override
	public Set<K> localKeySet() {
		return this.map.keySet();
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
	public Set<Entry<K, V>> entrySet() {
		return new CollectionWrapper(this.map.entries());
	}

	@Override
	public boolean containsKey(K key) {
		return this.map.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return this.map.containsValue(value);
	}

	@Override
	public boolean containsEntry(K key, V value) {
		return this.map.containsEntry(key, value);
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
		return this.map.get(key).size();
	}

	@Override
	public String addLocalEntryListener(EntryListener<K, V> listener) {
		throw new UnsupportedOperationException();
	}

	@Override
	public String addEntryListener(EntryListener<K, V> listener, boolean includeValue) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean removeEntryListener(String registrationId) {
		throw new UnsupportedOperationException();
	}

	@Override
	public String addEntryListener(EntryListener<K, V> listener, K key, boolean includeValue) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void lock(K key) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void lock(K key, long leaseTime, TimeUnit timeUnit) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean isLocked(K key) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean tryLock(K key) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean tryLock(K key, long time, TimeUnit timeunit) throws InterruptedException {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean tryLock(K key, long time, TimeUnit timeunit, long leaseTime, TimeUnit leaseTimeunit)
			throws InterruptedException {
		throw new UnsupportedOperationException();
	}

	@Override
	public void unlock(K key) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void forceUnlock(K key) {
		throw new UnsupportedOperationException();
	}

	@Override
	public LocalMultiMapStats getLocalMultiMapStats() {
		throw new UnsupportedOperationException();
	}

	@Override
	public <SuppliedValue, Result> Result aggregate(com.hazelcast.mapreduce.aggregation.Supplier<K, V, SuppliedValue> supplier,
			Aggregation<K, SuppliedValue, Result> aggregation) {
		throw new UnsupportedOperationException();
	}

	@Override
	public <SuppliedValue, Result> Result aggregate(com.hazelcast.mapreduce.aggregation.Supplier<K, V, SuppliedValue> supplier,
			Aggregation<K, SuppliedValue, Result> aggregation, JobTracker jobTracker) {
		throw new UnsupportedOperationException();
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @param <E>
	 */
	private static class CollectionWrapper<E> extends AbstractSet<E> {

		private final Collection<E> collection;

		public CollectionWrapper(Collection<E> collection) {
			this.collection = collection;
		}

		@Override
		public boolean equals(Object o) {
			return this.collection.equals(o);
		}

		@Override
		public int hashCode() {
			return this.collection.hashCode();
		}

		@Override
		public String toString() {
			return this.collection.toString();
		}

		@Override
		public Iterator<E> iterator() {
			return this.collection.iterator();
		}

		@Override
		public int size() {
			return this.collection.size();
		}

	}

}
