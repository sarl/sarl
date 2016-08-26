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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import com.hazelcast.core.EntryListener;
import com.hazelcast.core.EntryView;
import com.hazelcast.core.ExecutionCallback;
import com.hazelcast.core.ICompletableFuture;
import com.hazelcast.core.IMap;
import com.hazelcast.map.EntryProcessor;
import com.hazelcast.map.MapInterceptor;
import com.hazelcast.map.listener.MapListener;
import com.hazelcast.map.listener.MapPartitionLostListener;
import com.hazelcast.mapreduce.JobTracker;
import com.hazelcast.mapreduce.aggregation.Aggregation;
import com.hazelcast.mapreduce.aggregation.Supplier;
import com.hazelcast.monitor.LocalMapStats;
import com.hazelcast.query.Predicate;

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
public class HzMapMock<K, V> extends HashMap<K, V> implements IMap<K, V> {

	private static final long serialVersionUID = -2482265223106773425L;

	private final UUID name = UUID.randomUUID();

	/**
	 */
	public HzMapMock() {
		//
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void destroy() {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getName() {
		return this.name.toString();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getPartitionKey() {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getServiceName() {
		throw new UnsupportedOperationException();
	}

	// /** {@inheritDoc}
	// */
	// @Override
	// public String addEntryListener(EntryListener<K, V> arg0, boolean arg1) {
	// return UUID.randomUUID().toString();
	// }
	//
	// /** {@inheritDoc}
	// */
	// @Override
	// public String addEntryListener(EntryListener<K, V> arg0, K arg1, boolean arg2) {
	// return UUID.randomUUID().toString();
	// }
	//
	// /** {@inheritDoc}
	// */
	// @Override
	// public String addEntryListener(EntryListener<K, V> arg0, Predicate<K, V> arg1, boolean arg2) {
	// return UUID.randomUUID().toString();
	// }
	//
	// /** {@inheritDoc}
	// */
	// @Override
	// public String addEntryListener(EntryListener<K, V> arg0, Predicate<K, V> arg1, K arg2, boolean arg3) {
	// return UUID.randomUUID().toString();
	// }

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void addIndex(String arg0, boolean arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addInterceptor(MapInterceptor arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public <SuppliedValue, Result> Result aggregate(Supplier<K, V, SuppliedValue> arg0,
			Aggregation<K, SuppliedValue, Result> arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public <SuppliedValue, Result> Result aggregate(Supplier<K, V, SuppliedValue> arg0,
			Aggregation<K, SuppliedValue, Result> arg1, JobTracker arg2) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void delete(Object arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Set<java.util.Map.Entry<K, V>> entrySet(Predicate arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean evict(K arg0) {
		return remove(arg0) != null;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<K, Object> executeOnEntries(EntryProcessor arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<K, Object> executeOnEntries(EntryProcessor arg0, Predicate arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Object executeOnKey(K arg0, EntryProcessor arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<K, Object> executeOnKeys(Set<K> arg0, EntryProcessor arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void flush() {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void forceUnlock(K arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<K, V> getAll(Set<K> arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public ICompletableFuture<V> getAsync(K arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public ICompletableFuture<Void> setAsync(K arg0, V arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public ICompletableFuture<Void> setAsync(K arg0, V arg1, long arg2, TimeUnit arg3) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public EntryView<K, V> getEntryView(K arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public LocalMapStats getLocalMapStats() {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean isLocked(K arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Set<K> keySet(Predicate arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Set<K> localKeySet() {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Set<K> localKeySet(Predicate arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void lock(K arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void lock(K arg0, long arg1, TimeUnit arg2) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public V put(K arg0, V arg1, long arg2, TimeUnit arg3) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public ICompletableFuture<V> putAsync(K arg0, V arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public ICompletableFuture<V> putAsync(K arg0, V arg1, long arg2, TimeUnit arg3) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public V putIfAbsent(K arg0, V arg1) {
		if (!containsKey(arg0))
			return put(arg0, arg1);
		return null;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public V putIfAbsent(K arg0, V arg1, long arg2, TimeUnit arg3) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void putTransient(K arg0, V arg1, long arg2, TimeUnit arg3) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean remove(Object arg0, Object arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public ICompletableFuture<V> removeAsync(K arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean removeEntryListener(String arg0) {
		return true;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void removeInterceptor(String arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public V replace(K arg0, V arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean replace(K arg0, V arg1, V arg2) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void set(K arg0, V arg1) {
		put(arg0, arg1);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void set(K arg0, V arg1, long arg2, TimeUnit arg3) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public ICompletableFuture submitToKey(K arg0, EntryProcessor arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void submitToKey(K arg0, EntryProcessor arg1, ExecutionCallback arg2) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean tryLock(K arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean tryLock(K key, long time, TimeUnit timeunit, long leaseTime, TimeUnit leaseTimeunit)
			throws InterruptedException {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean tryLock(K arg0, long arg1, TimeUnit arg2) throws InterruptedException {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean tryPut(K arg0, V arg1, long arg2, TimeUnit arg3) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean tryRemove(K arg0, long arg1, TimeUnit arg2) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void unlock(K arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<V> values(Predicate arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void evictAll() {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void loadAll(boolean arg0) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void loadAll(Set<K> arg0, boolean arg1) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addLocalEntryListener(MapListener listener) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addLocalEntryListener(EntryListener listener) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addLocalEntryListener(MapListener listener, Predicate<K, V> predicate, boolean includeValue) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addLocalEntryListener(EntryListener listener, Predicate<K, V> predicate, boolean includeValue) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addLocalEntryListener(MapListener listener, Predicate<K, V> predicate, K key, boolean includeValue) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addLocalEntryListener(EntryListener listener, Predicate<K, V> predicate, K key, boolean includeValue) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addEntryListener(MapListener listener, boolean includeValue) {
		return UUID.randomUUID().toString();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addEntryListener(EntryListener listener, boolean includeValue) {
		return UUID.randomUUID().toString();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addPartitionLostListener(MapPartitionLostListener listener) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean removePartitionLostListener(String id) {
		throw new UnsupportedOperationException();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addEntryListener(MapListener listener, K key, boolean includeValue) {
		return UUID.randomUUID().toString();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addEntryListener(EntryListener listener, K key, boolean includeValue) {
		return UUID.randomUUID().toString();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addEntryListener(MapListener listener, Predicate<K, V> predicate, boolean includeValue) {
		return UUID.randomUUID().toString();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addEntryListener(EntryListener listener, Predicate<K, V> predicate, boolean includeValue) {
		return UUID.randomUUID().toString();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addEntryListener(MapListener listener, Predicate<K, V> predicate, K key, boolean includeValue) {
		return UUID.randomUUID().toString();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String addEntryListener(EntryListener listener, Predicate<K, V> predicate, K key, boolean includeValue) {
		return UUID.randomUUID().toString();
	}

}
