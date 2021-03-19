/**
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2021 the original authors or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License")
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
package io.sarl.sre.network.tests.units.services.mocks;

import com.hazelcast.aggregation.Aggregator;
import com.hazelcast.config.IndexConfig;
import com.hazelcast.core.EntryView;
import com.hazelcast.map.EntryProcessor;
import com.hazelcast.map.IMap;
import com.hazelcast.map.LocalMapStats;
import com.hazelcast.map.MapInterceptor;
import com.hazelcast.map.QueryCache;
import com.hazelcast.map.listener.MapListener;
import com.hazelcast.map.listener.MapPartitionLostListener;
import com.hazelcast.projection.Projection;
import com.hazelcast.query.Predicate;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.TimeUnit;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SarlSpecification("0.12")
@SarlElementType(10)
@SuppressWarnings("all")
public class IMapMock<K extends Object, V extends Object> implements IMap<K, V> {
  public final TreeMap<K, V> internalMap = CollectionLiterals.<K, V>newTreeMap(null);
  
  @Override
  @Pure
  public String toString() {
    return this.internalMap.toString();
  }
  
  @Override
  public boolean equals(final Object obj) {
    return this.internalMap.equals(obj);
  }
  
  public UUID addEntryListener(final MapListener listener, final boolean includeValue) {
    return UUID.randomUUID();
  }
  
  public UUID addEntryListener(final MapListener listener, final K key, final boolean includeValue) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public UUID addEntryListener(final MapListener listener, final Predicate<K, V> predicate, final boolean includeValue) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public UUID addEntryListener(final MapListener listener, final Predicate<K, V> predicate, final K key, final boolean includeValue) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void addIndex(final IndexConfig indexConfig) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public String addInterceptor(final MapInterceptor interceptor) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public UUID addLocalEntryListener(final MapListener listener) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public UUID addLocalEntryListener(final MapListener listener, final Predicate<K, V> predicate, final boolean includeValue) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public UUID addLocalEntryListener(final MapListener listener, final Predicate<K, V> predicate, final K key, final boolean includeValue) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public UUID addPartitionLostListener(final MapPartitionLostListener listener) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public <R extends Object> R aggregate(final Aggregator<? super Map.Entry<K, V>, R> aggregator) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public <R extends Object> R aggregate(final Aggregator<? super Map.Entry<K, V>, R> aggregator, final Predicate<K, V> predicate) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void clear() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean containsKey(final Object key) {
    return this.internalMap.containsKey(key);
  }
  
  public boolean containsValue(final Object value) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void delete(final Object key) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public Set<Map.Entry<K, V>> entrySet() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public Set<Map.Entry<K, V>> entrySet(final Predicate<K, V> predicate) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean evict(final K key) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void evictAll() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public <R extends Object> Map<K, R> executeOnEntries(final EntryProcessor<K, V, R> entryProcessor) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public <R extends Object> Map<K, R> executeOnEntries(final EntryProcessor<K, V, R> entryProcessor, final Predicate<K, V> predicate) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public <R extends Object> R executeOnKey(final K key, final EntryProcessor<K, V, R> entryProcessor) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public <R extends Object> Map<K, R> executeOnKeys(final Set<K> keys, final EntryProcessor<K, V, R> entryProcessor) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void flush() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void forceUnlock(final K key) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public V get(final Object key) {
    return this.internalMap.get(key);
  }
  
  public Map<K, V> getAll(final Set<K> keys) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public CompletionStage<V> getAsync(final K key) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public EntryView<K, V> getEntryView(final K key) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public LocalMapStats getLocalMapStats() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public QueryCache<K, V> getQueryCache(final String name) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public QueryCache<K, V> getQueryCache(final String name, final Predicate<K, V> predicate, final boolean includeValue) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public QueryCache<K, V> getQueryCache(final String name, final MapListener listener, final Predicate<K, V> predicate, final boolean includeValue) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean isLocked(final K key) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public Set<K> keySet() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public Set<K> keySet(final Predicate<K, V> predicate) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void loadAll(final boolean replaceExistingValues) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void loadAll(final Set<K> keys, final boolean replaceExistingValues) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public Set<K> localKeySet() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public Set<K> localKeySet(final Predicate<K, V> predicate) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void lock(final K key) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void lock(final K key, final long leaseTime, final TimeUnit timeUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public <R extends Object> Collection<R> project(final Projection<? super Map.Entry<K, V>, R> projection) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public <R extends Object> Collection<R> project(final Projection<? super Map.Entry<K, V>, R> projection, final Predicate<K, V> predicate) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public V put(final K key, final V value) {
    return this.internalMap.put(key, value);
  }
  
  public V put(final K key, final V value, final long ttl, final TimeUnit ttlUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public V put(final K key, final V value, final long ttl, final TimeUnit ttlUnit, final long maxIdle, final TimeUnit maxIdleUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void putAll(final Map<? extends K, ? extends V> m) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public CompletionStage<Void> putAllAsync(final Map<? extends K, ? extends V> map) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public CompletionStage<V> putAsync(final K key, final V value) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public CompletionStage<V> putAsync(final K key, final V value, final long ttl, final TimeUnit ttlUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public CompletionStage<V> putAsync(final K key, final V value, final long ttl, final TimeUnit ttlUnit, final long maxIdle, final TimeUnit maxIdleUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public V putIfAbsent(final K key, final V value) {
    return this.internalMap.putIfAbsent(key, value);
  }
  
  public V putIfAbsent(final K key, final V value, final long ttl, final TimeUnit ttlUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public V putIfAbsent(final K key, final V value, final long ttl, final TimeUnit ttlUnit, final long maxIdle, final TimeUnit maxIdleUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void putTransient(final K key, final V value, final long ttl, final TimeUnit ttlUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void putTransient(final K key, final V value, final long ttl, final TimeUnit ttlUnit, final long maxIdle, final TimeUnit maxIdleUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public V remove(final Object key) {
    return this.internalMap.remove(key);
  }
  
  public boolean remove(final Object key, final Object value) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void removeAll(final Predicate<K, V> predicate) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public CompletionStage<V> removeAsync(final K key) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean removeEntryListener(final UUID id) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean removeInterceptor(final String id) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean removePartitionLostListener(final UUID id) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public V replace(final K key, final V value) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean replace(final K key, final V oldValue, final V newValue) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void set(final K key, final V value) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void set(final K key, final V value, final long ttl, final TimeUnit ttlUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void set(final K key, final V value, final long ttl, final TimeUnit ttlUnit, final long maxIdle, final TimeUnit maxIdleUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public CompletionStage<Void> setAsync(final K key, final V value) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public CompletionStage<Void> setAsync(final K key, final V value, final long ttl, final TimeUnit ttlUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public CompletionStage<Void> setAsync(final K key, final V value, final long ttl, final TimeUnit ttlUnit, final long maxIdle, final TimeUnit maxIdleUnit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean setTtl(final K key, final long ttl, final TimeUnit timeunit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public <R extends Object> CompletionStage<R> submitToKey(final K key, final EntryProcessor<K, V, R> entryProcessor) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public <R extends Object> CompletionStage<Map<K, R>> submitToKeys(final Set<K> keys, final EntryProcessor<K, V, R> entryProcessor) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean tryLock(final K key) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean tryLock(final K key, final long time, final TimeUnit timeunit) throws InterruptedException {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean tryLock(final K key, final long time, final TimeUnit timeunit, final long leaseTime, final TimeUnit leaseTimeunit) throws InterruptedException {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean tryPut(final K key, final V value, final long timeout, final TimeUnit timeunit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean tryRemove(final K key, final long timeout, final TimeUnit timeunit) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void unlock(final K key) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public Collection<V> values() {
    return this.internalMap.values();
  }
  
  public Collection<V> values(final Predicate<K, V> predicate) {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public boolean isEmpty() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public int size() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public void destroy() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public String getName() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public String getPartitionKey() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  public String getServiceName() {
    throw new UnsupportedOperationException("TODO: auto-generated method stub");
  }
  
  @Override
  @Pure
  @SyntheticMember
  public int hashCode() {
    int result = super.hashCode();
    return result;
  }
  
  @SyntheticMember
  public IMapMock() {
    super();
  }
}
