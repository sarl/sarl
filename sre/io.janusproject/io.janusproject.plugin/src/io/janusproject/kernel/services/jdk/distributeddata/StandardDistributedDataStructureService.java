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

package io.janusproject.kernel.services.jdk.distributeddata;

import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import com.google.common.base.Supplier;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.util.concurrent.Service;
import io.janusproject.kernel.services.guava.DMultiMapView;
import io.janusproject.services.AbstractDependentService;
import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DMultiMap;
import io.janusproject.services.distributeddata.DistributedDataStructureService;

/**
 * Service providing tools for creating distributed data structures.
 *
 * <p>
 * This implementation creates standard Java collections that cannot be distributed other several JVM.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class StandardDistributedDataStructureService extends AbstractDependentService implements DistributedDataStructureService {

	@Override
	public final Class<? extends Service> getServiceType() {
		return DistributedDataStructureService.class;
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
		return new DMapView<>(name, Maps.<K, V>newHashMap());
	}

	@SuppressWarnings("unchecked")
	@Override
	public <K, V> DMap<K, V> getMap(String name, Comparator<? super K> comparator) {
		Map<K, V> map;
		if (comparator == null) {
			map = (Map<K, V>) Maps.newTreeMap();
		} else {
			map = Maps.newTreeMap(comparator);
		}
		return new DMapView<>(name, map);
	}

	@Override
	public <K, V> DMultiMap<K, V> getMultiMap(String name) {
		Map<K, Collection<V>> map = Maps.newHashMap();
		Multimap<K, V> multimap = Multimaps.newListMultimap(map, new ArrayListSupplier<V>());
		return new DMultiMapView<>(name, multimap);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public <K, V> DMultiMap<K, V> getMultiMap(String name, Comparator<? super K> comparator) {
		Map map;
		if (comparator == null) {
			map = Maps.newTreeMap();
		} else {
			map = Maps.newTreeMap(comparator);
		}
		Multimap<K, V> multimap = Multimaps.newListMultimap(map, new ArrayListSupplier<V>());
		return new DMultiMapView<>(name, multimap);
	}

	/**
	 * Suplier of list for Hazelcast.
	 *
	 * @param <K> type of the list elements.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class ArrayListSupplier<K> implements Supplier<List<K>> {

		/**
		 * Construct.
		 */
		ArrayListSupplier() {
			//
		}

		@Override
		public List<K> get() {
			return Lists.newArrayList();
		}
	}

}
