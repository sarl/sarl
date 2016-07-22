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

package io.janusproject.services.distributeddata;

import java.util.Comparator;

import io.janusproject.services.DependentService;

/**
 * Service that permits to manage data structures that are shared over a network.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface DistributedDataStructureService extends DependentService {

	/**
	 * Replies the {@link DMap} with the given name.
	 *
	 * <p>
	 * This function permits to create a map for keys without a natural order, and without a specific comparator. An object has a
	 * natural order if it implements {@link Comparable}.
	 *
	 * <p>
	 * This function is equivalent to create a <code>HashMap</code> in the standard Java API. If the keys have a natural order,
	 * i.e. they implement the {@link Comparable} interface, or if you are able to provide a {@link Comparator}, then it is
	 * recommended to use {@link #getMap(String, Comparator)} in place of the function {@link #getMap(String)}.
	 *
	 * @param <K> - types of the keys in the map.
	 * @param <V> - types of the values in the map.
	 * @param name - name of the shared map.
	 * @return the map.
	 * @see #getMap(String, Comparator)
	 */
	<K, V> DMap<K, V> getMap(String name);

	/**
	 * Replies the {@link DMap} with the given name.
	 *
	 * <p>
	 * This function permits to create a map for keys with a natural order, OR with a specific comparator. An object has a natural
	 * order if it implements {@link Comparable}. If the comparator is <code>null</code>, then the natural order of the keys is
	 * used.
	 *
	 * <p>
	 * This function is equivalent to create a <code>TreeMap</code> in the standard Java API. If the keys have not a natural
	 * order, i.e. they not implement the {@link Comparable} interface, or if you are not able to provide a {@link Comparator},
	 * then it is recommended to use {@link #getMap(String)} in place of the function {@link #getMap(String, Comparator)}.
	 *
	 * @param <K> - types of the keys in the map.
	 * @param <V> - types of the values in the map.
	 * @param name - name of the shared map.
	 * @param comparator - the comparator of the keys.
	 * @return the map.
	 * @see #getMap(String)
	 */
	<K, V> DMap<K, V> getMap(String name, Comparator<? super K> comparator);

	/**
	 * Replies the {@link DMultiMap} with the given name.
	 *
	 * <p>
	 * This function permits to create a map for keys without a natural order, and without a specific comparator. An object has a
	 * natural order if it implements {@link Comparable}.
	 *
	 * <p>
	 * This function is equivalent to create a <code>HashMap</code> in the standard Java API. If the keys have a natural order,
	 * i.e. they implement the {@link Comparable} interface, or if you are able to provide a {@link Comparator}, then it is
	 * recommended to use {@link #getMultiMap(String, Comparator)} in place of the function {@link #getMultiMap(String)}.
	 *
	 * @param <K> - types of the keys in the map.
	 * @param <V> - types of the values in the map.
	 * @param name - name of the shared multi-map.
	 * @return the map.
	 * @see #getMultiMap(String, Comparator)
	 */
	<K, V> DMultiMap<K, V> getMultiMap(String name);

	/**
	 * Replies the {@link DMultiMap} with the given name.
	 *
	 * <p>
	 * This function permits to create a map for keys with a natural order, OR with a specific comparator. An object has a natural
	 * order if it implements {@link Comparable}. If the comparator is <code>null</code>, then the natural order of the keys is
	 * used.
	 *
	 * <p>
	 * This function is equivalent to create a <code>TreeMap</code> in the standard Java API. If the keys have not a natural
	 * order, i.e. they not implement the {@link Comparable} interface, or if you are not able to provide a {@link Comparator},
	 * then it is recommended to use {@link #getMultiMap(String)} in place of the function
	 * {@link #getMultiMap(String, Comparator)}.
	 *
	 * @param <K> - types of the keys in the map.
	 * @param <V> - types of the values in the map.
	 * @param name - name of the shared multi-map.
	 * @param comparator - the comparator of the keys.
	 * @return the map.
	 * @see #getMultiMap(String)
	 */
	<K, V> DMultiMap<K, V> getMultiMap(String name, Comparator<? super K> comparator);

}
