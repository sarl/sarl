/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.core.util;

import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;

/** Factory of concurrent collections.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public interface ConcurrentCollectionFactory {

	/** Create a concurrent collection.
	 *
	 * @param <T> the type of the elements in the collection.
	 * @return a new instance of collection.
	 */
	<T> ConcurrentCollection<T> newCollection();

	/** Create a concurrent collection.
	 *
	 * @param <T> the type of the elements in the collection.
	 * @param toCopy the collection to copy into the new instance.
	 * @return a new instance of collection.
	 */
	<T> ConcurrentCollection<T> newCollection(Collection<T> toCopy);

	/** Create a concurrent list.
	 *
	 * @param <T> the type of the elements in the list.
	 * @return a new instance of list.
	 * @since 0.12
	 */
	<T> ConcurrentList<T> newList();

	/** Create a concurrent list.
	 *
	 * @param <T> the type of the elements in the list.
	 * @param toCopy the most to copy into the new instance.
	 * @return a new instance of list.
	 * @since 0.15
	 */
	<T> ConcurrentList<T> newList(List<T> toCopy);

	/** Create a concurrent set.
	 *
	 * @param <T> the type of the elements in the set.
	 * @param comparator the comparator of elements.
	 * @return a new instance of set.
	 */
	<T> ConcurrentSet<T> newSet(Comparator<? super T> comparator);

	/** Create a concurrent set.
	 *
	 * @param <T> the type of the elements in the set.
	 * @param comparator the comparator of elements.
	 * @param toCopy the set to copy into the new instance.
	 * @return a new instance of set.
	 */
	<T> ConcurrentSet<T> newSet(Comparator<? super T> comparator, Collection<T> toCopy);

	/** Create a concurrent map.
	 *
	 * @param <K> the type of the keys in the map.
	 * @param <V> the type of the values in the map.
	 * @param comparator the comparator of keys.
	 * @return a new instance of map.
	 * @since 0.15
	 */
	<K, V> ConcurrentMap<K, V> newMap(Comparator<? super K> comparator);

	/** Create a concurrent map.
	 *
	 * @param <K> the type of the keys in the map.
	 * @param <V> the type of the values in the map.
	 * @param comparator the comparator of keys.
	 * @param toCopy the map to copy into the new instance.
	 * @return a new instance of map.
	 * @since 0.15
	 */
	<K, V> ConcurrentMap<K, V> newMap(Comparator<? super K> comparator, Map<K, V> toCopy);

}

