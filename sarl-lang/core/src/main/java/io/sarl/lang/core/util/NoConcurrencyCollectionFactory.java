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

/** Factory of collections that are not thread-safe.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.12
 * @see DefaultConcurrentCollectionFactory
 */
public class NoConcurrencyCollectionFactory implements ConcurrentCollectionFactory {

	/** Constructor.
	 */
	public NoConcurrencyCollectionFactory() {
		//
	}

	@Override
	public <T> ConcurrentCollection<T> newCollection() {
		return new NoConcurrentCollectionLinkedDeque<>();
	}

	@Override
	public <T> ConcurrentCollection<T> newCollection(Collection<T> toCopy) {
		return new NoConcurrentCollectionLinkedDeque<>(toCopy);
	}

	@Override
	public <T> ConcurrentSet<T> newSet(Comparator<? super T> comparator) {
		return new NoConcurrentTreeSet<>(comparator);
	}

	@Override
	public <T> ConcurrentSet<T> newSet(Comparator<? super T> comparator, Collection<T> toCopy) {
		return new NoConcurrentTreeSet<>(toCopy);
	}

	@Override
	public <T> ConcurrentList<T> newList() {
		return new NoConcurrentCollectionLinkedDeque<>();
	}

	@Override
	public <T> ConcurrentList<T> newList(List<T> toCopy) {
		return new NoConcurrentCollectionLinkedDeque<>(toCopy);
	}

	@Override
	public <K, V> ConcurrentMap<K, V> newMap(Comparator<? super K> comparator) {
		return null;
	}

	@Override
	public <K, V> ConcurrentMap<K, V> newMap(Comparator<? super K> comparator, Map<K, V> toCopy) {
		// TODO Auto-generated method stub
		return null;
	}

}

