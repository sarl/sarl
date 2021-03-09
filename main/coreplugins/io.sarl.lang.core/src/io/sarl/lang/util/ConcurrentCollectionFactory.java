/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.lang.util;

import java.util.Collection;
import java.util.Comparator;

/** Factory of concurrent collections.
 *
 * @author $Author: sgalland$
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

	/** Create a concurrent set.
	 *
	 * @param <T> the type of the elements in the set.
	 * @param comparator the comparator of elements.
	 * @return a new instance of set.
	 */
	<T> ConcurrentSet<T> newSet(Comparator<? super T> comparator);

	/** Create a concurrent collection.
	 *
	 * @param <T> the type of the elements in the collection.
	 * @param comparator the comparator of elements.
	 * @param toCopy the collection to copy into the new instance.
	 * @return a new instance of collection.
	 */
	<T> ConcurrentSet<T> newSet(Comparator<? super T> comparator, Collection<T> toCopy);

}

