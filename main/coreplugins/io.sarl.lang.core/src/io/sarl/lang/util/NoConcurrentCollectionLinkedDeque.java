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
import java.util.LinkedList;

/** Represent a collection of objects which is not thread-safe.
 *
 * @param <T> the type of the objects in the collection.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
class NoConcurrentCollectionLinkedDeque<T> extends LinkedList<T> implements ConcurrentCollection<T> {

	private static final long serialVersionUID = -8579091213574817297L;

    /**
     * Constructs an empty deque.
     */
    NoConcurrentCollectionLinkedDeque() {
        super();
    }

    /**
     * Constructs a deque initially containing the elements of
     * the given collection, added in traversal order of the
     * collection's iterator.
     *
     * @param source the collection of elements to initially contain
     * @throws NullPointerException if the specified collection or any
     *         of its elements are null
     */
    NoConcurrentCollectionLinkedDeque(Collection<? extends T> source) {
    	super(source);
    }

}
