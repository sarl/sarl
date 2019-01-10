/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import java.util.Set;

/** A set that is synchronized (thread-safe).
 *
 * <p>All the operations on the set are synchronized with an internal mutex,
 * except <code>iterator()</code>.
 *
 * <p>The iterator replies by this set must be synchronized by hand by
 * the user of the iterator: <pre><code>
 * SynchronizedSet&lt;Object&gt; c;
 * synchronized(c.mutex()) {
 *   Iterator&lt;Object&gt; iterator = c.iterator();
 *   while (iterator.hasNext()) {
 *     Object element = iterator.next();
 *     // Do something with the element
 *   }
 * }
 * </code></pre>
 *
 * @param <E> - type of the elements in the set.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface SynchronizedSet<E> extends SynchronizedIterable<E>, Set<E> {
	//
}
