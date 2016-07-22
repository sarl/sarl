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

import com.google.common.collect.Multimap;

/**
 * Interface that represents a distributed multi-map.
 *
 * <p>
 * A multi-map is a map that is associating a key to multiple values.
 *
 * @param <K> the type of the keys.
 * @param <V> the type of the values.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface DMultiMap<K, V> extends Multimap<K, V> {

	/**
	 * Replies the name of the multimap.
	 *
	 * @return the name of the multimap.
	 */
	String getName();

	/**
	 * Replies if changes to the returned collections will update the underlying map, and vice versa. However, changes to the
	 * returned collection are not possible.
	 *
	 * @return <code>true</code> if the changes are applied to the underlying map, otherwise <code>false</code>.
	 */
	boolean isBackedCollection();

	/**
	 * Returns number of values matching to given key in the multimap.
	 *
	 * <p>
	 * <b>Warning:</b>
	 *
	 * <p>
	 * This method uses <tt>hashCode</tt> and <tt>equals</tt> of binary form of the <tt>key</tt>, not the actual implementations
	 * of <tt>hashCode</tt> and <tt>equals</tt> defined in <tt>key</tt>'s class.
	 *
	 * @param key the key whose values count are to be returned
	 * @return number of values matching to given key in the multimap.
	 */
	int valueCount(K key);

	/**
	 * Add listener on events on the DMultiMap.
	 *
	 * @param listener - the listener
	 */
	void addDMapListener(DMapListener<? super K, ? super V> listener);

	/**
	 * Remove listener on events on the DMultiMap.
	 *
	 * @param listener - the listener
	 */
	void removeDMapListener(DMapListener<? super K, ? super V> listener);

}
