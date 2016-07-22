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

import java.util.Map;

/**
 * Interface that represents a distributed map.
 *
 * @param <K> the type of the keys.
 * @param <V> the type of the values.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface DMap<K, V> extends Map<K, V> {

	/**
	 * Replies the name of the map.
	 *
	 * @return the name of the map.
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
	 * Add listener on events on the DMap.
	 *
	 * @param listener - the listener
	 */
	void addDMapListener(DMapListener<? super K, ? super V> listener);

	/**
	 * Remove listener on events on the DMap.
	 *
	 * @param listener - the listener
	 */
	void removeDMapListener(DMapListener<? super K, ? super V> listener);

}
