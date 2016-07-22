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

import java.util.EventListener;

/**
 * Listener on changes in a {@link DMap}.
 *
 * @param <K> - type of the keys.
 * @param <V> - type of the values.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface DMapListener<K, V> extends EventListener {

	/**
	 * Invoked when an entry is added in the map.
	 *
	 * @param key - the added key.
	 * @param value - the added value.
	 */
	void entryAdded(K key, V value);

	/**
	 * Invoked when the value of an entry has changed.
	 *
	 * @param key - the removed key.
	 * @param value - the removed value.
	 */
	void entryUpdated(K key, V value);

	/**
	 * Invoked when an entry was removed the map.
	 *
	 * @param key - the removed key.
	 * @param value - the removed value.
	 */
	void entryRemoved(K key, V value);

	/**
	 * Invoked when the map was cleared.
	 *
	 * @param localClearing - indicates if the clear in local (<code>true</code>) or for all the nodes on the network (
	 *        <code>false</code>).
	 */
	void mapCleared(boolean localClearing);

}
