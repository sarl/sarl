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

package io.janusproject.util;

import io.janusproject.services.distributeddata.DMapListener;

/**
 * Abstract implementation of a view on a Map for a distributed map.
 *
 * @param <K> - type of the keys.
 * @param <V> - type of the values.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractMapView<K, V> {

	/**
	 * The listeners.
	 */
	protected final ListenerCollection<DMapListener<? super K, ? super V>> listeners = new ListenerCollection<>();

	/**
	 * Construct.
	 */
	public AbstractMapView() {
		//
	}

	/**
	 * Fire the addition event.
	 *
	 * @param key - the added key.
	 * @param value - the added value.
	 */
	@SuppressWarnings("unchecked")
	protected void fireEntryAdded(K key, V value) {
		if (this.listeners != null) {
			for (DMapListener<? super K, ? super V> listener : this.listeners.getListeners(DMapListener.class)) {
				listener.entryAdded(key, value);
			}
		}
	}

	/**
	 * Fire the removal event.
	 *
	 * @param key - the removed key.
	 * @param value - the removed value.
	 */
	@SuppressWarnings("unchecked")
	protected void fireEntryRemoved(K key, V value) {
		if (this.listeners != null) {
			for (DMapListener<? super K, ? super V> listener : this.listeners.getListeners(DMapListener.class)) {
				listener.entryRemoved(key, value);
			}
		}
	}

	/**
	 * Fire the update event.
	 *
	 * @param key - the updated key.
	 * @param value - the new value.
	 */
	@SuppressWarnings("unchecked")
	protected void fireEntryUpdated(K key, V value) {
		for (DMapListener<? super K, ? super V> listener : this.listeners.getListeners(DMapListener.class)) {
			listener.entryUpdated(key, value);
		}
	}

	/**
	 * Fire the clearing event.
	 *
	 * @param localClearing - indicates if the clearing is done on the local node (if <code>true</code>), or on all the nodes (if
	 *        <code>false</code>).
	 */
	@SuppressWarnings("unchecked")
	protected void fireCleared(boolean localClearing) {
		for (DMapListener<? super K, ? super V> listener : this.listeners.getListeners(DMapListener.class)) {
			listener.mapCleared(localClearing);
		}
	}

	/**
	 * Add a listener.
	 *
	 * @param listener - the listener.
	 */
	public void addDMapListener(DMapListener<? super K, ? super V> listener) {
		this.listeners.add(DMapListener.class, listener);
	}

	/**
	 * Remove a listener.
	 *
	 * @param listener - the listener.
	 */
	public void removeDMapListener(DMapListener<? super K, ? super V> listener) {
		this.listeners.remove(DMapListener.class, listener);
	}

}
