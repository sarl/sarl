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

package io.janusproject.kernel.repository;

import java.util.Map.Entry;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.EventListener;

/**
 * An abstract repository providing the basic support of storage a collection a
 * participant's address and its related listener.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class ParticipantRepository {

	/**
	 * TODO change the key from address to UUID to enable the removal of the second in the unique repo and thus create EventSpaceRepository
	 * Map linking the unique address of an entity in the related space to the
	 * entity itself. This is local non-distributed map.
	 */
	private final ConcurrentSkipListMap<Address, EventListener> participants;

	/**
	 * Construct a <code>ParticipantRepository</code>.
	 */
	protected ParticipantRepository() {
		this.participants = new ConcurrentSkipListMap<>();
	}

	/**
	 * Replies the numbers of participants registered in this repository.
	 *
	 * @return the number of listeners.
	 */
	@Pure
	public int listenerCount() {
		return this.participants.size();

	}

	/**
	 * Replies if there is no participant registered in this repository.
	 *
	 * @return <code>true</code> if the repository is empty, <code>false</code> if
	 *         there is a least one participant.
	 */
	protected boolean isListenerEmpty() {
		return this.participants.isEmpty();
	}

	/**
	 * Replies if the given address is present inside this repository.
	 *
	 * @param key key to search in the repository.
	 * @return <code>true</code> if the given key is found in the repository,
	 *         <code>false</code> if the key is not present or <code>null</code>.
	 */
	protected boolean containsAddress(Address key) {
		return this.participants.containsKey(key);
	}

	/**
	 * Replies if the given participant is present inside this repository.
	 *
	 * @param value participant to search in the repository.
	 * @return <code>true</code> if the given participant is found in the
	 *         repository, <code>false</code> if the participant is not present or
	 *         <code>null</code>.
	 */
	protected boolean containsListener(EventListener value) {
		return this.participants.containsValue(value);
	}

	/**
	 * Replies the participant with the given address.
	 *
	 * @param key the address of the participant to retreive.
	 * @return the participant with the given address, or <code>null</code> if there
	 *         is no participant with the given address.
	 */
	protected EventListener getListener(Address key) {
		return this.participants.get(key);
	}

	/**
	 * Add a participant with the given address in this repository.
	 *
	 * @param key   address of the participant.
	 * @param value is the participant to map to the address.
	 * @return the participant that was previously associated to the given address.
	 */
	protected EventListener addListener(Address key, EventListener value) {
		return this.participants.put(key, value);

	}

	/**
	 * Remove the mapping from the given address to the associated participant.
	 *
	 * @param key address of the participant to remove.
	 * @return the participant for which the address was removed, <code>null</code>
	 *         if the given address was not found.
	 */
	protected EventListener removeListener(Address key) {
		return this.participants.remove(key);
	}

	/**
	 * Remove all the participants in this repository.
	 */
	protected void clearListeners() {
		this.participants.clear();
	}

	/**
	 * Replies all the addresses from the inside of this repository.
	 *
	 * @return the addresses in this repository.
	 */
	protected ConcurrentSkipListSet<Address> getAdresses() {
		return new ConcurrentSkipListSet<>(this.participants.keySet());
	}

	/**
	 * Replies all the participants from the inside of this repository.
	 *
	 * @return the participants.
	 */
	public LinkedBlockingQueue<EventListener> getListeners() {
		return new LinkedBlockingQueue<>(this.participants.values());
	}

	/**
	 * Replies the pairs of addresses and participants in this repository.
	 *
	 * @return the pairs of addresses and participants
	 */
	protected ConcurrentSkipListSet<Entry<Address, EventListener>> listenersEntrySet() {
		return new ConcurrentSkipListSet<>(this.participants.entrySet());
	}

}
