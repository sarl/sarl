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

import java.io.Serializable;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.locks.ReadWriteLock;

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.core.EventListener;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.util.concurrent.Collections3;

/**
 * An abstract repository providing the basic support of storage a collection a participant's address and its related listener.
 *
 * @param <ADDRESST> - the generic type representing the address of a participant in the related space. This type must remains
 *        small, less than M in memory and must be {@link java.io.Serializable}
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class ParticipantRepository<ADDRESST extends Serializable> {

	/**
	 * Map linking the unique address of an entity in the related space to the entity itself. This is local non-distributed map.
	 */
	private final Map<ADDRESST, EventListener> participants;

	/**
	 * Construct a <code>ParticipantRepository</code>.
	 */
	protected ParticipantRepository() {
		this.participants = new TreeMap<>();
	}

	/**
	 * Replies the numbers of participants registered in this repository.
	 *
	 * @return the number of listeners.
	 */
	@Pure
	public int listenerCount() {
		final ReadWriteLock lock = getLock();
		lock.readLock().lock();
		try {
			return this.participants.size();
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Replies if there is no participant registered in this repository.
	 *
	 * @return <code>true</code> if the repository is empty, <code>false</code> if there is a least one participant.
	 */
	protected boolean isListenerEmpty() {
		final ReadWriteLock lock = getLock();
		lock.readLock().lock();
		try {
			return this.participants.isEmpty();
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Replies if the given address is present inside this repository.
	 *
	 * @param key key to search in the repository.
	 * @return <code>true</code> if the given key is found in the repository, <code>false</code> if the key is not present or
	 *         <code>null</code>.
	 */
	protected boolean containsAddress(ADDRESST key) {
		final ReadWriteLock lock = getLock();
		lock.readLock().lock();
		try {
			return this.participants.containsKey(key);
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Replies if the given participant is present inside this repository.
	 *
	 * @param value participant to search in the repository.
	 * @return <code>true</code> if the given participant is found in the repository, <code>false</code> if the participant is not
	 *         present or <code>null</code>.
	 */
	protected boolean containsListener(EventListener value) {
		final ReadWriteLock lock = getLock();
		lock.readLock().lock();
		try {
			return this.participants.containsValue(value);
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Replies the participant with the given address.
	 *
	 * @param key the address of the participant to retreive.
	 * @return the participant with the given address, or <code>null</code> if there is no participant with the given address.
	 */
	protected EventListener getListener(ADDRESST key) {
		final ReadWriteLock lock = getLock();
		lock.readLock().lock();
		try {
			return this.participants.get(key);
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Add a participant with the given address in this repository.
	 *
	 * @param key address of the participant.
	 * @param value is the participant to map to the address.
	 * @return the participant that was previously associated to the given address.
	 */
	protected EventListener addListener(ADDRESST key, EventListener value) {
		final ReadWriteLock lock = getLock();
		lock.writeLock().lock();
		try {
			return this.participants.put(key, value);
		} finally {
			lock.writeLock().unlock();
		}
	}

	/**
	 * Remove the mapping from the given address to the associated participant.
	 *
	 * @param key address of the participant to remove.
	 * @return the participant for which the address was removed, <code>null</code> if the given address was not found.
	 */
	protected EventListener removeListener(ADDRESST key) {
		final ReadWriteLock lock = getLock();
		lock.writeLock().lock();
		try {
			return this.participants.remove(key);
		} finally {
			lock.writeLock().unlock();
		}
	}

	/**
	 * Remove all the participants in this repository.
	 */
	protected void clearListeners() {
		final ReadWriteLock lock = getLock();
		lock.writeLock().lock();
		try {
			this.participants.clear();
		} finally {
			lock.writeLock().unlock();
		}
	}

	/**
	 * Replies all the addresses from the inside of this repository.
	 *
	 * @return the addresses in this repository.
	 */
	protected SynchronizedSet<ADDRESST> getAdresses() {
		final ReadWriteLock lock = getLock();
		lock.readLock().lock();
		try {
			return Collections3.synchronizedSet(this.participants.keySet(), lock);
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Replies all the participants from the inside of this repository.
	 *
	 * @return the participants.
	 */
	public SynchronizedCollection<EventListener> getListeners() {
		final ReadWriteLock lock = getLock();
		lock.readLock().lock();
		try {
			return Collections3.synchronizedCollection(this.participants.values(), lock);
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Replies the pairs of addresses and participants in this repository.
	 *
	 * @return the pairs of addresses and participants
	 */
	protected Set<Entry<ADDRESST, EventListener>> listenersEntrySet() {
		final ReadWriteLock lock = getLock();
		lock.readLock().lock();
		try {
			return Collections3.synchronizedSet(this.participants.entrySet(), lock);
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Replies the lock to synchronize on this repository.
	 *
	 * @return the lock.
	 */
	@Pure
	public abstract ReadWriteLock getLock();

}
