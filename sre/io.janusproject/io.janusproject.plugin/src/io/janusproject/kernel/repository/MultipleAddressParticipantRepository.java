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

package io.janusproject.kernel.repository;

import java.io.Serializable;
import java.util.UUID;

import io.janusproject.services.distributeddata.DMultiMap;
import io.janusproject.services.distributeddata.DistributedDataStructureService;

import io.sarl.lang.core.EventListener;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.util.Collections3;

/**
 * Repository that maps participants to multiple addresses.
 *
 * <p>
 * This repository links the id of an entity to its various addresses in the related space.
 *
 * <p>
 * The repository must be distributed and synchronized all over the network by using data-structures that are provided by an
 * injected {@link DistributedDataStructureService}.
 *
 * @param <ADDRESST> - the generic type representing the address of a participant in the related space. This type must remains
 *        small, less than M in memory and must be {@link java.io.Serializable}
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class MultipleAddressParticipantRepository<ADDRESST extends Serializable> extends ParticipantRepository<ADDRESST> {

	/**
	 * Map linking the id of an entity to its various addresses in the related space. This map must be distributed and
	 * synchronized all over the network.
	 */
	private final DMultiMap<UUID, ADDRESST> participants;

	private final String distributedParticipantMapName;

	/**
	 * Constructs a <code>MultipleAddressParticipantRepository</code>.
	 *
	 * @param distributedParticipantMapName - name of the multimap over the network.
	 * @param repositoryImplFactory - factory that will be used to create the internal data structures.
	 */
	public MultipleAddressParticipantRepository(String distributedParticipantMapName,
			DistributedDataStructureService repositoryImplFactory) {
		super();
		this.distributedParticipantMapName = distributedParticipantMapName;
		this.participants = repositoryImplFactory.getMultiMap(this.distributedParticipantMapName, null);
	}

	/**
	 * Add a participant in this repository.
	 *
	 * @param address - address of a participant to insert in this repository.
	 * @param entity - participant to map to the given address.
	 * @return a.
	 */
	public ADDRESST registerParticipant(ADDRESST address, EventListener entity) {
		synchronized (mutex()) {
			addListener(address, entity);
			this.participants.put(entity.getID(), address);
		}
		return address;
	}

	/**
	 * Remove a participant from this repository.
	 *
	 * @param address - address of a participant to remove from this repository.
	 * @param entity - participant to unmap to the given address.
	 * @return a.
	 */
	public ADDRESST unregisterParticipant(ADDRESST address, EventListener entity) {
		synchronized (mutex()) {
			removeListener(address);
			this.participants.remove(entity.getID(), address);
		}
		return address;
	}

	/**
	 * Replies all the addresses of the participant with the given identifier.
	 *
	 * @param participant - the identifier of the participant.
	 * @return the collection of addresses. It may be <code>null</code> if the participant is unknown.
	 */
	public SynchronizedCollection<ADDRESST> getAddresses(UUID participant) {
		Object mutex = mutex();
		synchronized (mutex) {
			return Collections3.synchronizedCollection(this.participants.get(participant), mutex);
		}
	}

	/**
	 * Replies all the addresses in this repository.
	 *
	 * @return the collection of addresses.
	 */
	public SynchronizedCollection<ADDRESST> getParticipantAddresses() {
		Object mutex = mutex();
		synchronized (mutex) {
			return Collections3.synchronizedCollection(this.participants.values(), mutex);
		}
	}

	/**
	 * Replies all the participants in this repository.
	 *
	 * @return the collection of identifiers.
	 */
	public SynchronizedSet<UUID> getParticipantIDs() {
		Object mutex = mutex();
		synchronized (mutex) {
			return Collections3.synchronizedSet(this.participants.keySet(), mutex);
		}
	}

}
