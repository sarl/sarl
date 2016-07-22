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
import java.util.Map;
import java.util.UUID;

import io.janusproject.services.distributeddata.DistributedDataStructureService;

import io.sarl.lang.core.EventListener;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.util.Collections3;

/**
 * A repository of participants specific to a given space.
 *
 * <p>
 * This repository links the id of an entity to its various addresses in the related space.
 *
 * <p>
 * The repository must be distributed and synchronized all over the network by using data-structures that are provided by an
 * injected {@link DistributedDataStructureService}.
 *
 * @param <ADDRESST> - the generic type representing the address of a participant in the related space. This type must remains
 *        small, less than M in memory and must be {@link java.io.Serializable}.
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class UniqueAddressParticipantRepository<ADDRESST extends Serializable> extends ParticipantRepository<ADDRESST> {

	/**
	 * Map linking the id of an entity to its unique address in the related space. This map must be distributed and synchronized
	 * all over the network
	 */
	private final Map<UUID, ADDRESST> participants;

	private final String distributedParticipantMapName;

	/**
	 * Constructs a <code>UniqueAddressParticipantRepository</code>.
	 *
	 * @param distributedParticipantMapName - name of the multimap over the network.
	 * @param repositoryImplFactory - factory that will be used to create the internal data structures.
	 */
	public UniqueAddressParticipantRepository(String distributedParticipantMapName,
			DistributedDataStructureService repositoryImplFactory) {
		super();
		this.distributedParticipantMapName = distributedParticipantMapName;
		this.participants = repositoryImplFactory.getMap(this.distributedParticipantMapName, null);
	}

	/**
	 * Registers a new participant in this repository.
	 * 
	 * @param address - the address of the participant
	 * @param entity - the entity associated to the specified address
	 * @return the address of the participant
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
	 * @param entity - participant to remove from this repository.
	 * @return the address that was mapped to the given participant.
	 */
	public ADDRESST unregisterParticipant(EventListener entity) {
		return unregisterParticipant(entity.getID());
	}

	/**
	 * Remove a participant with the given ID from this repository.
	 *
	 * @param entityID - identifier of the participant to remove from this repository.
	 * @return the address that was mapped to the given participant.
	 */
	public ADDRESST unregisterParticipant(UUID entityID) {
		synchronized (mutex()) {
			removeListener(this.participants.get(entityID));
			return this.participants.remove(entityID);
		}
	}

	/**
	 * Replies the address associated to the given participant.
	 *
	 * @param entity - instance of a participant.
	 * @return the address of the participant with the given id.
	 */
	public ADDRESST getAddress(EventListener entity) {
		return getAddress(entity.getID());
	}

	/**
	 * Replies the address associated to the participant with the given identifier.
	 *
	 * @param id - identifier of the participant to retreive.
	 * @return the address of the participant with the given id.
	 */
	public ADDRESST getAddress(UUID id) {
		synchronized (mutex()) {
			return this.participants.get(id);
		}
	}

	/**
	 * Replies all the addresses of the participants that ar einside this repository.
	 *
	 * @return all the addresses.
	 */
	public SynchronizedCollection<ADDRESST> getParticipantAddresses() {
		Object mutex = mutex();
		synchronized (mutex) {
			return Collections3.synchronizedCollection(this.participants.values(), mutex);
		}
	}

	/**
	 * Replies the identifiers of all the participants in this repository.
	 *
	 * @return all the identifiers.
	 */
	public SynchronizedSet<UUID> getParticipantIDs() {
		Object mutex = mutex();
		synchronized (mutex) {
			return Collections3.synchronizedSet(this.participants.keySet(), mutex);
		}
	}

}
