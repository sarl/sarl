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

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentSkipListSet;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.EventListener;

/**
 * A repository of participants specific to a given space.
 *
 * <p>This repository links the id of an entity to its various addresses in the
 * related space.
 *
 * <p>The repository must be distributed and synchronized all over the network by
 * using data-structures that are provided by an injected
 * {@link DistributedDataStructureService}.
 *
 * <p>This class is thread-safe.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class UniqueAddressParticipantRepository extends ParticipantRepository {

	/**
	 * TODO Look for definitely removing this map and use a single collection and
	 * thus remove all synchronized in this class Map linking the id of an entity to
	 * its unique address in the related space. This map must be distributed and
	 * synchronized all over the network
	 */
	private final Map<UUID, Address> participants;

	private final String distributedParticipantMapName;

	/**
	 * Constructs a <code>UniqueAddressParticipantRepository</code>.
	 *
	 * @param distributedParticipantMapName name of the multimap over the network.
	 * @param repositoryImplFactory         factory that will be used to create the
	 *                                      internal data structures.
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
	 * @param address the address of the participant
	 * @param entity  the entity associated to the specified address
	 * @return the address of the participant
	 */
	public Address registerParticipant(Address address, EventListener entity) {
		synchronized (this) {
			addListener(address, entity);
			this.participants.put(entity.getID(), address);
		}
		return address;
	}

	/**
	 * Remove a participant from this repository.
	 *
	 * @param entity participant to remove from this repository.
	 * @return the address that was mapped to the given participant.
	 */
	@Inline("unregisterParticipant(($1).getID())")
	public Address unregisterParticipant(EventListener entity) {
		return unregisterParticipant(entity.getID());
	}

	/**
	 * Remove a participant with the given ID from this repository.
	 *
	 * @param entityID identifier of the participant to remove from this repository.
	 * @return the address that was mapped to the given participant.
	 */
	public Address unregisterParticipant(UUID entityID) {
		synchronized (this) {
			final Address adr = this.participants.get(entityID);
			removeListener(adr);
			this.participants.remove(entityID);
			return adr;
		}
	}

	/**
	 * Replies the address associated to the given participant.
	 *
	 * @param entity instance of a participant.
	 * @return the address of the participant with the given id.
	 */
	@Pure
	@Inline("getAddress(($1).getID())")
	public Address getAddress(EventListener entity) {
		return getAddress(entity.getID());
	}

	/**
	 * Replies the address associated to the participant with the given identifier.
	 *
	 * @param id identifier of the participant to retreive.
	 * @return the address of the participant with the given id.
	 */
	@Pure
	public Address getAddress(UUID id) {
		return this.participants.get(id);
	}

	/**
	 * Replies all the addresses of the participants that ar einside this
	 * repository.
	 *
	 * @return all the addresses.
	 */
	/*@Pure
	public Collection<Address> getParticipantAddresses() {
			return this.participants.values()
	}*/

	/**
	 * Replies the identifiers of all the participants in this repository.
	 *
	 * @return all the identifiers.
	 */
	@Pure
	public ConcurrentSkipListSet<UUID> getParticipantIDs() {
		return new ConcurrentSkipListSet<>(this.participants.keySet());
	}

}
