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

package io.janusproject.kernel.space;

import java.util.concurrent.locks.ReadWriteLock;

import com.google.inject.Inject;
import com.google.inject.Provider;

import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.distributeddata.DistributedDataStructureService;

import io.sarl.core.OpenEventSpace;
import io.sarl.core.ParticipantJoined;
import io.sarl.core.ParticipantLeft;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.SpaceID;

/**
 * Default implementation of an event space.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class EventSpaceImpl extends AbstractEventSpace implements OpenEventSpace {

	private final ContextSpaceService contextRepository;

	/**
	 * Constructs an event space.
	 *
	 * @param id identifier of the space.
	 * @param factory factory that is used to create the internal data structure.
	 * @param contextRepository service for accessing the repository of contexts.
	 * @param lockProvider a provider of synchronization locks.
	 */
	@Inject
	public EventSpaceImpl(SpaceID id, DistributedDataStructureService factory, ContextSpaceService contextRepository,
			Provider<ReadWriteLock> lockProvider) {
		super(id, factory, lockProvider);
		this.contextRepository = contextRepository;
	}

	@Override
	public Address register(EventListener entity) {
		final Address a = getParticipantInternalDataStructure().registerParticipant(new Address(getSpaceID(), entity.getID()), entity);
		fireParticipantJoined(a);
		return a;
	}

	@Override
	public Address unregister(EventListener entity) {
		final Address a = getParticipantInternalDataStructure().unregisterParticipant(entity);
		fireParticipantLeft(a);
		return a;
	}

	/**
	 * Fires an {@link ParticipantJoined} event into the default space of the current Context to notify other context's members
	 * that a new agent joined this space.
	 * @param newAgentAddress - the address of the agent.
	 */
	protected final void fireParticipantJoined(Address newAgentAddress) {
		final AgentContext enclosingContext = this.contextRepository.getContext(newAgentAddress.getSpaceID().getContextID());
		final EventSpace defSpace = enclosingContext.getDefaultSpace();
		defSpace.emit(
				// No need to give an event source because the event's source is explicitly set below.
				null,
				new ParticipantJoined(new Address(defSpace.getSpaceID(), newAgentAddress.getUUID()),
						newAgentAddress.getSpaceID()),
						it -> !it.getUUID().equals(newAgentAddress.getUUID()));
	}

	/**
	 * Fires an {@link ParticipantLeft} event into the default space of the current Context to notify other context's members
	 * that an agent left this space.
	 * @param agentAddress - address of the agent leaving the space.
	 */
	protected final void fireParticipantLeft(Address agentAddress) {
		final AgentContext enclosingContext = this.contextRepository.getContext(agentAddress.getSpaceID().getContextID());
		final EventSpace defSpace = enclosingContext.getDefaultSpace();
		//Since this agent may already have quit the default space at the moment this event is sent,
		//it is mandatory to recreate an address to be sure the event has a source
		defSpace.emit(
				// No need to give an event source because the event's source is explicitly set below.
				null,
				new ParticipantLeft(new Address(defSpace.getSpaceID(), agentAddress.getUUID()),
									agentAddress.getSpaceID()),
									it -> !it.getUUID().equals(agentAddress.getUUID()));
	}

}
