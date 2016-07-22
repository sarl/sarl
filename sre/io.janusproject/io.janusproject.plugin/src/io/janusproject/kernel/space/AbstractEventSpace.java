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

package io.janusproject.kernel.space;

import java.util.UUID;

import com.google.inject.Inject;
import io.janusproject.kernel.repository.UniqueAddressParticipantRepository;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.NetworkService;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.util.Collections3;
import io.sarl.util.Scopes;

/**
 * Abstract implementation of an event space.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractEventSpace extends SpaceBase {

	/**
	 * List of participants in this space. DO MISS TO BE SYNCHRONIZED ON THE PARTICIPANT REPOSITORY.
	 */
	protected final UniqueAddressParticipantRepository<Address> participants;

	/**
	 * Logging service.
	 */
	@Inject
	protected LogService logger;

	/**
	 * Executor service.
	 */
	@Inject
	protected ExecutorService executorService;

	/**
	 * Network service.
	 */
	@Inject
	private NetworkService network;

	/**
	 * Constructs an event space.
	 *
	 * @param id - identifier of the space.
	 * @param factory - factory that is used to create the internal data structure.
	 */
	public AbstractEventSpace(SpaceID id, DistributedDataStructureService factory) {
		super(id);
		this.participants = new UniqueAddressParticipantRepository<>(getID().getID().toString() + "-participants", //$NON-NLS-1$
				factory);
	}

	/**
	 * Replies the address associated to the given participant.
	 *
	 * @param entity - instance of a participant.
	 * @return the address of the participant with the given id.
	 */
	public final Address getAddress(EventListener entity) {
		return getAddress(entity.getID());
	}

	/**
	 * Replies the address associated with the given id.
	 *
	 * @param id - the identifier of the participant.
	 * @return the address.
	 */
	public Address getAddress(UUID id) {
		synchronized (this.participants) {
			return this.participants.getAddress(id);
		}
	}

	/**
	 * Emit the given event in the given scope.
	 *
	 * <p>
	 * This function emits on the internal event bus of the agent (call to {@link #doEmit(Event, Scope)}), and on the network.
	 *
	 * @param event - the event to emit.
	 * @param scope - description of the scope of the event, i.e. the receivers of the event.
	 * @see #emit(Event)
	 */
	public final void emit(Event event, Scope<Address> scope) {
		assert (event != null);
		assert (event.getSource() != null) : "Every event must have a source"; //$NON-NLS-1$
		assert this.getID().equals(event.getSource().getSpaceId()) : "The source address must belong to this space"; //$NON-NLS-1$

		try {
			this.network.publish(scope, event);
			doEmit(event, scope);
		} catch (Throwable e) {
			this.logger.error(AbstractEventSpace.class, "CANNOT_EMIT_EVENT", event, scope, e); //$NON-NLS-1$
		}

	}

	/**
	 * Emit the given event.
	 *
	 * <p>
	 * This function emits on the internal event bus of the agent (call to {@link #doEmit(Event, Scope)}), and on the network.
	 *
	 * @param event - the event to emit.
	 * @see #emit(Event, Scope)
	 */
	public final void emit(Event event) {
		emit(event, Scopes.<Address>allParticipants());
	}

	/**
	 * Do the emission of the event.
	 *
	 * <p>
	 * This function emits the event <strong>only on the internal event bus</strong> of the agents.
	 *
	 * @param event - the event to emit.
	 * @param scope - description of the scope of the event, i.e. the receivers of the event.
	 */
	protected void doEmit(Event event, Scope<? super Address> scope) {
		synchronized (this.participants) {
			for (EventListener agent : this.participants.getListeners()) {
				if (scope.matches(getAddress(agent))) {
					// TODO Verify the agent is still alive and running
					this.executorService.submit(new AsyncRunner(agent, event));
				}
			}
		}
	}

	@Override
	public SynchronizedSet<UUID> getParticipants() {
		synchronized (this.participants) {
			return Collections3.unmodifiableSynchronizedSet(this.participants.getParticipantIDs());
		}
	}

	@Override
	public String toString() {
		return getID().toString();
	}

	@SuppressWarnings("unchecked")
	@Override
	public void eventReceived(SpaceID space, Scope<?> scope, Event event) {
		try {
			AbstractEventSpace.this.doEmit(event, (Scope<Address>) scope);
		} catch (Exception e) {
			this.logger.error(AbstractEventSpace.class, "INVALID_EMIT", e); //$NON-NLS-1$
		}
	}

	/**
	 * Asynchronous runner.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class AsyncRunner implements Runnable {

		private final EventListener agent;

		private final Event event;

		/**
		 * Construct.
		 * 
		 * @param agent the agent listener.
		 * @param event the event.
		 */
		AsyncRunner(EventListener agent, Event event) {
			this.agent = agent;
			this.event = event;
		}

		@Override
		public void run() {
			this.agent.receiveEvent(this.event);
		}

		@Override
		public String toString() {
			return "[agent=" + this.agent + "; event=" + this.event + "]"; //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
		}

	}

}
