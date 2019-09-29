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

import java.text.MessageFormat;
import java.util.UUID;
import java.util.concurrent.locks.ReadWriteLock;

import com.google.inject.Inject;
import com.google.inject.Provider;

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
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.util.Scopes;
import io.sarl.util.concurrent.Collections3;

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
	 * List of participants in this space.
	 */
	private final UniqueAddressParticipantRepository<Address> participants;

	/**
	 * Constructs an event space.
	 *
	 * @param id identifier of the space.
	 * @param factory factory that is used to create the internal data structure.
	 * @param lockProvider a provider of synchronization locks.
	 */
	public AbstractEventSpace(SpaceID id, DistributedDataStructureService factory,
			Provider<ReadWriteLock> lockProvider) {
		super(id);
		this.participants = new UniqueAddressParticipantRepository<>(getSpaceID().getID().toString() + "-participants", //$NON-NLS-1$
				factory, lockProvider);
	}

	/** Replies the internal datastructure that stores the participants to this space.
	 *
	 * @return the internal data structure.
	 */
	protected UniqueAddressParticipantRepository<Address> getParticipantInternalDataStructure() {
		return this.participants;
	}

	/**
	 * Replies the address associated to the given participant.
	 *
	 * @param entity instance of a participant.
	 * @return the address of the participant with the given id.
	 */
	public final Address getAddress(EventListener entity) {
		return getAddress(entity.getID());
	}

	/**
	 * Replies the address associated with the given id.
	 *
	 * @param id the identifier of the participant.
	 * @return the address.
	 */
	public Address getAddress(UUID id) {
		return getParticipantInternalDataStructure().getAddress(id);
	}

	/**
	 * Emit the given event in the given scope.
	 *
	 * <p>This function emits on the internal event bus of the agent (call to {@link #doEmit(Event, Scope)}), and on the network.
	 *
	 * @param eventSource the source of the event.
	 * @param event the event to emit.
	 * @param scope description of the scope of the event, i.e. the receivers of the event.
	 * @since 2.0.6.0
	 */
	public final void emit(UUID eventSource, Event event, Scope<Address> scope) {
		assert event != null;
		ensureEventSource(eventSource, event);
		assert getSpaceID().equals(event.getSource().getSpaceID()) : "The source address must belong to this space"; //$NON-NLS-1$
		try {
			final Scope<Address> scopeInstance = (scope == null) ? Scopes.<Address>allParticipants() : scope;
			try {
				this.network.publish(scopeInstance, event);
			} catch (Throwable e) {
				this.logger.getKernelLogger().severe(MessageFormat.format(Messages.AbstractEventSpace_2, event, scope, e));
			}
			doEmit(event, scopeInstance);
		} catch (Throwable e) {
			this.logger.getKernelLogger().severe(MessageFormat.format(Messages.AbstractEventSpace_0, event, scope, e));
		}

	}

	/** Ensure that the given event has a source.
	 *
	 * @param eventSource the source of the event.
	 * @param event the event to emit.
	 * @since 2.0.6.0
	 */
	protected void ensureEventSource(UUID eventSource, Event event) {
		if (event.getSource() == null) {
			if (eventSource != null) {
				event.setSource(new Address(getSpaceID(), eventSource));
			} else {
				throw new AssertionError("Every event must have a source"); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Do the emission of the event.
	 *
	 * <p>This function emits the event <strong>only on the internal event bus</strong> of the agents.
	 *
	 * @param event the event to emit.
	 * @param scope description of the scope of the event, i.e. the receivers of the event.
	 */
	protected void doEmit(Event event, Scope<? super Address> scope) {
		assert scope != null;
		assert event != null;
		final UniqueAddressParticipantRepository<Address> particips = getParticipantInternalDataStructure();
		final SynchronizedCollection<EventListener> listeners = particips.getListeners();
		final ReadWriteLock lock = listeners.getLock();
		lock.readLock().lock();
		try {
			for (final EventListener listener : listeners) {
				final Address adr = getAddress(listener);
				if (scope.matches(adr)) {
					this.executorService.submit(new AsyncRunner(listener, event));
				}
			}
		} finally {
			lock.readLock().unlock();
		}
	}

	@Override
	public SynchronizedSet<UUID> getParticipants() {
		return Collections3.unmodifiableSynchronizedSet(getParticipantInternalDataStructure().getParticipantIDs());
	}

	@Override
	public String toString() {
		return getSpaceID().toString();
	}

	@SuppressWarnings("unchecked")
	@Override
	public void eventReceived(SpaceID space, Scope<?> scope, Event event) {
		try {
			AbstractEventSpace.this.doEmit(event, (Scope<Address>) scope);
		} catch (Exception e) {
			this.logger.getKernelLogger().severe(MessageFormat.format(Messages.AbstractEventSpace_1, e));
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
