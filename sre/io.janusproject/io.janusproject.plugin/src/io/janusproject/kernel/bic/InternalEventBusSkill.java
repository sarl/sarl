/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.janusproject.kernel.bic;

import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.google.common.base.Throwables;
import com.google.common.collect.Queues;
import com.google.inject.Inject;

import io.janusproject.kernel.bic.internaleventdispatching.AgentInternalEventsDispatcher;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.spawn.SpawnService;
import io.janusproject.services.spawn.SpawnService.AgentKillException;

import io.sarl.core.AgentSpawned;
import io.sarl.core.Destroy;
import io.sarl.core.Initialize;
import io.sarl.core.Logging;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.ClearableReference;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.Skill;

/**
 * Janus implementation of an internal skill that provides an event dispatcher to notify the different components/behaviors of an
 * agent.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class InternalEventBusSkill extends BuiltinSkill implements InternalEventBusCapacity {

	private static int installationOrder = -1;

	/**
	 * State of the owner.
	 */
	private final AtomicReference<OwnerState> state = new AtomicReference<>(OwnerState.NEW);

	/**
	 * Implementation of an EventListener linked to the owner of this skill.
	 */
	private final AgentEventListener agentAsEventListener;

	/**
	 * Reference to the event dispatcher. It is the mean of routing of the events inside the context of an agent. The agent itself
	 * and the behaviors are connected to this dispatcher.
	 */
	@Inject
	private AgentInternalEventsDispatcher eventDispatcher;

	@Inject
	private LogService logger;

	@Inject
	private SpawnService spawnService;

	/**
	 * Address of the agent in the inner space.
	 */
	private final Address agentAddressInInnerDefaultSpace;

	private ClearableReference<Skill> skillBufferLogging;

	/**
	 * @param agent - reference to the owner of this skill.
	 * @param addressInInnerDefaultSpace - address of the owner of this skill in its inner default space.
	 */
	InternalEventBusSkill(Agent agent, Address addressInInnerDefaultSpace) {
		super(agent);
		this.agentAsEventListener = new AgentEventListener();
		this.agentAddressInInnerDefaultSpace = addressInInnerDefaultSpace;
	}

	/** Replies the Logging skill as fast as possible.
	 *
	 * @return the skill
	 */
	protected final Logging getLoggingSkill() {
		if (this.skillBufferLogging == null || this.skillBufferLogging.get() == null) {
			this.skillBufferLogging = $getSkill(Logging.class);
		}
		return $castSkill(Logging.class, this.skillBufferLogging);
	}

	@Override
	public int getInstallationOrder() {
		if (installationOrder < 0) {
			installationOrder = installationOrder(this);
		}
		return installationOrder;
	}

	@Override
	protected String attributesToString() {
		return super.attributesToString() + ", state = " + this.state //$NON-NLS-1$
				+ ", addressInDefaultspace = " + this.agentAddressInInnerDefaultSpace; //$NON-NLS-1$
	}

	@Override
	public OwnerState getOwnerState() {
		return this.state.get();
	}

	@Override
	public Address getInnerDefaultSpaceAddress() {
		return this.agentAddressInInnerDefaultSpace;

	}

	@Override
	protected void install() {
		this.eventDispatcher.register(getOwner());
	}

	@Override
	protected void uninstall() {
		this.eventDispatcher.unregisterAll();
	}

	@Override
	public void registerEventListener(Object listener) {
		this.eventDispatcher.register(listener);
	}

	@Override
	public void unregisterEventListener(Object listener) {
		this.eventDispatcher.unregister(listener);
	}

	@Override
	public void selfEvent(Event event) {
		// Ensure that the event source is the agent itself!
		event.setSource(getInnerDefaultSpaceAddress());
		// If the event must be fired only by the
		// agent itself, it is treated in this function.
		// Otherwise, it is given to the asynchronous
		// listener.
		final Class<? extends Event> eventType = event.getClass();
		if (Initialize.class.equals(eventType)) {
			// Immediate synchronous dispatching of Initialize event
			try {
				this.eventDispatcher.immediateDispatch(event);
				this.state.set(OwnerState.RUNNING);

			} catch (Exception e) {
				// Log the exception
				final Logging loggingCapacity = getLoggingSkill();
				if (loggingCapacity != null) {
					loggingCapacity.error(Messages.InternalEventBusSkill_3, e);
				} else {
					final LogRecord record = new LogRecord(Level.SEVERE, Messages.InternalEventBusSkill_3);
			        record.setThrown(Throwables.getRootCause(e));
					this.logger.log(record);
				}
				// If we have an exception within the agent's initialization, we kill the agent.
				this.state.set(OwnerState.RUNNING);
				// Asynchronous kill of the event.
				this.agentAsEventListener.killOrMarkAsKilled();
			}
		} else if (Destroy.class.equals(eventType)) {
			// Immediate synchronous dispatching of Destroy event
			synchronized (this.state) {
				this.state.set(OwnerState.DESTROYED);
			}
			this.eventDispatcher.immediateDispatch(event);
		} else if (AsynchronousAgentKillingEvent.class.equals(eventType)) {
			// Asynchronous kill of the event.
			this.agentAsEventListener.killOrMarkAsKilled();
		} else {
			// Asynchronous parallel dispatching of this event
			this.agentAsEventListener.receiveEvent(event);
		}
		this.logger.debug(Messages.InternalEventBusSkill_0, event);
	}

	@Override
	public final EventListener asEventListener() {
		return this.agentAsEventListener;
	}

	/**
	 * Definition of the listener on events on the agent's bus.
	 *
	 * @author $Author: srodriguez$
	 * @author $Author: ngaud$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class AgentEventListener implements EventListener {

		private Queue<Event> buffer = Queues.newConcurrentLinkedQueue();

		private final UUID aid;

		private final AtomicBoolean isKilled = new AtomicBoolean(false);

		@SuppressWarnings("synthetic-access")
		AgentEventListener() {
			this.aid = InternalEventBusSkill.this.getOwner().getID();
		}

		@Override
		public UUID getID() {
			return this.aid;
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void receiveEvent(Event event) {
			final Class<? extends Event> eventType = event.getClass();
			assert (!Initialize.class.equals(eventType)) && !Destroy.class.equals(eventType)
					&& !AsynchronousAgentKillingEvent.class.equals(eventType) : "Unsupported type of event: " + event; //$NON-NLS-1$
			if (AgentSpawned.class.equals(eventType) && this.aid.equals(((AgentSpawned) event).agentID)) {
				// This permits to ensure that the killing event
				// is correctly treated when fired from the initialization
				// handler.
				fireEnqueuedEvents(InternalEventBusSkill.this);
				if (this.isKilled.get()) {
					killOwner(InternalEventBusSkill.this);
					return;
				}
			}

			switch (InternalEventBusSkill.this.state.get()) {
			case NEW:
				this.buffer.add(event);
				break;
			case RUNNING:
				fireEnqueuedEvents(InternalEventBusSkill.this);
				InternalEventBusSkill.this.eventDispatcher.asyncDispatch(event);
				break;
			case DESTROYED:
				// Dropping messages since agent is dying
				InternalEventBusSkill.this.logger.debug(Messages.InternalEventBusSkill_1, event);
				break;
			default:
				throw new IllegalStateException();
			}

		}

		@SuppressWarnings("synthetic-access")
		private void fireEnqueuedEvents(InternalEventBusSkill skill) {
			final Queue<Event> queue = this.buffer;
			if (queue != null && !queue.isEmpty()) {
				this.buffer = null;
				for (final Event evt : queue) {
					skill.eventDispatcher.asyncDispatch(evt);
				}
			}
		}

		@SuppressWarnings("synthetic-access")
		private void killOwner(InternalEventBusSkill skill) {
			try {
				skill.spawnService.killAgent(this.aid);
			} catch (AgentKillException e) {
				skill.logger.error(Messages.InternalEventBusSkill_2, this.aid, e);
			}
		}

		@SuppressWarnings("synthetic-access")
		void killOrMarkAsKilled() {
			this.isKilled.set(true);
			final OwnerState state = InternalEventBusSkill.this.state.get();
			if (state != null && state != OwnerState.NEW) {
				killOwner(InternalEventBusSkill.this);
			}

		}

	}

}
