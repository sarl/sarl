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

package io.janusproject.kernel.bic;

import java.text.MessageFormat;
import java.util.Collection;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.google.common.base.Throwables;
import com.google.common.collect.Queues;
import com.google.inject.Inject;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

import io.janusproject.kernel.bic.internaleventdispatching.AgentInternalEventsDispatcher;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.spawn.SpawnService;

import io.sarl.core.AgentSpawned;
import io.sarl.core.Destroy;
import io.sarl.core.Initialize;
import io.sarl.core.Logging;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.Skill;
import io.sarl.lang.util.ClearableReference;

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
	private final AtomicReference<OwnerState> ownerState = new AtomicReference<>(OwnerState.UNSTARTED);

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

	/** Constructor.
	 * @param agent reference to the owner of this skill.
	 * @param addressInInnerDefaultSpace address of the owner of this skill in its inner default space.
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

	/** {@inheritDoc}
	 *
	 * @deprecated since 0.10
	 */
	@Override
	@Deprecated
	public int getInstallationOrder() {
		if (installationOrder < 0) {
			installationOrder = installationOrder(this);
		}
		return installationOrder;
	}

	@Override
	@Pure
	public void toString(ToStringBuilder builder) {
		super.toString(builder);
		builder.add("state", getOwnerState()); //$NON-NLS-1$
		builder.add("addressInDefaultspace", this.agentAddressInInnerDefaultSpace); //$NON-NLS-1$
	}

	@Override
	public OwnerState getOwnerState() {
		return this.ownerState.get();
	}

	/** Change the owner state.
	 *
	 * @param state the state.
	 */
	void setOwnerState(OwnerState state) {
		assert state != null;
		this.ownerState.set(state);
	}

	@Override
	public Address getInnerDefaultSpaceAddress() {
		return this.agentAddressInInnerDefaultSpace;

	}

	@Override
	protected void install() {
		this.eventDispatcher.register(getOwner(), null, null);
	}

	@Override
	protected void uninstall(UninstallationStage stage) {
		if (stage == UninstallationStage.POST_DESTROY_EVENT) {
			final Destroy event = new Destroy();
			event.setSource(getInnerDefaultSpaceAddress());
			final Agent owner = getOwner();
			this.eventDispatcher.unregisterAll(subscriber -> {
				// Behaviors are already destroyed by AgentLifeCycleSupport#agentDestroy
				if (subscriber != owner && !(subscriber instanceof Behavior)) {
					this.eventDispatcher.immediateDispatchTo(subscriber, event);
				}
			});
		}
	}

	/**
	 * {@inheritDoc}
	 * @deprecated see {@link #registerEventListener(Object, boolean, Function1, Object[])}.
	 */
	@Override
	@Deprecated
	public void registerEventListener(Object listener) {
		registerEventListener(listener, true, null);
	}

	@Override
	public void registerEventListener(Object listener, boolean fireInitializeEvent,
			Function1<? super Event, ? extends Boolean> filter,
			Object... initiallizationParameters) {
		if (fireInitializeEvent) {
			final OwnerState state = getOwnerState();
			if (state == OwnerState.INITIALIZING || state == OwnerState.ALIVE) {
				this.eventDispatcher.register(listener, filter, subscriber -> {
					final Initialize event = new Initialize(getOwner().getID(), initiallizationParameters);
					event.setSource(getInnerDefaultSpaceAddress());
					this.eventDispatcher.immediateDispatchTo(subscriber, event);
				});
			} else {
				this.eventDispatcher.register(listener, filter, null);
			}
		} else {
			this.eventDispatcher.register(listener, filter, null);
		}
	}

	/**
	 * {@inheritDoc}
	 * @deprecated see {@link #unregisterEventListener(Object, boolean)}.
	 */
	@Override
	@Deprecated
	public void unregisterEventListener(Object listener) {
		unregisterEventListener(listener, true);
	}

	@Override
	public void unregisterEventListener(Object listener, boolean fireDestroyEvent) {
		if (fireDestroyEvent) {
			final OwnerState state = getOwnerState();
			if (state == OwnerState.INITIALIZING || state == OwnerState.ALIVE) {
				this.eventDispatcher.unregister(listener, subscriber -> {
					final Destroy event = new Destroy();
					event.setSource(getInnerDefaultSpaceAddress());
					this.eventDispatcher.immediateDispatchTo(subscriber, event);
				});
			} else {
				this.eventDispatcher.unregister(listener, null);
			}
		} else {
			this.eventDispatcher.unregister(listener, null);
		}
	}

	/** This function runs the initialization of the agent.
	 *
	 * @param event the {@link Initialize} occurrence.
	 */
	private void runInitializationStage(Event event) {
		// Immediate synchronous dispatching of Initialize event
		try {
			setOwnerState(OwnerState.INITIALIZING);
			try {
				this.eventDispatcher.immediateDispatch(event);
			} finally {
				setOwnerState(OwnerState.ALIVE);
			}
			this.agentAsEventListener.fireEnqueuedEvents(this);
			if (this.agentAsEventListener.isKilled.get()) {
				this.agentAsEventListener.killOwner(InternalEventBusSkill.this);
			}
		} catch (Exception e) {
			// Log the exception
			final Logging loggingCapacity = getLoggingSkill();
			if (loggingCapacity != null) {
				loggingCapacity.error(Messages.InternalEventBusSkill_3, e);
			} else {
				final LogRecord record = new LogRecord(Level.SEVERE, Messages.InternalEventBusSkill_3);
				this.logger.getKernelLogger().log(
						this.logger.prepareLogRecord(record, this.logger.getKernelLogger().getName(),
								Throwables.getRootCause(e)));
			}
			// If we have an exception within the agent's initialization, we kill the agent.
			setOwnerState(OwnerState.ALIVE);
			// Asynchronous kill of the event.
			this.agentAsEventListener.killOrMarkAsKilled();
		}
	}

	/** This function runs the destruction of the agent.
	 *
	 * @param event the {@link Destroy} occurrence.
	 */
	private void runDestructionStage(Event event) {
		// Immediate synchronous dispatching of Destroy event
		try {
			setOwnerState(OwnerState.DYING);
			try {
				this.eventDispatcher.immediateDispatch(event);
			} finally {
				setOwnerState(OwnerState.DEAD);
			}
		} catch (Exception e) {
			// Log the exception
			final Logging loggingCapacity = getLoggingSkill();
			if (loggingCapacity != null) {
				loggingCapacity.error(Messages.InternalEventBusSkill_4, e);
			} else {
				final LogRecord record = new LogRecord(Level.SEVERE, Messages.InternalEventBusSkill_4);
				this.logger.getKernelLogger().log(
						this.logger.prepareLogRecord(record, this.logger.getKernelLogger().getName(),
								Throwables.getRootCause(e)));
			}
		}
	}

	@Override
	public boolean hasRegisteredEventListener(Class<?> type) {
		return this.eventDispatcher.hasRegisteredEventListener(type);
	}

	@Override
	public <T> int getRegisteredEventListeners(Class<T> type, Collection<? super T> collection) {
		return this.eventDispatcher.getRegisteredEventListeners(type, collection);
	}

	@Override
	public void selfEvent(Event event) {
		// If the event must be fired only by the
		// agent itself, it is treated in this function.
		// Otherwise, it is given to the asynchronous
		// listener.
		final Class<? extends Event> eventType = event.getClass();
		if (Initialize.class.equals(eventType)) {
			// Ensure that the event source is the agent itself!
			event.setSource(getInnerDefaultSpaceAddress());
			runInitializationStage(event);
		} else if (Destroy.class.equals(eventType)) {
			// Ensure that the event source is the agent itself!
			event.setSource(getInnerDefaultSpaceAddress());
			runDestructionStage(event);
		} else if (AsynchronousAgentKillingEvent.class.equals(eventType)) {
			// Asynchronous kill of the event.
			this.agentAsEventListener.killOrMarkAsKilled();
		} else if (getOwnerState().isEventHandling()) {
			// Ensure that the event source is the agent itself!
			event.setSource(getInnerDefaultSpaceAddress());
			// Asynchronous parallel dispatching of this event
			this.agentAsEventListener.receiveEvent(event);
		}
		//this.logger.debug(Messages.InternalEventBusSkill_0, event);
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

		/** Indicates if the agent has requested to be killed.
		 */
		final AtomicBoolean isKilled = new AtomicBoolean(false);

		private Queue<Event> buffer = Queues.newConcurrentLinkedQueue();

		private final UUID aid;

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
			if (AgentSpawned.class.equals(eventType) && ((AgentSpawned) event).agentIdentifiers.contains(this.aid)) {
				// This permits to ensure that the killing event
				// is correctly treated when fired from the initialization
				// handler.
				fireEnqueuedEvents(InternalEventBusSkill.this);
				if (this.isKilled.get()) {
					killOwner(InternalEventBusSkill.this);
					return;
				}
			}

			switch (getOwnerState()) {
			case UNSTARTED:
			case INITIALIZING:
				assert getOwnerState().isEventHandling();
				this.buffer.add(event);
				break;
			case ALIVE:
				assert getOwnerState().isEventHandling();
				fireEnqueuedEvents(InternalEventBusSkill.this);
				InternalEventBusSkill.this.eventDispatcher.asyncDispatch(event);
				break;
			case DYING:
			case DEAD:
				// Dropping messages since agent is dying
				assert !getOwnerState().isEventHandling();
				InternalEventBusSkill.this.logger.getKernelLogger().fine(MessageFormat.format(Messages.InternalEventBusSkill_1, event));
				break;
			default:
				throw new IllegalStateException();
			}

		}

		@SuppressWarnings("synthetic-access")
		void fireEnqueuedEvents(InternalEventBusSkill skill) {
			final Queue<Event> queue = this.buffer;
			if (queue != null && !queue.isEmpty()) {
				this.buffer = null;
				for (final Event evt : queue) {
					skill.eventDispatcher.asyncDispatch(evt);
				}
			}
		}

		@SuppressWarnings("synthetic-access")
		boolean killOwner(InternalEventBusSkill skill) {
			return skill.spawnService.killAgent(this.aid);
		}

		boolean killOrMarkAsKilled() {
			this.isKilled.set(true);
			final OwnerState state = getOwnerState();
			assert state != null;
			if (state == OwnerState.ALIVE) {
				return killOwner(InternalEventBusSkill.this);
			}
			return false;
		}

	}

}
