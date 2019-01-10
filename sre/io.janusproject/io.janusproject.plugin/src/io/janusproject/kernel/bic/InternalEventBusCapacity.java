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

import java.util.Collection;

import org.eclipse.xtext.xbase.lib.Functions.Function1;

import io.sarl.core.Destroy;
import io.sarl.core.Initialize;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;

/**
 * Capacity that provides an event bus to notify the different components of an agent.
 *
 * <p><strong>This capacity is provided by the Janus kernel, not SARL.</strong>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface InternalEventBusCapacity extends Capacity {

	/**
	 * Replies the state of the owner of the event bus.
	 *
	 * @return the state of the agent.
	 */
	OwnerState getOwnerState();

	/**
	 * Register the given object on the event bus for receiving any event.
	 *
	 * @param listener the listener on the SARL events.
	 * @deprecated see {@link #registerEventListener(Object, boolean, Function1, Object[])}.
	 */
	@Deprecated
	void registerEventListener(Object listener);

	/**
	 * Register the given object on the event bus for receiving any event.
	 *
	 * <p>If the filter is provided, it will be used for determining if the given behavior accepts a specific event.
	 * If the filter function replies {@code true} for a specific event as argument, the event is fired in the
	 * behavior context. If the filter function replies {@code false}, the event is not fired in the behavior context.
	 *
	 * @param listener the listener on the SARL events.
	 * @param fireInitializeEvent indicates if the {@code Initialize} event should be fired to the listener if the agent is alive.
	 * @param filter the filter function.
	 * @param initializationParameters the parameters to pass to the behavior for its initialization.
	 * @since 0.8
	 */
	void registerEventListener(Object listener, boolean fireInitializeEvent,
			Function1<? super Event, ? extends Boolean> filter,
			Object... initializationParameters);

	/**
	 * Unregister the given object on the event bus for receiving any event.
	 *
	 * @param listener the listener on the SARL events.
	 * @deprecated see {@link #unregisterEventListener(Object, boolean)}.
	 */
	@Deprecated
	void unregisterEventListener(Object listener);

	/**
	 * Unregister the given object on the event bus for receiving any event.
	 *
	 * @param listener the listener on the SARL events.
	 * @param fireDestroyEvent indicates if the {@code Destroy} event should be fired to the listener if the agent is alive.
	 * @since 0.5
	 */
	void unregisterEventListener(Object listener, boolean fireDestroyEvent);

	/**
	 * Replies if at least one event listener of the given type was registered.
	 *
	 * @param type the type of the expected event listener.
	 * @return <code>true</code> if one event listener of the given type is registered; otherwise <code>false</code>.
	 */
	boolean hasRegisteredEventListener(Class<?> type);

	/**
	 * Fill the given collection with the events listeners of the given type.
	 *
	 * @param <T> the type of the expected event listener.
	 * @param type the type of the expected event listener.
	 * @param collection the collection to fill.
	 * @return the number of added elements.
	 */
	<T> int getRegisteredEventListeners(Class<T> type, Collection<? super T> collection);

	/**
	 * Sends an event to itself using its defaultInnerAddress as source. Used for platform level event dispatching (i.e.
	 * {@link Initialize} and {@link Destroy})
	 *
	 * @param event event to propagate into the agent.
	 */
	void selfEvent(Event event);

	/**
	 * Replies the event listener linked to the owner of this capacity.
	 *
	 * @return the event listener of the owner of this skill.
	 */
	EventListener asEventListener();

	/**
	 * Replies the address of the agent in its inner default space.
	 *
	 * @return the address of the agent in its inner default space.
	 */
	Address getInnerDefaultSpaceAddress();

	/**
	 * Describe the states of the owner of an event bus.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	enum OwnerState {
		/**
		 * The owner of the event bus is unstarted: before initialization process.
		 */
		UNSTARTED {
			@Override
			public boolean isEventHandling() {
				return true;
			}
		},

		/**
		 * The owner of the event bus is under creation.
		 */
		INITIALIZING {
			@Override
			public boolean isEventHandling() {
				return true;
			}
		},

		/**
		 * The owner of the event bus is running.
		 */
		ALIVE {
			@Override
			public boolean isEventHandling() {
				return true;
			}
		},

		/**
		 * The owner of the event bus is under destruction.
		 */
		DYING {
			@Override
			public boolean isEventHandling() {
				return false;
			}
		},

		/**
		 * The owner of the event bus was destroyed.
		 */
		DEAD {
			@Override
			public boolean isEventHandling() {
				return false;
			}
		};

		/** Replies if the state accepts event handling.
		 *
		 * @return {@code true} if the state accept event handling.
		 * @since 0.5
		 */
		public abstract boolean isEventHandling();
	}

	/** Context aware implementation of the {@link InternalEventBusCapacity}.
	 *
	 * @param <C> type of the capacity.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.5
	 * @ExcludeFromApidoc
	 */
	class ContextAwareCapacityWrapper<C extends InternalEventBusCapacity>
			extends Capacity.ContextAwareCapacityWrapper<C>
			implements InternalEventBusCapacity {

		/** Constructor.
		 *
		 * @param capacity the original capacity implementation.
		 * @param caller the caller of the capacity.
		 */
		public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
			super(capacity, caller);
		}

		@Override
		public OwnerState getOwnerState() {
			try {
				ensureCallerInLocalThread();
				return this.capacity.getOwnerState();
			} finally {
				resetCallerInLocalThread();
			}
		}

		/** {@inheritDoc}.
		 * @deprecated see {@link #getRegisteredEventListeners(Class, Collection)}
		 */
		@Override
		@Deprecated
		public void registerEventListener(Object listener) {
			try {
				ensureCallerInLocalThread();
				this.capacity.registerEventListener(listener);
			} finally {
				resetCallerInLocalThread();
			}
		}

		@Override
		public void registerEventListener(Object listener, boolean fireInitializeEvent,
				Function1<? super Event, ? extends Boolean> filter,
				Object... initializationParameters) {
			try {
				ensureCallerInLocalThread();
				this.capacity.registerEventListener(listener, fireInitializeEvent, filter, initializationParameters);
			} finally {
				resetCallerInLocalThread();
			}
		}

		/** {@inheritDoc}.
		 * @deprecated see {@link #unregisterEventListener(Object, boolean)}
		 */
		@Override
		@Deprecated
		public void unregisterEventListener(Object listener) {
			try {
				ensureCallerInLocalThread();
				this.capacity.unregisterEventListener(listener);
			} finally {
				resetCallerInLocalThread();
			}
		}

		@Override
		public void unregisterEventListener(Object listener, boolean fireDestroyEvent) {
			try {
				ensureCallerInLocalThread();
				this.capacity.unregisterEventListener(listener, fireDestroyEvent);
			} finally {
				resetCallerInLocalThread();
			}
		}

		@Override
		public boolean hasRegisteredEventListener(Class<?> type) {
			try {
				ensureCallerInLocalThread();
				return this.capacity.hasRegisteredEventListener(type);
			} finally {
				resetCallerInLocalThread();
			}
		}

		@Override
		public <T> int getRegisteredEventListeners(Class<T> type, Collection<? super T> collection) {
			try {
				ensureCallerInLocalThread();
				return this.capacity.getRegisteredEventListeners(type, collection);
			} finally {
				resetCallerInLocalThread();
			}
		}

		@Override
		public void selfEvent(Event event) {
			try {
				ensureCallerInLocalThread();
				this.capacity.selfEvent(event);
			} finally {
				resetCallerInLocalThread();
			}
		}

		@Override
		public EventListener asEventListener() {
			try {
				ensureCallerInLocalThread();
				return this.capacity.asEventListener();
			} finally {
				resetCallerInLocalThread();
			}
		}

		@Override
		public Address getInnerDefaultSpaceAddress() {
			try {
				ensureCallerInLocalThread();
				return this.capacity.getInnerDefaultSpaceAddress();
			} finally {
				resetCallerInLocalThread();
			}
		}

	}

}
