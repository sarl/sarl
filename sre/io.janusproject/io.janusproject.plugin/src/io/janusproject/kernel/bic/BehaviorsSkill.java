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

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.xtext.xbase.lib.Functions.Function1;

import io.sarl.core.Behaviors;
import io.sarl.core.InnerContextAccess;
import io.sarl.core.Schedules;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.Skill;
import io.sarl.lang.util.ClearableReference;
import io.sarl.lang.util.SynchronizedIterable;
import io.sarl.util.concurrent.Collections3;
import io.sarl.util.concurrent.NoReadWriteLock;

/**
 * Janus implementation of SARL's {@link Behaviors} built-in capacity.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BehaviorsSkill extends BuiltinSkill implements Behaviors {

	private static int installationOrder = -1;

	private final Address agentAddressInInnerDefaultSpace;

	private ClearableReference<Skill> skillBufferInternalEventBusCapacity;

	private ClearableReference<Skill> skillBufferInnerContextAccess;

	private ClearableReference<Skill> skillBufferSchedules;

	/** Constructor.
	 * @param agent owner of this skill.
	 * @param agentAddressInInnerDefaultSpace address of the owner of this skill in its inner default space.
	 */
	BehaviorsSkill(Agent agent, Address agentAddressInInnerDefaultSpace) {
		super(agent);
		this.agentAddressInInnerDefaultSpace = agentAddressInInnerDefaultSpace;
	}

	/** Replies the InternalEventBusCapacity skill as fast as possible.
	 *
	 * @return the skill
	 */
	protected final InternalEventBusCapacity getInternalEventBusCapacitySkill() {
		if (this.skillBufferInternalEventBusCapacity == null || this.skillBufferInternalEventBusCapacity.get() == null) {
			this.skillBufferInternalEventBusCapacity = $getSkill(InternalEventBusCapacity.class);
		}
		return $castSkill(InternalEventBusCapacity.class, this.skillBufferInternalEventBusCapacity);
	}

	/** Replies the InnerContextAccess skill as fast as possible.
	 *
	 * @return the skill
	 */
	protected final InnerContextAccess getInnerContextAccessSkill() {
		if (this.skillBufferInnerContextAccess == null || this.skillBufferInnerContextAccess.get() == null) {
			this.skillBufferInnerContextAccess = $getSkill(InnerContextAccess.class);
		}
		return $castSkill(InnerContextAccess.class, this.skillBufferInnerContextAccess);
	}

	/** Replies the Schedules skill as fast as possible.
	 *
	 * @return the skill
	 */
	protected final SchedulesSkill getSchedulesSkill() {
		if (this.skillBufferSchedules == null || this.skillBufferSchedules.get() == null) {
			this.skillBufferSchedules = $getSkill(Schedules.class);
		}
		return $castSkill(SchedulesSkill.class, this.skillBufferSchedules);
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
	public boolean hasRegisteredBehavior() {
		return getSkill(InternalEventBusCapacity.class).hasRegisteredEventListener(Behavior.class);
	}

	@Override
	public SynchronizedIterable<Behavior> getRegisteredBehaviors() {
		final Collection<Behavior> behaviors = new ArrayList<>();
		getSkill(InternalEventBusCapacity.class).getRegisteredEventListeners(Behavior.class, behaviors);
		return Collections3.unmodifiableSynchronizedIterable(behaviors, NoReadWriteLock.SINGLETON);
	}

	@Override
	public Behavior registerBehavior(Behavior attitude, Function1<? super Event, ? extends Boolean> filter,
			Object... initializationParameters) {
		getInternalEventBusCapacitySkill().registerEventListener(attitude, true, filter, initializationParameters);
		return attitude;
	}

	@Override
	public synchronized Behavior unregisterBehavior(Behavior attitude) {
		getSchedulesSkill().unregisterTasksForBehavior(attitude);
		getInternalEventBusCapacitySkill().unregisterEventListener(attitude, true);
		return attitude;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void wake(Event evt) {
		wake(evt, $DEFAULT_VALUE$WAKE_0);
	}

	@Override
	public synchronized void wake(Event evt, Scope<Address> scope) {
		// Use the inner space so all behaviors (even agents inside the holon
		// running in distant kernels) are notified. The event will return into
		// the agent via the inner default space add call internalReceiveEvent
		// for real posting

		final InnerContextAccess context = getInnerContextAccessSkill();

		if ((!(context instanceof InnerContextSkill)) || ((InnerContextSkill) context).hasInnerContext()) {
			final EventSpace defSpace = context.getInnerContext().getDefaultSpace();
			defSpace.emit(getID(), evt, scope);
		} else {
			// Do not call getInnerContext(), which is creating the inner context automatically.
			// In place, try to send the event inside the agent only (and its behaviors).
			final InternalEventBusCapacity eventBus = getSkill(InternalEventBusCapacity.class);
			if (scope == null || scope.matches(eventBus.getInnerDefaultSpaceAddress())) {
				final EventListener listener = eventBus.asEventListener();
				assert listener != null;
				evt.setSource(this.agentAddressInInnerDefaultSpace);
				listener.receiveEvent(evt);
			}
		}

	}

	@Override
	public EventListener asEventListener() {
		return getInternalEventBusCapacitySkill().asEventListener();
	}

}
