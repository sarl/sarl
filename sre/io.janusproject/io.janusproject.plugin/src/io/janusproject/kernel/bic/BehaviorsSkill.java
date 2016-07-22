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

package io.janusproject.kernel.bic;

import io.sarl.core.Behaviors;
import io.sarl.core.InnerContextAccess;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Skill;

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
class BehaviorsSkill extends Skill implements Behaviors {

	private final Address agentAddressInInnerDefaultSpace;

	/**
	 * @param agent - owner of this skill.
	 * @param agentAddressInInnerDefaultSpace - address of the owner of this skill in its inner default space.
	 */
	BehaviorsSkill(Agent agent, Address agentAddressInInnerDefaultSpace) {
		super(agent);
		this.agentAddressInInnerDefaultSpace = agentAddressInInnerDefaultSpace;
	}

	@Override
	public synchronized Behavior registerBehavior(Behavior attitude) {
		getSkill(InternalEventBusCapacity.class).registerEventListener(attitude);
		return attitude;
	}

	@Override
	public synchronized Behavior unregisterBehavior(Behavior attitude) {
		getSkill(InternalEventBusCapacity.class).unregisterEventListener(attitude);
		return attitude;
	}

	@Override
	public synchronized void wake(Event evt) {
		// Use the inner space so all behaviors (even agents inside the holon
		// running in distant kernels) are notified. The event will return into
		// the agent via the inner default space add call internalReceiveEvent
		// for real posting

		InnerContextAccess context = getSkill(InnerContextAccess.class);

		if ((!(context instanceof InnerContextSkill)) || ((InnerContextSkill) context).hasInnerContext()) {
			EventSpace defSpace = context.getInnerContext().getDefaultSpace();
			evt.setSource(defSpace.getAddress(getOwner().getID()));
			defSpace.emit(evt);
		} else {
			// Do not call getInnerContext(), which is creating the inner context automatically.
			// In place, try to send the event inside the agent only (and its behaviors).
			EventListener listener = getSkill(InternalEventBusCapacity.class).asEventListener();
			assert (listener != null);
			evt.setSource(this.agentAddressInInnerDefaultSpace);
			listener.receiveEvent(evt);
		}

	}

	@Override
	public EventListener asEventListener() {
		return getSkill(InternalEventBusCapacity.class).asEventListener();
	}

}
