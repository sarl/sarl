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

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import com.google.inject.Inject;
import io.janusproject.services.contextspace.ContextSpaceService;

import io.sarl.core.InnerContextAccess;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.util.Collections3;
import io.sarl.util.OpenEventSpace;

/**
 * Janus implementation of SARL's {@link InnerContextSkill} built-in capacity.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class InnerContextSkill extends Skill implements InnerContextAccess {

	private final Address agentAddressInInnerDefaultSpace;

	/**
	 * Context inside the agent.
	 */
	private AgentContext innerContext;

	@Inject
	private ContextSpaceService contextService;

	/**
	 * @param agent - owner of this skill.
	 * @param agentAddressInInnerDefaultSpace - address of the owner of this skill in its default space.
	 */
	InnerContextSkill(Agent agent, Address agentAddressInInnerDefaultSpace) {
		super(agent);
		this.agentAddressInInnerDefaultSpace = agentAddressInInnerDefaultSpace;
	}

	/**
	 * Replies if the inner context was instanciated. To create the inner context, call {@link #getInnerContext()}
	 *
	 * @return <code>true</code> if an instance of inner context exists, otherwise <code>false</code>.
	 */
	synchronized boolean hasInnerContext() {
		return this.innerContext != null;
	}

	/**
	 * Force to reset the inner context. This function does not update the context repository.
	 *
	 * <p>
	 * Do not call this function, exception if you are sure that the setting of the inner context to <code>null</code> only does
	 * not introduce problems.
	 */
	synchronized void resetInnerContext() {
		this.innerContext = null;
	}

	@Override
	protected String attributesToString() {
		return super.attributesToString() + ", addressInDefaultspace = " + this.agentAddressInInnerDefaultSpace; //$NON-NLS-1$
	}

	@Override
	protected void uninstall() {
		AgentContext context = this.innerContext;
		this.innerContext = null;
		if (context != null) {
			// Unregister the agent from the default space
			EventListener listener = getSkill(InternalEventBusCapacity.class).asEventListener();
			((OpenEventSpace) context.getDefaultSpace()).unregister(listener);
			// Destroy the context
			this.contextService.removeContext(context);
		}
	}

	@Override
	public synchronized AgentContext getInnerContext() {
		if (this.innerContext == null) {
			// Create the inner context.
			this.innerContext = this.contextService.createContext(
					this.agentAddressInInnerDefaultSpace.getSpaceId().getContextID(),
					this.agentAddressInInnerDefaultSpace.getSpaceId().getID());
			// Register the agent in the default space
			EventListener listener = getSkill(InternalEventBusCapacity.class).asEventListener();
			OpenEventSpace defSpace = (OpenEventSpace) this.innerContext.getDefaultSpace();
			defSpace.register(listener);
		}
		return this.innerContext;
	}

	@Override
	public synchronized boolean hasMemberAgent() {
		if (this.innerContext != null) {
			Set<UUID> participants = this.innerContext.getDefaultSpace().getParticipants();
			assert (participants != null);
			return ((participants.size() > 1) || ((participants.size() == 1) && (!participants.contains(getOwner().getID()))));
		}
		return false;
	}

	@Override
	public synchronized int getMemberAgentCount() {
		if (this.innerContext != null) {
			SynchronizedSet<UUID> participants = this.innerContext.getDefaultSpace().getParticipants();
			assert (participants != null);
			int count = participants.size();
			if (participants.contains(getOwner().getID())) {
				--count;
			}
			return count;
		}
		return 0;
	}

	@Override
	public synchronized SynchronizedSet<UUID> getMemberAgents() {
		if (this.innerContext != null) {
			SynchronizedSet<UUID> participants = this.innerContext.getDefaultSpace().getParticipants();
			assert (participants != null);
			Set<UUID> members = new HashSet<>();
			UUID myId = getOwner().getID();
			for (UUID id : participants) {
				if (!id.equals(myId)) {
					members.add(id);
				}
			}
			return Collections3.synchronizedSet(members, members);
		}
		return Collections3.emptySynchronizedSet();
	}

	@Override
	public boolean isInnerDefaultSpace(Space space) {
		return isInnerDefaultSpace(space.getID());
	}

	@Override
	public boolean isInnerDefaultSpace(SpaceID spaceID) {
		return spaceID.equals(innerContext.getDefaultSpace().getID());
	}

	@Override
	public boolean isInnerDefaultSpace(UUID spaceID) {
		return spaceID.equals(this.innerContext.getDefaultSpace().getID().getID());
	}

	@Override
	public boolean isInInnerDefaultSpace(Event event) {
		if (event != null) {
			Address adr = event.getSource();
			if (adr != null) {
				return isInnerDefaultSpace(adr.getSpaceId());
			}
		}
		return false;
	}

}
