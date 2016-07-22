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

import java.util.UUID;

import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Lifecycle;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.util.Scopes;

/**
 * Skill to access to the default interaction context.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class DefaultContextInteractionsSkill extends Skill implements DefaultContextInteractions {

	private AgentContext parentContext;

	private EventSpace defaultSpace;

	private Address addressInParentDefaultSpace;

	/**
	 * Constructs a <code>DefaultContextInteractionsImpl</code>.
	 *
	 * @param agent - owner of the skill.
	 * @param parentContext - reference to the parent context.
	 */
	DefaultContextInteractionsSkill(Agent agent, AgentContext parentContext) {
		super(agent);
		this.parentContext = parentContext;
	}

	@Override
	protected String attributesToString() {
		return super.attributesToString() + ", parentContext = " + this.parentContext //$NON-NLS-1$
				+ ", defaultSpace = " + this.defaultSpace //$NON-NLS-1$
				+ ", addressInDefaultspace = " + this.addressInParentDefaultSpace; //$NON-NLS-1$
	}

	@Override
	protected void install() {
		this.defaultSpace = this.parentContext.getDefaultSpace();
	}

	@Override
	public void emit(Event event) {
		event.setSource(getDefaultAddress());
		this.defaultSpace.emit(event);
	}

	@Override
	public void emit(Event event, Scope<Address> scope) {
		event.setSource(getDefaultAddress());
		this.defaultSpace.emit(event, scope);
	}

	@Override
	public Address getDefaultAddress() {
		Address adr = this.addressInParentDefaultSpace;
		if (adr == null) {
			adr = this.defaultSpace.getAddress(getOwner().getID());
			assert (adr != null) : "The agent has no address in the default space"; //$NON-NLS-1$
			this.addressInParentDefaultSpace = adr;
		}
		return adr;
	}

	@Override
	public AgentContext getDefaultContext() {
		return this.parentContext;
	}

	@Override
	public EventSpace getDefaultSpace() {
		return this.defaultSpace;
	}

	@Override
	public void receive(UUID receiverID, Event event) {
		Address recAddr = this.defaultSpace.getAddress(receiverID);
		this.emit(event, Scopes.addresses(recAddr));
	}

	@Override
	public UUID spawn(Class<? extends Agent> agentType, Object... params) {
		return getSkill(Lifecycle.class).spawnInContext(agentType, this.parentContext, params);
	}

	@Override
	public boolean isDefaultSpace(Space space) {
		return isDefaultSpace(space.getID());
	}

	@Override
	public boolean isDefaultSpace(SpaceID space) {
		return isDefaultSpace(space.getID());
	}

	@Override
	public boolean isDefaultSpace(UUID space) {
		return space.equals(this.defaultSpace.getID().getID());
	}

	@Override
	public boolean isInDefaultSpace(Event event) {
		if (event != null) {
			Address adr = event.getSource();
			if (adr != null) {
				return isDefaultSpace(adr.getSpaceId());
			}
		}
		return false;
	}

	@Override
	public boolean isDefaultContext(AgentContext context) {
		return isDefaultContext(context.getID());
	}

	@Override
	public boolean isDefaultContext(UUID contextID) {
		return contextID.equals(this.defaultSpace.getID().getContextID());
	}

}
