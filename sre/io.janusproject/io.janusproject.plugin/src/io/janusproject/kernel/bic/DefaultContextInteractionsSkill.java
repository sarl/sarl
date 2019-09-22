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

import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

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
import io.sarl.lang.util.ClearableReference;
import io.sarl.util.Scopes;

/**
 * Skill to access to the default interaction context.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DefaultContextInteractionsSkill extends BuiltinSkill implements DefaultContextInteractions {

	private static int installationOrder = -1;

	private AgentContext parentContext;

	private EventSpace defaultSpace;

	private Address addressInParentDefaultSpace;

	private ClearableReference<Skill> skillBufferLifecycle;

	/**
	 * Constructs a <code>DefaultContextInteractionsImpl</code>.
	 *
	 * @param agent owner of the skill.
	 * @param parentContext reference to the parent context.
	 */
	DefaultContextInteractionsSkill(Agent agent, AgentContext parentContext) {
		super(agent);
		this.parentContext = parentContext;
	}

	/** Replies the Lifecycle skill as fast as possible.
	 *
	 * @return the skill
	 */
	protected final Lifecycle getLifecycleSkill() {
		if (this.skillBufferLifecycle == null || this.skillBufferLifecycle.get() == null) {
			this.skillBufferLifecycle = $getSkill(Lifecycle.class);
		}
		return $castSkill(Lifecycle.class, this.skillBufferLifecycle);
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
		builder.add("parentContext", this.parentContext); //$NON-NLS-1$
		builder.add("defaultSpace", this.defaultSpace); //$NON-NLS-1$
		builder.add("addressInDefaultspace", this.addressInParentDefaultSpace); //$NON-NLS-1$
	}

	@Override
	protected void install() {
		this.defaultSpace = this.parentContext.getDefaultSpace();
	}

	@Override
	public void emit(Event event) {
		this.defaultSpace.emit(getOwner().getID(), event, null);
	}

	@Override
	public void emit(Event event, Scope<Address> scope) {
		this.defaultSpace.emit(getOwner().getID(), event, scope);
	}

	@Override
	public Address getDefaultAddress() {
		Address adr = this.addressInParentDefaultSpace;
		if (adr == null) {
			adr = this.defaultSpace.getAddress(getOwner().getID());
			assert adr != null : "The agent has no address in the default space"; //$NON-NLS-1$
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

	/** {@inheritDoc}.
	 * @deprecated see {@link #willReceive(UUID, Event)}
	 */
	@Deprecated
	@Override
	public void receive(UUID receiverID, Event event) {
		willReceive(receiverID, event);
	}

	@Override
	public void willReceive(UUID receiverID, Event event) {
		final Address recAddr = this.defaultSpace.getAddress(receiverID);
		this.emit(event, Scopes.addresses(recAddr));
	}

	/** {@inheritDoc}.
	 * @deprecated See {@link Lifecycle} capacity.
	 */
	@Override
	@Deprecated
	public UUID spawn(Class<? extends Agent> agentType, Object... params) {
		return getLifecycleSkill().spawnInContext(agentType, this.parentContext, params);
	}

	@Override
	public boolean isDefaultSpace(Space space) {
		return isDefaultSpace(space.getSpaceID());
	}

	@Override
	public boolean isDefaultSpace(SpaceID space) {
		return isDefaultSpace(space.getID());
	}

	@Override
	public boolean isDefaultSpace(UUID space) {
		return space.equals(this.defaultSpace.getSpaceID().getID());
	}

	@Override
	public boolean isInDefaultSpace(Event event) {
		if (event != null) {
			final Address adr = event.getSource();
			if (adr != null) {
				return isDefaultSpace(adr.getSpaceID());
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
		return contextID.equals(this.defaultSpace.getSpaceID().getContextID());
	}

}
