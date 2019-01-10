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
import java.util.List;
import java.util.UUID;

import com.google.inject.Inject;

import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.spawn.SpawnService;

import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Lifecycle;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Skill;
import io.sarl.lang.util.ClearableReference;

/**
 * Skill that permits to manage the life cycle of the agents.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class LifecycleSkill extends BuiltinSkill implements Lifecycle {

	private static int installationOrder = -1;

	@Inject
	private SpawnService spawnService;

	private ClearableReference<Skill> skillBufferInternalEventBusCapacity;

	private ClearableReference<Skill> skillDefaultContextInteraction;

	/**
	 * Constructs the skill.
	 *
	 * @param agent owner of the skill.
	 */
	LifecycleSkill(Agent agent) {
		super(agent);
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

	/** Replies the InternalEventBusCapacity skill as fast as possible.
	 *
	 * @return the skill
	 */
	protected final DefaultContextInteractions getDefaultContextInteractionsSkill() {
		if (this.skillDefaultContextInteraction == null || this.skillDefaultContextInteraction.get() == null) {
			this.skillDefaultContextInteraction = $getSkill(DefaultContextInteractions.class);
		}
		return $castSkill(DefaultContextInteractions.class, this.skillDefaultContextInteraction);
	}

	@Override
	public int getInstallationOrder() {
		if (installationOrder < 0) {
			installationOrder = installationOrder(this);
		}
		return installationOrder;
	}

	@Override
	public UUID spawn(Class<? extends Agent> agentType, Object... params) {
		final List<UUID> ids = this.spawnService.spawn(
				1,
				getOwner().getID(),
				getDefaultContextInteractionsSkill().getDefaultContext(),
				null, agentType, params);
		if (ids.isEmpty()) {
			return null;
		}
		return ids.get(0);
	}

	@Override
	public Collection<UUID> spawn(int nbAgents, Class<? extends Agent> agentType, Object... params) {
		return this.spawnService.spawn(nbAgents,
				getOwner().getID(),
				getDefaultContextInteractionsSkill().getDefaultContext(),
				null, agentType, params);
	}

	@Override
	public UUID spawnInContext(Class<? extends Agent> agentType, AgentContext context, Object... params) {
		final List<UUID> ids = this.spawnService.spawn(1,
				getOwner().getID(),
				context, null, agentType, params);
		if (ids.isEmpty()) {
			return null;
		}
		return ids.get(0);
	}

	@Override
	public Collection<UUID> spawnInContext(int nbAgents, Class<? extends Agent> agentClass, AgentContext context,
			Object... params) {
		return this.spawnService.spawn(nbAgents,
				getOwner().getID(),
				context, null, agentClass, params);
	}

	@Override
	public UUID spawnInContextWithID(Class<? extends Agent> agentClass, UUID agentID, AgentContext context, Object... params) {
		final List<UUID> ids = this.spawnService.spawn(1,
				getOwner().getID(),
				context, agentID, agentClass, params);
		if (ids.isEmpty()) {
			return null;
		}
		return ids.get(0);
	}

	@Override
	public void killMe() {
		// The agent should be killed by a specific asynchronous event.
		// This event is supported by the internal event bus implementation.
		final InternalEventBusCapacity busCapacity = getInternalEventBusCapacitySkill();
		busCapacity.selfEvent(new AsynchronousAgentKillingEvent());
		// Never return from the killMe
		Thread.yield();
		ExecutorService.neverReturn();
	}

}
