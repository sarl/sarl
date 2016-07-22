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

import com.google.inject.Inject;
import io.janusproject.services.executor.ChuckNorrisException;
import io.janusproject.services.spawn.SpawnService;

import io.sarl.core.Lifecycle;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Skill;

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
class LifecycleSkill extends Skill implements Lifecycle {

	@Inject
	private SpawnService spawnService;

	/**
	 * Constructs the skill.
	 *
	 * @param agent - owner of the skill.
	 */
	LifecycleSkill(Agent agent) {
		super(agent);
	}

	@Override
	public UUID spawnInContext(Class<? extends Agent> agentType, AgentContext context, Object... params) {
		return this.spawnService.spawn(context, null, agentType, params);
	}

	@Override
	public UUID spawnInContextWithID(Class<? extends Agent> agentClass, UUID agentID, AgentContext context, Object... params) {
		return this.spawnService.spawn(context, agentID, agentClass, params);
	}

	@Override
	public void killMe() {
		// The agent should be killed by a specific asynchronous event.
		// This event is supported by the internal event bus implementation.
		InternalEventBusCapacity busCapacity = getSkill(InternalEventBusCapacity.class);
		busCapacity.selfEvent(new AsynchronousAgentKillingEvent());
		throw new ChuckNorrisException();
	}

	/**
	 * This runtie exception is thrown when an agent cannot be killed.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class AgentKillException extends RuntimeException {

		private static final long serialVersionUID = 3186824315988212481L;

		/**
		 * Construct.
		 *
		 * @param exception - cause
		 */
		AgentKillException(io.janusproject.services.spawn.SpawnService.AgentKillException exception) {
			super(exception.getMessage(), exception);
		}

		/**
		 * Replies the agent that cannot be killed.
		 *
		 * @return the agent.
		 */
		public UUID getAgent() {
			return ((io.janusproject.services.spawn.SpawnService.AgentKillException) getCause()).getAgent();
		}

	}

}
