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

package io.janusproject.services.spawn;

import java.util.UUID;

import io.janusproject.services.DependentService;
import org.arakhne.afc.vmutil.locale.Locale;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;

/**
 * This service provides the tools to manage the life-cycle of the agents.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface SpawnService extends DependentService {

	/**
	 * Spawn an agent of the given type, and pass the parameters to its initialization function.
	 *
	 * @param parent - the parent entity that is creating the agent.
	 * @param agentClazz - the type of the agent to spawn.
	 * @param agentId - the identifier of the agent to spawn. If <code>null</code> the identifier is randomly selected.
	 * @param params - the list of the parameters to pass to the agent initialization function.
	 * @return the identifier of the agent, never <code>null</code>.
	 */
	UUID spawn(AgentContext parent, UUID agentId, Class<? extends Agent> agentClazz, Object... params);

	/**
	 * Kill the agent with the given identifier.
	 *
	 * @param agentID - the identifier of the agent to kill.
	 * @throws AgentKillException - thrown when the agent cannot be killed.
	 */
	void killAgent(UUID agentID) throws AgentKillException;

	/**
	 * Add a listener on the changes in the current state of an agent.
	 *
	 * @param id - identifier of the agent.
	 * @param agentLifecycleListener - the listener on the any change in the life-cycle of the agent.
	 */
	void addSpawnServiceListener(UUID id, SpawnServiceListener agentLifecycleListener);

	/**
	 * Add a listener on the changes in the current state of an agent.
	 *
	 * @param agentLifecycleListener - the listener on the any change in the life-cycle of the agent.
	 */
	void addSpawnServiceListener(SpawnServiceListener agentLifecycleListener);

	/**
	 * Remove a listener on the changes in the current state of an agent.
	 *
	 * @param id - identifier of the agent.
	 * @param agentLifecycleListener - the listener on the any change in the life-cycle of the agent.
	 */
	void removeSpawnServiceListener(UUID id, SpawnServiceListener agentLifecycleListener);

	/**
	 * Remove a listener on the changes in the current state of an agent.
	 *
	 * @param agentLifecycleListener - the listener on the any change in the life-cycle of the agent.
	 */
	void removeSpawnServiceListener(SpawnServiceListener agentLifecycleListener);

	/**
	 * Add a listener on the changes related to the kernel agent.
	 *
	 * @param listener - listener on the spawning events in the local kernel.
	 */
	void addKernelAgentSpawnListener(KernelAgentSpawnListener listener);

	/**
	 * Remove a listener on the changes related to the kernel agent.
	 *
	 * @param listener - listener on the spawning events in the local kernel.
	 */
	void removeKernelAgentSpawnListener(KernelAgentSpawnListener listener);

	/**
	 * Exception occurs when an agent cannot be killed.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	class AgentKillException extends Exception {

		private static final long serialVersionUID = -7911946040378324212L;

		private final UUID agent;

		/**
		 * @param agent - id of the agent that cannot be killed.
		 */
		public AgentKillException(UUID agent) {
			super(Locale.getString(SpawnService.class, "AGENT_KILL_EXCEPTION", agent)); //$NON-NLS-1$
			this.agent = agent;
			fillInStackTrace();
		}

		/**
		 * @param agent - id of the agent that cannot be killed.
		 * @param cause - the exception that is the cause of the killing discarding.
		 */
		public AgentKillException(UUID agent, Throwable cause) {
			super(Locale.getString(SpawnService.class, "AGENT_KILL_EXCEPTION_WITH_CAUSE", //$NON-NLS-1$
					agent, cause), cause);
			this.agent = agent;
		}

		/**
		 * Replies the id of the agent that cannot be skilled.
		 *
		 * @return the agent id.
		 */
		public UUID getAgent() {
			return this.agent;
		}

	}

}
