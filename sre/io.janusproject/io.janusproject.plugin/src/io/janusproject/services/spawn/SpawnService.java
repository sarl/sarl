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

package io.janusproject.services.spawn;

import java.util.List;
import java.util.UUID;

import io.janusproject.services.DependentService;

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
	 * Spawn agents of the given type, and pass the parameters to its initialization function.
	 *
	 * @param nbAgents the number of agents to spawn.
	 * @param spawningAgent the agent which is spawning.
	 * @param parent the parent entity that is creating the agents.
	 * @param agentClazz the type of the agents to spawn.
	 * @param agentId the identifier of the agent to spawn. If <code>null</code> the identifier is randomly selected.
	 *     If {@code nbAgents} is greater than 1, the agent identifier must be {@code null}.
	 * @param params the list of the parameters to pass to the agent initialization function.
	 * @return the identifiers of the agents, never <code>null</code>.
	 */
	List<UUID> spawn(int nbAgents, UUID spawningAgent, AgentContext parent, UUID agentId, Class<? extends Agent> agentClazz, Object... params);

	/**
	 * Kill the agent with the given identifier.
	 *
	 * @param agentID the identifier of the agent to kill.
	 * @return {@code true} if the agent was killed by this call; {@code false} if the agent
	 *     is unknown or already killed.
	 */
	boolean killAgent(UUID agentID);

	/**
	 * Add a listener on the changes in the current state of an agent.
	 *
	 * @param id identifier of the agent.
	 * @param agentLifecycleListener the listener on the any change in the life-cycle of the agent.
	 */
	void addSpawnServiceListener(UUID id, SpawnServiceListener agentLifecycleListener);

	/**
	 * Add a listener on the changes in the current state of an agent.
	 *
	 * @param agentLifecycleListener the listener on the any change in the life-cycle of the agent.
	 */
	void addSpawnServiceListener(SpawnServiceListener agentLifecycleListener);

	/**
	 * Remove a listener on the changes in the current state of an agent.
	 *
	 * @param id identifier of the agent.
	 * @param agentLifecycleListener the listener on the any change in the life-cycle of the agent.
	 */
	void removeSpawnServiceListener(UUID id, SpawnServiceListener agentLifecycleListener);

	/**
	 * Remove a listener on the changes in the current state of an agent.
	 *
	 * @param agentLifecycleListener the listener on the any change in the life-cycle of the agent.
	 */
	void removeSpawnServiceListener(SpawnServiceListener agentLifecycleListener);

	/**
	 * Add a listener on the changes related to the kernel agent.
	 *
	 * @param listener listener on the spawning events in the local kernel.
	 */
	void addKernelAgentSpawnListener(KernelAgentSpawnListener listener);

	/**
	 * Remove a listener on the changes related to the kernel agent.
	 *
	 * @param listener listener on the spawning events in the local kernel.
	 */
	void removeKernelAgentSpawnListener(KernelAgentSpawnListener listener);

}
