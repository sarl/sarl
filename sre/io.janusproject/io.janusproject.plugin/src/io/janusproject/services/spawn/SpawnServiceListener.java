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

import java.util.EventListener;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;

/**
 * Listener on events related to the life-cycle of an agent.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface SpawnServiceListener extends EventListener {

	/**
	 * Invoked when the agent is spawned.
	 *
	 * @param parent - the context in which the agent was created.
	 * @param agentID - the agent.
	 * @param initializationParameters - list of parameters that were passed to the agent.
	 */
	void agentSpawned(AgentContext parent, Agent agentID, Object[] initializationParameters);

	/**
	 * Invoked when the agent is destroyed.
	 * 
	 * @param agent - the agent.
	 */
	void agentDestroy(Agent agent);

}
