/**
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.core;

import io.sarl.core.AgentKilled;
import io.sarl.core.AgentSpawned;
import io.sarl.core.Destroy;
import io.sarl.lang.annotation.EarlyExit;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Capacity;
import java.util.UUID;

/**
 * Lifecycle related actions.
 */
@SuppressWarnings("all")
public interface Lifecycle extends Capacity {
  /**
   * Spawns a new member agent in the parent's context (parentID)
   * @fires AgentSpawned inside the default context of the parent. The source of the event is this agent.
   */
  @FiredEvent(AgentSpawned.class)
  public abstract UUID spawnInContext(final Class<? extends Agent> agentClass, final AgentContext context, final Object... params);
  
  /**
   * Spawns a new member agent in the parent's context (parentID)
   * @fires AgentSpawned inside the default context of the parent. The source of the event is this agent.
   */
  @FiredEvent(AgentSpawned.class)
  public abstract UUID spawnInContextWithID(final Class<? extends Agent> agentClass, final UUID agentID, final AgentContext context, final Object... params);
  
  /**
   * Kills this agent.
   * This action must automatically unregister this agent from the default context
   * and therefore all its spaces including the DefaultSpace.
   * If this is a composable agent, it must not have any members before calling this action.
   * Otherwise a RuntimeException will be thrown.
   * @fires AgentKilled in DefaultSpace of all Contexts to which this agent belongs
   * @fires Destroy inside the agent
   * @throws RuntimeException when the agent has members
   */
  @EarlyExit
  @FiredEvent({ AgentKilled.class, Destroy.class })
  public abstract void killMe();
}
