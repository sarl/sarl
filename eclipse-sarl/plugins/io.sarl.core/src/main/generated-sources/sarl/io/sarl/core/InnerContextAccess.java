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

import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.util.SynchronizedSet;
import java.util.UUID;
import org.eclipse.xtext.xbase.lib.Pure;

@SuppressWarnings("all")
public interface InnerContextAccess extends Capacity {
  /**
   * Replies the inner context.
   */
  @Pure
  public abstract AgentContext getInnerContext();
  
  /**
   * Replies if the calling agent has other agents
   * as members of its inner context.
   * A member agent is an agent which is not the
   * calling agent, and is a member of at least
   * one space of the inner context.
   */
  @Pure
  public abstract boolean hasMemberAgent();
  
  /**
   * Replies the number of agents that are members
   * of the inner context of the calling agent.
   * A member agent is an agent which is not the
   * calling agent, and is a member of at least
   * one space of the inner context.
   */
  @Pure
  public abstract int getMemberAgentCount();
  
  /**
   * Replies all the member agents in the inner context.
   * A member agent is an agent which is not the
   * calling agent, and is a member of at least
   * one space of the inner context.
   */
  @Pure
  public abstract SynchronizedSet<UUID> getMemberAgents();
  
  /**
   * Replies if the given space is the default space of the inner context.
   * 
   * @param space - the space to test.
   * @return <code>true</code> if the given space is the default space of
   * the inner context. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isInnerDefaultSpace(final Space space);
  
  /**
   * Replies if the given identifier is the identifier of the
   * default space of the inner context.
   * 
   * @param spaceID - the identifier to test.
   * @return <code>true</code> if the given identifier is the identifier
   * of the default space of the inner context. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isInnerDefaultSpace(final SpaceID spaceID);
  
  /**
   * Replies if the given identifier is the identifier of the
   * default space of the inner context.
   * 
   * @param spaceID - the identifier to test.
   * @return <code>true</code> if the given identifier is the identifier
   * of the default space of the inner context. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isInnerDefaultSpace(final UUID spaceID);
  
  /**
   * Replies if the given event was emitted in the default space
   * of the inner context.
   * 
   * @param event - the event to test.
   * @return <code>true</code> if the given event was emitted
   * in the default space of the inner context. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isInInnerDefaultSpace(final Event event);
}
