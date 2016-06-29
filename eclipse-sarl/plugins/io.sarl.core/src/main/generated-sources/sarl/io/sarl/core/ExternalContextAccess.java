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

import io.sarl.core.ContextJoined;
import io.sarl.core.ContextLeft;
import io.sarl.core.MemberJoined;
import io.sarl.core.MemberLeft;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.util.SynchronizedCollection;
import java.util.UUID;
import org.eclipse.xtext.xbase.lib.Pure;

@SuppressWarnings("all")
public interface ExternalContextAccess extends Capacity {
  /**
   * Replies all contexts this agent is a member of, including the default context
   */
  @Pure
  public abstract SynchronizedCollection<AgentContext> getAllContexts();
  
  /**
   * Replies the AgentContext for the given ID.
   * The agent must have joined the context before calling this action or use its parentContextID
   * @see io.sarl.lang.core.Agent#getParentID
   * @see #join
   * 
   * @throws UnknownContextException if the context specified context is not known by the agent.
   * @param contextID the ID of the context to get.
   */
  @Pure
  public abstract AgentContext getContext(final UUID contextID);
  
  /**
   * Joins a new parent context (a new super holon).
   * <p>
   * The agent will be involved in the context with the ID given by <var>contextID</var>.
   * The parameter <var>expectedDefaultSpaceID</var> is only used to check if
   * the caller of this function knows the ID of the default space in the context to
   * be involved in. Note that the context must already exists, and the default space
   * inside this context must have the sams ID as <var>expectedDefaultSpaceID</var>.
   * If the given <var>expectedDefaultSpaceID</var> does not match the ID of the
   * default space in the context <var>contextID</var>, then the access to the context
   * is forbidden.
   * <p>
   * This actions registers the agent in the default Space of the context with the
   * ID <var>contextID</var>.
   * .
   * @fires ContextJoined in its inner Context default space (Behaviors#wake).
   * @fires MemberJoined in its parent Context default Space
   */
  @FiredEvent({ ContextJoined.class, MemberJoined.class })
  public abstract void join(final UUID contextID, final UUID expectedDefaultSpaceID);
  
  /**
   * Leaves the parent's context.
   * @fires ContextLeft in its inner Context default space (Behaviors#wake).
   * @fires MemberLeft in its parent Context default Space
   */
  @FiredEvent({ ContextLeft.class, MemberLeft.class })
  public abstract void leave(final UUID contextID);
  
  /**
   * Replies if the given event was emitted in the given space.
   * 
   * @param event - the event to test.
   * @param space - the space in which the event may be.
   * @return <code>true</code> if the given event was emitted in the
   * given space. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isInSpace(final Event event, final Space space);
  
  /**
   * Replies if the given event was emitted in the space with
   * the given identifier..
   * 
   * @param event - the event to test.
   * @param spaceID - the identifier of the space in which the event may be.
   * @return <code>true</code> if the given event was emitted in the
   * space with the given identifier. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isInSpace(final Event event, final SpaceID spaceID);
  
  /**
   * Replies if the given event was emitted in the space with
   * the given identifier..
   * 
   * @param event - the event to test.
   * @param spaceID - the identifier of the space in which the event may be.
   * @return <code>true</code> if the given event was emitted in the
   * space with the given identifier. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isInSpace(final Event event, final UUID spaceID);
}
