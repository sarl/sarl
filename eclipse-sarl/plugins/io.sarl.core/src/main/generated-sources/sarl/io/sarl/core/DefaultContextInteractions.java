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

import io.sarl.core.AgentSpawned;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import java.util.UUID;
import javax.annotation.Generated;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Gives access to the DefaultContext of the agent and common actions on its default space.
 * Defines a set of commonly accessed action performed on the Default Space
 * of the Default Context of the agent.
 */
@SuppressWarnings("all")
public interface DefaultContextInteractions extends Capacity {
  /**
   * Returns the Default context fo the agent
   */
  @Pure
  public abstract AgentContext getDefaultContext();
  
  /**
   * Returns the Default Space of the Default Context.
   * Equivalent to defaultContext.defaultSpace
   */
  @Pure
  public abstract EventSpace getDefaultSpace();
  
  /**
   * Return the Address of the agent in the Default Space of the Default Context.
   * Equivalent to defaultContext.defaultSpace.address(this.id)
   */
  @Pure
  public abstract Address getDefaultAddress();
  
  /**
   * Emits a given event with the provided scope in the DefaultSpace of the DefaultContext.
   * Equivalent to defaultContext.defaultSpace.emit(e,scope)
   */
  @DefaultValueSource
  public abstract void emit(final Event e, @DefaultValue("io.sarl.core.DefaultContextInteractions#EMIT_0") final Scope<Address> scope);
  
  /**
   * Default value for the parameter scope
   */
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  @SarlSourceCode(" null")
  public final static Scope<Address> $DEFAULT_VALUE$EMIT_0 = null;
  
  /**
   * Inverse signature of send. Useful to send events using the agent's UUID.
   * Example:
   *  If you need to send an event to a newly created agent you should usually do :
   * <code>
   * ...
   * const mySpawnArgs = ...
   * const myAgentAddr = MyAgent.spawn(mySpawnArgs)
   * defaultSpace.emit(new StartWorkingEvent, AddressScope::scope(myAgentAddr)
   * </code>
   *  The equivalent using receive could be written:
   * <code>
   * ...
   * MyAgent.spawn(mySpawnArgs).receive(new StartWorkingEvent)
   * </code>
   */
  public abstract void receive(final UUID receiver, final Event e);
  
  /**
   * Replies if the given space is the default space of the default context.
   * 
   * @param space - the space to test.
   * @return <code>true</code> if the given space is the default space of
   * the default context. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isDefaultSpace(final Space space);
  
  /**
   * Replies if the given identifier is the identifier of the default space of
   * the default context.
   * 
   * @param space - the space to test.
   * @return <code>true</code> if the given identifier is the identifier
   * of the default space of the default context. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isDefaultSpace(final SpaceID space);
  
  /**
   * Replies if the given identifier is the identifier of the default space of
   * the default context.
   * 
   * @param space - the space to test.
   * @return <code>true</code> if the given identifier is the identifier
   * of the default space of the default context. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isDefaultSpace(final UUID space);
  
  /**
   * Replies if the given event was emitted in the default space of
   * the default context.
   * 
   * @param event - the event to test.
   * @return <code>true</code> if the given event is emitted in the default
   * space of the default context. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isInDefaultSpace(final Event event);
  
  /**
   * Replies if the given context is the default context.
   * 
   * @param context - the agent context to test.
   * @return <code>true</code> if the given context is the default context.
   * Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isDefaultContext(final AgentContext context);
  
  /**
   * Replies if the given identifier is the identifier of the default context.
   * 
   * @param contextID - the identifier of the context.
   * @return <code>true</code> if the given identifier is the identifier
   * of the default context. Otherwise <code>false</code>.
   * @since 0.2
   */
  @Pure
  public abstract boolean isDefaultContext(final UUID contextID);
  
  /**
   * Spawns a new Agent inside the default context of this agent.
   * This action must automatically register the newly created agent
   * within the default space of the context.
   * @fires AgentSpawned in DefaultSpace
   */
  @FiredEvent(AgentSpawned.class)
  public abstract UUID spawn(final Class<? extends Agent> aAgent, final Object... params);
  
  /**
   * Emits a given event with the provided scope in the DefaultSpace of the DefaultContext.
   * Equivalent to defaultContext.defaultSpace.emit(e,scope)
   */
  @DefaultValueUse("io.sarl.lang.core.Event,io.sarl.lang.core.Scope<io.sarl.lang.core.Address>")
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  public abstract void emit(final Event e);
}
