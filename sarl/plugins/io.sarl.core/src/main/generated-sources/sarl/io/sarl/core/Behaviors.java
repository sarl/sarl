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

import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Agent behaviors management.
 */
@SuppressWarnings("all")
public interface Behaviors extends Capacity {
  /**
   * Register a Behavior for the owner agent.
   * The new Behavior will react to all declared events received by the agent.
   */
  public abstract Behavior registerBehavior(final Behavior attitude);
  
  /**
   * Unregisters a behavior for the owner agent
   */
  public abstract Behavior unregisterBehavior(final Behavior attitude);
  
  /**
   * Wake the agent's behaviors reacting to the Event evt.
   */
  public abstract void wake(final Event evt);
  
  /**
   * Replies the interface to dispatch an event to agent's Behaviors
   */
  @Pure
  public abstract EventListener asEventListener();
}
