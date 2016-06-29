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

import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;
import javax.annotation.Generated;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Initialization event for an Agent.
 * The source of this event is the agent itself.
 * This event is only received by the agent's native behavior.
 */
@SarlSpecification("0.4")
@SuppressWarnings("all")
public class Initialize extends Event {
  public Object[] parameters;
  
  /**
   * Construct an event. The source of the event is unknown.
   */
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  public Initialize() {
    super();
  }
  
  /**
   * Construct an event.
   * @param source - address of the agent that is emitting this event.
   */
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  public Initialize(final Address source) {
    super(source);
  }
  
  @Override
  @Pure
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    Initialize other = (Initialize) obj;
    if (this.parameters == null) {
      if (other.parameters != null)
        return false;
    } else if (!this.parameters.equals(other.parameters))
      return false;
    return super.equals(obj);
  }
  
  @Override
  @Pure
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.parameters== null) ? 0 : this.parameters.hashCode());
    return result;
  }
  
  /**
   * Returns a String representation of the Initialize event's attributes only.
   */
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  @Pure
  protected String attributesToString() {
    StringBuilder result = new StringBuilder(super.attributesToString());
    result.append("parameters  = ").append(this.parameters);
    return result.toString();
  }
  
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  private final static long serialVersionUID = 1230698605L;
}
