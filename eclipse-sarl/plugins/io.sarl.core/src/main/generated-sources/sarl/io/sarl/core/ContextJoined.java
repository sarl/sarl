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
import io.sarl.lang.core.Event;
import java.util.UUID;
import javax.annotation.Generated;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Notifies behaviors the agent has joined a new superholon
 */
@SarlSpecification("0.4")
@SuppressWarnings("all")
public class ContextJoined extends Event {
  public final UUID holonContextID;
  
  public final UUID defaultSpaceID;
  
  public ContextJoined(final UUID pid, final UUID sid) {
    this.holonContextID = pid;
    this.defaultSpaceID = sid;
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
    ContextJoined other = (ContextJoined) obj;
    if (this.holonContextID == null) {
      if (other.holonContextID != null)
        return false;
    } else if (!this.holonContextID.equals(other.holonContextID))
      return false;
    if (this.defaultSpaceID == null) {
      if (other.defaultSpaceID != null)
        return false;
    } else if (!this.defaultSpaceID.equals(other.defaultSpaceID))
      return false;
    return super.equals(obj);
  }
  
  @Override
  @Pure
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.holonContextID== null) ? 0 : this.holonContextID.hashCode());
    result = prime * result + ((this.defaultSpaceID== null) ? 0 : this.defaultSpaceID.hashCode());
    return result;
  }
  
  /**
   * Returns a String representation of the ContextJoined event's attributes only.
   */
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  @Pure
  protected String attributesToString() {
    StringBuilder result = new StringBuilder(super.attributesToString());
    result.append("holonContextID  = ").append(this.holonContextID);
    result.append("defaultSpaceID  = ").append(this.defaultSpaceID);
    return result.toString();
  }
  
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  private final static long serialVersionUID = 3200207935L;
}
