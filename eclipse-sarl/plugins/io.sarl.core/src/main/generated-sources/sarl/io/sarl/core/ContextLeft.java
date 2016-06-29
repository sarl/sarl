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
 * Notifies behaviors the agent has left a superholon
 */
@SarlSpecification("0.4")
@SuppressWarnings("all")
public class ContextLeft extends Event {
  public final UUID holonContextID;
  
  public ContextLeft(final UUID pid) {
    this.holonContextID = pid;
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
    ContextLeft other = (ContextLeft) obj;
    if (this.holonContextID == null) {
      if (other.holonContextID != null)
        return false;
    } else if (!this.holonContextID.equals(other.holonContextID))
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
    return result;
  }
  
  /**
   * Returns a String representation of the ContextLeft event's attributes only.
   */
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  @Pure
  protected String attributesToString() {
    StringBuilder result = new StringBuilder(super.attributesToString());
    result.append("holonContextID  = ").append(this.holonContextID);
    return result.toString();
  }
  
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  private final static long serialVersionUID = 4074523787L;
}
