/**
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2020 the original authors or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.api.naming.name;

import com.google.common.util.concurrent.Service;
import io.sarl.api.naming.name.SarlName;
import io.sarl.lang.annotation.PrivateAPI;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import java.net.URI;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * This class represents a service name.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@SarlSpecification("0.12")
@SarlElementType(10)
@SuppressWarnings("all")
public class ServiceName extends SarlName {
  @Accessors
  private final Class<? extends Service> serviceType;
  
  /**
   * Constructor.
   * 
   * @param uri the uri of the context.
   * @param service the type of service.
   */
  @PrivateAPI
  public ServiceName(final URI uri, final Class<? extends Service> service) {
    super(uri);
    this.serviceType = service;
  }
  
  @Override
  @Pure
  @SyntheticMember
  public boolean equals(final Object obj) {
    return super.equals(obj);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public int hashCode() {
    int result = super.hashCode();
    return result;
  }
  
  @Override
  @Pure
  @SyntheticMember
  public ServiceName clone() {
    try {
      return (ServiceName) super.clone();
    } catch (Throwable exception) {
      throw new Error(exception);
    }
  }
  
  @SyntheticMember
  private static final long serialVersionUID = -105695148L;
  
  @Pure
  public Class<? extends Service> getServiceType() {
    return this.serviceType;
  }
}
