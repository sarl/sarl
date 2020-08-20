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
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.api.naming.namespace;

import com.google.common.util.concurrent.Service;
import io.sarl.api.naming.name.SarlName;
import io.sarl.api.naming.parser.INameParser;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import java.net.URI;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * This service enables to manage the name spaces into the SRE.
 * 
 * <p>Each object within the SRE may be identified by a name, stored into an {@link SarlName}.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@SarlSpecification("0.12")
@SarlElementType(11)
@SuppressWarnings("all")
public interface NamespaceService extends Service {
  /**
   * Replies the name parser that is used by this service.
   * 
   * @return The name parser
   * @since 0.11
   */
  @Pure
  INameParser getNameParser();
  
  /**
   * Finds and replies the object with the given name and of the given type.
   * 
   * @param name the name of the object. See the documentation of {@link NamespaceService}
   *     for details.
   * @return the root context.  A {@code null} value is replied if the object is not found.
   */
  @Pure
  <T extends Object> T findObject(final SarlName name, final Class<T> type);
  
  /**
   * Finds and replies the object with the given name and of the given type.
   * 
   * @param name the name of the object. See the documentation of {@link NamespaceService}
   * for details.
   * @return the object with the given name. A {@code null} value is replied if the object is not found.
   */
  @Pure
  Object findObject(final SarlName name);
  
  /**
   * Finds and replies the object with the given name and of the given type.
   * 
   * @param name the name of the object. See the documentation of {@link NamespaceService}
   * for details.
   * @return the root context.  A {@code null} value is replied if the object is not found.
   * @since 0.11
   */
  @Pure
  default <T extends Object> T findObject(final String name, final Class<T> type) {
    INameParser parser = this.getNameParser();
    URI nameURI = parser.decode(name);
    if ((nameURI == null)) {
      return null;
    }
    SarlName sarlName = parser.decode(nameURI);
    if ((sarlName == null)) {
      return null;
    }
    return this.<T>findObject(sarlName, type);
  }
  
  /**
   * Finds and replies the object with the given name and of the given type.
   * 
   * @param name the name of the object. See the documentation of {@link NamespaceService}
   * for details.
   * @return the object with the given name. A {@code null} value is replied if the object is not found.
   * @since 0.11
   */
  @Pure
  default Object findObject(final String name) {
    INameParser parser = this.getNameParser();
    URI nameURI = parser.decode(name);
    if ((nameURI == null)) {
      return null;
    }
    SarlName sarlName = parser.decode(nameURI);
    if ((sarlName == null)) {
      return null;
    }
    return this.findObject(sarlName);
  }
}
