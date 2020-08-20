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
package io.sarl.api.naming.parser;

import io.sarl.api.naming.name.SarlName;
import io.sarl.api.naming.parser.ISchemeNameParser;
import io.sarl.api.naming.scheme.NameScheme;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import java.net.URI;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.StringExtensions;

/**
 * Parser of names
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
public interface INameParser {
  /**
   * Parse the string representation of the name, and create the URI representation.
   * 
   * @param name the string representation of the name.
   * @return the URI representation of the given name, or {@code null} if the given string cannot be parsed.
   */
  @Pure
  default URI decode(final String name) {
    boolean _isNullOrEmpty = StringExtensions.isNullOrEmpty(name);
    if ((!_isNullOrEmpty)) {
      try {
        URI uri = URI.create(name);
        return this.normalize(uri);
      } catch (final Throwable _t) {
        if (_t instanceof Throwable) {
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
    }
    return null;
  }
  
  /**
   * Parse the string representation of the name, and create the URI representation.
   * 
   * @param name the string representation of the name.
   * @return the URI representation of the given name, or {@code null} if the given string cannot be parsed.
   */
  @Pure
  URI normalize(final URI name);
  
  /**
   * Parse the URI of the name, and create the name object.
   * The argument must be a normalized URI that is computed by {@link #normalize(URI)}.
   * 
   * @param name the normalized URI representation of the name. See {@link #normalize(URI)}.
   * @return the name, or {@code null} if the given URI cannot be parsed.
   */
  @Pure
  SarlName decode(final URI name);
  
  /**
   * Register a name parser for a specific scheme.
   * 
   * @param parser the name parser, never {@code null}.
   */
  void addSchemeNameParser(final ISchemeNameParser<?> parser);
  
  /**
   * Unregister a name parser for a specific scheme.
   * 
   * @param scheme the associated scheme, never {@code null}.
   * @return the name parser that was associated to the name protocol.
   */
  ISchemeNameParser<?> removeSchemeNameParser(final NameScheme scheme);
}
