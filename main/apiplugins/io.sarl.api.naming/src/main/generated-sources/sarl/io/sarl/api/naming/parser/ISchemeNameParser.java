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
import io.sarl.api.naming.scheme.NameScheme;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import java.net.URI;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * A parser of names that is accepting URI-based syntax for a specific scheme.
 * 
 * @param <N> the type of the name that is the result of the decoding.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@SarlSpecification("0.12")
@SarlElementType(11)
@SuppressWarnings("all")
public interface ISchemeNameParser<N extends SarlName> {
  /**
   * Replies the name scheme that is supported by this parser.
   */
  @Pure
  NameScheme getScheme();
  
  /**
   * Refactor the given URI in order to fit the name specification.
   */
  @Pure
  URI refactor(final URI name);
  
  /**
   * Decode the name.
   */
  @Pure
  N decode(final URI name);
}
