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

import io.sarl.api.naming.name.BehaviorName;
import io.sarl.api.naming.parser.AbstractSchemeNameParser;
import io.sarl.api.naming.scheme.NameScheme;
import io.sarl.bootstrap.SREClassLoader;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Behavior;
import java.net.URI;
import java.util.StringTokenizer;
import java.util.UUID;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Parser of behavior names that is accepting URI-based syntax.
 * 
 * <p>The different types of names are: <ul>
 * <li>{@code "behavior:[/]{0-2}agentId/behaviorName[/behaviorIndex][#fragmentName]"}</li>
 * <li>{@code "behavior:[/]{0-2}contextId/agentId/behaviorName[/behaviorIndex][#fragmentName]"}</li>
 * <li>{@code "behavior:[/]{0-2}contextId/spaceId/agentId/behaviorName[/behaviorIndex][#fragmentName]"}</li>
 * </ul>
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
public class BehaviorSchemeNameParser extends AbstractSchemeNameParser<BehaviorName> {
  /**
   * Constructor.
   * 
   * @param scheme the name scheme that is supported by this parser. By default it is {@link NameScheme.BEHAVIOR}.
   */
  @DefaultValueSource
  public BehaviorSchemeNameParser(@DefaultValue("io.sarl.api.naming.parser.BehaviorSchemeNameParser#NEW_0") final NameScheme scheme) {
    super(scheme);
  }
  
  /**
   * Default value for the parameter scheme
   */
  @SyntheticMember
  @SarlSourceCode("NameScheme::BEHAVIOR")
  private static final NameScheme $DEFAULT_VALUE$NEW_0 = NameScheme.BEHAVIOR;
  
  @Pure
  @Override
  public URI refactor(final URI name) {
    return this.refactor(name, 2, 5, true);
  }
  
  @Pure
  @Override
  public BehaviorName decode(final URI name) {
    try {
      String _path = name.getPath();
      final StringTokenizer tokenizer = new StringTokenizer(_path, "/");
      boolean _hasMoreTokens = tokenizer.hasMoreTokens();
      if (_hasMoreTokens) {
        final String token0 = tokenizer.nextToken();
        boolean _hasMoreTokens_1 = tokenizer.hasMoreTokens();
        if (_hasMoreTokens_1) {
          final String token1 = tokenizer.nextToken();
          boolean _hasMoreTokens_2 = tokenizer.hasMoreTokens();
          if (_hasMoreTokens_2) {
            final String token2 = tokenizer.nextToken();
            boolean _hasMoreTokens_3 = tokenizer.hasMoreTokens();
            if (_hasMoreTokens_3) {
              final String token3 = tokenizer.nextToken();
              boolean _hasMoreTokens_4 = tokenizer.hasMoreTokens();
              if (_hasMoreTokens_4) {
                final String token4 = tokenizer.nextToken();
                UUID _fromString = UUID.fromString(token0);
                UUID _fromString_1 = UUID.fromString(token1);
                UUID _fromString_2 = UUID.fromString(token2);
                Class<?> _loadClass = SREClassLoader.loadClass(token3, true, this.getClass().getClassLoader());
                int _parseUnsignedInt = Integer.parseUnsignedInt(token4, 10);
                return new BehaviorName(name, _fromString, _fromString_1, _fromString_2, 
                  ((Class<? extends Behavior>) _loadClass), _parseUnsignedInt);
              }
              try {
                Class<?> _forName = Class.forName(token2);
                Class<? extends Behavior> beh = ((Class<? extends Behavior>) _forName);
                UUID _fromString_3 = UUID.fromString(token0);
                UUID _fromString_4 = UUID.fromString(token1);
                int _parseUnsignedInt_1 = Integer.parseUnsignedInt(token3, 10);
                return new BehaviorName(name, _fromString_3, null, _fromString_4, beh, _parseUnsignedInt_1);
              } catch (final Throwable _t) {
                if (_t instanceof Throwable) {
                  UUID _fromString_5 = UUID.fromString(token0);
                  UUID _fromString_6 = UUID.fromString(token1);
                  UUID _fromString_7 = UUID.fromString(token2);
                  Class<?> _forName_1 = Class.forName(token3);
                  return new BehaviorName(name, _fromString_5, _fromString_6, _fromString_7, ((Class<? extends Behavior>) _forName_1), 
                    (-1));
                } else {
                  throw Exceptions.sneakyThrow(_t);
                }
              }
            }
            try {
              Class<?> _forName = Class.forName(token1);
              Class<? extends Behavior> beh = ((Class<? extends Behavior>) _forName);
              UUID _fromString_3 = UUID.fromString(token0);
              int _parseUnsignedInt_1 = Integer.parseUnsignedInt(token2, 10);
              return new BehaviorName(name, null, null, _fromString_3, beh, _parseUnsignedInt_1);
            } catch (final Throwable _t) {
              if (_t instanceof Throwable) {
                UUID _fromString_4 = UUID.fromString(token0);
                UUID _fromString_5 = UUID.fromString(token1);
                Class<?> _forName_1 = Class.forName(token2);
                return new BehaviorName(name, _fromString_4, null, _fromString_5, ((Class<? extends Behavior>) _forName_1), 
                  (-1));
              } else {
                throw Exceptions.sneakyThrow(_t);
              }
            }
          }
          UUID _fromString_3 = UUID.fromString(token0);
          Class<?> _forName = Class.forName(token1);
          return new BehaviorName(name, null, null, _fromString_3, 
            ((Class<? extends Behavior>) _forName), (-1));
        }
      }
      return null;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Constructor.
   * 
   * @optionalparam scheme the name scheme that is supported by this parser. By default it is {@link NameScheme.BEHAVIOR}.
   */
  @DefaultValueUse("io.sarl.api.naming.scheme.NameScheme")
  @SyntheticMember
  public BehaviorSchemeNameParser() {
    this($DEFAULT_VALUE$NEW_0);
  }
}
