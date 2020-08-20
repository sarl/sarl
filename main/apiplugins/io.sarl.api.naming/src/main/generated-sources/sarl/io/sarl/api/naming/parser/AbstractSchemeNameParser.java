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
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import java.net.URI;
import java.util.StringTokenizer;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.StringExtensions;

/**
 * Abstact implementation of a parser of names that is accepting URI-based syntax for a specific scheme
 * 
 * @param <N> the type of the name that is the result of the decoding.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@SarlSpecification("0.12")
@SarlElementType(10)
@SuppressWarnings("all")
public abstract class AbstractSchemeNameParser<N extends SarlName> implements ISchemeNameParser<N> {
  @Accessors
  private final NameScheme scheme;
  
  /**
   * Constructor.
   * 
   * @param scheme the scheme that is supported by this name parser.
   */
  protected AbstractSchemeNameParser(final NameScheme scheme) {
    class $AssertEvaluator$ {
      final boolean $$result;
      $AssertEvaluator$() {
        this.$$result = (scheme != null);
      }
    }
    assert new $AssertEvaluator$().$$result;
    this.scheme = scheme;
  }
  
  /**
   * Refactors the URI.
   * 
   * @param uri the URI to refactor.
   * @param minElements is the expected minimum number of path components.
   * @param maxElements is the expected maximum number of path components.
   * @param lastIsInteger indicates if the last component could be an integer. In this case,
   * the expected number of not-integer components becomes [{@code minElements}, {@code maxElements - 1}].
   * @return the new URI.
   */
  @DefaultValueSource
  protected URI refactor(final URI uri, final int minElements, final int maxElements, @DefaultValue("io.sarl.api.naming.parser.AbstractSchemeNameParser#REFACTOR_0") final boolean lastIsInteger) {
    try {
      final StringBuilder newPath = new StringBuilder();
      boolean _isNullOrEmpty = StringExtensions.isNullOrEmpty(uri.getHost());
      if ((!_isNullOrEmpty)) {
        newPath.append("/").append(uri.getHost());
      }
      boolean _isNullOrEmpty_1 = StringExtensions.isNullOrEmpty(uri.getPath());
      if ((!_isNullOrEmpty_1)) {
        boolean _startsWith = uri.getPath().startsWith("/");
        if ((!_startsWith)) {
          newPath.append("/");
        }
        newPath.append(uri.getPath());
      }
      int _length = newPath.length();
      if ((_length == 0)) {
        boolean _isNullOrEmpty_2 = StringExtensions.isNullOrEmpty(uri.getSchemeSpecificPart());
        if ((!_isNullOrEmpty_2)) {
          boolean _startsWith_1 = uri.getSchemeSpecificPart().startsWith("/");
          if ((!_startsWith_1)) {
            newPath.append("/");
          }
          newPath.append(uri.getSchemeSpecificPart());
        }
      }
      final String path = newPath.toString();
      boolean _isEmpty = path.isEmpty();
      if ((!_isEmpty)) {
        final String validatedPath = this.validatePath(path, minElements, maxElements, lastIsInteger);
        boolean _isNullOrEmpty_3 = StringExtensions.isNullOrEmpty(validatedPath);
        if ((!_isNullOrEmpty_3)) {
          String _scheme = uri.getScheme();
          String _string = validatedPath.toString();
          String _fragment = uri.getFragment();
          return new URI(_scheme, null, _string, _fragment);
        }
      }
      return null;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Default value for the parameter lastIsInteger
   */
  @SyntheticMember
  @SarlSourceCode("false")
  private static final boolean $DEFAULT_VALUE$REFACTOR_0 = false;
  
  /**
   * Validate the path.
   * 
   * @param path the path to validate.
   * @param minElements is the expected minimum number of path components.
   * @param maxElements is the expected maximum number of path components.
   * @param lastIsInteger indicates if the last component could be an integer. In this case,
   * the expected number of not-integer components becomes [{@code minElements}, {@code maxElements - 1}].
   * @return the validated path.
   */
  @DefaultValueSource
  protected String validatePath(final String path, final int minElements, final int maxElements, @DefaultValue("io.sarl.api.naming.parser.AbstractSchemeNameParser#VALIDATEPATH_0") final boolean lastIsInteger) {
    boolean _isNullOrEmpty = StringExtensions.isNullOrEmpty(path);
    if (_isNullOrEmpty) {
      String _xifexpression = null;
      boolean _contains = new IntegerRange(minElements, maxElements).contains(0);
      if (_contains) {
        _xifexpression = "";
      } else {
        _xifexpression = null;
      }
      return _xifexpression;
    }
    StringTokenizer tokenizer = new StringTokenizer(path, "/");
    StringBuilder buffer = new StringBuilder();
    String lastComponent = null;
    int nb = 0;
    while (tokenizer.hasMoreTokens()) {
      {
        String token = tokenizer.nextToken();
        boolean _isNullOrEmpty_1 = StringExtensions.isNullOrEmpty(token);
        if (_isNullOrEmpty_1) {
          return null;
        }
        buffer.append("/").append(token);
        lastComponent = token;
        nb++;
      }
    }
    if (lastIsInteger) {
      try {
        Integer.parseUnsignedInt(lastComponent, 10);
        boolean _contains_1 = new IntegerRange((minElements + 1), maxElements).contains(nb);
        if (_contains_1) {
          return buffer.toString();
        }
      } catch (final Throwable _t) {
        if (_t instanceof Throwable) {
          boolean _contains_2 = new IntegerRange(minElements, (maxElements - 1)).contains(nb);
          if (_contains_2) {
            return buffer.toString();
          }
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
    } else {
      boolean _contains_1 = new IntegerRange(minElements, maxElements).contains(nb);
      if (_contains_1) {
        return buffer.toString();
      }
    }
    return null;
  }
  
  /**
   * Default value for the parameter lastIsInteger
   */
  @SyntheticMember
  @SarlSourceCode("false")
  private static final boolean $DEFAULT_VALUE$VALIDATEPATH_0 = false;
  
  /**
   * Refactors the URI.
   * 
   * @param uri the URI to refactor.
   * @param minElements is the expected minimum number of path components.
   * @param maxElements is the expected maximum number of path components.
   * @optionalparam lastIsInteger indicates if the last component could be an integer. In this case,
   * the expected number of not-integer components becomes [{@code minElements}, {@code maxElements - 1}].
   * @return the new URI.
   */
  @DefaultValueUse("java.net.URI,int,int,boolean")
  @SyntheticMember
  protected final URI refactor(final URI uri, final int minElements, final int maxElements) {
    return refactor(uri, minElements, maxElements, $DEFAULT_VALUE$REFACTOR_0);
  }
  
  /**
   * Validate the path.
   * 
   * @param path the path to validate.
   * @param minElements is the expected minimum number of path components.
   * @param maxElements is the expected maximum number of path components.
   * @optionalparam lastIsInteger indicates if the last component could be an integer. In this case,
   * the expected number of not-integer components becomes [{@code minElements}, {@code maxElements - 1}].
   * @return the validated path.
   */
  @DefaultValueUse("java.lang.String,int,int,boolean")
  @SyntheticMember
  protected final String validatePath(final String path, final int minElements, final int maxElements) {
    return validatePath(path, minElements, maxElements, $DEFAULT_VALUE$VALIDATEPATH_0);
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
  
  @Pure
  public NameScheme getScheme() {
    return this.scheme;
  }
}
