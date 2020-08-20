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
import io.sarl.api.naming.parser.INameParser;
import io.sarl.api.naming.parser.ISchemeNameParser;
import io.sarl.api.naming.scheme.NameScheme;
import io.sarl.api.naming.scheme.NameSchemes;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import java.net.URI;
import java.util.Set;
import java.util.TreeMap;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.StringExtensions;

/**
 * Default implementation of a parser of names that is accepting URI-based syntax.
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
public class UriBasedNameParser implements INameParser {
  private final TreeMap<NameScheme, ISchemeNameParser<?>> schemeNameParser = CollectionLiterals.<NameScheme, ISchemeNameParser<?>>newTreeMap(null);
  
  /**
   * Construct a name parser based on the given scheme parsers.
   * 
   * @param parsers the set of scheme name parsers.
   */
  @DefaultValueSource
  @SuppressWarnings("raw_type")
  public UriBasedNameParser(@DefaultValue("io.sarl.api.naming.parser.UriBasedNameParser#NEW_0") final Set<ISchemeNameParser> parsers) {
    if (((parsers != null) && (!parsers.isEmpty()))) {
      this.setSchemeNameParsers(parsers);
    }
  }
  
  /**
   * Default value for the parameter parsers
   */
  @SyntheticMember
  @SarlSourceCode("null")
  private static final Set $DEFAULT_VALUE$NEW_0 = null;
  
  /**
   * Change the set of scheme name parsers that is used by this base name parser.
   * 
   * <p>This function could be overridden and annotated in subtypes in order to be used by the Guice injector.
   * 
   * @param parsers the set of scheme name parsers.
   */
  @SuppressWarnings("raw_type")
  public void setSchemeNameParsers(final Set<ISchemeNameParser> parsers) {
    if ((parsers != null)) {
      this.schemeNameParser.clear();
      for (final ISchemeNameParser parser : parsers) {
        this.addSchemeNameParser(parser);
      }
    }
  }
  
  @Override
  public void addSchemeNameParser(final ISchemeNameParser<?> parser) {
    class $AssertEvaluator$ {
      final boolean $$result;
      $AssertEvaluator$() {
        this.$$result = (parser != null);
      }
    }
    assert new $AssertEvaluator$().$$result;
    this.schemeNameParser.put(parser.getScheme(), parser);
  }
  
  @Override
  public ISchemeNameParser<?> removeSchemeNameParser(final NameScheme scheme) {
    ISchemeNameParser<?> _xblockexpression = null;
    {
      class $AssertEvaluator$ {
        final boolean $$result;
        $AssertEvaluator$() {
          this.$$result = (scheme != null);
        }
      }
      assert new $AssertEvaluator$().$$result;
      _xblockexpression = this.schemeNameParser.remove(scheme);
    }
    return _xblockexpression;
  }
  
  @Pure
  public URI normalize(final URI name) {
    try {
      final String scheme = name.getScheme();
      final NameScheme schemeObj = NameSchemes.getSchemeObject(scheme);
      if (((((schemeObj != null) && StringExtensions.isNullOrEmpty(name.getQuery())) && StringExtensions.isNullOrEmpty(name.getUserInfo())) && (name.getPort() == (-1)))) {
        ISchemeNameParser<?> parser = this.schemeNameParser.get(schemeObj);
        if ((parser != null)) {
          return parser.refactor(name);
        }
      }
    } catch (final Throwable _t) {
      if (_t instanceof Throwable) {
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return null;
  }
  
  @Pure
  @Override
  public SarlName decode(final URI name) {
    try {
      if ((((name != null) && (name.getPath() != null)) && name.getPath().startsWith("/"))) {
        NameScheme scheme = NameSchemes.getSchemeObject(name.getScheme());
        if ((scheme != null)) {
          ISchemeNameParser<?> parser = this.schemeNameParser.get(scheme);
          if ((parser != null)) {
            return parser.decode(name);
          }
        }
      }
    } catch (final Throwable _t) {
      if (_t instanceof Throwable) {
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return null;
  }
  
  /**
   * Construct a name parser based on the given scheme parsers.
   * 
   * @optionalparam parsers the set of scheme name parsers.
   */
  @DefaultValueUse("java.util.Set<io.sarl.api.naming.parser.ISchemeNameParser>")
  @SyntheticMember
  public UriBasedNameParser() {
    this($DEFAULT_VALUE$NEW_0);
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
}
