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

import io.sarl.api.naming.name.SarlName;
import io.sarl.api.naming.namespace.AbstractNamespaceService;
import io.sarl.api.naming.namespace.INamespaceFinder;
import io.sarl.api.naming.scheme.NameScheme;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import java.util.Set;
import java.util.TreeMap;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Implementation of a namespace service that uses the namespace finders.
 * The finders are in charge of finding a specific type of element.
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
public class FinderBasedNamespaceService extends AbstractNamespaceService {
  @SuppressWarnings("raw_type")
  private final TreeMap<NameScheme, INamespaceFinder> finders = CollectionLiterals.<NameScheme, INamespaceFinder>newTreeMap(null);
  
  /**
   * Change the set of namespace finders that is used by this service.
   * 
   * <p>This function could be overridden and annotated in subtypes in order to be used by the Guice injector.
   * 
   * @param finders the set of finders.
   */
  @SuppressWarnings("raw_type")
  public void setNamespaceFinders(final Set<INamespaceFinder> finders) {
    if ((finders != null)) {
      this.finders.clear();
      for (final INamespaceFinder finder : finders) {
        this.addNamespaceFinder(finder);
      }
    }
  }
  
  /**
   * Add a namespace finder.
   * 
   * @param finder the namespace finder to add.
   */
  public void addNamespaceFinder(final INamespaceFinder<?, ?> finder) {
    this.finders.put(finder.getScheme(), finder);
  }
  
  /**
   * Remove a namespace finder.
   * 
   * @param scheme the scheme of the finder to remove.
   */
  public void removeNamespaceFinder(final NameScheme scheme) {
    this.finders.remove(scheme);
  }
  
  /**
   * Find an object by ignoring the fragment component of the given name
   * 
   * @param name the name of the element to search for.
   * @return the element with the given name, or {@code null} it is was not found.
   */
  public Object findObjectWithoutFragment(final SarlName name) {
    final INamespaceFinder finder = this.finders.get(name.getScheme());
    if ((finder != null)) {
      return finder.find(name);
    }
    return null;
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
  
  @SyntheticMember
  public FinderBasedNamespaceService() {
    super();
  }
}
