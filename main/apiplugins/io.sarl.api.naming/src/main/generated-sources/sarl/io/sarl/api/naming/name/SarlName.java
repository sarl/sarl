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

import io.sarl.api.naming.scheme.NameScheme;
import io.sarl.api.naming.scheme.NameSchemes;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import java.io.Serializable;
import java.lang.ref.WeakReference;
import java.net.URI;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.StringExtensions;

/**
 * This class represents a component name. Each component in a name is an atomic name.
 * 
 * <p>The components of a name are numbered. The indexes of a
 * name with N components range from 0 up to, but not including, N.
 * An empty compound name has no components.
 * 
 * <h1>Multithreaded Access</h1>
 * A {@code SName} instance is not synchronized against concurrent
 * multithreaded access. Multiple threads trying to access and modify a
 * {@code SName} should lock the object.
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
public abstract class SarlName implements Cloneable, Serializable, Comparable<SarlName> {
  private transient WeakReference<Object> associatedObject;
  
  private final URI uri;
  
  private NameScheme scheme;
  
  protected SarlName(final URI uri) {
    class $AssertEvaluator$ {
      final boolean $$result;
      $AssertEvaluator$() {
        this.$$result = (uri != null);
      }
    }
    assert new $AssertEvaluator$().$$result;
    this.uri = uri;
  }
  
  @Override
  @Pure
  public String toString() {
    return this.uri.toString();
  }
  
  /**
   * Replies the scheme of this name.
   */
  @Pure
  public NameScheme getScheme() {
    if ((this.scheme == null)) {
      this.scheme = NameSchemes.getSchemeObject(this.toURI().getScheme());
    }
    return this.scheme;
  }
  
  @Pure
  @Override
  public int compareTo(final SarlName name) {
    int _xifexpression = (int) 0;
    if ((name == null)) {
      _xifexpression = 1;
    } else {
      _xifexpression = this.uri.compareTo(name.uri);
    }
    return _xifexpression;
  }
  
  /**
   * Replies the URI associated to this name.
   */
  @Pure
  public URI toURI() {
    return this.uri;
  }
  
  /**
   * Replies the name of the fragment.
   */
  @Pure
  public String getFragment() {
    return this.uri.getFragment();
  }
  
  /**
   * Replies if this name has a fragment associated to it.
   */
  @Pure
  public boolean hasFragment() {
    boolean _isNullOrEmpty = StringExtensions.isNullOrEmpty(this.uri.getFragment());
    return (!_isNullOrEmpty);
  }
  
  /**
   * Replies the associated object.
   */
  @Pure
  public final Object getAssociatedObject() {
    WeakReference<Object> _associatedObject = this.associatedObject;
    Object _get = null;
    if (_associatedObject!=null) {
      _get=_associatedObject.get();
    }
    return _get;
  }
  
  /**
   * Change the associated object.
   */
  public final void setAssociatedObject(final Object obj) {
    WeakReference<Object> _xifexpression = null;
    if ((obj == null)) {
      _xifexpression = null;
    } else {
      _xifexpression = new WeakReference<Object>(obj);
    }
    this.associatedObject = _xifexpression;
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
  public SarlName clone() {
    try {
      return (SarlName) super.clone();
    } catch (Throwable exception) {
      throw new Error(exception);
    }
  }
  
  @SyntheticMember
  private static final long serialVersionUID = -4780923653L;
}
