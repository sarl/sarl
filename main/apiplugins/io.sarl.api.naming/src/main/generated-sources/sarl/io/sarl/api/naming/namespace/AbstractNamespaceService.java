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

import com.google.common.base.Objects;
import com.google.common.util.concurrent.AbstractService;
import io.sarl.api.naming.name.SarlName;
import io.sarl.api.naming.namespace.FieldAccess;
import io.sarl.api.naming.namespace.NamespaceService;
import io.sarl.api.naming.parser.INameParser;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import java.lang.reflect.Field;
import javax.inject.Inject;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Abstract implementation of a service that manages name spaces into the SRE.
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
public abstract class AbstractNamespaceService extends AbstractService implements NamespaceService {
  private INameParser nameParser;
  
  @Override
  protected final void doStart() {
    try {
      this.onStart();
      this.notifyStarted();
    } catch (final Throwable _t) {
      if (_t instanceof Throwable) {
        final Throwable exception = (Throwable)_t;
        this.notifyFailed(exception);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
  }
  
  /**
   * Do something when starting the service.
   * 
   * <p>This function is called from {@link #doStart()}
   */
  protected void onStart() {
  }
  
  @Override
  protected final void doStop() {
    try {
      this.onStop();
      this.notifyStopped();
    } catch (final Throwable _t) {
      if (_t instanceof Throwable) {
        final Throwable exception = (Throwable)_t;
        this.notifyFailed(exception);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
  }
  
  /**
   * Do something when stopping the service.
   * 
   * <p>This function is called from {@link #doStop()}
   */
  protected void onStop() {
  }
  
  @Pure
  @Override
  public INameParser getNameParser() {
    return this.nameParser;
  }
  
  /**
   * Change the name parser used by this service.
   * 
   * @param parser the name parser, never {@code null}
   * @since 0.11
   */
  @Inject
  public void setNameParser(final INameParser parser) {
    this.nameParser = parser;
  }
  
  @Override
  @Pure
  public final <T extends Object> T findObject(final SarlName name, final Class<T> type) {
    class $AssertEvaluator$ {
      final boolean $$result;
      $AssertEvaluator$() {
        this.$$result = (type != null);
      }
    }
    assert new $AssertEvaluator$().$$result;
    Object obj = this.findObject(name);
    if (((obj != null) && type.isInstance(obj))) {
      return type.cast(obj);
    }
    return null;
  }
  
  @Override
  @Pure
  public final Object findObject(final SarlName name) {
    if ((name == null)) {
      return null;
    }
    Object associatedObject = name.getAssociatedObject();
    if ((associatedObject == null)) {
      Object obj = this.findObjectWithoutFragment(name);
      if (((obj != null) && name.hasFragment())) {
        FieldAccess field = AbstractNamespaceService.getDeclaredField(obj, name);
        if ((field != null)) {
          associatedObject = field;
        }
        return field;
      } else {
        try {
          associatedObject = obj;
        } catch (final Throwable _t) {
          if (_t instanceof ClassCastException) {
          } else {
            throw Exceptions.sneakyThrow(_t);
          }
        }
      }
    }
    return associatedObject;
  }
  
  @Pure
  private static FieldAccess getDeclaredField(final Object obj, final SarlName name) {
    Class<?> type = obj.getClass();
    while (((type != null) && (!Objects.equal(Object.class, type)))) {
      {
        try {
          Field field = type.getDeclaredField(name.getFragment());
          if ((field != null)) {
            return new FieldAccess(name, field, obj);
          }
        } catch (final Throwable _t) {
          if (_t instanceof Throwable) {
          } else {
            throw Exceptions.sneakyThrow(_t);
          }
        }
        type = type.getSuperclass();
      }
    }
    return null;
  }
  
  /**
   * Find an object with the given name, but ignoring the fragment.
   * 
   * @param name the name of the object, never {@code null}.
   * @return the object, or {@code null} if the object was not found.
   */
  protected abstract Object findObjectWithoutFragment(final SarlName name);
  
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
  public AbstractNamespaceService() {
    super();
  }
}
