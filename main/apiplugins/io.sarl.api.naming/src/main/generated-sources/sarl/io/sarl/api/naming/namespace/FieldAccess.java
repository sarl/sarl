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
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import org.eclipse.xtend.lib.annotations.AccessorType;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Accessor to a field.
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
public class FieldAccess {
  @Accessors(AccessorType.PUBLIC_GETTER)
  private final SarlName name;
  
  @Accessors(AccessorType.PUBLIC_GETTER)
  private final Field field;
  
  @Accessors(AccessorType.PUBLIC_GETTER)
  private final Object instance;
  
  /**
   * Constructor.
   * 
   * @param name the name of the field.
   * @param field the field declaration.
   * @param instance the object to have access to.
   */
  public FieldAccess(final SarlName name, final Field field, final Object instance) {
    class $AssertEvaluator$ {
      final boolean $$result;
      $AssertEvaluator$() {
        boolean _isStatic = Modifier.isStatic(field.getModifiers());
        this.$$result = (!_isStatic);
      }
    }
    assert new $AssertEvaluator$().$$result;
    this.name = name;
    this.field = field;
    this.instance = instance;
    this.field.setAccessible(true);
  }
  
  /**
   * Replies the value of the field.
   * 
   * @return the field's value.
   */
  @Pure
  public Object get() {
    try {
      return this.field.get(this.instance);
    } catch (final Throwable _t) {
      if (_t instanceof Throwable) {
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return null;
  }
  
  /**
   * Change the value of the field.
   * 
   * @param value the field's value.
   * @return the value before setting.
   * @throws IllegalArgumentException if the given value cannot be assigned to the field.
   */
  public Object set(final Object value) throws IllegalArgumentException {
    try {
      final Object oldValue = this.field.get(this.instance);
      this.field.set(this.instance, value);
      return oldValue;
    } catch (final Throwable _t) {
      if (_t instanceof IllegalArgumentException) {
        final IllegalArgumentException ex = (IllegalArgumentException)_t;
        throw ex;
      } else if (_t instanceof Throwable) {
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return null;
  }
  
  @Pure
  @Override
  public String toString() {
    return this.field.getName();
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
  public SarlName getName() {
    return this.name;
  }
  
  @Pure
  public Field getField() {
    return this.field;
  }
  
  @Pure
  public Object getInstance() {
    return this.instance;
  }
}
