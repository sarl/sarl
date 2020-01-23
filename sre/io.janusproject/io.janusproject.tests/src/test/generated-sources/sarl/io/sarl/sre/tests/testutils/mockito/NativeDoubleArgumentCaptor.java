/**
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2019 the original authors or authors.
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
package io.sarl.sre.tests.testutils.mockito;

import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import java.util.List;
import org.eclipse.xtext.xbase.lib.Pure;
import org.mockito.ArgumentMatchers;
import org.mockito.internal.matchers.CapturingMatcher;
import org.mockito.internal.util.Primitives;

/**
 * An argument captor dedicated to a primitive double argument.
 * 
 * <p>This object enables to capture primitive double arguments that
 * cannot be capture with a {@code Double} argument captor.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
@SarlSpecification("0.11")
@SarlElementType(10)
@SuppressWarnings("all")
public final class NativeDoubleArgumentCaptor {
  private final CapturingMatcher<Double> capturingMatcher = new CapturingMatcher<Double>();
  
  private NativeDoubleArgumentCaptor() {
  }
  
  /**
   * Create an argument captor for primitive double values
   */
  @Pure
  public static NativeDoubleArgumentCaptor forPrimitive() {
    return new NativeDoubleArgumentCaptor();
  }
  
  /**
   * Capture the parameter value.
   */
  @Pure
  public double capture() {
    ArgumentMatchers.doubleThat(this.capturingMatcher);
    return ((Primitives.<Double>defaultValue(double.class)) == null ? 0 : (double) Primitives.<Double>defaultValue(double.class)) ;
  }
  
  /**
   * Replies the captured value.
   */
  @Pure
  public double getValue() {
    return ((this.capturingMatcher.getLastValue()) == null ? 0 : (this.capturingMatcher.getLastValue()).doubleValue());
  }
  
  /**
   * Replies all the captured values.
   */
  @Pure
  public List<Double> getAllValues() {
    return this.capturingMatcher.getAllValues();
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
