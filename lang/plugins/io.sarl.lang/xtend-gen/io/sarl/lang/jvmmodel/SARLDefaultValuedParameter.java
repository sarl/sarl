/**
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.jvmmodel;

import com.google.common.base.Objects;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XExpression;

/**
 * This class permits to wrap a default value when building the function signatures.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
class SARLDefaultValuedParameter {
  public final XExpression expr;
  
  public final JvmTypeReference type;
  
  public SARLDefaultValuedParameter(final XExpression e, final JvmTypeReference type) {
    this.expr = e;
    this.type = type;
  }
  
  public String toString() {
    boolean _equals = Objects.equal(this.expr, null);
    if (_equals) {
      return null;
    }
    return this.expr.toString();
  }
}
