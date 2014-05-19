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

import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.FormalParameter;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import org.eclipse.emf.common.util.EList;
import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * This class permits to compare the action signatures.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
class SARLActionSignatureComparator implements Comparator<ActionSignature> {
  public int compare(final ActionSignature a, final ActionSignature b) {
    String _name = a.getName();
    String _name_1 = b.getName();
    int cmp = _name.compareTo(_name_1);
    if ((cmp != 0)) {
      return cmp;
    }
    EList<FormalParameter> _params = a.getParams();
    EList<FormalParameter> _params_1 = b.getParams();
    return this.compare(_params, _params_1);
  }
  
  public int compare(final List<FormalParameter> a, final List<FormalParameter> b) {
    int _size = a.size();
    int _size_1 = b.size();
    int cmp = (Integer.valueOf(_size).compareTo(Integer.valueOf(_size_1)));
    if ((cmp != 0)) {
      return cmp;
    }
    final Iterator<FormalParameter> i1 = a.iterator();
    final Iterator<FormalParameter> i2 = b.iterator();
    boolean _and = false;
    boolean _hasNext = i1.hasNext();
    if (!_hasNext) {
      _and = false;
    } else {
      boolean _hasNext_1 = i2.hasNext();
      _and = (_hasNext && _hasNext_1);
    }
    boolean _while = _and;
    while (_while) {
      {
        FormalParameter _next = i1.next();
        JvmTypeReference _parameterType = _next.getParameterType();
        String _identifier = _parameterType.getIdentifier();
        FormalParameter _next_1 = i2.next();
        JvmTypeReference _parameterType_1 = _next_1.getParameterType();
        String _identifier_1 = _parameterType_1.getIdentifier();
        int _compareTo = _identifier.compareTo(_identifier_1);
        cmp = _compareTo;
        if ((cmp != 0)) {
          return cmp;
        }
      }
      boolean _and_1 = false;
      boolean _hasNext_2 = i1.hasNext();
      if (!_hasNext_2) {
        _and_1 = false;
      } else {
        boolean _hasNext_3 = i2.hasNext();
        _and_1 = (_hasNext_2 && _hasNext_3);
      }
      _while = _and_1;
    }
    return 0;
  }
}
