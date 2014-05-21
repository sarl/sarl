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

import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

/**
 * This class permits to compare the signatures when
 * building the additional signatures.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
class SARLAdditionalSignatureComparator implements Comparator<List<String>> {
  public int compare(final List<String> a, final List<String> b) {
    int _size = a.size();
    int _size_1 = b.size();
    int cmp = (Integer.valueOf(_size).compareTo(Integer.valueOf(_size_1)));
    if ((cmp != 0)) {
      return cmp;
    }
    final Iterator<String> i1 = a.iterator();
    final Iterator<String> i2 = b.iterator();
    boolean _and = false;
    boolean _hasNext = i1.hasNext();
    if (!_hasNext) {
      _and = false;
    } else {
      boolean _hasNext_1 = i2.hasNext();
      _and = _hasNext_1;
    }
    boolean _while = _and;
    while (_while) {
      {
        String _next = i1.next();
        String _next_1 = i2.next();
        int _compareTo = _next.compareTo(_next_1);
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
        _and_1 = _hasNext_3;
      }
      _while = _and_1;
    }
    return 0;
  }
}
