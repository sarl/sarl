/*
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
package io.sarl.lang.signature;

import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.FormalParameter;

import java.util.Comparator;
import java.util.Iterator;

/**
 * A key for signatures.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ActionSignatureComparator implements Comparator<ActionSignature> {

	/**
	 */
	public ActionSignatureComparator() {
		
	}
	
	/** {@inheritDoc}
	 */
	public int compare(ActionSignature a, ActionSignature b) {
		if (a==b) return 0;
		if (a==null) return Integer.MIN_VALUE;
		if (b==null) return Integer.MAX_VALUE;
		int cmp = Integer.compare(a.getParams().size(), b.getParams().size());
		if (cmp!=0) return cmp;
		Iterator<FormalParameter> i1 = a.getParams().iterator();
		Iterator<FormalParameter> i2 = b.getParams().iterator();
		while (i1.hasNext() && i2.hasNext()) {
			FormalParameter p1 = i1.next();
			FormalParameter p2 = i2.next();
			cmp = p1.getParameterType().getIdentifier().compareTo(
					p2.getParameterType().getIdentifier());
			if (cmp!=0) return cmp;
		}
		return 0;
	}
	
}
