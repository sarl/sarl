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
package io.sarl.lang.util;

import io.sarl.lang.sarl.FormalParameter;

import java.util.Comparator;
import java.util.Iterator;

import org.eclipse.emf.common.util.EList;
import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * Comparator of lists of formal parameters.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class FormalParameterListComparator implements Comparator<EList<FormalParameter>> {

	/**
	 */
	public FormalParameterListComparator() {
		
	}
	
	/** {@inheritDoc}
	 */
	@Override
	public int compare(EList<FormalParameter> a, EList<FormalParameter> b) {
		if (a==b) return 0;
		if (a==null) return Integer.MIN_VALUE;
		if (b==null) return Integer.MAX_VALUE;
		int cmp = Integer.compare(a.size(), b.size());
		if (cmp!=0) return cmp;
		Iterator<FormalParameter> i1 = a.iterator();
		Iterator<FormalParameter> i2 = b.iterator();
		while (i1.hasNext() && i2.hasNext()) {
			FormalParameter p1 = i1.next();
			FormalParameter p2 = i2.next();
			if (p1!=p2) {
				if (p1==null) return Integer.MIN_VALUE;
				if (p2==null) return Integer.MAX_VALUE;
				JvmTypeReference t1 = p1.getParameterType();
				JvmTypeReference t2 = p2.getParameterType();
				if (t1!=t2) {
					if (t1==null) return Integer.MIN_VALUE;
					if (t2==null) return Integer.MAX_VALUE;
					cmp = t1.getIdentifier().compareTo(t2.getIdentifier());
					if (cmp!=0) return cmp;
				}
			}
		}
		return 0;
	}
	
}
