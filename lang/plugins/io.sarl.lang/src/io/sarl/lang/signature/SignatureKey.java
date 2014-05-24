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

import java.util.Iterator;

import org.eclipse.emf.common.util.BasicEList;

/**
 * A key for signatures.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SignatureKey extends BasicEList<String> implements Comparable<SignatureKey> {

	private static final long serialVersionUID = 4692246488417437647L;

	/**
	 */
	protected SignatureKey() {
		
	}
	
	/** {@inheritDoc}
	 */
	@Override
	public SignatureKey clone() {
		return (SignatureKey)super.clone();
	}
	
	/** {@inheritDoc}
	 */
	@Override
	public String toString() {
		Iterator<String> iterator = iterator();
		if (iterator.hasNext()) {
			StringBuilder b = new StringBuilder();
			b.append(iterator.next());
			while (iterator.hasNext()) {
				b.append(","); //$NON-NLS-1$
				b.append(iterator.next());
			}
			return b.toString();
		}
		return ""; //$NON-NLS-1$
	}

	/** {@inheritDoc}
	 */
	public int compareTo(SignatureKey o) {
		if (o==null) return Integer.MAX_VALUE;
		int cmp = Integer.compare(size(), o.size());
		if (cmp!=0) return cmp;
		Iterator<String> i1 = iterator();
		Iterator<String> i2 = o.iterator();
		while (i1.hasNext() && i2.hasNext()) {
			cmp = i1.next().compareTo(i2.next());
			if (cmp!=0) return cmp;
		}
		return 0;
	}
	
	/** Replies the action key associate to this signature key.
	 * 
	 * @param actionName - the id of the action.
	 * @return the action key.
	 */
	public ActionKey toActionKey(String actionName) {
		return new ActionKey(actionName, this);
	}
	
}
