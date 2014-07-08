/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.signature;

import java.io.Serializable;

/**
 * A key for actions (name+signature).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ActionKey implements Cloneable, Serializable, Comparable<ActionKey> {

	private static final long serialVersionUID = 1025783725060379851L;

	private String function;
	private SignatureKey signature;

	/**
	 * @param function - name of the function for which this signature is created.
	 * @param signature - description of the parameters of the function.
	 */
	protected ActionKey(String function, SignatureKey signature) {
		this.function = function;
		this.signature = signature;
	}

	/** Replies the name of the function for this key.
	 *
	 * @return the name.
	 */
	public String getFunctionName() {
		return this.function;
	}

	/** Replies the name of the function for this key.
	 *
	 * @return the name.
	 */
	public SignatureKey getSignature() {
		return this.signature;
	}

	@Override
	public ActionKey clone() {
		try {
			ActionKey k;
			k = (ActionKey) super.clone();
			k.signature = this.signature.clone();
			return k;
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (obj instanceof ActionKey) {
			ActionKey k = (ActionKey) obj;
			return this.function.equals(k.function)
					&& this.signature.equals(k.signature);
		}
		return false;
	}

	@Override
	public int hashCode() {
		int h = 1;
		h = 31 * h + this.function.hashCode();
		h = 31 * h + this.signature.hashCode();
		return h;
	}

	@Override
	public String toString() {
		return this.function + "(" + this.signature + ")"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public int compareTo(ActionKey o) {
		if (o == null) {
			return Integer.MAX_VALUE;
		}
		int cmp = this.function.compareTo(o.function);
		if (cmp != 0) {
			return cmp;
		}
		return this.signature.compareTo(o.signature);
	}

}
