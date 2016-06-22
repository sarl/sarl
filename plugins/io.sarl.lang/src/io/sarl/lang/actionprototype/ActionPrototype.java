/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.actionprototype;

import java.io.Serializable;

/**
 * Definition of the prototype of an action (name+parameters).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ActionPrototype implements Cloneable, Serializable, Comparable<ActionPrototype> {

	private static final long serialVersionUID = -3797441140684942274L;

	private String function;

	private ActionParameterTypes signature;

	/**
	 * @param function - name of the function for which this signature is created.
	 * @param signature - description of the parameters of the function.
	 */
	public ActionPrototype(String function, ActionParameterTypes signature) {
		this.function = function;
		this.signature = signature;
	}

	/** Replies the name of the action for this prototype.
	 *
	 * @return the name.
	 */
	public String getActionName() {
		return this.function;
	}

	/** Replies the types of the formal parameters.
	 *
	 * @return the name.
	 */
	public ActionParameterTypes getParametersTypes() {
		return this.signature;
	}

	@Override
	public ActionPrototype clone() {
		try {
			ActionPrototype k;
			k = (ActionPrototype) super.clone();
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

		if (obj == null) {
			return false;
		}

		if (this.getClass() == obj.getClass()) {
			ActionPrototype k = (ActionPrototype) obj;
			return this.function.equals(k.function)
					&& this.signature.equals(k.signature);
		}
		return false;
	}

	@Override
	public int hashCode() {
		int hash = 1;
		hash = 31 * hash + this.function.hashCode();
		hash = 31 * hash + this.signature.hashCode();
		return hash;
	}

	@Override
	public String toString() {
		return this.function + "(" + this.signature + ")"; //$NON-NLS-1$//$NON-NLS-2$
	}

	@Override
	public int compareTo(ActionPrototype otherProto) {
		if (otherProto == null) {
			return Integer.MAX_VALUE;
		}
		int cmp = this.function.compareTo(otherProto.function);
		if (cmp != 0) {
			return cmp;
		}
		return this.signature.compareTo(otherProto.signature);
	}

	/** Replies the string that permits to identify the action prototype according to the Java variable name.
	 *
	 * @return the identifier.
	 */
	public String toActionId() {
		StringBuilder b = new StringBuilder();
		b.append(getActionName());
		for (String type : this.signature) {
			b.append("_"); //$NON-NLS-1$
			for (char c : type.replaceAll("(\\[\\])|\\*", "Array").toCharArray()) { //$NON-NLS-1$//$NON-NLS-2$
				if (Character.isJavaIdentifierPart(c)) {
					b.append(c);
				}
			}
		}
		return b.toString();
	}

}
