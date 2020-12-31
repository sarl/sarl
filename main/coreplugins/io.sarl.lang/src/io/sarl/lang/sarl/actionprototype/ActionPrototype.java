/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.lang.sarl.actionprototype;

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

	private final boolean isRaw;

	/** Constructor.
	 * @param function name of the function for which this signature is created.
	 * @param signature description of the parameters of the function.
	 * @param isRaw indicates if the prototype is raw (without type parameter) or not (with type parameters).
	 */
	public ActionPrototype(String function, ActionParameterTypes signature, boolean isRaw) {
		this.function = function;
		this.signature = signature;
		this.isRaw = isRaw;
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
			final ActionPrototype prototype = (ActionPrototype) super.clone();
			prototype.signature = this.signature.clone();
			return prototype;
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
			final ActionPrototype prototype = (ActionPrototype) obj;
			return this.function.equals(prototype.function)
					&& this.signature.equals(prototype.signature);
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
		return this.function + "(" + this.signature.toString(this.isRaw) + ")"; //$NON-NLS-1$//$NON-NLS-2$
	}

	@Override
	public int compareTo(ActionPrototype otherProto) {
		if (otherProto == null) {
			return Integer.MAX_VALUE;
		}
		final int cmp = this.function.compareTo(otherProto.function);
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
		final StringBuilder b = new StringBuilder();
		b.append(getActionName());
		for (final String type : this.signature) {
			b.append("_"); //$NON-NLS-1$
			for (final char c : type.replaceAll("(\\[\\])|\\*", "Array").toCharArray()) { //$NON-NLS-1$//$NON-NLS-2$
				if (Character.isJavaIdentifierPart(c)) {
					b.append(c);
				}
			}
		}
		return b.toString();
	}

}
