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

import java.util.Iterator;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import org.eclipse.emf.common.util.BasicEList;

/**
 * A definition of the types of the formal parameters of an action.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ActionParameterTypes extends BasicEList<String> implements Comparable<ActionParameterTypes> {

	private static final long serialVersionUID = 8389816963923769014L;

	private final boolean isVarargs;

	/** Constructor.
	 * @param isVarArgs indicates if this signature has the varargs flag.
	 * @param initialCapacity initial capacity of the array.
	 */
	public ActionParameterTypes(boolean isVarArgs, int initialCapacity) {
		super(initialCapacity);
		this.isVarargs = isVarArgs;
	}

	/** Parse the given string and create a signature.
	 *
	 * <p>The format of the text is the same as the one replied by {@link #toString()}.
	 *
	 * @param text the text that contains the signature to parse.
	 */
	public ActionParameterTypes(String text) {
		assert text != null;
		final String[] elements = text.split("\\s*,\\s*"); //$NON-NLS-1$
		this.isVarargs = elements.length > 0 && elements[elements.length - 1].endsWith("*"); //$NON-NLS-1$
		if (this.isVarargs) {
			elements[elements.length - 1] = elements[elements.length - 1].replaceFirst(
					"\\*$", "[]");  //$NON-NLS-1$//$NON-NLS-2$
		}
		for (final String p : elements) {
			if (!Strings.isNullOrEmpty(p) && !"void".equals(p) && !"java.lang.Void".equals(p)) { //$NON-NLS-1$//$NON-NLS-2$
				add(p);
			}
		}
	}

	@Override
	public boolean equals(Object object) {
		if (this == object) {
			return true;
		}

		if (object == null) {
			return false;
		}

		if (this.getClass() == object.getClass() && object instanceof ActionParameterTypes && super.equals(object)) {
			final ActionParameterTypes types = (ActionParameterTypes) object;
			return this.isVarargs == types.isVarargs;
		}
		return false;
	}

	@Override
	public int hashCode() {
		return Objects.hashCode(super.hashCode(), this.isVarargs);
	}

	/** Replies if this signature has a variatic parameter.
	 *
	 * @return <code>true</code> if the last element is variatic.
	 */
	public boolean isVarArg() {
		return this.isVarargs;
	}

	/** Replies if this signature is for Void.
	 *
	 * @return <code>true</code> if the signature is for Void.
	 */
	public boolean isVoid() {
		return size() == 0;
	}

	@Override
	public ActionParameterTypes clone() {
		return (ActionParameterTypes) super.clone();
	}

	@Override
	public String toString() {
		return toString(false);
	}

	/** Replies the string representation of this list of parameters.
	 *
	 * @param isRaw indicates if the output should be raw (without type parameters) or not (with type parameters).
	 * @return the string representation of the parameter list.
	 */
	public String toString(boolean isRaw) {
		if (!isEmpty()) {
			final StringBuilder b = new StringBuilder();
			final int size = size() - 1;
			for (int i = 0; i < size; ++i) {
				if (i > 0) {
					b.append(","); //$NON-NLS-1$
				}
				final String elt;
				if (isRaw) {
					elt = get(i).replaceFirst("\\<.*\\>", ""); //$NON-NLS-1$ //$NON-NLS-2$
				} else {
					elt = get(i);
				}
				b.append(elt);
			}
			String lastElement = get(size);
			if (isVarArg()) {
				lastElement = lastElement.replaceFirst("\\[\\]$", "*");  //$NON-NLS-1$//$NON-NLS-2$
			}
			if (isRaw) {
				lastElement = lastElement.replaceFirst("\\<.*\\>", ""); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (size > 0) {
				b.append(","); //$NON-NLS-1$
			}
			b.append(lastElement);
			return b.toString();
		}
		return ""; //$NON-NLS-1$
	}

	@Override
	public int compareTo(ActionParameterTypes otherTypes) {
		if (otherTypes == null) {
			return Integer.MAX_VALUE;
		}
		int cmp = Integer.compare(size(), otherTypes.size());
		if (cmp != 0) {
			return cmp;
		}
		final Iterator<String> i1 = iterator();
		final Iterator<String> i2 = otherTypes.iterator();
		while (i1.hasNext() && i2.hasNext()) {
			final String s1 = i1.next();
			final String s2 = i2.next();
			cmp = s1.compareTo(s2);
			if (cmp != 0) {
				return cmp;
			}
		}
		return 0;
	}

	/** Replies the action prototype associate to this list of parameters.
	 *
	 * @param actionName the id of the action.
	 * @return the action key.
	 */
	public ActionPrototype toActionPrototype(String actionName) {
		return new ActionPrototype(actionName, this, false);
	}

	/** Replies the action prototype associate to this list of parameters with the type parameters.
	 *
	 * @param actionName the id of the action.
	 * @return the action key without the type parameters.
	 */
	public ActionPrototype toRawActionPrototype(String actionName) {
		return new ActionPrototype(actionName, this, true);
	}

}
