/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

	private static final long serialVersionUID = -7553816659790345857L;

	private final boolean isVarargs;

	/**
	 * @param isVarArgs - indicates if this signature has the varargs flag.
	 * @param initialCapacity - initional capacity of the array.
	 */
	SignatureKey(boolean isVarArgs, int initialCapacity) {
		super(initialCapacity);
		this.isVarargs = isVarArgs;
	}

	/** Parse the given string and create a signature.
	 * <p>
	 * The format of the text is the same as the one replied by {@link #toString()}.
	 *
	 * @param text - the text that contains the signature to parse.
	 */
	SignatureKey(String text) {
		assert (text != null);
		String[] elements = text.split("\\s*,\\s*"); //$NON-NLS-1$
		this.isVarargs = (elements.length > 0
				&& elements[elements.length - 1].endsWith("*")); //$NON-NLS-1$
		if (this.isVarargs) {
			elements[elements.length - 1] = elements[elements.length - 1].replaceFirst(
					"\\*$", "[]");  //$NON-NLS-1$//$NON-NLS-2$
		}
		for (String p : elements) {
			add(p);
		}
	}

	/** Replies if this signature has a variatic parameter.
	 *
	 * @return <code>true</code> if the last element is variatic.
	 */
	public boolean isVarargs() {
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
	public SignatureKey clone() {
		return (SignatureKey) super.clone();
	}

	@Override
	public String toString() {
		if (!isEmpty()) {
			StringBuilder b = new StringBuilder();
			int size = size() - 1;
			for (int i = 0; i < size; ++i) {
				if (i > 0) {
					b.append(","); //$NON-NLS-1$
				}
				b.append(get(i));
			}
			String lastElement = get(size);
			if (isVarargs()) {
				lastElement = lastElement.replaceFirst("\\[\\]$", "*");  //$NON-NLS-1$//$NON-NLS-2$
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
	public int compareTo(SignatureKey o) {
		if (o == null) {
			return Integer.MAX_VALUE;
		}
		int cmp = Integer.compare(size(), o.size());
		if (cmp != 0) {
			return cmp;
		}
		Iterator<String> i1 = iterator();
		Iterator<String> i2 = o.iterator();
		String s1;
		String s2;
		while (i1.hasNext() && i2.hasNext()) {
			s1 = i1.next();
			s2 = i2.next();
			cmp = s1.compareTo(s2);
			if (cmp != 0) {
				return cmp;
			}
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
