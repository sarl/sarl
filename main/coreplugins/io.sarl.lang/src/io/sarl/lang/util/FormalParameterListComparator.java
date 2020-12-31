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

package io.sarl.lang.util;

import java.io.Serializable;
import java.util.Comparator;
import java.util.Iterator;

import org.eclipse.emf.common.util.EList;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * Comparator of lists of formal parameters.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class FormalParameterListComparator implements Comparator<EList<? extends XtendParameter>>, Serializable {
	private static final long serialVersionUID = 1565477564314061872L;

	@Override
	public int compare(EList<? extends XtendParameter> left, EList<? extends XtendParameter> right) {
		if (left == right) {
			return 0;
		}
		if (left == null) {
			return Integer.MIN_VALUE;
		}
		if (right == null) {
			return Integer.MAX_VALUE;
		}
		int cmp = Integer.compare(left.size(), right.size());
		if (cmp == 0) {
			final Iterator<? extends XtendParameter> i1 = left.iterator();
			final Iterator<? extends XtendParameter> i2 = right.iterator();
			while (cmp == 0 && i1.hasNext() && i2.hasNext()) {
				cmp = compare(i1.next(), i2.next());
			}
		}
		return cmp;
	}

	/** Compare the two formal parameters.
	 *
	 * @param p1 the first parameter to compare.
	 * @param p2 the second parameter to compare.
	 * @return A negative value if <code>p1</code> is
	 *     lower than <code>p2</code>, a positive value if
	 *     <code>p1</code> is greater than <code>p2</code>,
	 *     otherwise <code>0</code>.
	 */
	public static int compare(XtendParameter p1, XtendParameter p2) {
		if (p1 != p2) {
			if (p1 == null) {
				return Integer.MIN_VALUE;
			}
			if (p2 == null) {
				return Integer.MAX_VALUE;
			}
			final JvmTypeReference t1 = p1.getParameterType();
			final JvmTypeReference t2 = p2.getParameterType();
			if (t1 != t2) {
				final int cmp;
				if (t1 == null) {
					cmp = Integer.MIN_VALUE;
				} else if (t2 == null) {
					cmp = Integer.MAX_VALUE;
				} else {
					cmp = t1.getIdentifier().compareTo(t2.getIdentifier());
				}
				if (cmp != 0) {
					return cmp;
				}
			}
		}
		return 0;
	}

}
