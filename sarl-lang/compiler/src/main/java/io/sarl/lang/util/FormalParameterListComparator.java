/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import org.eclipse.emf.common.util.EList;
import org.eclipse.xtend.core.xtend.XtendParameter;

/**
 * Comparator of lists of formal parameters.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
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
		var cmp = Integer.compare(left.size(), right.size());
		if (cmp == 0) {
			final var i1 = left.iterator();
			final var i2 = right.iterator();
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
	 * @return A negative value if {@code p1} is
	 *     lower than {@code p2}, a positive value if
	 *     {@code p1} is greater than {@code p2},
	 *     otherwise {@code 0}.
	 */
	public static int compare(XtendParameter p1, XtendParameter p2) {
		if (p1 != p2) {
			if (p1 == null) {
				return Integer.MIN_VALUE;
			}
			if (p2 == null) {
				return Integer.MAX_VALUE;
			}
			final var t1 = p1.getParameterType();
			final var t2 = p2.getParameterType();
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
