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
package io.sarl.lang.util;

import io.sarl.lang.sarl.BehaviorUnit;

import java.util.Comparator;

import org.eclipse.xtext.xbase.XExpression;

/**
 * Comparator of BehaviorUnit.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BehaviorUnitComparator implements Comparator<BehaviorUnit> {

	/** Construct a comparator of behavior units.
	 */
	public BehaviorUnitComparator() {
		//
	}

	@Override
	public int compare(BehaviorUnit o1, BehaviorUnit o2) {
		if (o1 == o2) {
			return 0;
		}
		if (o1 == null) {
			return Integer.MIN_VALUE;
		}
		if (o2 == null) {
			return Integer.MAX_VALUE;
		}
		String n1 = o1.getName().getIdentifier();
		String n2 = o2.getName().getIdentifier();
		int cmp = n1.compareTo(n2);
		if (cmp != 0) {
			return cmp;
		}
		return compare(o1.getGuard(), o2.getGuard());
	}

	/** Compare two Xtext expressions.
	 *
	 * @param e1 - the first expression to compare.
	 * @param e2 - the second expression to compare.
	 * @return A negative value if <code>e1</code> is
	 * lower than <code>e2</code>, a positive value if
	 * <code>e1</code> is greater than <code>e2</code>,
	 * otherwise <code>0</code>.
	 */
	public static int compare(XExpression e1, XExpression e2) {
		if (e1 == e2) {
			return 0;
		}
		if (e1 == null) {
			return Integer.MIN_VALUE;
		}
		if (e2 == null) {
			return Integer.MAX_VALUE;
		}
		return e1.toString().compareTo(e2.toString());
	}

}
