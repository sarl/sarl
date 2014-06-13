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

import io.sarl.lang.sarl.BehaviorUnit;

import java.util.Comparator;

import org.eclipse.xtext.xbase.XExpression;

/**
 * Comaprator of BehaviorUnit.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BehaviorUnitComparator implements Comparator<BehaviorUnit> {

	/**
	 */
	public BehaviorUnitComparator() {
		//
	}

	/** {@inheritDoc}
	 */
	@Override
	public int compare(BehaviorUnit o1, BehaviorUnit o2) {
		if (o1==o2) return 0;
		if (o1==null) return Integer.MIN_VALUE;
		if (o2==null) return Integer.MAX_VALUE;
		String n1 = o1.getEvent().getIdentifier();
		String n2 = o2.getEvent().getIdentifier();
		int cmp = n1.compareTo(n2);
		if (cmp!=0) return cmp;
		XExpression e1 = o1.getGuard();
		XExpression e2 = o2.getGuard();
		if (e1==e2) return 0;
		if (e1==null) return Integer.MIN_VALUE;
		if (e2==null) return Integer.MAX_VALUE;
		return e1.toString().compareTo(e2.toString());
	}
	
}
