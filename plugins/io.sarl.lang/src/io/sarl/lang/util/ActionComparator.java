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

import io.sarl.lang.sarl.SarlAction;

import java.util.Comparator;

/**
 * Comparator of ActionSignature.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ActionComparator implements Comparator<SarlAction> {

	private final FormalParameterListComparator comparator = new FormalParameterListComparator();

	/** Construct a comparator of action signatures.
	 */
	public ActionComparator() {
		//
	}

	@Override
	public int compare(SarlAction a, SarlAction b) {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		return this.comparator.compare(a.getParameters(), b.getParameters());
	}

}
