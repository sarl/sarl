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

import io.sarl.lang.sarl.SarlAction;

/**
 * Comparator of ActionSignature.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ActionComparator implements Comparator<SarlAction>, Serializable {
	private static final long serialVersionUID = 6693376551313660666L;

	private final FormalParameterListComparator comparator = new FormalParameterListComparator();

	@Override
	public int compare(SarlAction left, SarlAction right) {
		if (left == right) {
			return 0;
		}
		if (left == null) {
			return Integer.MIN_VALUE;
		}
		if (right == null) {
			return Integer.MAX_VALUE;
		}
		return this.comparator.compare(left.getParameters(), right.getParameters());
	}

}
