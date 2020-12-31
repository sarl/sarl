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

import org.eclipse.xtext.common.types.JvmVisibility;

/**
 * Comparator of JvmVisility according to the visility level.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JvmVisibilityComparator implements Comparator<JvmVisibility>, Serializable {
	private static final long serialVersionUID = 2651322953936550928L;

	private static int getVisibilityLevel(JvmVisibility visibility) {
		switch (visibility) {
		case PRIVATE:
			return 0;
		case DEFAULT:
			return 1;
		case PROTECTED:
			return 2;
		case PUBLIC:
			return 3;
		default:
			return 1;
		}
	}

	@Override
	public int compare(JvmVisibility o1, JvmVisibility o2) {
		if (o1 == o2) {
			return 0;
		}
		if (o1 == null) {
			return Integer.MIN_VALUE;
		}
		if (o2 == null) {
			return Integer.MAX_VALUE;
		}
		return getVisibilityLevel(o1) - getVisibilityLevel(o2);
	}

}
