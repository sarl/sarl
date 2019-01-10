/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.janusproject.util;

import java.util.Comparator;

/**
 * Comparators.
 *
 * @author $Author: galland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class Comparators {

	private Comparators() {
		//
	}

	/** Comparator of classes.
	 */
	@SuppressWarnings("checkstyle:all")
	public static final Comparator<Class<?>> CLASS_COMPARATOR = (o1, o2) -> {
		if (o1 == o2) {
			return 0;
		}
		if (o1 == null) {
			return Integer.MIN_VALUE;
		}
		if (o2 == null) {
			return Integer.MAX_VALUE;
		}
		final String n1 = o1.getCanonicalName();
		final String n2 = o2.getCanonicalName();
		if (n1 == n2) {
			return 0;
		}
		if (n1 == null) {
			return Integer.MIN_VALUE;
		}
		if (n2 == null) {
			return Integer.MAX_VALUE;
		}
		return n1.compareTo(n2);
	};

	/** Comparator of objects.
	 */
	@SuppressWarnings("checkstyle:all")
	public static final Comparator<Object> OBJECT_COMPARATOR = (o1, o2) -> {
		if (o1 == o2) {
			return 0;
		}
		if (o1 == null) {
			return Integer.MIN_VALUE;
		}
		if (o2 == null) {
			return Integer.MAX_VALUE;
		}
		final int id1 = System.identityHashCode(o1);
		final int id2 = System.identityHashCode(o2);
		return Integer.compare(id1, id2);
	};

}
