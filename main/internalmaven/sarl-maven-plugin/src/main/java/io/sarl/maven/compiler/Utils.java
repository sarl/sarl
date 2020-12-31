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

package io.sarl.maven.compiler;

import org.apache.maven.artifact.versioning.ArtifactVersion;

/** Utilities for the SARL maven plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public final class Utils {

	private Utils() {
		//
	}

	/** Replies the group id of the SARL maven plugin.
	 *
	 * @return the group identifier.
	 */
	public static String getSarlMavenPluginGroupId() {
		return Messages.Utils_0;
	}

	/** Replies the artifact id of the SARL maven plugin.
	 *
	 * @return the artifact identifier.
	 */
	public static String getSarlMavenPluginArtifactId() {
		return Messages.Utils_1;
	}

	/** Compare the major and minor components of the two given version numbers.
	 * The other components are ignored.
	 *
	 * @param first is the first version number.
	 * @param second is the second version number.
	 * @return A negative value if {@code first} is lower than {@code second}; a positive value if {@code first} is
	 *     greater than {@code second}; otherwise {@code 0} if {@code first} and {@code second} are equal.
	 * @since 0.10
	 */
	public static int compareMajorMinorVersions(final ArtifactVersion first, final ArtifactVersion second) {
		if (first == second) {
			return 0;
		}
		if (first == null) {
			return Integer.MIN_VALUE;
		}
		if (second == null) {
			return Integer.MAX_VALUE;
		}
		int na = first.getMajorVersion();
		int nb = first.getMajorVersion();
		final int cmp = na - nb;
		if (cmp != 0) {
			return cmp;
		}
		na = first.getMinorVersion();
		nb = first.getMinorVersion();
		return na - nb;
	}

}
