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
package io.sarl.eclipse.util;

import org.osgi.framework.Version;

import com.google.common.base.Strings;


/** Utilities.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class Utilities {

	/** Empty string.
	 */
	public static final String EMPTY_STRING = ""; //$NON-NLS-1$

	/**
	 */
	private Utilities() {
		//
	}

	/** Null-safe version parser.
	 *
	 * @param version - the version string.
	 * @return the version.
	 */
	public static Version parseVersion(String version) {
		if (!Strings.isNullOrEmpty(version)) {
			try {
				return Version.parseVersion(version);
			} catch (Throwable _) {
				//
			}
		}
		return null;
	}

	/** Null-safe compare a version number to a range of version numbers.
	 *
	 * The minVersion must be strictly lower to the maxVersion. Otherwise
	 * the behavior is not predictible.
	 *
	 * @param version - the version to compare to the range; must not be <code>null</code>.
	 * @param minVersion - the minimal version in the range (inclusive); could be <code>null</code>.
	 * @param maxVersion - the maximal version in the range (exclusive); could be <code>null</code>.
	 * @return a negative number if the version in lower than the minVersion.
	 * A positive number if the version is greater than or equal to the maxVersion.
	 * <code>0</code> if the version is between minVersion and maxVersion.
	 */
	public static int compareVersionToRange(Version version, Version minVersion, Version maxVersion) {
		assert (minVersion == null || maxVersion == null || minVersion.compareTo(maxVersion) < 0);
		if (version == null) {
			return Integer.MIN_VALUE;
		}
		if (minVersion != null && compareVersionsNoQualifier(version, minVersion) < 0) {
			return -1;
		}
		if (maxVersion != null && compareVersionsNoQualifier(version, maxVersion) >= 0) {
			return 1;
		}
		return 0;
	}

	private static int compareVersionsNoQualifier(Version a, Version b) {
		if (a == b) {
			return 0;
		}

		int result = a.getMajor() - b.getMajor();
		if (result != 0) {
			return result;
		}

		result = a.getMinor() - b.getMinor();
		if (result != 0) {
			return result;
		}

		return a.getMicro() - b.getMicro();
	}

	/** Null-safe comparison.
	 *
	 * @param <T> - type of the comparable element.
	 * @param a - the first object.
	 * @param b - the second object.
	 * @return Negative number if a lower than b.
	 * Positive number if a greater than b.
	 * <code>0</code> if a is equal to b.
	 */
	public static <T> int compareTo(Comparable<T> a, T b) {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		assert (a != null && b != null);
		return a.compareTo(b);
	}

}
