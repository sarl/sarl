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

package io.sarl.m2e.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import org.apache.maven.artifact.Artifact;
import org.osgi.framework.Version;

/**
 * M2E utilities.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class M2EUtilities {

	/** The qualifier string that is representing a snapshot version
	 * in the OGSi framework.
	 */
	public static final String SNAPSHOT_QUALIFIER = "qualifier"; //$NON-NLS-1$

	/** The pattern that is matching a SNAPSHOT version.
	 */
	public static final Pattern SNAPSHOT_VERSION_PATTERN = Pattern.compile("^(.*)" //$NON-NLS-1$
			+ Pattern.quote("-" + Artifact.SNAPSHOT_VERSION) //$NON-NLS-1$
			+ "$"); //$NON-NLS-1$

	private static final int MAX_NUMBER = 999999999;

	private M2EUtilities() {
		//
	}

	/** Compare two OSGI versions.
	 *
	 * @param v1 first OSGI version.
	 * @param v2 second OSGI version.
	 * @return an integer that the sign indicates if v1 is lower, equal ot greater than v2.
	 */
	public static int compareOsgiVersions(String v1, String v2) {
		return Version.parseVersion(v1).compareTo(Version.parseVersion(v2));
	}

	/** Compare two Maven versions.
	 *
	 * @param v1 first Maven version.
	 * @param v2 second Maven version.
	 * @return an integer that the sign indicates if v1 is lower, equal ot greater than v2.
	 */
	public static int compareMavenVersions(String v1, String v2) {
		return parseMavenVersion(v1).compareTo(parseMavenVersion(v2));
	}

	/** Maven version parser.
	 *
	 * @param version the version string.
	 * @return the version.
	 */
	public static Version parseMavenVersion(String version) {
		if (Strings.isNullOrEmpty(version)) {
			return new Version(0, 0, 0);
		}

		// Detect the snapshot
		final boolean isSnapshot;
		final String coreVersion;
		Matcher matcher = Artifact.VERSION_FILE_PATTERN.matcher(version);
		if (matcher.matches()) {
			coreVersion = matcher.group(1);
			isSnapshot = true;
		} else {
			matcher = SNAPSHOT_VERSION_PATTERN.matcher(version);
			if (matcher.matches()) {
				coreVersion = matcher.group(1);
				isSnapshot = true;
			} else {
				coreVersion = version;
				isSnapshot = false;
			}
		}

		// Parse the numbers
		final String[] parts = coreVersion.split("[.]"); //$NON-NLS-1$
		final int[] numbers = new int[] {0, 0, 0};
		int i = 0;
		while (i < numbers.length && i < parts.length) {
			try {
				numbers[i] = Integer.parseInt(parts[i]);
				++i;
			} catch (Exception exception) {
				// Force the exit of the loop since a number cannot be find.
				i = numbers.length;
			}
		}
		// Reply
		if (isSnapshot) {
			return new Version(numbers[0], numbers[1], numbers[2], SNAPSHOT_QUALIFIER);
		}
		return new Version(numbers[0], numbers[1], numbers[2]);
	}

	/** Compute the theoretic version just before the given one.
	 *
	 * @param vers the version.
	 * @return the previous version.
	 * @since 0.10
	 */
	@SuppressWarnings("checkstyle:magicnumber")
	public static String getPreviousOsgiVersion(String vers) {
		return getPreviousOsgiVersion(vers).toString();
	}

	/** Compute the theoretic version just before the given one.
	 *
	 * @param vers the version.
	 * @return the previous version.
	 * @since 0.10
	 */
	@SuppressWarnings("checkstyle:magicnumber")
	public static Version getPreviousOsgiVersion(Version vers) {
		int major = vers.getMajor();
		int minor = vers.getMinor();
		int micro = vers.getMicro();
		if (micro <= 0) {
			micro = MAX_NUMBER;
			--minor;
		}
		if (minor <= 0) {
			micro = MAX_NUMBER;
			minor = MAX_NUMBER;
			--major;
		}
		return new Version(major, minor, micro);
	}

}
