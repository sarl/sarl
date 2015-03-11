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
package io.sarl.m2e;

import org.apache.maven.artifact.ArtifactUtils;
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

	private M2EUtilities() {
		//
	}

	/** Maven version parser.
	 *
	 * @param version - the version string.
	 * @return the version.
	 */
	public static Version parseMavenVersion(String version) {
		boolean isSnapshot = ArtifactUtils.isSnapshot(version);
		String[] parts = version.split("[.]"); //$NON-NLS-1$
		int minor = 0;
		int micro = 0;
		int major = Integer.parseInt(parts[0]);
		if (parts.length > 1) {
			minor = Integer.parseInt(parts[1]);
			if (parts.length > 1) {
				if (isSnapshot) {
					parts[2] = parts[2].replaceFirst("\\-.+$", ""); //$NON-NLS-1$//$NON-NLS-2$
				}
				micro = Integer.parseInt(parts[2]);
			}
		}
		if (isSnapshot) {
			return new Version(major, minor, micro, "qualifier"); //$NON-NLS-1$
		}
		return new Version(major, minor, micro);
	}

}
