/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.sarlspecification;

import org.osgi.framework.Version;

import io.sarl.lang.annotation.SarlSpecification;

/** Check if a given agent class follows a specific version of the SARL specifications.
 *
 * @author $Author: ssgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public class StandardSarlSpecificationChecker implements SarlSpecificationChecker {

	@Override
	@SuppressWarnings("checkstyle:magicnumber")
	public float getSarlSpecificationVersion(Class<?> type) {
		if (type != null) {
			final SarlSpecification annotationInstance = type.getAnnotation(SarlSpecification.class);
			if (annotationInstance != null) {
				final String versionString = annotationInstance.value();
				final Version version = Version.parseVersion(versionString);
				if (!Version.emptyVersion.equals(version)) {
					int minor = version.getMinor();
					float factor = 1;
					while (minor > 0) {
						factor *= 10;
						minor /= 10;
					}
					return version.getMajor() + version.getMinor() / factor;
				}
			}
		}
		return Float.NaN;
	}

}
