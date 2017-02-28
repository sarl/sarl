/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import com.google.inject.ImplementedBy;

import io.sarl.lang.SARLVersion;

/** Check if a given agent class follows a specific version of the SARL specifications.
 *
 * @author $Author: ssgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
@ImplementedBy(StandardSarlSpecificationChecker.class)
public interface SarlSpecificationChecker {

	/** Replies the supported SARL specification version of the given type.
	 *
	 * @param type the type from which the SARL specification version should be extracted.
	 * @return The SARL specification version. The version is a floating-point number, as for
	 *     {@link SARLVersion#SPECIFICATION_RELEASE_VERSION}. The value {@link Float#NaN} is replied
	 *     if the given type has no marker related to the SARL specification version.
	 */
	float getSarlSpecificationVersion(Class<?> type);

	/** Compare the SARL specification version associated to the given type to the version of the
	 * current SARL.
	 *
	 * @param type the type to test.
	 * @return a negative integer value if the type's version is lower than the version of the current SARL,
	 *     zero if the two versions are equal, a positive integer value if type's version is greater
	 *     than the version of the current SARL.
	 */
	default int compareToSarlSpecificationVersion(Class<?> type) {
		return Float.compare(getSarlSpecificationVersion(type), SARLVersion.SPECIFICATION_RELEASE_VERSION);
	}

	/** Replies if the given type is a SARL element that is following the specification of the current SARL version.
	 *
	 * @param type the type to test.
	 * @return <code>true</code> if the given type follows the specification of the current version.
	 */
	default boolean isValidSarlElement(Class<?> type) {
		// TODO: Unless we have reach the version 1.0, all the versions are incompatible.
		return getSarlSpecificationVersion(type) == SARLVersion.SPECIFICATION_RELEASE_VERSION;
	}

}
