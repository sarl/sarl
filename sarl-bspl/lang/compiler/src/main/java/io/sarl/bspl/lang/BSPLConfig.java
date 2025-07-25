/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.bspl.lang;

/**
 * Provides the constants for the SARL-BSPL projects.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public final class BSPLConfig {

	/** Path of the main source files within a Maven project.
	 */
	public static final String FOLDER_MAVEN_MAIN_PREFIX = "src/main"; //$NON-NLS-1$

	/** Path of the SARL-BSPL source files.
	 */
	public static final String FOLDER_SOURCE_BSPL = FOLDER_MAVEN_MAIN_PREFIX + "/bspl"; //$NON-NLS-1$

	/** Path of the generated source files.
	 */
	public static final String FOLDER_SOURCE_GENERATED = FOLDER_MAVEN_MAIN_PREFIX + "/generated-sources/bspl"; //$NON-NLS-1$

	private BSPLConfig() {
		//
	}

}
