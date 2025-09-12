/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.ui;

/**
 * Provides the constants for the SARL projects.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SARLUiConfig {

	/** Namespace for the extension points.
	 */
	public static final String NAMESPACE = "io.sarl.lang.ui"; //$NON-NLS-1$

	/** Extension point for extra language generators.
	 */
	public static final String EXTENSION_POINT_EXTRA_LANGUAGE_GENERATORS = "extraGenerators"; //$NON-NLS-1$

	private SARLUiConfig() {
		//
	}

}
