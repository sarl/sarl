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

package io.janusproject;


/**
 * Describes the version of the Janus platform.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 2.0.5.0
 */
@SuppressWarnings("all")
public final class JanusVersion {

	/** The version number of the current release of the Janus platform.
	 */
	public static final String JANUS_RELEASE_VERSION = "2.0.9.0"; //$NON-NLS-1$

	/** Flag that indicates if the current Janus platform is a stable release.
	 *
	 * <p>A stable release is a platform that will be not more compiled and generated.
	 */
	public static final boolean IS_STABLE = false;

	private JanusVersion() {
		//
	}

}
