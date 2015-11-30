/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.maven.docs.generator;


/** Configuration flags from Maven.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
final class MavenConfig {

	private static boolean isSectionNumbering;

	private MavenConfig() {
		//
	}

	/** Replies if the section are automatically numbered.
	 *
	 * @return <code>true</code> if sections are numbered.
	 */
	public static boolean isSectionNumbering() {
		return isSectionNumbering;
	}

	/** Set if the section are automatically numbered.
	 *
	 * @param enable - <code>true</code> if sections are numbered.
	 */
	public static void setSectionNumbering(boolean enable) {
		isSectionNumbering = enable;
	}

}
