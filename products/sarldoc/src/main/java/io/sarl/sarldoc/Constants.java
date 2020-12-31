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

package io.sarl.sarldoc;

/** Constants for sarlc.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public final class Constants {

	/** Name of the default doclet class.
	 */
	public static final String DEFAULT_DOCLET = "io.sarl.docs.doclet.SarlDoclet"; //$NON-NLS-1$

	/** Default name of the sarldoc program.
	 */
	public static final String PROGRAM_NAME = "sarldoc"; //$NON-NLS-1$

	/** Name of the option for defining the output directory for HTML documentation, without the {@code -} and the {@code /} prefixes.
	 */
	public static final String DOCUMENTATION_OUTPUT_DIRECTORY_OPTION = "docdirectory"; //$NON-NLS-1$

	private Constants() {
		//
	}

}
