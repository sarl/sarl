/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.sarldoc;

import javax.annotation.processing.Generated;

/** Constants for sarlc.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version sarldoc 0.13.0 20230919-093100
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarldoc
 * @since 0.10
 */
public final class Constants {

	/** Default name of the sarldoc program.
	 *
	 * <p>This constant is automatically updated by the Maven compilation process. DO NOT CHANGE IT MANUALLY.
	 */
	@Generated(value = "maven")
	public static final String PROGRAM_NAME = "sarldoc"; //$NON-NLS-1$

	/** Name of the option for defining the output directory for HTML documentation, without the {@code -} and the {@code /} prefixes.
	 */
	public static final String DOCUMENTATION_OUTPUT_DIRECTORY_OPTION = "docdirectory"; //$NON-NLS-1$

	/** Path of the output folder for documentations.
	 */
	public static final String FOLDER_DOCUMENTATION = "target/sarl-api-docs"; //$NON-NLS-1$

	private Constants() {
		//
	}

}
