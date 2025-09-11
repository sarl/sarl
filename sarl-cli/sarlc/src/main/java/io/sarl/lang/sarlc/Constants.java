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

package io.sarl.lang.sarlc;

import javax.annotation.processing.Generated;

/** Constants for sarlc.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version sarlc 0.15.1 20250911-224827
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarlc
 * @since 0.8
 */
public final class Constants {

	/** Default name of the sarlc program.
	 *
	 * <p>This constant is automatically updated by the Maven compilation processs. DO NOT CHANGE IT MANUALLY.
	 */
	@Generated(value = "maven")
	public static final String PROGRAM_NAME = "sarlc"; //$NON-NLS-1$

	/** Name of the option for defining the output directory from SARL to Java, without the {@code -} and the {@code /} prefixes.
	 */
	public static final String SARL_OUTPUT_DIRECTORY_OPTION = "directory"; //$NON-NLS-1$

	/** Name of the option for defining the output directory for the Java class files, without the {@code -} and the {@code /} prefixes.
	 */
	public static final String JAVA_OUTPUT_DIRECTORY_OPTION = "outputdir"; //$NON-NLS-1$

	private Constants() {
		//
	}

}
