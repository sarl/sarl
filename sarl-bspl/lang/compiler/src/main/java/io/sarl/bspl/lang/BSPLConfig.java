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

	/** Name of the root folder for BSPL files.
	 */
	public static final String BSPL_FOLDER_SIMPLENAME = "bspl"; //$NON-NLS-1$

	/** Name of the root folder for SARL files.
	 */
	public static final String SARL_FOLDER_SIMPLENAME = "sarl"; //$NON-NLS-1$

	/** Path of the source files within a Maven project.
	 */
	public static final String FOLDER_MAVEN_SRC_PREFIX = "src"; //$NON-NLS-1$

	/** Name of the root folder for main source files.
	 */
	public static final String MAIN_FOLDER_SIMPLENAME = "main"; //$NON-NLS-1$

	/** Name of the root folder for test files.
	 */
	public static final String TEST_FOLDER_SIMPLENAME = "test"; //$NON-NLS-1$

	/** Name of the root folder for all the generated files by a SARL compiler.
	 *
	 * @since 0.15
	 */
	public static final String GENERATED_SOURCE_ROOT_FOLDER_SIMPLENAME = "generated-sources"; //$NON-NLS-1$

	/** Path of the main source files within a Maven project.
	 */
	public static final String FOLDER_MAVEN_MAIN_PREFIX = FOLDER_MAVEN_SRC_PREFIX + "/" + MAIN_FOLDER_SIMPLENAME; //$NON-NLS-1$

	/** Path of the SARL-BSPL source files.
	 */
	public static final String FOLDER_SOURCE_BSPL = FOLDER_MAVEN_MAIN_PREFIX + "/" + BSPL_FOLDER_SIMPLENAME; //$NON-NLS-1$

	/** Path of the generated source files.
	 */
	public static final String FOLDER_SOURCE_GENERATED = FOLDER_MAVEN_MAIN_PREFIX + "/" + GENERATED_SOURCE_ROOT_FOLDER_SIMPLENAME + "/" + SARL_FOLDER_SIMPLENAME; //$NON-NLS-1$ //$NON-NLS-2$

	/** Path of the test source files within a Maven project.
	 */
	public static final String FOLDER_MAVEN_TEST_PREFIX = FOLDER_MAVEN_SRC_PREFIX + "/" + TEST_FOLDER_SIMPLENAME; //$NON-NLS-1$

	/** Path of the SARL-BSPL test source files.
	 */
	public static final String FOLDER_TEST_SOURCE_GENERATED = FOLDER_MAVEN_TEST_PREFIX + "/" + GENERATED_SOURCE_ROOT_FOLDER_SIMPLENAME + "/" + SARL_FOLDER_SIMPLENAME; //$NON-NLS-1$ //$NON-NLS-2$

	/** Name of the output configuration that is dedicated to tests.
	 */
	public static final String TEST_OUTPUT_CONFIGURATION = "DEFAULT_TEST_OUTPUT"; //$NON-NLS-1$

	private BSPLConfig() {
		//
	}

}
