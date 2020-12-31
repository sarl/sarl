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

package io.sarl.lang;

/**
 * Provides the constants for the SARL projects.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SARLConfig {

	/** Path of the main source files within a Maven project.
	 * @since 0.8
	 */
	public static final String FOLDER_MAVEN_MAIN_PREFIX = "src/main"; //$NON-NLS-1$

	/** Path of the Java source files.
	 */
	public static final String FOLDER_SOURCE_JAVA = FOLDER_MAVEN_MAIN_PREFIX + "/java"; //$NON-NLS-1$

	/** Path of the SARL source files.
	 */
	public static final String FOLDER_SOURCE_SARL = FOLDER_MAVEN_MAIN_PREFIX + "/sarl"; //$NON-NLS-1$

	/** Path of the generated source files.
	 */
	public static final String FOLDER_SOURCE_GENERATED = FOLDER_MAVEN_MAIN_PREFIX + "/generated-sources/sarl"; //$NON-NLS-1$

	/** Path of the test source files within a Maven project.
	 * @since 0.8
	 */
	public static final String FOLDER_MAVEN_TEST_PREFIX = "src/test"; //$NON-NLS-1$

	/** Path of the Java source files.
	 */
	public static final String FOLDER_TEST_SOURCE_SARL = FOLDER_MAVEN_TEST_PREFIX + "/sarl"; //$NON-NLS-1$

	/** Path of the SARL source files.
	 */
	public static final String FOLDER_TEST_SOURCE_GENERATED = FOLDER_MAVEN_TEST_PREFIX + "/generated-sources/sarl"; //$NON-NLS-1$

	/** Name of the output configuration that is dedicated to tests.
	 *
	 * @since 0.8
	 */
	public static final String TEST_OUTPUT_CONFIGURATION = "DEFAULT_TEST_OUTPUT"; //$NON-NLS-1$

	/** Path of the generated source files that should be no more used when creating
	 * new projects. This value is the default generation folder form Xtext.
	 */
	public static final String FOLDER_SOURCE_GENERATED_XTEXT = "src-gen"; //$NON-NLS-1$

	/** Path of the resource files.
	 */
	public static final String FOLDER_RESOURCES = "src/main/resources"; //$NON-NLS-1$

	/** Path of the binary files.
	 */
	public static final String FOLDER_BIN = "target/classes"; //$NON-NLS-1$

	/** Path of the binary test files.
	 */
	public static final String FOLDER_TEST_BIN = "target/test-classes"; //$NON-NLS-1$

	/** Path of the temporary files.
	 */
	public static final String FOLDER_TMP = "target/sarl-build"; //$NON-NLS-1$

	/** URL of the Javadoc for SARL API.
	 */
	public static final String JAVADOC_URL = "http://www.sarl.io/docs/api/"; //$NON-NLS-1$

	private SARLConfig() {
		//
	}

}
