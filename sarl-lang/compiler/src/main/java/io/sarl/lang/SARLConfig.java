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

package io.sarl.lang;

import org.eclipse.xtext.util.JavaVersion;

import io.sarl.lang.core.SARLVersion;

/**
 * Provides the constants for the SARL projects.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SARLConfig {

	/**
	 * Default SARL file extension (with the dot).
	 *
	 * <p>TODO Find a way to get it from the io.sarl.lang code itself (which is generated).
	 *
	 * @since 0.15
	 */
	public static final String SARL_FILE_EXTENSION = ".sarl"; //$NON-NLS-1$

	/**
	 * Default SARL file extension (with the dot).
	 *
	 * <p>TODO Find a way to get it from the io.sarl.lang code itself (which is generated).
	 *
	 * @since 0.15
	 */
	public static final String[] SARL_FILE_EXTENSIONS = { SARL_FILE_EXTENSION, ".Sarl", ".SARL" }; //$NON-NLS-1$ //$NON-NLS-2$

	/** Path of the source files within a Maven project.
	 * @since 0.15
	 */
	public static final String FOLDER_MAVEN_SRC_PREFIX = "src"; //$NON-NLS-1$

	/** Name of the root folder for main source files.
	 *
	 * @since 0.15
	 */
	public static final String MAIN_FOLDER_SIMPLENAME = "main"; //$NON-NLS-1$

	/** Name of the root folder for SARL files.
	 *
	 * @since 0.15
	 */
	public static final String SARL_FOLDER_SIMPLENAME = "sarl"; //$NON-NLS-1$

	/** Name of the root folder for test files.
	 *
	 * @since 0.15
	 */
	public static final String TEST_FOLDER_SIMPLENAME = "test"; //$NON-NLS-1$

	/** Name of the root folder for integration test files.
	 *
	 * @since 0.15
	 */
	public static final String INTEGRATION_TEST_FOLDER_SIMPLENAME = "it"; //$NON-NLS-1$

	/** Name of the root folder for all the generated files by a SARL compiler.
	 *
	 * @since 0.15
	 */
	public static final String GENERATED_SOURCE_ROOT_FOLDER_SIMPLENAME = "generated-sources"; //$NON-NLS-1$

	/** Name of the root folder for resources.
	 *
	 * @since 0.15
	 */
	public static final String RESOURCE_FOLDER_SIMPLENAME = "resources"; //$NON-NLS-1$

	/** Path of the main source files within a Maven project.
	 * @since 0.8
	 */
	public static final String FOLDER_MAVEN_MAIN_PREFIX = FOLDER_MAVEN_SRC_PREFIX + "/" + MAIN_FOLDER_SIMPLENAME; //$NON-NLS-1$

	/** Path of the Java source files.
	 */
	public static final String FOLDER_SOURCE_JAVA = FOLDER_MAVEN_MAIN_PREFIX + "/java"; //$NON-NLS-1$

	/** Path of the SARL source files.
	 */
	public static final String FOLDER_SOURCE_SARL = FOLDER_MAVEN_MAIN_PREFIX + "/" + SARL_FOLDER_SIMPLENAME; //$NON-NLS-1$

	/** Path of the generated source files.
	 */
	public static final String FOLDER_SOURCE_GENERATED = FOLDER_MAVEN_MAIN_PREFIX + "/" + GENERATED_SOURCE_ROOT_FOLDER_SIMPLENAME + "/" + SARL_FOLDER_SIMPLENAME; //$NON-NLS-1$ //$NON-NLS-2$

	/** Path of the test source files within a Maven project.
	 * @since 0.8
	 */
	public static final String FOLDER_MAVEN_TEST_PREFIX = FOLDER_MAVEN_SRC_PREFIX + "/" + TEST_FOLDER_SIMPLENAME; //$NON-NLS-1$

	/** Path of the test Java source files.
	 */
	public static final String FOLDER_TEST_SOURCE_SARL = FOLDER_MAVEN_TEST_PREFIX + "/" + SARL_FOLDER_SIMPLENAME; //$NON-NLS-1$

	/** Path of the integration test source files within a Maven project.
	 * @since 0.13
	 */
	public static final String FOLDER_MAVEN_INTEGRATION_TEST_PREFIX = FOLDER_MAVEN_SRC_PREFIX + "/" + INTEGRATION_TEST_FOLDER_SIMPLENAME; //$NON-NLS-1$

	/** Path of the integration test's Java source files.
	 * @since 0.13
	 */
	public static final String FOLDER_INTEGRATION_TEST_SOURCE_SARL = FOLDER_MAVEN_INTEGRATION_TEST_PREFIX + "/" + SARL_FOLDER_SIMPLENAME; //$NON-NLS-1$

	/** Path of the SARL source files.
	 */
	public static final String FOLDER_TEST_SOURCE_GENERATED = FOLDER_MAVEN_TEST_PREFIX + "/" + GENERATED_SOURCE_ROOT_FOLDER_SIMPLENAME + "/" + SARL_FOLDER_SIMPLENAME; //$NON-NLS-1$ //$NON-NLS-2$

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
	public static final String FOLDER_RESOURCES = FOLDER_MAVEN_SRC_PREFIX + "/" + MAIN_FOLDER_SIMPLENAME + "/" + RESOURCE_FOLDER_SIMPLENAME; //$NON-NLS-1$ //$NON-NLS-2$

	/** Path of the test resource files.
	 *
	 * @since 0.13
	 */
	public static final String FOLDER_TEST_RESOURCES = FOLDER_MAVEN_SRC_PREFIX + "/" + TEST_FOLDER_SIMPLENAME + "/" + RESOURCE_FOLDER_SIMPLENAME; //$NON-NLS-1$ //$NON-NLS-2$

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

	/** Replies the minimal version of the JDK that must be used for running the SARL compilation
	 * tools (IDE, or sarlc), e.g. {@code 17}.
	 *
	 * @return the version.
	 * @since 0.14
	 */
	public static JavaVersion getMinimumJavaVersionForCompilation() {
		return JavaVersion.fromQualifier(SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
	}

	/** Replies the minimal version of the JDK that must be used on the classpath of SARL projects, e.g. {@code 17}.
	 *
	 * @return the version.
	 * @since 0.14
	 */
	public static JavaVersion getMinimumJavaVersionForRunning() {
		return JavaVersion.fromQualifier(SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
	}

}
