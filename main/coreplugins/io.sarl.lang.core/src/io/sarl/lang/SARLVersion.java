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
 * Describes the specification of the SARL language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.3.0
 */
@SuppressWarnings("all")
public final class SARLVersion {

	/** Version number of the SARL specification.
	 *
	 * <p>The version number is usually composed of two digits that
	 * represent the version of the SARL specification and tools.
	 */
	public static final String SPECIFICATION_RELEASE_VERSION_STRING = "0.13"; //$NON-NLS-1$

	/** The version number of the current release of the SARL library.
	 *
	 * <p>Usually, the two first digits are the same as the ones of {@link #SPECIFICATION_RELEASE_VERSION_STRING}.
	 *
	 * @see #SARL_RELEASE_VERSION_OSGI
	 * @see #SARL_RELEASE_VERSION_MAVEN
	 */
	public static final String SARL_RELEASE_VERSION = "0.13.0"; //$NON-NLS-1$

	/** The version number in OSGI format of the current release of the SARL library.
	 *
	 * <p>Usually, the two first digits are the same as the ones of {@link #SPECIFICATION_RELEASE_VERSION_STRING}.
	 *
	 * @see #SARL_RELEASE_VERSION
	 * @see #SARL_RELEASE_VERSION_MAVEN
	 */
	public static final String SARL_RELEASE_VERSION_OSGI = "0.13.0.qualifier"; //$NON-NLS-1$

	/** The version number in Maven format of the current release of the SARL library.
	 *
	 * <p>Usually, the two first digits are the same as the ones of {@link #SPECIFICATION_RELEASE_VERSION_STRING}.
	 *
	 * @see #SARL_RELEASE_VERSION
	 * @see #SARL_RELEASE_VERSION_OSGI
	 */
	public static final String SARL_RELEASE_VERSION_MAVEN = "0.13.0-SNAPSHOT"; //$NON-NLS-1$

	/** The status of the SARL specification.
	 *
	 * <p>Usually, this status is also displayed in the
	 * <a href="http://www.sarl.io/docs/suite/io/sarl/docs/SARLDocumentationSuite.html">reference documentation of SARL</a>.
	 */
	public static final String RELEASE_STATUS = "Draft Release"; //$NON-NLS-1$

	/** Flag that indicates if the current SARL library is a stable release.
	 *
	 * <p>A stable release is collection of libraries that will be not more compiled and generated.
	 */
	public static final boolean IS_STABLE = false;

	/** The minimal Xtext version to use SARL features.
	 */
	public static final String MINIMAL_XTEXT_VERSION = "2.25.0"; //$NON-NLS-1$

	/** The minimal version of the JDK that must be used for running the SARL compilation tools (IDE, or sarlc).
	 *
	 * @since 0.10
	 */
	public static final String MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT = "11"; //$NON-NLS-1$

	/** The first incompatible version of the JDK that must NOT be used for running the SARL compilation tools (IDE, or sarlc).
	 *
	 * @since 0.10
	 */
	public static final String INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT = "15"; //$NON-NLS-1$

	/** The minimal version of the JDK that must be used on the classpath of SARL projects.
	 *
	 * @since 0.10
	 */
	public static final String MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH = "11"; //$NON-NLS-1$

	/** The first incompatible version of the JDK that must NOT be used on the classpath of SARL projects.
	 *
	 * @since 0.10
	 */
	public static final String INCOMPATIBLE_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH = "15"; //$NON-NLS-1$

	private SARLVersion() {
		//
	}

}
