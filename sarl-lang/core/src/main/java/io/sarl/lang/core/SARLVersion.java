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

package io.sarl.lang.core;

import javax.annotation.processing.Generated;

import org.eclipse.xtext.xbase.lib.XbaseGenerated;

/**
 * Describes the specification of the SARL language.
 *
 * <p>The constants in this file are automatically updated by the Maven compilation process. DO NOT CHANGE THEM MANUALLY.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.3.0
 */
@SuppressWarnings("all")
public final class SARLVersion {

	/** Version number of the SARL specification, e.g. {@code 0.13}.
	 *
	 * <p>The version number is usually composed of two digits that
	 * represent the version of the SARL specification and tools.
	 */
	@Generated(value = "maven")
	public static final String SPECIFICATION_RELEASE_VERSION_STRING = "0.15"; //$NON-NLS-1$

	/** The version number of the current release of the SARL library, e.g. {@code 0.13.0}.
	 *
	 * <p>Usually, the two first digits are the same as the ones of {@link #SPECIFICATION_RELEASE_VERSION_STRING}.
	 *
	 * @see #SARL_RELEASE_VERSION_OSGI
	 * @see #SARL_RELEASE_VERSION_MAVEN
	 */
	@Generated(value = "maven")
	public static final String SARL_RELEASE_VERSION = "0.15.0"; //$NON-NLS-1$

	/** The version number in OSGI format of the current release of the SARL library, e.g. {@code 0.13.0.qualifier}.
	 *
	 * <p>Usually, the two first digits are the same as the ones of {@link #SPECIFICATION_RELEASE_VERSION_STRING}.
	 *
	 * @see #SARL_RELEASE_VERSION
	 * @see #SARL_RELEASE_VERSION_MAVEN
	 */
	@Generated(value = "maven")
	public static final String SARL_RELEASE_VERSION_OSGI = "0.15.0.qualifier"; //$NON-NLS-1$

	/** The version number in Maven format of the current release of the SARL library, e.g. {@code 0.13.0-SNAPSHOT}.
	 *
	 * <p>Usually, the two first digits are the same as the ones of {@link #SPECIFICATION_RELEASE_VERSION_STRING}.
	 *
	 * @see #SARL_RELEASE_VERSION
	 * @see #SARL_RELEASE_VERSION_OSGI
	 */
	@Generated(value = "maven")
	public static final String SARL_RELEASE_VERSION_MAVEN = "0.15.0-SNAPSHOT"; //$NON-NLS-1$

	/** The status of the SARL specification, e.g. {@code Draft Release}.
	 *
	 * <p>Usually, this status is also displayed in the
	 * <a href="http://www.sarl.io/docs/suite/io/sarl/docs/SARLDocumentationSuite.html">reference documentation of SARL</a>.
	 */
	@Generated(value = "maven")
	public static final String RELEASE_STATUS = "Draft Release"; //$NON-NLS-1$

	/** Flag that indicates if the current SARL library is a stable release.
	 *
	 * <p>A stable release is collection of libraries that will be not more compiled and generated.
	 */
	@Generated(value = "maven")
	public static final boolean IS_STABLE = false;

	/** The minimal Xtext version to use SARL features, e.g. {@code 2.31.0}.
	 */
	@Generated(value = "maven")
	public static final String MINIMAL_XTEXT_VERSION = "2.40.0"; //$NON-NLS-1$

	/** The minimal version of the JDK that must be used for running the SARL compilation tools (IDE, or sarlc), e.g. {@code 17}.
	 *
	 * @since 0.10
	 */
	@Generated(value = "maven")
	public static final String MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT = "21"; //$NON-NLS-1$

	/** The first incompatible version of the JDK that must NOT be used for running the SARL compilation tools (IDE, or sarlc), e.g. {@code 18}.
	 *
	 * @since 0.10
	 */
	@Generated(value = "maven")
	public static final String INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT = "27"; //$NON-NLS-1$

	/** The minimal version of the JDK that must be used on the classpath of SARL projects, e.g. {@code 17}.
	 *
	 * @since 0.10
	 */
	@Generated(value = "maven")
	public static final String MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH = "21"; //$NON-NLS-1$

	/** The first incompatible version of the JDK that must NOT be used on the classpath of SARL projects, e.g. {@code 18}.
	 *
	 * @since 0.10
	 */
	@Generated(value = "maven")
	public static final String INCOMPATIBLE_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH = "27"; //$NON-NLS-1$

	private SARLVersion() {
		//
	}

}
