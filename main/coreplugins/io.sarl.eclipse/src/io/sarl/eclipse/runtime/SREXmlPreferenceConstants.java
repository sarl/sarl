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

package io.sarl.eclipse.runtime;

/**
 * Constants that are representing elements to be saved into the XML preferences that are associated to the SRE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public final class SREXmlPreferenceConstants {

	/** Attribute name of the SRE's library path (the jar file).
	 */
	public static final String XML_LIBRARY_PATH = "libraryPath"; //$NON-NLS-1$

	/** Attribute name of the SRE's name.
	 */
	public static final String XML_SRE_NAME = "name"; //$NON-NLS-1$

	/** Attribute name of the SRE's main class.
	 */
	public static final String XML_MAIN_CLASS = "mainClass"; //$NON-NLS-1$

	/** Attribute name of the SRE's bootstrap.
	 */
	public static final String XML_BOOTSTRAP = "bootstrap"; //$NON-NLS-1$

	/** Node name of a library used by the SRE.
	 */
	public static final String XML_LIBRARY_LOCATION = "libraryLocation"; //$NON-NLS-1$

	/** Attribute name of system path of the SRE's library.
	 */
	public static final String XML_SYSTEM_LIBRARY_PATH = "systemLibraryPath"; //$NON-NLS-1$

	/** Attribute name of package root path of the SRE's library.
	 */
	public static final String XML_PACKAGE_ROOT_PATH = "packageRootPath"; //$NON-NLS-1$

	/** Attribute name of source path of the SRE's library.
	 */
	public static final String XML_SOURCE_PATH = "sourcePath"; //$NON-NLS-1$

	/** Attribute name of javadoc path of the SRE's library.
	 */
	public static final String XML_JAVADOC_PATH = "javadoc"; //$NON-NLS-1$

	private SREXmlPreferenceConstants() {
		//
	}

}
